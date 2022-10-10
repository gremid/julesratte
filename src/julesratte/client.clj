(ns julesratte.client
  "MediaWiki API client, ported from Python's `mwclient` lib."
  (:require
   [clojure.string :as str]
   [taoensso.timbre :as log]
   [hato.client :as hc]
   [jsonista.core :as json]
   [manifail :as f]
   [manifold.deferred :as d]
   [manifold.time]
   [manifold.stream :as s]))

;; ## API request parameter handling
;;
;; Maps collection values to pipe-separated strings and removes `nil` values.
;;
;; See https://www.mediawiki.org/wiki/API:Data_formats#Multivalue_parameters for
;; details.

(defn transform-params-kv
  [[k v]]
  [k (if (coll? v)
       (if (some #(str/includes? % "|") v)
         (str/join "\u001f" (cons "" v))
         (str/join "|" v))
       v)])

(def transform-params-xf
  (comp
   (filter (comp some? second))
   (map transform-params-kv)))

(defn transform-params
  [req]
  (into {} transform-params-xf req))

;; ## API request configuration

(def default-http-client
  (hc/build-http-client {:connect-timeout 5000
                         :version         :http-2}))

(defn create-session-client
  []
  (hc/build-http-client {:connect-timeout 5000
                         :version         :http-2
                         :cookie-policy   :all}))

(defn config-for-endpoint
  ([url]
   (config-for-endpoint url default-http-client))
  ([url http-client]
   {:http-client  http-client
    :url          url
    :max-retries  3
    :max-requests 100
    :warn->error? true}))

(defn endpoint-url
  ([host]
   (endpoint-url "https" host))
  ([scheme host]
   (endpoint-url scheme host "/w/api.php"))
  ([scheme host path]
   (str scheme "://" host path)))

(def user-agent
  "julesratte/1.0 (https://github.com/gremid/julesratte)")

(def base-request
  {:method      :post
   :headers     {"accept"     "application/json"
                 "user-agent" user-agent}
   :form-params {:action        "query"
                 :format        "json"
                 :formatversion "2"
                 :errorformat   "plaintext"
                 :maxlag        "5"}
   :async?      true})

(defn request-with-params
  [& params]
  (apply update base-request :form-params assoc params))

(defn json-response?
  [{:keys [content-type]}]
  (= :application/json content-type))

(defn read-json
  [v]
  (json/read-value v json/keyword-keys-object-mapper))

(defn write-json
  [v]
  (json/write-value-as-string v json/keyword-keys-object-mapper))

(defn parse-response
  [response]
  (try
    (cond-> response
      (json-response? response) (update :body read-json))
    (catch Throwable t
      (d/error-deferred
       (ex-info "Error parsing JSON response" response t)))))

(defn error->msg
  [{:keys [module code text]}]
  (str " * "
       (when module (format "[%s] " module))
       (when code (format "[%s] " code))
       text))

(defn response->error
  [{{:keys [errors warnings]} :body}]
  (str/join
   \newline
   (concat
    ["MediaWiki API error/warnings:" ""]
    (when errors (cons "Errors:" (map error->msg errors)))
    (when (and errors warnings) [""])
    (when warnings (cons "Warnings:" (map error->msg warnings))))))

(def request-delay
  (atom 0))

(defn delay-request
  []
  (let [request-delay' @request-delay]
    (if (pos? request-delay')
      (manifold.time/in (* request-delay' 1000) #(reset! request-delay 0))
      (d/success-deferred 0))))

(defn retry?
  [{{:keys [errors]} :body}]
  (some #{"maxlag" "ratelimited"} (map :code errors)))

(defn error?
  [{{::keys [warn->error?]}   :request
    {:keys [errors warnings]} :body}]
  (or errors (and warn->error? warnings)))

(defn handle-response
  "Check for error/warnings in response and extract body."
  [{:keys [uri request] :as response}]
  (log/tracef "%s :: %s -> %s"
              uri (get request :body)
              (select-keys response [:request-time :status]))
  (let [retry-after (get-in response [:headers "retry-after"] "0")]
    (reset! request-delay (Integer/parseInt retry-after)))
  (cond
    (retry? response) (d/error-deferred f/retry)
    (error? response) (d/error-deferred
                       (ex-info (response->error response) response))
    :else             response))

(defn handle-error
  [^Throwable e]
  (or
   (and (f/marker? e) e)
   (d/error-deferred (manifail.Aborted. e))))

(defn handle-abort
  [^manifail.Aborted e]
  (d/error-deferred (ex-cause e)))

(defn configure
  [{:keys [url http-client] :as _config} request]
  (->
   request
   (assoc :url url :http-client http-client)
   (update :form-params transform-params)))

(defn do-request!
  [config request]
  (d/->deferred (hc/request (configure config request))))

(defn request!
  [{:keys [max-retries] :as config} request]
  (->
   (f/with-retries (f/retries max-retries)
     (->
      (delay-request)
      (d/chain (fn [& _] (do-request! config request)))
      (d/chain parse-response handle-response)
      (d/catch handle-error)))
   (d/catch manifail.Aborted handle-abort)))

(defn requests!
  "Stream of API requests/responses based on continuations."
  ([{:keys [max-requests] :as config} initial-request]
   (let [responses (s/stream)]
     (d/loop [request initial-request remaining (dec max-requests)]
       (when-not (s/closed? responses)
         (-> (request! config request)
             (d/chain (fn [{{:keys [continue]} :body :as response}]
                        (s/put! responses response)
                        (if (and continue (pos? remaining))
                          (d/recur
                           (update request :form-params merge continue)
                           (dec remaining))
                          (s/close! responses))))
             (d/catch' (fn [e]
                         (log/debugf e "Error during API request %s" request)
                         (s/close! responses))))))
     responses)))

;; ## Retrieve information about a MediaWiki instance.

(defn- parse-version-num
  "Parses numeric version components."
  [v]
  (let [n (re-find #"\d+" v)]
    (if n (Integer/parseInt n) v)))

(defn- parse-version
  "Parses MediaWiki version strings."
  [version]
  (as-> version $
      (str/replace $ #"^MediaWiki\s*" "")
      (str/split $ #"\.")
      (map parse-version-num $)))

(defn parse-info
  [{{{ui :userinfo site :general :keys [namespaces]} :query} :body}]
  {:user       (some-> ui :name)
   :groups     (into (sorted-set) (some-> ui :groups))
   :rights     (into (sorted-set) (map keyword) (some-> ui :rights))
   :version    (some-> site :generator parse-version)
   :write-api? (-> site :writeapi some?)
   :site       site
   :namespaces (vals namespaces)})

(def info-request
  (request-with-params
   :action "query"
   :meta   #{"siteinfo" "userinfo"}
   :siprop #{"general" "namespaces"}
   :uiprop #{"groups" "rights"}))

(defn info
  "Retrieve information about a MediaWiki instance."
  [config]
  (d/chain (request! config info-request) parse-info))
