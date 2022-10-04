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

;; ## API request paramater handling
;;
;; Maps collection values to pipe-separated strings and removes `nil` values.
;;
;; See https://www.mediawiki.org/wiki/API:Data_formats#Multivalue_parameters for
;; details.

(declare transform-params)

(defn transform-params-kv
  [[k v]]
  [k (cond
       ;; Recursion
       (map? v)
       (transform-params v)
       ;; Collection transform
       (coll? v)
       (if (some #(str/includes? % "|") v)
         (str/join "\u001f" (cons "" v))
         (str/join "|" v))
       ;; Default
       :else v)])

(def transform-params-xf
  (comp
   (filter (comp some? second))
   (map transform-params-kv)))

(defn transform-params
  [req]
  (into {} transform-params-xf req))

;; ## API request configuration

(defn build-http-client
  [config]
  (hc/build-http-client (merge {:connect-timeout 5000
                                :version         :http-2 #_:http-1.1}
                               config)))

(def default-http-client
  (build-http-client {}))

(defn base-request
  ([host]
   (base-request host default-http-client))
  ([host http-client]
   {:method        :post
    :action        "query"
    :format        "json"
    :formatversion "2"
    :errorformat   "plaintext"
    :maxlag       "5"
    :headers       {"accept" "application/json"}
    :user-agent    "julesratte/1.0 (https://github.com/gremid/julesratte)"
    :async?        true
    :http-client   http-client
    ::host         host
    ::max-retries  3
    ::max-requests 100}))

(defn session-base-request
  [host]
  (base-request host (build-http-client {:cookie-policy :all})))

(defn build-url
  [{::keys [scheme host path script extension]}]
  (str (or scheme "https")
       "://"
       host
       (or path "/w/")
       (or script "api")
       (or extension ".php")))

(def http-client-params
  #{:method :url :headers :async? :http-client :user-agent})

(def select-params-xf
  (remove (comp (some-fn namespace http-client-params) first)))

(defn select-params
  [req]
  (into {} select-params-xf req))

(defn build-request
  [req]
  (let [params-key (condp = (req :method)
                     :get  :query-params
                     :post :form-params
                     :body)
        params-map (-> req select-params transform-params)]
    (assoc req :url (build-url req) params-key params-map)))

(defn json-response?
  [{:keys [content-type]}]
  (= :application/json content-type))

(defn read-json
  [v]
  (json/read-value v json/keyword-keys-object-mapper))

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
  [errors]
  (some #{"maxlag" "ratelimited"} (map :code errors)))

(def get-request-params
  (some-fn :query-string :body))

(defn handle-response
  "Check for error/warnings in response and extract body."
  [{:keys [uri request] {:keys [errors warnings]} :body :as response}]
  (log/tracef "%s :: %s -> %s"
              uri (get-request-params request)
              (select-keys response [:request-time :status]))
  (let [retry-after (get-in response [:headers "retry-after"] "0")]
    (reset! request-delay (Integer/parseInt retry-after)))
  (cond
    (retry? errors)      (d/error-deferred f/retry)
    (or errors warnings) (d/error-deferred
                          (ex-info (response->error response) response))
    :else                response))

(defn handle-error
  [^Throwable e]
  (or
   (and (f/marker? e) e)
   (d/error-deferred (manifail.Aborted. e))))

(defn handle-abort
  [^manifail.Aborted e]
  (d/error-deferred (ex-cause e)))

(defn request
  [{::keys [max-retries] :as req}]
  (->
   (f/with-retries (f/retries max-retries)
     (->
      (delay-request)
      (d/chain (fn [_] (-> req build-request hc/request d/->deferred)))
      (d/chain parse-response handle-response)
      (d/catch handle-error)))
   (d/catch manifail.Aborted handle-abort)))

(defn requests
  "Stream of API requests/responses based on continuations."
  ([{::keys [max-requests] :as initial-req}]
   (let [responses (s/stream)]
     (d/loop [req initial-req remaining (dec max-requests)]
       (when-not (s/closed? responses)
         (-> (request req)
             (d/chain (fn [{{:keys [continue]} :body :as response}]
                        (s/put! responses response)
                        (if (and continue (pos? remaining))
                          (d/recur (merge req continue) (dec remaining))
                          (s/close! responses))))
             (d/catch' (fn [e]
                         (log/debugf e "Error during API request %s" req)
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

(defn build-info-request
  [req]
  (assoc req
         :action "query"
         :meta   #{"siteinfo" "userinfo"}
         :siprop #{"general" "namespaces"}
         :uiprop #{"groups" "rights"}))

(defn info
  "Retrieve information about a MediaWiki instance."
  [req]
  (-> req build-info-request request (d/chain parse-info)))
