(ns julesratte.client
  "MediaWiki API client, ported from Python's `mwclient` lib."
  (:require
   [again.core :as again]
   [clojure.string :as str]
   [hato.client :as hc]
   [jsonista.core :as json]
   [taoensso.timbre :as log]))

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

(defn api-endpoint
  ([host]
   (api-endpoint "https" host))
  ([scheme host]
   (api-endpoint scheme host "/w/api.php"))
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
                 :maxlag        "5"}})

(def ^:dynamic *http-client*
  (hc/build-http-client {:connect-timeout 5000
                         :version         :http-2}))

(defn request-with-params
  [& params]
  (-> (apply update base-request :form-params assoc params)
      (update :form-params transform-params)
      (assoc :http-client *http-client*)))

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
      (throw (ex-info "Error parsing JSON response" response t)))))

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

(defn retry?
  [{{:keys [errors]} :body}]
  (some #{"maxlag" "ratelimited"} (map :code errors)))

(def ^:dynamic *warning->error?*
  false)

(defn error?
  [{{:keys [errors warnings]} :body}]
  (or errors (and *warning->error?* warnings)))

(def request-delay
  (atom 0))

(defn handle-response
  "Check for error/warnings in response and extract body."
  [{:keys [uri request] :as response}]
  (log/tracef "%s :: %s -> %s"
              uri (get request :body)
              (select-keys response [:request-time :status]))
  (let [retry-after (get-in response [:headers "retry-after"] "0")]
    (reset! request-delay (parse-long retry-after)))
  (cond
    (retry? response) (ex-info (response->error response)
                               (assoc response ::retry? true))
    (error? response) (ex-info (response->error response)
                               (assoc response ::retry? false))
    :else             response))

(def ^:dynamic *max-retries*
  3)

(defn request-retry-strategy
  []
  (->> (again/multiplicative-strategy 500 1.5)
       (again/randomize-strategy 0.5)
       (again/max-retries *max-retries*)
       (again/max-duration 10000)
       (cons @request-delay)))

(defn retry-request?
  [{::again/keys [status exception]}]
  (when (and (= status :retry) (some-> exception ex-data ::retry? false?))
    (log/warnf exception "Aborting request")
    ::again/fail))

(defn request!
  [request]
  (again/with-retries
    {::again/strategy (request-retry-strategy)
     ::again/callback retry-request?}
    (-> (hc/request request) (parse-response) (handle-response))))

(def ^:dynamic *max-requests*
  100)

(defn requests!
  "Seq of API requests/responses based on continuations."
  ([request]
   (requests! request (dec *max-requests*)))
  ([request remaining]
   (let [{{:keys [continue]} :body :as response} (request! request)]
     (lazy-seq
      (cons response
            (when (and continue (pos? remaining))
              (requests! (update request :form-params merge continue)
                         (dec remaining))))))))

;; ## Retrieve information about a MediaWiki instance.

(defn- parse-version-num
  "Parses numeric version components."
  [v]
  (let [n (re-find #"\d+" v)]
    (if n (parse-long n) v)))

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

(defn info-request
  [url]
  (->
   (request-with-params
    :action "query"
    :meta   #{"siteinfo" "userinfo"}
    :siprop #{"general" "namespaces"}
    :uiprop #{"groups" "rights"})
   (assoc :url url)))

(defn info
  "Retrieve information about a MediaWiki instance."
  [url]
  (-> (info-request url) (request!) (parse-info)))

(comment
  (info (api-endpoint "www.wikidata.org")))
