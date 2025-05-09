(ns julesratte.client
  "MediaWiki API client, ported from Python's `mwclient` lib."
  (:require
   [again.core :as again]
   [clojure.string :as str]
   [hato.client :as hc]
   [julesratte.json :as json]
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

(defn api-url
  ([host]
   (api-url "https" host))
  ([scheme host]
   (api-url scheme host "/w/api.php"))
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

(defn build-http-client
  [opts]
  (->> opts
       (merge {:connect-timeout 5000
               :version         :http-2
               :cookie-policy   :all})
       (hc/build-http-client)))

(def ^:dynamic *http-client*
  (build-http-client {}))

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
    (retry? response) (throw (ex-info (response->error response)
                                      (assoc response ::retry? true)))
    (error? response) (throw (ex-info (response->error response)
                                      (assoc response ::retry? false)))
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
  [url form-params]
  (again/with-retries
    {::again/strategy (request-retry-strategy)
     ::again/callback retry-request?}
    (-> (assoc base-request :url url :http-client *http-client*)
        (update :form-params merge form-params)
        (update :form-params transform-params)
        (hc/request)
        (json/parse-http-response)
        (handle-response))))

(defn requests!
  "Seq of API requests/responses based on continuations."
  ([url form-params]
   (requests! url form-params 100))
  ([url form-params num-requests]
   (let [{{:keys [continue]} :body :as response} (request! url form-params)
         num-requests                            (dec num-requests)]
     (lazy-seq
      (cons
       response
       (when (and continue (pos? num-requests))
         (requests! url (merge form-params continue) num-requests)))))))

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

(defn info
  "Retrieve information about a MediaWiki instance."
  [url]
  (parse-info (request! url {:meta   #{"siteinfo" "userinfo"}
                             :siprop #{"general" "namespaces"}
                             :uiprop #{"groups" "rights"}})))

(comment
  (info (api-url "www.wikidata.org")))

;; ## Authentication

(defn login!
  [url user password]
  (let [response (request! url {:meta "tokens" :type "login"})
        lg-token (get-in response [:body :query :tokens :logintoken])
        response (request! url {:action     "login"
                                :lgname     user
                                :lgpassword password
                                :lgtoken    lg-token})
        result   (get-in response [:body :login :result])]
    (when-not (= "Success" result)
      (throw (ex-info "Login failed" response)))))

(defn logout!
  [url]
  (let [csrf-token (-> (request! url {:meta "tokens"})
                       (get-in [:body :query :tokens :csrftoken]))]
    (request! url {:action "logout" :token csrf-token})))

(defmacro with-login
  [credentials & body]
  `(let [credentials# ~credentials
         url#         (:url credentials#)]
     (login! url# (:user credentials#) (:password credentials#))
     (try ~@body (finally (logout! url#)))))
