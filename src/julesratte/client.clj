(ns julesratte.client
  "MediaWiki API client, ported from Python's `mwclient` lib."
  (:require
   [clojure.string :as str]
   [com.potetm.fusebox.rate-limit :as rl]
   [com.potetm.fusebox.retry :as retry]
   [hato.client :as hc]
   [julesratte.json :as json]
   [taoensso.telemere :as tel]))

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

(def request-delay-ms
  (atom 0))

(defn handle-response
  "Check for error/warnings in response and extract body."
  [response]
  (tel/event! ::response {:data response})
  (let [retry-after (get-in response [:headers "retry-after"] "0")]
    (reset! request-delay-ms (* 1000 (parse-long retry-after))))
  (cond
    (retry? response) (throw (ex-info (response->error response)
                                      (assoc response ::retry? true)))
    (error? response) (throw (ex-info (response->error response)
                                      (assoc response ::retry? false)))
    :else             response))

(def ^:dynamic *max-retries*
  3)

(defn retry-request?
  [n _ms ex]
  (if (or (>= n *max-retries*) (some-> ex ex-data ::retry? false?))
    (do (tel/error! ::request-aborted ex) false)
    true))

(defn retry-delay
  [n _ms _ex]
  (max @request-delay-ms (retry/jitter 0.5 (retry/delay-exp 500 n))))

(def ^:dynamic *retry*
  (retry/init {::retry/retry? retry-request?
               ::retry/delay  retry-delay}))

(def ^:dynamic *rate-limit*
  (rl/init {::rl/bucket-size     1
            ::rl/period-ms       (long (/ 60000 80))
            ::rl/wait-timeout-ms 10000}))

(defn request!
  [url form-params]
  (->>
    (-> (assoc base-request :url url :http-client *http-client*)
        (update :form-params merge form-params)
        (update :form-params transform-params)
        (hc/request)
        (json/parse-http-response)
        (handle-response))
    (retry/with-retry *retry*)
    (rl/with-rate-limit *rate-limit*)))

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

(defn csrf-token
  [url]
  (-> (request! url {:meta "tokens"})
      (get-in [:body :query :tokens :csrftoken])))

(defn logout!
  [url]
  (request! url {:action "logout" :token (csrf-token url)}))

(defmacro with-login
  [credentials & body]
  `(let [credentials# ~credentials
         url#         (:url credentials#)]
     (login! url# (:user credentials#) (:password credentials#))
     (try ~@body (finally (logout! url#)))))
