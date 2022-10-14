(ns julesratte.auth
  (:require
   [julesratte.client :as client]
   [manifold.deferred :as d]))

(defn get-login-token
  [response]
  (get-in response [:body :query :tokens :logintoken]))

(def login-token-request
  (client/request-with-params
   :action "query"
   :meta "tokens"
   :type "login"))

(defn query-login-token!
  [config]
  (d/chain (client/request! config login-token-request) get-login-token))

(defn do-login!
  [config user password login-token]
  (->> (client/request-with-params
        :action "login"
        :lgname user
        :lgpassword password
        :lgtoken login-token)
       (client/request! config)))

(defn assert-successful-login
  [response]
  (if (= "Success" (get-in response [:body :login :result]))
    response
    (d/error-deferred (ex-info "Login failed" response))))

(defn login!
  [config user password]
  (d/chain
   (query-login-token! config)
   (partial do-login! config user password)
   assert-successful-login))

(defn get-csrf-token
  [response]
  (get-in response [:body :query :tokens :csrftoken]))

(def csrf-token-request
  (client/request-with-params
   :action "query"
   :meta "tokens"))

(defn query-csrf-token!
  [config]
  (d/chain (client/request! config csrf-token-request) get-csrf-token))

(defn logout-callback
  [config csrf-token]
  (->> (client/request-with-params :action "logout" :token csrf-token)
       (client/request! config)))

(defn logout!
  [config]
  (d/chain (query-csrf-token! config) (partial logout-callback config)))

(defn login-callback
  [config f _response]
  (d/finally (f config) (partial logout! config)))

(defn with-login-session
  [config user password f]
  (d/chain (login! config user password) (partial login-callback config f)))
