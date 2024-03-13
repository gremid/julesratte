(ns julesratte.auth
  (:require
   [hato.client :as hc]
   [julesratte.client :as client]))

(defn session-http-client
  []
  (hc/build-http-client {:connect-timeout 5000
                         :version         :http-2
                         :cookie-policy   :all}))

(defn session-request
  [url & params]
  (-> (apply client/request-with-params params)
      (assoc :url url)))

(defn login-token-request
  [url]
  (session-request url :action "query" :meta "tokens" :type "login"))

(defn login-request
  [url user password token]
  (session-request url :action "login" :lgname user :lgpassword password
                   :lgtoken token))

(defn csrf-token-request
  [url]
  (session-request url :action "query" :meta "tokens"))

(defn logout-request
  [url csrf-token]
  (session-request url :action "logout" :token csrf-token))

(defn login!
  [url user password]
  (let [request  (login-token-request url)
        response (client/request! request)
        token    (get-in response [:body :query :tokens :logintoken])
        request  (login-request url user password token)
        response (client/request! request)
        result   (get-in response [:body :login :result])]
    (when-not (= "Success" result)
      (throw (ex-info "Login failed" response)))))

(defn logout!
  [url]
  (as-> url  $
    (client/request! (csrf-token-request $))
    (get-in $ [:body :query :tokens :csrftoken])
    (client/request! (logout-request url $))))


(defmacro with-login
  [credentials & body]
  `(let [credentials# ~credentials
         url#      (:url credentials#)]
     (binding [client/*http-client* (session-http-client)]
       (login! url# (:user credentials#) (:password credentials#))
       (try ~@body (finally (logout! url#))))))
