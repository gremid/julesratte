(ns julesratte.auth
  (:require
   [hato.client :as hc]
   [julesratte.client :as client]))

(defn session-http-client
  []
  (hc/build-http-client {:connect-timeout 5000
                         :version         :http-2
                         :cookie-policy   :all}))

(def login-token-request
  (client/request-with-params {:action "query" :meta "tokens" :type "login"}))

(defn login-request
  [user password token]
  (client/request-with-params {:action "login"
                               :lgname user
                               :lgpassword password
                               :lgtoken token}))

(def csrf-token-request
  (client/request-with-params {:action "query" :meta "tokens"}))

(defn csrf-token
  [url]
  (-> (client/request! (assoc csrf-token-request :url url))
      (get-in [:body :query :tokens :csrftoken])))

(defn logout-request
  [csrf-token]
  (client/request-with-params {:action "logout" :token csrf-token}))

(defn login!
  [url user password]
  (let [request  (assoc login-token-request :url url)
        response (client/request! request)
        token    (get-in response [:body :query :tokens :logintoken])
        request  (assoc (login-request user password token) :url url)
        response (client/request! request)
        result   (get-in response [:body :login :result])]
    (when-not (= "Success" result)
      (throw (ex-info "Login failed" response)))))

(defn logout!
  [url]
  (client/request! (assoc (logout-request (csrf-token url)) :url url)))

(defmacro with-login
  [credentials & body]
  `(let [credentials# ~credentials
         url#      (:url credentials#)]
     (binding [client/*http-client* (session-http-client)]
       (login! url# (:user credentials#) (:password credentials#))
       (try ~@body (finally (logout! url#))))))
