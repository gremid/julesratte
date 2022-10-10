(ns julesratte.auth
  (:require
   [julesratte.client :as client]
   [manifold.deferred :as d]))

(defn get-login-token
  [response]
  (get-in response [:body :query :tokens :logintoken]))

(defn query-login-token!
  [req]
  (->
   (merge req {:action "query" :meta "tokens" :type "login"})
   (client/request)
   (d/chain get-login-token)))

(defn do-login!
  [req user password login-token]
  (->>
   {:action "login" :lgname user :lgpassword password :lgtoken login-token}
   (merge req)
   (client/request)))

(defn assert-successful-login
  [response]
  (if (= "Success" (get-in response [:body :login :result]))
    response
    (d/error-deferred (ex-info "Login failed" response))))

(defn login!
  [req user password]
  (->
   (query-login-token! req)
   (d/chain (partial do-login! req user password) assert-successful-login)))


(defn get-csrf-token
  [response]
  (get-in response [:body :query :tokens :csrftoken]))

(defn query-csrf-token!
  [req]
  (-> (client/request (merge req {:action "query" :meta "tokens"}))
      (d/chain get-csrf-token)))

(defn logout-callback
  [req csrf-token]
  (client/request (merge req {:action "logout" :token csrf-token})))

(defn logout!
  [req]
  (d/chain (query-csrf-token! req) (partial logout-callback req)))

(defn login-callback
  [req f _response]
  (->
   (d/future (f req))
   (d/finally (partial logout! req))))

(defn with-login-session
  [req user password f]
  (d/chain (login! req user password) (partial login-callback req f)))
