(ns julesratte.auth-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [julesratte.auth :as auth]
   [julesratte.client :as client])
  (:import
   (io.github.cdimascio.dotenv Dotenv)))

(def ^Dotenv dot-env
  (.. (Dotenv/configure) (ignoreIfMissing) (load)))

(defn getenv
  [k]
  (or (System/getenv k) (.get dot-env k)))

(defn credentials-from-env
  []
  (let [user     (getenv "JULESRATTE_TEST_USER")
        password (getenv "JULESRATTE_TEST_PASSWORD")]
    (when (and user password)
      {:url      (client/api-endpoint "test.wikipedia.org")
       :user     user
       :password password})))

(deftest request-authenticated-wiki-info
  (if-let [{:keys [url user] :as credentials} (credentials-from-env)]
    (auth/with-login credentials
      (let [info        (client/info url)
            wiki-user   (get info :user)
            wiki-groups (get info :groups)]
        (is (str/starts-with? user wiki-user))
        (is (= "user" (get wiki-groups "user")))))
    (is true)))
