(ns julesratte.auth-test
  (:require [julesratte.auth :as auth]
            [julesratte.client :as client]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str])
  (:import io.github.cdimascio.dotenv.Dotenv))

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
      [user password])))

(deftest request-authenticated-wiki-info
  (if-let [credentials (credentials-from-env)]
    (let [config      (client/config-for-endpoint
                       (client/endpoint-url "test.wikipedia.org")
                       (client/create-session-client))
          user        (first credentials)
          password    (second credentials)
          response    (auth/with-login-session config user password client/info)
          response    (deref response)
          wiki-user   (get response :user)
          wiki-groups (get response :groups)]
      (is (str/starts-with? user wiki-user))
      (is (= "user" (get wiki-groups "user"))))
    (is true)))
