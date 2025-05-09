(ns julesratte.client-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [julesratte.client :as jr])
  (:import
   (io.github.cdimascio.dotenv Dotenv)))

(deftest request-wiki-infos
  (let [isos  ["de" "en" "fr" "es" "hu"]
        urls  (map (comp jr/api-url #(str % ".wikipedia.org")) isos)
        infos (pmap jr/info urls)]
    (is (every? :version infos))))

(deftest request-geo-coords
  (let [response (jr/request!
                  (jr/api-url "de.wikipedia.org")
                  {:prop   "coordinates"
                   :titles ["Oslo" "Kopenhagen" "Berlin"]})
        response (get-in response [:body :query :pages])]
    (is (every? :coordinates response))))

(deftest request-categories
  (let [responses (jr/requests!
                   (jr/api-url "test.wikipedia.org")
                   {:list "allcategories" :aclimit "100"}
                   3)]
    (is (= 3 (count responses)))))

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
      {:url      (jr/api-url "test.wikipedia.org")
       :user     user
       :password password})))

(deftest request-authenticated-wiki-info
  (if-let [{:keys [url user] :as credentials} (credentials-from-env)]
    (jr/with-login credentials
      (let [info        (jr/info url)
            wiki-user   (get info :user)
            wiki-groups (get info :groups)]
        (is (str/starts-with? user wiki-user))
        (is (= "user" (get wiki-groups "user")))))
    (is true)))
