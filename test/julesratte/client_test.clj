(ns julesratte.client-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]
   [julesratte.page :as page]
   [julesratte.wikitext :as wt]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

(deftest request-wiki-infos
  (let [langs ["de" "en" "fr" "es" "hu"]
        reqs (map (comp client/base-request #(str % ".wikipedia.org")) langs)
        infos (map client/info reqs)
        infos (deref (apply d/zip infos))]
    (is (every? :version infos))))

(deftest request-geo-coords
  (let [req      (assoc (client/base-request "de.wikipedia.org")
                   :prop   "coordinates"
                   :titles ["Oslo" "Kopenhagen" "Berlin"])
        response (deref (client/request req))
        response (get-in response [:body :query :pages])]
    (is (every? :coordinates response))))

(deftest request-categories
  (let [req (assoc (client/base-request "test.wikipedia.org")
                   ::client/max-requests 3
                   :list                 "allcategories"
                   :aclimit              "100")
        responses (s/stream->seq (client/requests req))]
    (is (= 3 (count responses)))))
