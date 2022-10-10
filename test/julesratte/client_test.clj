(ns julesratte.client-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

(deftest request-wiki-infos
  (let [langs   ["de" "en" "fr" "es" "hu"]
        configs (map (comp client/config-for-endpoint
                           client/endpoint-url
                           #(str % ".wikipedia.org")) langs)
        infos   (map client/info configs)
        infos   (deref (apply d/zip infos))]
    (is (every? :version infos))))

(deftest request-geo-coords
  (let [config   (client/config-for-endpoint
                  (client/endpoint-url "de.wikipedia.org"))
        request  (client/request-with-params
                 :prop   "coordinates"
                 :titles ["Oslo" "Kopenhagen" "Berlin"])
        response (deref (client/request! config request))
        response (get-in response [:body :query :pages])]
    (is (every? :coordinates response))))

(deftest request-categories
  (let [config    (client/config-for-endpoint
                   (client/endpoint-url "test.wikipedia.org"))
        config    (assoc config :max-requests 3)
        request   (client/request-with-params
                 :list    "allcategories"
                 :aclimit "100")
        responses (s/stream->seq (client/requests! config request))]
    (is (= 3 (count responses)))))
