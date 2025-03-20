(ns julesratte.client-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]))

(deftest request-wiki-infos
  (let [isos  ["de" "en" "fr" "es" "hu"]
        urls  (map (comp client/api-endpoint #(str % ".wikipedia.org")) isos)
        infos (pmap client/info urls)]
    (is (every? :version infos))))

(deftest request-geo-coords
  (let [request  (-> (client/request-with-params
                      {:prop   "coordinates"
                       :titles ["Oslo" "Kopenhagen" "Berlin"]})
                     (assoc :url (client/api-endpoint "de.wikipedia.org")))
        response (client/request! request)
        response (get-in response [:body :query :pages])]
    (is (every? :coordinates response))))

(deftest request-categories
  (let [request (->
                 (client/request-with-params {:list "allcategories" :aclimit "100"})
                 (assoc :url (client/api-endpoint "test.wikipedia.org")))]
    (binding [client/*max-requests* 3]
      (is (= 3 (count (client/requests! request)))))))
