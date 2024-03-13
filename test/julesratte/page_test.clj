(ns julesratte.page-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]
   [julesratte.page :as page]))

(deftest request-pages-by-title
  (let [url    (client/api-endpoint "de.wiktionary.org")
        titles #{"Pfirsich" "Apfel" "Birnen" "Erdbeeren"}
        pages  (apply page/request-by-title url titles)]
    (is (= titles (into #{} (map :title pages))))))

(deftest request-random-pages
  (let [url   (client/api-endpoint "de.wikipedia.org")
        pages (page/request-random url 5)]
    (is (<= 5 (count pages)))))
