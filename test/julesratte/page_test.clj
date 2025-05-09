(ns julesratte.page-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as jr]
   [julesratte.page :as jr.page]))

(deftest request-pages-by-title
  (let [url    (jr/api-url "de.wiktionary.org")
        titles #{"Pfirsich" "Apfel" "Birnen" "Erdbeeren"}
        pages  (apply jr.page/request-by-title url titles)]
    (is (= titles (into #{} (map :title pages))))))

(deftest request-random-pages
  (let [url   (jr/api-url "de.wikipedia.org")
        pages (jr.page/request-random url 5)]
    (is (<= 5 (count pages)))))
