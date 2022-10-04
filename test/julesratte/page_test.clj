(ns julesratte.page-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]
   [julesratte.page :as page]
   [manifold.stream :as s]))

(deftest request-pages-by-title
  (let [titles #{"Pfirsich" "Apfel" "Birnen" "Erdbeeren"}
        req    (client/base-request "de.wiktionary.org")
        pages  (apply page/request-by-title req titles)
        pages  (vec (s/stream->seq pages))]
    (is (= titles (into #{} (map :title pages))))))

(deftest request-random-pages
  (let [req   (client/base-request "de.wikipedia.org")
        pages (page/request-random req 5)
        pages (vec (s/stream->seq pages))]
    (is (<= 5 (count pages)))))
