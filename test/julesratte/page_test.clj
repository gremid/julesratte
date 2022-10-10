(ns julesratte.page-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.client :as client]
   [julesratte.page :as page]
   [manifold.stream :as s]))

(deftest request-pages-by-title
  (let [titles #{"Pfirsich" "Apfel" "Birnen" "Erdbeeren"}
        config (client/config-for-endpoint
                (client/endpoint-url "de.wiktionary.org"))
        pages  (apply page/request-by-title config titles)
        pages  (vec (s/stream->seq pages))]
    (is (= titles (into #{} (map :title pages))))))

(deftest request-random-pages
  (let [config (client/config-for-endpoint
                (client/endpoint-url "de.wikipedia.org"))
        pages  (page/request-random config 5)
        pages  (vec (s/stream->seq pages))]
    (is (<= 5 (count pages)))))
