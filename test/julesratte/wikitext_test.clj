(ns julesratte.wikitext-test
  (:require [julesratte.wikitext :as wt]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]))

(def sample-wkt-entry
  (wt/parse (slurp (io/resource "julesratte/hallo.txt"))))

(deftest parse-result
  (is (= :parsed-wikitext-page (wt/class->keyword (class sample-wkt-entry)))))


