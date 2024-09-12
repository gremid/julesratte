(ns julesratte.wikidata-test
  (:require [julesratte.wikidata :as wd]
            [clojure.test :refer [deftest is]]))

(deftest entity-description
  (is (map? (wd/describe (wd/entity "Wikidata")))))

