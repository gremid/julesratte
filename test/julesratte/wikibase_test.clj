(ns julesratte.wikibase-test
  (:require [julesratte.wikibase :as jr.wb]
            [clojure.test :refer [deftest is]]))

(deftest entity-description
  (is (string? (jr.wb/describe (jr.wb/entity "Wikidata")))))

