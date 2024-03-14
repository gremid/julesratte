(ns julesratte.wikidata.properties
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.java.io :as io]
   [jsonista.core :as json]
   [clojure.string :as str]))

(defn label->kw
  [s]
  (-> s
      (str/replace #"[ /]" "-")
      (str/replace #"[\(\)\'\,;\"]" "")
      (csk/->kebab-case-keyword)))

(def wdt
  (with-open [r (io/reader (io/resource "julesratte/wikidata/properties.json"))]
    (reduce
     (fn [m [id label]] (assoc m (label->kw label) (keyword (str "wdt/" id))))
     {}
     (json/read-value r))))

(def wdt->label
  (reduce (fn [m [label wdt]] (assoc m wdt label)) {} wdt))
