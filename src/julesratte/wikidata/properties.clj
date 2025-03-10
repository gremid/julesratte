(ns julesratte.wikidata.properties
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.java.io :as io]
   [jsonista.core :as json]
   [clojure.string :as str]
   [clojure.java.process :refer [exec]]))

(def properties-source
  (io/resource "julesratte/wikidata/properties.json"))

(defn download!
  [& _]
  (spit (io/file properties-source)
        (exec "docker" "run" "--rm" "maxlath/wikibase-cli:17.0.10"
              "props" "-e" "https://query.wikidata.org/sparql")))

(defn label->kw
  [s]
  (-> s
      (str/replace #"[ /]" "-")
      (str/replace #"[\(\)\'\,;\"]" "")
      (csk/->kebab-case-keyword)))

(def wdt
  (with-open [r (io/reader properties-source)]
    (reduce
     (fn [m [id label]] (assoc m (label->kw (or label id)) (keyword (str "wdt/" id))))
     {}
     (json/read-value r))))

(def wdt->label
  (reduce (fn [m [label wdt]] (assoc m wdt label)) {} wdt))
