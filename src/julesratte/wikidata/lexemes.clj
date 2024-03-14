(ns julesratte.wikidata.lexemes
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [julesratte.json :as json]
   [taoensso.timbre :as log])
  (:import
   (java.util.zip GZIPInputStream)))

(def parse-dump-xf
  (comp
   (map str/trim)
   (filter #(< 2 (count %)))
   (map #(str/replace % #",$" ""))
   (partition-all 32)
   (mapcat (partial pmap json/read-value))))

(def dump-resource
  (io/resource "julesratte/wikidata/lexemes.json.gz"))

(defn -main
  [& _]
  (log/handle-uncaught-jvm-exceptions!)
  (try
    (with-open [stream (io/input-stream dump-resource)
                stream (GZIPInputStream. stream)
                reader (io/reader stream)]
      (let [n (count (sequence parse-dump-xf (line-seq reader)))]
        (log/infof "Lexemes: %,d" n)))
    (finally
      (shutdown-agents))))
