(ns julesratte.wikidata.lexemes
  (:require
   [clojure.java.io :as io]
   [clojure.java.process :as p]
   [clojure.string :as str]
   [julesratte.json :as jr.json]
   [taoensso.timbre :as log])
  (:import
   (java.util.zip GZIPInputStream)))

(def parse-dump-xf
  (comp
   (map str/trim)
   (filter #(< 2 (count %)))
   (map #(str/replace % #",$" ""))
   (partition-all 32)
   (mapcat (partial pmap jr.json/read-value))))

(def dump-url
  "https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz")

(defn read-dump
  "Download latest lexeme dump via curl.

  JDK's HTTP client fails to download the resource completely."
  []
  (->
   (p/start {:err :discard} "curl" "-s" dump-url)
   (p/stdout)
   (io/input-stream) (GZIPInputStream.) (io/reader)))

(defn parse-dump
  [reader]
  (sequence parse-dump-xf (line-seq reader)))

(defn -main
  [& _]
  (log/handle-uncaught-jvm-exceptions!)
  (try
    (with-open [dump (read-dump)]
      (doseq [batch (partition-all 1000 (parse-dump dump))]
        (log/info (first batch))))
    (finally
      (shutdown-agents))))

(comment
  (with-open [r (read-dump)]
    (into [] (comp (drop 100) (take 20)) (parse-dump r))))
