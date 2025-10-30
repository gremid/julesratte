(ns julesratte.wikidata.lexemes
  (:require
   [clojure.java.io :as io]
   [clojure.java.process :as p]
   [clojure.string :as str]
   [julesratte.json :as jr.json])
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

(comment
  (with-open [r (read-dump)]
    (into [] (comp (drop 100) (take 20)) (parse-dump r))))
