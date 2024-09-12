(ns julesratte.wikidata.lexemes
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hato.client :as hc]
   [julesratte.json :as json]
   [julesratte.wikidata :as wd]
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

(defn open-local-dump
  []
  (io/input-stream (io/resource "julesratte/wikidata/lexemes.json.gz")))

(defn open-remote-dump
  []
  (->
   "https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz"
   (hc/get {:as :stream :version :http-1.1}) :body))

(defn read-dump
  [stream]
  (let [gz-stream (GZIPInputStream. stream)
        reader    (io/reader gz-stream)]
    (sequence parse-dump-xf (line-seq reader))))

(defn -main
  [& _]
  (log/handle-uncaught-jvm-exceptions!)
  (try
    (with-open [input (open-remote-dump)]
      (doseq [batch (partition-all 1000 (read-dump input))]
        (log/info (wd/clojurize-claims (first batch)))))
    (finally
      (shutdown-agents))))
