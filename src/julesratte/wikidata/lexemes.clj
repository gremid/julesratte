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
   (mapcat (partial pmap (comp wd/clojurize json/read-value)))))

(defn read-dump
  []
  (->
   "https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz"
   (hc/get {:as :stream :version :http-1.1}) :body
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
