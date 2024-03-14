(ns build
  (:require
   [clojure.java.io :as io])
  (:import (java.lang ProcessBuilder$Redirect)))


(defn check!
  [^Process proc]
  (.waitFor ^Process proc)
  (let [exit-status (.exitValue proc)]
    (when-not (zero? exit-status)
      (throw (ex-info (str "Error executing command: " exit-status)
                      {:proc proc})))))

(defn proc!
  ([cmd]
   (proc! cmd "."))
  ([cmd dir]
   (.. (ProcessBuilder. (into-array String cmd))
       (directory (io/file dir))
       (redirectInput ProcessBuilder$Redirect/INHERIT)
       (redirectOutput ProcessBuilder$Redirect/INHERIT)
       (redirectError ProcessBuilder$Redirect/INHERIT)
       (start))))

(def check-proc!
  (comp check! proc!))

(defn proc->str!
  [cmd]
  (let [proc   (.. (ProcessBuilder. (into-array String cmd))
                   (redirectInput ProcessBuilder$Redirect/INHERIT)
                   (redirectError ProcessBuilder$Redirect/INHERIT)
                   (start))
        output (atom "")
        reader (future
                 (with-open [r (io/reader (.getInputStream ^Process proc))]
                   (reset! output (slurp r))))]
    (check! proc)
    @reader
    @output))

(defn update-wd-props
  [& _]
  (let [wd-props (proc->str! ["docker" "run" "--rm" "-it" "maxlath/wikibase-cli"
                              "props" "-e" "https://query.wikidata.org/sparql"])]
    (-> (io/file "src" "julesratte" "wikidata" "properties.json")
        (doto (io/make-parents))
        (spit wd-props))))

(def lexemes-dump-url
  "https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz")

(def lexemes-dump
  (io/file "resources" "julesratte" "wikidata" "lexemes.json.gz"))

(defn download-lexemes-dump
  [& _]
  (io/make-parents lexemes-dump)
  (check-proc! ["curl" "-o" (str lexemes-dump) lexemes-dump-url]))
