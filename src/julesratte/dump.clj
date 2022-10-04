(ns julesratte.dump
  "Download Wikimedia dumps and parse XML-encoded page revisions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [julesratte.wikitext :as wt])
  (:import java.io.InputStream
           [javax.xml.stream XMLEventReader XMLInputFactory]
           [javax.xml.stream.events Attribute Characters EndElement StartElement XMLEvent]
           javax.xml.transform.stream.StreamSource
           org.apache.commons.compress.compressors.CompressorStreamFactory))

;; ## Download

(defn bunzip2
  [is]
  (.. (CompressorStreamFactory.)
      (createCompressorInputStream CompressorStreamFactory/BZIP2 is)))

(defn ->input-stream
  "Download non-existing dump files to the local store."
  ([wiki]
   (->input-stream wiki "pages-meta-current"))
  ([wiki dump-type]
   (->input-stream wiki dump-type "latest"))
  ([wiki dump-type timestamp]
   (let [compression "bz2"
         filename (format "%s-%s-%s.xml.%s" wiki timestamp dump-type compression)
         url (format "https://dumps.wikimedia.org/%s/%s/%s" wiki timestamp filename)]
     (bunzip2 (io/input-stream url)))))

;; ## XML parsing

(def xml-size-limit-sys-props
  "System properties related to XML parsing."
  ["entityExpansionLimit" "totalEntitySizeLimit" "jdk.xml.totalEntitySizeLimit"])

(defn config-xml-parser-for-large-dump!
  "Maximize limits for parsing huge XML documents."
  []
  (log/tracef (str "Setting system properties to max values for "
                   "parsing huge XML documents (%s)") xml-size-limit-sys-props)
  (doseq [sys-prop xml-size-limit-sys-props]
    (System/setProperty sys-prop (str (Integer/MAX_VALUE)))))

;; Configure XML parser for huge XML documents unless `$JULESRATTE_SMALL_DUMPS`
;; is set.

(when-not (System/getenv "JULESRATTE_SMALL_DUMPS")
  (config-xml-parser-for-large-dump!))

;; Convert XML stream processing events to Clojure data structures (maps,
;; strings).

(defn- xml-start-name
  [^StartElement event]
  (.. event getName getLocalPart))

(defn- xml-end-name
  [^EndElement event]
  (.. event getName getLocalPart))

(defn- xml-attrs
  [^StartElement event]
  (->>
   (for [^Attribute attr (iterator-seq (.getAttributes event))]
     [(keyword (.. attr getName getLocalPart)) (.getValue attr)])
   (into (hash-map))))

(defn- xml-text
  [^Characters event]
  (.getData event))

(defn- xml-event
  [^XMLEvent event]
  (cond
    (.isStartElement event) (merge {:< (xml-start-name event)} (xml-attrs event))
    (.isEndElement event) {:> (xml-end-name event)}
    (.isCharacters event) (xml-text event)
    :else nil))

(def ^:dynamic ^XMLInputFactory xml-input-factory
  (doto (XMLInputFactory/newInstance)
    (.setProperty XMLInputFactory/IS_COALESCING true)))

(defn ^XMLEventReader xml-event-reader
  "Read XML stream events from a given input stream."
  [^InputStream is]
  (.. xml-input-factory (createXMLEventReader (StreamSource. is))))

(defn xml-events->seq
  "Convert XML stream events to Clojure data."
  [^XMLEventReader events]
  (->> (iterator-seq events) (map xml-event) (keep identity)))

;; ## Revision extraction

(def ^:private property-element?
  "Elements in Wikimedia dumps for which we collect string contents."
  #{"title" "timestamp" "text" "username" "comment"})

(defn xml-parse
  "Parse XML stream events from a dump into a sequence of revisions."
  ([events]
   (xml-parse events (transient {})))
  ([[evt & events] ctx]
   (cond
     (map? evt)
     (cond
       (property-element? (evt :<))
       (lazy-seq (xml-parse events (assoc! ctx :sb (StringBuilder.))))

       (property-element? (evt :>))
       (let [text (-> (.toString ^StringBuilder (ctx :sb)) str/trim not-empty)
             ctx (-> ctx (assoc! (-> evt :> keyword) text) (dissoc! :sb))]
         (lazy-seq (xml-parse events ctx)))

       (= "revision" (evt :>))
       (cons (select-keys ctx [:title :timestamp :text :username :comment])
             (lazy-seq
              (xml-parse
               events
               (dissoc! ctx :timestamp :text :username :comment))))

       (= "page" (evt :>))
       (lazy-seq (xml-parse events (dissoc! ctx :title)))

       :else
       (lazy-seq (xml-parse events ctx)))
     (string? evt)
     (do
       (when-let [^StringBuilder sb (ctx :sb)] (.append sb ^String evt))
       (lazy-seq (xml-parse events ctx))))))

(defn parse-revisions
  [xml-events]
  (-> xml-events xml-events->seq xml-parse))

(defn parse-revision
  [{:keys [text] :as revision}]
  (cond-> revision
    (not-empty text) (assoc :ast (wt/parse text))))
