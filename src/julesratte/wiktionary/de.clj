(ns julesratte.wiktionary.de
  (:require
   [clojure.string :as str]
   [julesratte.dump :as jr.dump]
   [julesratte.page :as jr.page]
   [julesratte.wikitext :as jr.wt]
   [clojure.java.io :as io])
  (:import
   (org.sweble.wikitext.parser.nodes WtDefinitionList WtDefinitionListDef WtHeading WtName WtSection WtTemplate WtTemplateArgument WtTemplateArguments WtValue)))

(defn find-nodes-of-type
  [clz node]
  (filter #(instance? clz %) (jr.wt/tree node)))

(defn find-children-of-type
  [clz node]
  (filter #(instance? clz %) (seq node)))

(defn section-level?
  [n ^WtSection node]
  (= n (.getLevel node)))

(def text
  (comp not-empty str/trim jr.wt/text))

(defn find-names
  [node]
  (map text (find-children-of-type WtName node)))

(defn find-values
  [node]
  (map text (find-children-of-type WtValue node)))

(defn find-args
  [node]
  (->> (find-children-of-type WtTemplateArguments node)
       (mapcat #(find-children-of-type WtTemplateArgument %))
       (map (juxt (comp first find-names) (comp first find-values)))
       (into [])))

(defn find-node-by-type-and-name
  [t k node]
  (->> (find-nodes-of-type t node)
       (filter #(some #{k} (find-names %)))))

(def find-templates-by-name
  (partial find-node-by-type-and-name WtTemplate))

(def find-heading-by-name
  (partial find-node-by-type-and-name WtHeading))

;; ## Categorizing pages

(defn entry-page?
  "Dictionary entries are in the main namespace."
  [title]
  (and (jr.page/main-namespace? title)
       (not (or (re-seq #"^Archiv " title)
                (re-seq #"^Liste " title)
                (re-seq #" \(Konjugation\)$" title)
                (re-seq #" \(Deklination\)$" title)))))

;; ## Parse types

(defn find-titled-def-list
  [title node]
  (->> (jr.wt/tree node)
       (drop-while #(or (not (instance? WtTemplate %))
                        (nil? (some #{title} (find-names %)))))
       (filter #(instance? WtDefinitionList %))
       (first)))

(defn parse-sense-ref-range
  [s]
  (try
    (let [[f t] (->> (str/split s #"\s*[\-–—]\s*") (take 2) (map parse-long))]
      (if t (range f (inc t)) (list f)))
    (catch java.lang.NumberFormatException _ (list s))))

(def sense-ref-pattern
  #"^\s*\[([^\]]+)\]\s*")

(defn parse-sense-references
  [node]
  (when-let [[_ ref-spec] (some->> node text (re-find sense-ref-pattern))]
    (->> (str/split ref-spec #"\s*,\s*")
         (map not-empty) (filter some?)
         (mapcat parse-sense-ref-range)
         (into (sorted-set)))))

(defn remove-sense-references
  [s]
  (some-> s (str/replace sense-ref-pattern "") str/trim not-empty))

(defn type->senses
  [node]
  (when-let [senses (find-titled-def-list "Bedeutungen" node)]
    (->> (find-children-of-type WtDefinitionListDef senses)
         (map (comp remove-sense-references text))
         (remove nil?))))

(defn type->references
  [node]
  (when-let [refs (find-titled-def-list "Referenzen" node)]
    (->> (find-children-of-type WtDefinitionListDef refs)
         (map (comp remove-sense-references text))
         (remove nil?))))

(defn type->pos
  "Part-of-Speech tags are given in the heading of each type.

  The language of the entry is given as a redundant tag and removed."
  [node]
  (some->> (find-templates-by-name "Wortart" node)
           (mapcat find-args)
           (map second)
           (seq)
           (into #{})))

(defn type->genus
  [node]
  (some->> (find-nodes-of-type WtTemplate node)
           (mapcat find-names)
           (filter #{"m" "f" "n"})
           (seq)
           (into #{})))

(defn summary-name?
  [s]
  (and s (str/includes? s "Übersicht") (str/includes? s "Deutsch")))

(defn type->summary
  [node]
  (some->> (find-nodes-of-type WtTemplate node)
           (filter #(some summary-name? (find-names %)))
           (mapcat find-args)
           (filter (every-pred first second))
           (seq)
           (into [])))

(defn type->pronounciation
  [node]
  (when-let [pronounciation (find-titled-def-list "Aussprache" node)]
    (->> (find-children-of-type WtDefinitionListDef pronounciation)
         (filter #(find-templates-by-name "IPA" %))
         (mapcat #(find-templates-by-name "Lautschrift" %))
         (mapcat find-args)
         (map second)
         (first))))

(defn parse-type
  [entry node]
  (let [[heading]      (find-nodes-of-type WtHeading node)
        pos            (some-> heading type->pos)
        genus          (some-> heading type->genus)
        summary        (type->summary node)
        pronounciation (type->pronounciation node)
        senses         (type->senses node)
        refs           (type->references node)
        translations   (->> (find-templates-by-name "Ü" node)
                            (map find-args)
                            (mapcat (fn [[[_ lang] [_ form]]]
                                      (when (and lang form)
                                        (list {:lang lang
                                               :form form}))))
                            (seq))]
    (when (or pos genus summary pronounciation senses refs)
      (list (cond-> entry
              pos            (assoc :pos pos)
              genus          (assoc :genus genus)
              summary        (assoc :summary summary)
              pronounciation (assoc :pronounciation pronounciation)
              senses         (assoc :senses senses)
              refs           (assoc :references refs)
              translations   (assoc :translations translations))))))

(defn types
  [entry node]
  (->> (find-nodes-of-type WtSection node)
       (filter #(section-level? 3 %))
       (mapcat #(parse-type entry %))
       (seq)))


;; ## Parse entries

(defn heading->form
  "The heading of an entry contains the headword. Tags like language and
  part-of-speech, added to the heading in parentheses after the headword, are
  trimmed."
  [node]
  (some-> node text (str/replace-first #"\s*\([^)]+\)" "")))

(defn heading->lang
  "The language of the entry is given in the heading as a template value."
  [node]
  (some->> (find-templates-by-name "Sprache" node)
           (mapcat find-args)
           (map second)
           (first)))

(defn parse-entry
  [node]
  (let [[heading] (find-nodes-of-type WtHeading node)
        form      (some-> heading heading->form)
        lang      (some-> heading heading->lang)
        entry     (cond-> nil form (assoc :form form) lang (assoc :lang lang))
        types     (types entry node)]
    (when (or entry types)
      (list (cond-> entry types (assoc :types types))))))

(defn entries
  [node]
  (->> (find-nodes-of-type WtSection node)
       (filter #(section-level? 2 %))
       (mapcat parse-entry)
       (seq)))


(def dump-xml-resource
  (io/resource "julesratte/wiktionary/de.xml"))

(defn -main
  [& _]
  (try
    (with-open [input     (io/input-stream dump-xml-resource)
                xml-input (jr.dump/xml-event-reader input)]
      (let [revisions (jr.dump/parse-revisions xml-input)
            revisions (filter (comp entry-page? :title) revisions)
            revisions (filter (comp not-empty :text) revisions)
            texts     (pmap (comp jr.wt/parse :text) revisions)
            entries   (mapcat entries texts)
            entries   (filter #(= "Deutsch" (:lang %)) entries)]
        (binding [*print-length*   nil
                  *print-dup*      nil
                  *print-level*    nil
                  *print-readably* true]
          (doseq [e entries] (println (pr-str e))))))
    (finally
      (shutdown-agents))))

(def dump-edn-resource
  (io/resource "julesratte/wiktionary/de.edn"))

(defn read-dump
  []
  (let [reader    (io/reader dump-edn-resource)
        dump      (sequence (map read-string) (line-seq reader))]
    (reify
      clojure.lang.Seqable
      (seq [_] (seq dump))

      java.io.Closeable
      (close [_] (.close ^java.io.Closeable reader)))))

(defn inflected-type?
  [{:keys [pos]}]
  (some
   #{"Deklinierte Form" "Dekliniertes Gerundivum"
     "Konjugierte Form" "Partizip I" "Partizip II"
     "Komparativ" "Superlativ" }
   pos))

(defn multiword-expression?
  [{:keys [pos]}]
  (some
   #{"Wortverbindung" "Redewendung" "Sprichwort" "Geflügeltes Wort"}
   pos))

(def morphological-categories
  {"Akkusativ Plural"            "Akkusativ Plural",
   "Akkusativ Plural 1"          "Akkusativ Plural",
   "Akkusativ Plural 1*"         "Akkusativ Plural",
   "Akkusativ Plural 2"          "Akkusativ Plural",
   "Akkusativ Plural 2*"         "Akkusativ Plural",
   "Akkusativ Plural 3"          "Akkusativ Plural",
   "Akkusativ Plural 4"          "Akkusativ Plural",
   "Akkusativ Plural gemischt"   "Akkusativ Plural",
   "Akkusativ Plural schwach"    "Akkusativ Plural",
   "Akkusativ Plural stark"      "Akkusativ Plural",
   "Akkusativ Plural*"           "Akkusativ Plural",
   "Akkusativ Plural**"          "Akkusativ Plural",
   "Akkusativ Singular"          "Akkusativ Singular",
   "Akkusativ Singular 1"        "Akkusativ Singular",
   "Akkusativ Singular 1*"       "Akkusativ Singular",
   "Akkusativ Singular 2"        "Akkusativ Singular",
   "Akkusativ Singular 2*"       "Akkusativ Singular",
   "Akkusativ Singular 3"        "Akkusativ Singular",
   "Akkusativ Singular 3*"       "Akkusativ Singular",
   "Akkusativ Singular 4"        "Akkusativ Singular",
   "Akkusativ Singular gemischt" "Akkusativ Singular",
   "Akkusativ Singular schwach"  "Akkusativ Singular",
   "Akkusativ Singular stark"    "Akkusativ Singular",
   "Akkusativ Singular*"         "Akkusativ Singular",
   "Dativ Plural"                "Dativ Plural",
   "Dativ Plural 1"              "Dativ Plural",
   "Dativ Plural 1*"             "Dativ Plural",
   "Dativ Plural 2"              "Dativ Plural",
   "Dativ Plural 2*"             "Dativ Plural",
   "Dativ Plural 3"              "Dativ Plural",
   "Dativ Plural 4"              "Dativ Plural",
   "Dativ Plural gemischt"       "Dativ Plural",
   "Dativ Plural schwach"        "Dativ Plural",
   "Dativ Plural stark"          "Dativ Plural",
   "Dativ Plural*"               "Dativ Plural",
   "Dativ Plural**"              "Dativ Plural",
   "Dativ Singular"              "Dativ Singular",
   "Dativ Singular 1"            "Dativ Singular",
   "Dativ Singular 1*"           "Dativ Singular",
   "Dativ Singular 1**"          "Dativ Singular",
   "Dativ Singular 2"            "Dativ Singular",
   "Dativ Singular 2*"           "Dativ Singular",
   "Dativ Singular 3"            "Dativ Singular",
   "Dativ Singular 3*"           "Dativ Singular",
   "Dativ Singular 4"            "Dativ Singular",
   "Dativ Singular gemischt"     "Dativ Singular",
   "Dativ Singular gemischt*"    "Dativ Singular",
   "Dativ Singular schwach"      "Dativ Singular",
   "Dativ Singular schwach*"     "Dativ Singular",
   "Dativ Singular stark"        "Dativ Singular",
   "Dativ Singular stark*"       "Dativ Singular",
   "Dativ Singular*"             "Dativ Singular",
   "Dativ Singular**"            "Dativ Singular",
   "Genitiv Plural"              "Genitiv Plural",
   "Genitiv Plural 1"            "Genitiv Plural",
   "Genitiv Plural 1*"           "Genitiv Plural",
   "Genitiv Plural 2"            "Genitiv Plural",
   "Genitiv Plural 2*"           "Genitiv Plural",
   "Genitiv Plural 3"            "Genitiv Plural",
   "Genitiv Plural 4"            "Genitiv Plural",
   "Genitiv Plural gemischt"     "Genitiv Plural",
   "Genitiv Plural schwach"      "Genitiv Plural",
   "Genitiv Plural stark"        "Genitiv Plural",
   "Genitiv Plural*"             "Genitiv Plural",
   "Genitiv Plural**"            "Genitiv Plural",
   "Genitiv Singular"            "Genitiv Singular",
   "Genitiv Singular 1"          "Genitiv Singular",
   "Genitiv Singular 1*"         "Genitiv Singular",
   "Genitiv Singular 1**"        "Genitiv Singular",
   "Genitiv Singular 2"          "Genitiv Singular",
   "Genitiv Singular 2*"         "Genitiv Singular",
   "Genitiv Singular 2**"        "Genitiv Singular",
   "Genitiv Singular 3"          "Genitiv Singular",
   "Genitiv Singular 3*"         "Genitiv Singular",
   "Genitiv Singular 4"          "Genitiv Singular",
   "Genitiv Singular gemischt"   "Genitiv Singular",
   "Genitiv Singular gemischt*"  "Genitiv Singular",
   "Genitiv Singular schwach"    "Genitiv Singular",
   "Genitiv Singular schwach*"   "Genitiv Singular",
   "Genitiv Singular stark"      "Genitiv Singular",
   "Genitiv Singular stark*"     "Genitiv Singular",
   "Genitiv Singular*"           "Genitiv Singular",
   "Genitiv Singular**"          "Genitiv Singular",
   ;; "Genus"                       "Genus",
   ;; "Genus 1"                     "Genus",
   ;; "Genus 2"                     "Genus",
   ;; "Genus 3"                     "Genus",
   ;; "Genus 4"                     "Genus",
   "Hilfsverb"                   "Hilfsverb",
   "Hilfsverb*"                  "Hilfsverb",
   "Hilfsverb2"                  "Hilfsverb",
   "Imperativ Plural"            "Imperativ Plural",
   "Imperativ Plural*"           "Imperativ Plural",
   "Imperativ Plural**"          "Imperativ Plural",
   "Imperativ Singular"          "Imperativ Singular",
   "Imperativ Singular 2"        "Imperativ Singular",
   "Imperativ Singular*"         "Imperativ Singular",
   "Imperativ Singular**"        "Imperativ Singular",
   "Imperativ Singular***"       "Imperativ Singular",
   "Komparativ"                  "Komparativ",
   "Komparativ*"                 "Komparativ",
   "Komparativ**"                "Komparativ",
   "Konjunktiv II_ich"           "Konjunktiv II",
   "Konjunktiv II_ich*"          "Konjunktiv II",
   "Konjunktiv II_ich**"         "Konjunktiv II",
   "Konjunktiv II_ich***"        "Konjunktiv II",
   "Nominativ Plural"            "Nominativ Plural",
   "Nominativ Plural 1"          "Nominativ Plural",
   "Nominativ Plural 1*"         "Nominativ Plural",
   "Nominativ Plural 2"          "Nominativ Plural",
   "Nominativ Plural 2*"         "Nominativ Plural",
   "Nominativ Plural 3"          "Nominativ Plural",
   "Nominativ Plural 4"          "Nominativ Plural",
   "Nominativ Plural gemischt"   "Nominativ Plural",
   "Nominativ Plural schwach"    "Nominativ Plural",
   "Nominativ Plural stark"      "Nominativ Plural",
   "Nominativ Plural stark*"     "Nominativ Plural",
   "Nominativ Plural*"           "Nominativ Plural",
   "Nominativ Plural**"          "Nominativ Plural",
   "Nominativ Singular"          "Nominativ Singular",
   "Nominativ Singular 1"        "Nominativ Singular",
   "Nominativ Singular 2"        "Nominativ Singular",
   "Nominativ Singular 3"        "Nominativ Singular",
   "Nominativ Singular 4"        "Nominativ Singular",
   "Nominativ Singular gemischt" "Nominativ Singular",
   "Nominativ Singular schwach"  "Nominativ Singular",
   "Nominativ Singular stark"    "Nominativ Singular",
   "Nominativ Singular*"         "Nominativ Singular",
   "Partizip II"                 "Partizip II",
   "Partizip II*"                "Partizip II",
   "Partizip II**"               "Partizip II",
   "Positiv"                     "Positiv",
   "Präsens_du"                  "Präsens_du",
   "Präsens_du*"                 "Präsens_du",
   "Präsens_du**"                "Präsens_du",
   "Präsens_er, sie, es"         "Präsens_er, sie, es",
   "Präsens_er, sie, es*"        "Präsens_er, sie, es",
   "Präsens_er, sie, es**"       "Präsens_er, sie, es",
   "Präsens_ich"                 "Präsens_ich",
   "Präsens_ich*"                "Präsens_ich",
   "Präsens_ich**"               "Präsens_ich",
   "Präteritum_ich"              "Präteritum_ich",
   "Präteritum_ich*"             "Präteritum_ich",
   "Präteritum_ich**"            "Präteritum_ich",
   "Superlativ"                  "Superlativ",
   "Superlativ*"                 "Superlativ",
   "Superlativ**"                "Superlativ"})

(defn assoc-morphological-table
  [{:keys [summary] :as lexeme-type}]
  (let [table (reduce
               (fn [m [k v]]
                 (let [k (morphological-categories k)
                       v (when-not (= v "—") v)]
                   (cond-> m (and k v) (update k (fnil conj []) v))))
               {}
               summary)]
    (cond-> lexeme-type (seq table) (assoc :morphology table))))

(def collator
  (doto (java.text.Collator/getInstance java.util.Locale/GERMAN)
    (.setStrength java.text.Collator/PRIMARY)))

(defn collation-key
  [s]
  (.getCollationKey ^java.text.Collator collator s))

(comment
  (with-open [dump (read-dump)]
    (->> (seq dump)
         (mapcat :types)
         (remove inflected-type?)
         (remove multiword-expression?)
         (filter #(some #{"Eigenname" "Nachname" "Ortsnamengrundwort"
                          "Straßenname" "Toponym" "Vorname"} (:pos %)))
         (map assoc-morphological-table)
         (filter :morphology)
         (map (juxt :form :pos))
         (sort-by (comp collation-key first))))
  
  (with-open [dump (read-dump)]
    (into []
          (comp (mapcat :types)
                (remove inflected-type?)
                (filter #(some #{"Eigenname" "Nachname" "Ortsnamengrundwort"
                                 "Straßenname" "Toponym" "Vorname"} (:pos %)))
                (map assoc-morphological-table)
                (filter :morphology)
                (map (juxt :form :pos :genus :morphology))
                (take 100))
          (seq dump)))

  (with-open [input     (io/input-stream dump-xml-resource)
              xml-input (jr.dump/xml-event-reader input)]
    (let [revisions (jr.dump/parse-revisions xml-input)
          revisions (filter (comp entry-page? :title) revisions)
          revisions (filter (comp not-empty :text) revisions)
          texts     (pmap (comp wt/parse :text) revisions)
          entries   (mapcat entries texts)
          entries   (filter #(= "Deutsch" (:lang %)) entries)
          types     (mapcat :types entries)
          types     (remove (some-fn inflected-type? multiword-expression?) types)
          danish?   (fn [{:keys [translations]}]
                      (some (comp #{"da"} :lang) translations))
          types     (filter danish? types)]
      (into [] (comp (drop (rand-int 1000))(take 10) (map (juxt :form :pos :translations))) types)
      #_(count types))))
