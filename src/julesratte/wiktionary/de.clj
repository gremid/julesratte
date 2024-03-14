(ns julesratte.wiktionary.de
  (:require
   [clojure.string :as str]
   [julesratte.dump :as dump]
   [julesratte.page :as page]
   [julesratte.wikitext :as wt])
  (:import
   (org.sweble.wikitext.parser.nodes WtDefinitionList WtDefinitionListDef WtHeading WtName WtSection WtTemplate WtTemplateArgument WtTemplateArguments WtValue)))

(defn find-nodes-of-type
  [clz node]
  (filter #(instance? clz %) (wt/tree node)))

(defn find-children-of-type
  [clz node]
  (filter #(instance? clz %) (seq node)))

(defn section-level?
  [n ^WtSection node]
  (= n (.getLevel node)))

(def text
  (comp not-empty str/trim wt/text))

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

(defn find-templates-by-name
  [k node]
  (->> (find-nodes-of-type WtTemplate node)
       (filter #(some #{k} (find-names %)))))

;; ## Categorizing pages

(defn entry-page?
  "Dictionary entries are in the main namespace."
  [title]
  (and (page/main-namespace? title)
       (not (or (re-seq #"^Archiv " title)
                (re-seq #"^Liste " title)
                (re-seq #" \(Konjugation\)$" title)
                (re-seq #" \(Deklination\)$" title)))))

;; ## Parse types

(defn find-titled-def-list
  [title node]
  (->> (wt/tree node)
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
        refs           (type->references node)]
    (when (or pos genus summary pronounciation senses refs)
      (list (cond-> entry
              pos            (assoc :pos pos)
              genus          (assoc :genus genus)
              summary        (assoc :summary summary)
              pronounciation (assoc :pronounciation pronounciation)
              senses         (assoc :senses senses)
              refs           (assoc :references refs))))))

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


(defn -main
  [& _]
  (try
    (with-open [input     (dump/->input-stream "dewiktionary")
                xml-input (dump/xml-event-reader input)]
      (let [revisions (dump/parse-revisions xml-input)
            revisions (filter (comp entry-page? :title) revisions)
            revisions (filter (comp not-empty :text) revisions)
            texts     (pmap (comp wt/parse :text) revisions)
            entries   (mapcat entries texts)
            entries   (filter #(= "Deutsch" (:lang %)) entries)]
        (binding [*print-length*   nil
                  *print-dup*      nil
                  *print-level*    nil
                  *print-readably* true]
          (doseq [t (mapcat :types entries)] (println (pr-str t))))))
    (finally
      (shutdown-agents))))
