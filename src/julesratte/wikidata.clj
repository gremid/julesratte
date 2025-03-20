(ns julesratte.wikidata
  (:require
   [clojure.java.io :as io]
   [clojure.walk]
   [com.yetanalytics.flint :as f]
   [hato.client :as hc]
   [julesratte.client :as client]
   [julesratte.json :as json]
   [clojure.string :as str]
   [camel-snake-kebab.core :as csk]))

(def ^:dynamic *lang*
  :en)

(def prefixes
  "RDF prefixes automatically supported by the WikiData query service."
  {:bd       "<http://www.bigdata.com/rdf#>"
   :cc       "<http://creativecommons.org/ns#>"
   :dct      "<http://purl.org/dc/terms/>"
   :geo      "<http://www.opengis.net/ont/geosparql#>"
   :hint     "<http://www.bigdata.com/queryHints#>"
   :ontolex  "<http://www.w3.org/ns/lemon/ontolex#>"
   :owl      "<http://www.w3.org/2002/07/owl#>"
   :p        "<http://www.wikidata.org/prop/>"
   :pq       "<http://www.wikidata.org/prop/qualifier/>"
   :pqn      "<http://www.wikidata.org/prop/qualifier/value-normalized/>"
   :pqv      "<http://www.wikidata.org/prop/qualifier/value/>"
   :pr       "<http://www.wikidata.org/prop/reference/>"
   :prn      "<http://www.wikidata.org/prop/reference/value-normalized/>"
   :prov     "<http://www.w3.org/ns/prov#>"
   :prv      "<http://www.wikidata.org/prop/reference/value/>"
   :ps       "<http://www.wikidata.org/prop/statement/>"
   :psn      "<http://www.wikidata.org/prop/statement/value-normalized/>"
   :psv      "<http://www.wikidata.org/prop/statement/value/>"
   :rdf      "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
   :rdfs     "<http://www.w3.org/2000/01/rdf-schema#>"
   :schema   "<http://schema.org/>"
   :skos     "<http://www.w3.org/2004/02/skos/core#>"
   :wd       "<http://www.wikidata.org/entity/>"
   :wdata    "<http://www.wikidata.org/wiki/Special:EntityData/>"
   :wdno     "<http://www.wikidata.org/prop/novalue/>"
   :wdref    "<http://www.wikidata.org/reference/>"
   :wds      "<http://www.wikidata.org/entity/statement/>"
   :wdt      "<http://www.wikidata.org/prop/direct/>"
   :wdtn     "<http://www.wikidata.org/prop/direct-normalized/>"
   :wdv      "<http://www.wikidata.org/value/>"
   :wikibase "<http://wikiba.se/ontology#>"
   :xsd      "<http://www.w3.org/2001/XMLSchema#>"})

(def uri->prefix
  (reduce
   (fn [m [k v]] (assoc m (subs v 1 (dec (count v))) (str (name k))))
   {}
   prefixes))

(defn uri->keyword
  [pattern uri]
  (let [[_ base-uri trimmed-value] (re-find pattern uri)]
    (when trimmed-value
      (when-let [prefix (uri->prefix base-uri)]
        (keyword prefix trimmed-value)))))

(defn clojurize-query-result-value
  [{:keys [type value datatype] :as v}]
  (condp = type
    "uri"     (or (uri->keyword #"(.*#)(.*)$" value)
                  (uri->keyword #"(.*/)([^/]*)$" value)
                  value)
    "literal" (condp = datatype
                "http://www.w3.org/2001/XMLSchema#decimal" (parse-double value)
                "http://www.w3.org/2001/XMLSchema#integer" (parse-long value)
                "http://www.w3.org/2001/XMLSchema#dateTime" value
                value)
    v))

(defn clojurize-query-result
  [{{{:keys [bindings]} :results} :body}]
  (mapv
   #(reduce-kv (fn [m k v] (assoc m k (clojurize-query-result-value v))) {} %)
   bindings))

(def simple-datatype?
  #{"commonsMedia" "external-id" "string" "time" "url"
    "wikibase-item" "wikibase-lexeme" "wikibase-sense"})

(defn clojurize-data
  [{:keys [datatype] {v :value} :datavalue :as data}]
  (cond-> data (simple-datatype? datatype) (assoc :datavalue v)))

(defn clojurize*
  [v]
  (cond-> v
    (:hash v)                      (dissoc :hash)
    (:mainsnak v)                  (dissoc :id)
    (:entity-type v)               (->> :id (keyword "wd"))
    (:datatype v)                  (clojurize-data)
    (:property v)                  (update :property keyword)
    (and (:language v) (:value v)) (:value)))

(defn clojurize
  [v]
  (clojure.walk/postwalk clojurize* v))

(defn clean-up-symbols-and-seqs
  "Remove the namespace portion of namespaced symbols in `q`. We need to
  do this because clojure's backtick automatically interns symbols in
  the current namespace, but the SPARQL DSL wants unnamespaced
  symbols. Likewise, convert LazySeqs to proper Lists for the benefit
  of Flint's error checking."
  [e]
  (cond (symbol? e) (symbol (name e))
        (seq? e)    (apply list e)
        :else       e))

(defn format-query
  [q]
  (->
   (clojure.walk/postwalk clean-up-symbols-and-seqs q)
   (update :prefixes merge prefixes)
   (update :where conj [:service :wikibase/label
                        [[:bd/serviceParam :wikibase/language
                          (str "[AUTO_LANGUAGE]," (name *lang*))]]])
   (f/format-query)))

(defn query
  ([q]
   (query "https://query.wikidata.org/sparql" q))
  ([url q]
   (-> {:method       :get
        :url          url
        :query-params {:query  (format-query q)
                       :format "json"}}
       (hc/request)
       (json/parse-http-response)
       (clojurize-query-result))))

(defn label->kw
  [s]
  (-> s
      (str/replace #"[ /\-–]+" "-")
      (str/replace #"[^A-Za-z0-9\.\-]" "")
      (csk/->kebab-case-keyword)))

(def properties-source
  (io/resource "julesratte/wikidata/properties.edn"))

(def properties-source-file
  (io/file "src/julesratte/wikidata/properties.edn"))

(defn update-properties!
  [& _]
  (->>
   `{:select [?property ?propertyLabel]
     :where  [{?property {:wikibase/propertyType #{?type}}}]}
   (query)
   (map (juxt (comp label->kw :propertyLabel) (comp keyword name :property)))
   (reduce (fn [m [k v]] (assoc m k v)) {})
   (pr)
   (binding [*out*            w
             *print-length*   nil
             *print-dup*      nil
             *print-level*    nil
             *print-readably* true])
   (with-open [w (io/writer properties-source-file)])))

(def wdt
  (read-string (slurp properties-source)))

(def wdt->label
  (reduce (fn [m [label wdt]] (assoc m wdt label)) {} wdt))


(defn entity
  [label & criteria]
  (->
   `{:select [?item ?sitelinks]
     :where  [[?item :rdfs/label {~*lang* ~label}]
              ~@(mapv (fn [[p e]] `[?item ~p ~e])
                      (partition 2 criteria))
              ;; no :instance-of / :subclass-of wikidata properties
              ;; and no disambiguation pages
              [:minus [[?item (cat (* :wdt/P31) (+ :wdt/P279)) :wd/Q18616576]
                       [?item :wdt/P31 :wd/Q4167410]]]]
     :limit  1}
   (query) (first) (:item)))

(defn label
  ([id]
   (label *lang* id))
  ([lang id]
   (-> `{:select [?label]
         :where  [[~id :rdfs/label ?label]
                  [:filter (= (lang ?label) ~(name lang))]]}
       (query) (first) (:label) (or id))))

(defn describe
  ([id]
   (describe *lang* id))
  ([lang id]
   (-> `{:select [?description]
         :where  [[~id :schema/description ?description]
                  [:filter (= (lang ?description) ~(name lang))]]}
       (query) (first) (:description))))

(defn parse-search-result
  [{:keys [:description :id :display]}]
  {:id          (keyword "wd" id)
   :description description
   :label       (get-in display [:label :value])})

(def parse-search-results
  (partial map parse-search-result))

(def wikidata-api-endpoint
  (client/api-endpoint "www.wikidata.org"))

(defn search
  ([text]
   (search wikidata-api-endpoint "item" text))
  ([type text]
   (search wikidata-api-endpoint type text))
  ([url type text]
   (-> (client/request-with-params
        {:action   "wbsearchentities"
         :search   text
         :language (name *lang*)
         :uselang  (name *lang*)
         :type     type})
       (assoc :url url)
       (client/request!)
       (get-in [:body :search])
       (parse-search-results))))


(defn entity-data-url
  [item-id]
  (str "https://www.wikidata.org/wiki/Special:EntityData/" item-id ".json"))

(defn parse-entities
  [entities]
  (reduce-kv
   (fn [m id entity] (assoc m (keyword "wd" (name id)) (clojurize entity)))
   {}
   entities))

(defn get-entities
  ([ids]
   (get-entities wikidata-api-endpoint ids))
  ([url ids]
   (-> (client/request-with-params
        {:action "wbgetentities"
         :ids    (mapv name ids)})
       (assoc :url url)
       (client/request!)
       (get-in [:body :entities])
       (parse-entities))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; wdno = no value!
  (query `{:select [?human ?humanLabel]
           :where  [{?human {:wdt/P31  #{:wd/Q5}
                             :rdf/type #{:wdno/P40}}}]
           :limit  20}))
(comment
  (let [url (client/api-endpoint "www.wikidata.org")]
    (->> (search url "lexeme" "Ferien")
         (map :id) (get-entities url) vals #_(map :lemmas))))

(comment
  (->> `{:select [?property ?propertyLabel]
         :where  [{?property {:wikibase/propertyType #{?type}}}]}
       (query)
       (map (juxt :property :propertyLabel))
       (reduce (fn [m [id label]] (assoc m (label->kw label) id)) {}))

  (->> `{:select [?property ?propertyLabel]
         :where  [{?property {:wikibase/propertyType #{?type}}}]}
       (query)
       (map (juxt (comp label->kw :propertyLabel) (comp keyword name :property)))
       (reduce (fn [m [k v]] (assoc m k v)) {})
       (keys)))
