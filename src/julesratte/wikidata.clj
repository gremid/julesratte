(ns julesratte.wikidata
  (:require
   [clojure.walk]
   [com.yetanalytics.flint :as f]
   [hato.client :as hc]
   [julesratte.json :as json]
   [julesratte.wikidata.properties :refer [wdt->label]]))

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
  "Reverse lookup table from RDF namespace url to prefix name. Used by
  `uri->keyword`."
  (reduce
   (fn [m [k v]] (assoc m (subs v 1 (dec (count v))) (str (name k))))
   {}
   prefixes))

(defn uri->keyword
  "Use regex `pattern` to extract the base and final portions of `uri`
  and convert them into a namespaced keyword. Returns nil if the
  pattern match is not successful."
  [pattern uri]
  (let [[_ base-uri trimmed-value] (re-find pattern uri)]
    (when (uri->prefix base-uri)
      (when trimmed-value
        (keyword (uri->prefix base-uri) trimmed-value)))))

(defn clojurize-uri
  [value]
  (or (uri->keyword #"(.*#)(.*)$" value)
      (uri->keyword #"(.*/)([^/]*)$" value)
      value))

(defn clojurize-literal
  [datatype value]
  (condp = datatype
    "http://www.w3.org/2001/XMLSchema#decimal"  (parse-double value)
    "http://www.w3.org/2001/XMLSchema#integer"  (parse-long value)
    "http://www.w3.org/2001/XMLSchema#dateTime" value #_ (tick/instant value)
    nil))

(defn clojurize-values*
  [[k {:keys [type value datatype] :as v}]]
  (let [value (condp = type
                "uri"     (clojurize-uri value)
                "literal" (clojurize-literal datatype value))]
    [k (or value v)]))

(defn clojurize-values
  "Convert the values in `result` to Clojure types."
  [result]
  (into {} (map clojurize-values*) result))

(defn parse-query-result
  [response]
  (->>
   (get-in (json/parse-http-response response) [:body :results :bindings])
   (mapv clojurize-values)))

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

(def ^:dynamic *lang*
  :en)

(defn wb-label-service
  []
  [:service :wikibase/label
   [[:bd/serviceParam :wikibase/language
     (str "[AUTO_LANGUAGE]," (name *lang*))]]])

(defn format-query
  [q]
  (->
   (clojure.walk/postwalk clean-up-symbols-and-seqs q)
   (update :prefixes merge prefixes)
   (update :where conj (wb-label-service))
   (f/format-query)))

(defn query
  [q]
  (-> {:method       :get
       :url          "https://query.wikidata.org/sparql"
       :query-params {:query  (format-query q)
                      :format "json"}}
      (hc/request)
      (parse-query-result)))

(defn entity-query
  [lang label criteria]
  `{:select   [?item ?sitelinks]
    :where    [[?item :rdfs/label {~lang ~label}]
               [?item :wikibase/sitelinks ?sitelinks]
               ~@(mapv (fn [[p e]] `[?item ~p ~e])
                       (partition 2 criteria))
               ;; no :instance-of / :subclass-of wikidata properties
               ;; and no disambiguation pages
               [:minus [[?item (cat (* :wdt/P31) (+ :wdt/P279)) :wd/Q18616576]
                        [?item :wdt/P31 :wd/Q4167410]]]]
    ;; choose the entity with the most sitelinks on Wikipedia
    :order-by [(desc ?sitelinks)]
    :limit    1})

(defn entity
  [label & criteria]
  (let [[lang label'] (if (map? label) (first label) [*lang* label])]
    (-> (entity-query lang label' criteria) (query) (first) (:item))))

(defn wdt->wd
  [arc]
  (let [prefix (namespace arc)
        id     (name arc)]
    (if (#{"p" "ps" "wdt"} prefix) (keyword "wd" id) arc)))

(defn label-query
  [lang id]
  `{:select [?label]
    :where  [[~(wdt->wd id) :rdfs/label ?label]
             [:filter (= (lang ?label) ~(name lang))]]})

(defn label
  ([id]
   (label *lang* id))
  ([lang id]
   (let [rdfs-label (-> (label-query lang id) (query) (first) (:label))]
     (or rdfs-label id))))

(defn describe-query
  [lang id]
  `{:select [?description]
    :where  [[~(wdt->wd id) :schema/description ?description]
             [:filter (= (lang ?description) ~(name lang))]]})

(defn describe
  ([id]
   (describe *lang* id))
  ([lang id]
   (-> (describe-query lang id) (query) (first) (:description))))

(defn parse-search-result
  [{:keys [:description :id :display] :as _vs}]
  {:id          (keyword "wd" id)
   :description description
   :label       (get-in display [:label :value])})

(defn parse-search-results
  [response]
  (->> (get-in (json/parse-http-response response) [:body :search])
       (map parse-search-result)))

(defn search
  ([text]
   (search *lang* text))
  ([lang text]
   (-> {:method       :get
        :url          "https://www.wikidata.org/w/api.php"
        :query-params {:action   "wbsearchentities"
                       :search   text
                       :language (name lang)
                       :uselang  (name lang)
                       :type     "item"
                       :format   "json"}}
       (hc/request)
       (parse-search-results))))

(defn clojurize-claim
  "Convert the values in `result` to Clojure types."
  [{:keys [datatype] {:keys [value] :as datavalue} :datavalue :as _snak}]
  (condp = datatype
    "monolingualtext"   {(-> value :language keyword) (-> value :text)}
    "wikibase-entityid" (keyword (str "wd/" (-> value :id)))
    "wikibase-item"     (keyword (str "wd/" (-> value :id)))
    "wikibase-lexeme"   (keyword (str "wd/" (-> value :id)))
    "commonsMedia"      value
    "string"            value
    "external-id"       value
    "time"              value
    "url"               value
    "quantity"          {:amount (value :amount)
                         :units  (value :unit)}
    [datatype datavalue]))

(defn clojurize-claims*
  [m [prop snaks]]
  (let [label  (wdt->label (->> prop name (str "wdt/") keyword))
        values (mapv (comp clojurize-claim :mainsnak) snaks)]
    (assoc m label values)))

(defn clojurize-claims
  [entity-data]
  (update entity-data :claims #(reduce clojurize-claims* {} %)))

(defn entity-data-url
  [item-id]
  (str "https://www.wikidata.org/wiki/Special:EntityData/" item-id ".json"))

(defn entity-data
  [item]
  (let [item-id (name item)]
    (-> {:method       :get
         :url          (entity-data-url item-id)
         :query-params {:format "json"}}
        (hc/request)
        (json/parse-http-response)
        (get-in [:body :entities])
        (get (keyword item-id))
        (clojurize-claims))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; wdno = no value!
  (query `{:select [?human ?humanLabel]
           :where  [{?human {:wdt/P31  #{:wd/Q5}
                             :rdf/type #{:wdno/P40}}}]
           :limit  20}))
