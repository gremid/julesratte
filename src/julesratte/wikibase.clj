(ns julesratte.wikibase
  (:require
   [clojure.walk]
   [com.yetanalytics.flint :as f]
   [hato.client :as hc]
   [julesratte.json :as jr.json]))

(defn prefixes
  [ns]
  {:bd       "<http://www.bigdata.com/rdf#>"
   :cc       "<http://creativecommons.org/ns#>"
   :dct      "<http://purl.org/dc/terms/>"
   :geo      "<http://www.opengis.net/ont/geosparql#>"
   :hint     "<http://www.bigdata.com/queryHints#>"
   :ontolex  "<http://www.w3.org/ns/lemon/ontolex#>"
   :owl      "<http://www.w3.org/2002/07/owl#>"
   :prov     "<http://www.w3.org/ns/prov#>"
   :rdf      "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
   :rdfs     "<http://www.w3.org/2000/01/rdf-schema#>"
   :schema   "<http://schema.org/>"
   :skos     "<http://www.w3.org/2004/02/skos/core#>"
   :wikibase "<http://wikiba.se/ontology#>"
   :xsd      "<http://www.w3.org/2001/XMLSchema#>"
   :e        (str "<" ns "entity/>")
   :es       (str "<" ns "entity/statement/>")
   :p        (str "<" ns "prop/>")
   :pd       (str "<" ns "prop/direct/>")
   :pdn      (str "<" ns "prop/direct-normalized/>")
   :pno      (str "<" ns "prop/novalue/>")
   :pq       (str "<" ns "prop/qualifier/>")
   :pqn      (str "<" ns "prop/qualifier/value-normalized/>")
   :pqv      (str "<" ns "prop/qualifier/value/>")
   :pr       (str "<" ns "prop/reference/>")
   :prn      (str "<" ns "prop/reference/value-normalized/>")
   :prv      (str "<" ns "prop/reference/value/>")
   :ps       (str "<" ns "prop/statement/>")
   :psn      (str "<" ns "prop/statement/value-normalized/>")
   :psv      (str "<" ns "prop/statement/value/>")
   :ref      (str "<" ns "reference/>")
   :v        (str "<" ns "value/>")})

(def ^:dynamic *query-api-url*
  "https://query.wikidata.org/sparql")

(def ^:dynamic *prefixes*
  (prefixes "http://www.wikidata.org/"))

(def ^:dynamic *lang*
  "en")

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
   (update :prefixes merge *prefixes*)
   (update :where conj [:service :wikibase/label
                        [[:bd/serviceParam :wikibase/language
                          (str "[AUTO_LANGUAGE]," *lang*)]]])
   (f/format-query)))

(defn clojurize-query-result-value
  [{:keys [type value datatype] :as v}]
  (condp = type
    "literal" (condp = datatype
                "http://www.w3.org/2001/XMLSchema#decimal"  (parse-double value)
                "http://www.w3.org/2001/XMLSchema#integer"  (parse-long value)
                "http://www.w3.org/2001/XMLSchema#dateTime" value
                value)
    "uri"     (str "<" value ">")
    v))

(defn clojurize-query-result
  [{{{:keys [bindings]} :results} :body}]
  (mapv
   #(reduce-kv
     (fn [m k v] (assoc m k (clojurize-query-result-value v)))
     {} %)
   bindings))

(defn query
  ([q]
   (->>
    (format-query q)
    (assoc-in {:method       :get
               :url          *query-api-url*
               :query-params {:format "json"}}
              [:query-params :query])
    (hc/request)
    (jr.json/parse-http-response)
    (clojurize-query-result))))

(defn entity
  [label & criteria]
  (->>
   `{:select [?item]
     :where  [[?item :rdfs/label {~(keyword *lang*) ~label}]
              ~@(mapv (fn [[p e]] `[?item ~p ~e]) (partition 2 criteria))]
     :limit  1}
   (query) (first) (:item)))

(defn label
  [id]
  (->>
   `{:select [?label]
     :where  [[~id :rdfs/label ?label]
              [:filter (= (lang ?label) ~*lang*)]]}
   (query) (first) (:label) (or id)))

(defn describe
  [id]
  (->>
   `{:select [?description]
     :where  [[~id :schema/description ?description]
              [:filter (= (lang ?description) ~*lang*)]]}
   (query) (first) (:description)))
