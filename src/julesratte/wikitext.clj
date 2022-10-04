(ns julesratte.wikitext
  "Wikitext parsing and AST handling."
  (:require [camel-snake-kebab.core :as csk]
            [clojure.data.zip :as dz]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import java.io.Writer
           [org.sweble.wikitext.parser.nodes WikitextNodeFactory WtExternalLink WtInternalLink WtNode WtSection WtTemplate WtTemplateArgument WtText WtUrl]
           org.sweble.wikitext.parser.ParserConfig
           [org.sweble.wikitext.parser.utils NonExpandingParser WtRtDataPrinter]))

(def node-package-name
  (.getName (.getPackage WtNode)))

(def keyword->class
  (memoize
   #(Class/forName (str node-package-name ".Wt" (csk/->PascalCaseString %)))))

(def class->keyword
  (memoize
   (fn [cls]
     (-> cls .getSimpleName
         (str/replace #"^Wt" "")
         (str/replace #"Impl$" "")
         (csk/->kebab-case-keyword)))))

(defn node->sexp
  [n]
  (vec
   (list*
    (class->keyword (class n))
    (if-let [children (seq n)] (map node->sexp children) (list (pr-str n))))))

;; ## Parsing

(def ^NonExpandingParser parser
  (NonExpandingParser.))

(def ^WikitextNodeFactory node-factory
  (.getNodeFactory ^ParserConfig (.getConfig parser)))

(defn ^WtNode parse
  [^String content]
  (.parseArticle parser content ""))

;; ## Debug output of ASTs (print and dump)

(defmethod print-method WtNode
  [^WtNode v ^Writer w]
  (WtRtDataPrinter/print w v))

(declare text)

;; ## AST zippers

(defn- unsupported-make-node
  [node children]
  (throw (ex-info "Cannot modify Wikitext AST"
                  {:node node :children children})))

(defn zipper
  "A read-only zipper for the given Wikitext node."
  [^WtNode node]
  (zip/zipper seq seq unsupported-make-node node))


;; ## Textual content of AST nodes

(defmulti text class)

(defmethod text WtUrl
  [^WtUrl n]
  (str (.getProtocol n) ":" (.getPath n)))

(defmethod text WtText
  [^WtText n]
  (.getContent n))

(defmethod text WtTemplate
  [^WtTemplate t]
  (->>
   [(not-empty (text (.getName t)))
    (not-empty (str/join ", " (map text (.getArgs t))))]
   (remove nil?)
   (str/join "|")))

(defmethod text WtTemplateArgument
  [^WtTemplateArgument arg]
  (->> [(.getName arg) (.getValue arg)]
       (map text)
       (remove empty?)
       (str/join ": ")))

(defmethod text WtExternalLink
  [^WtExternalLink n]
  (text (if (.hasTitle n) (.getTitle n) (.getTarget n))))

(defmethod text WtInternalLink
  [^WtInternalLink n]
  (let [t (text (if (.hasTitle n) (.getTitle n) (.getTarget n)))
        pre (.getPrefix n)
        post (.getPostfix n)]
    (str pre t post)))

(defmethod text WtNode
  [^WtNode n]
  (apply str (map text n)))

;; ## Zipper-based navigation of ASTs

(declare nodes->)

(defn class=
  "Predicate based on the AST node's class."
  [clz]
  (fn [loc]
    (filter #(instance? clz (zip/node %))
            (if (dz/auto? loc)
              (dz/children-auto loc)
              (list (dz/auto true loc))))))

(defn text=
  "Predicate based on the AST node's textual content."
  [s]
  (fn [loc]
    (= s (-> loc zip/node text))))

(defn- seq-test
  [preds]
  (fn [loc]
    (and (seq (apply nodes-> loc preds)) (list loc))))

(defn nodes->
  [loc & preds]
  (dz/mapcat-chain loc preds
                   #(cond (keyword? %) (class= (keyword->class %))
                          (class? %) (class= %)
                          (string? %) (text= %)
                          (vector? %) (seq-test %))))

(defn node->
  [loc & preds]
  (first (apply nodes-> loc preds)))

;; ## Node functions (text content, key-value access)

(defn text-content
  "The trimmed text content of a WikiText node"
  [loc]
  (-> loc zip/node text str/trim not-empty))

(defn template-name
  [loc]
  (node-> loc :name text-content))

(defn template-values
  "Extracts the argument values of a template node"
  [loc]
  (nodes-> loc :template-arguments :template-argument :value text-content))

(defn section-level
  [level]
  (comp
   (partial filter #(= level (.getLevel ^WtSection (zip/node %))))
   (class= (keyword->class :section))))
