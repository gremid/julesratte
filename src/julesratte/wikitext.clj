(ns julesratte.wikitext
  "Wikitext parsing and AST handling."
  (:require [camel-snake-kebab.core :as csk]
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

(def ->keyword
  (memoize csk/->kebab-case-keyword))

(def class->keyword
  (memoize
   (fn [cls]
     (-> cls .getSimpleName
         (str/replace #"^Wt" "")
         (str/replace #"Impl$" "")
         ->keyword))))

;; ## Parsing

(def ^NonExpandingParser parser
  (NonExpandingParser.))

(def ^WikitextNodeFactory node-factory
  (.getNodeFactory ^ParserConfig (.getConfig parser)))

(defn parse ^WtNode
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

(defn tree
  [^WtNode node]
  (tree-seq seq seq node))

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

;; Copyright (c) Chris Houser, April 2008. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(defn right-locs
  "Returns a lazy sequence of locations to the right of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (right-locs (zip/right loc))))))

(defn left-locs
  "Returns a lazy sequence of locations to the left of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (left-locs (zip/left loc))))))

(defn leftmost?
  "Returns true if there are no more nodes to the left of location loc."
  [loc]
  (nil? (zip/left loc)))

(defn rightmost?
  "Returns true if there are no more nodes to the right of location loc."
  [loc]
  (nil? (zip/right loc)))

(defn children
  "Returns a lazy sequence of all immediate children of location loc,
  left-to-right."
  [loc]
  (when (and loc (zip/branch? loc)) (right-locs (zip/down loc))))

(defn descendants'
  "Returns a lazy sequence of all descendants of location loc, in
  depth-first order, left-to-right, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (mapcat descendants' (children loc))))))

(defn subtree
  "All descendants of loc, excluding loc itself."
  [loc]
  (seq (rest (descendants' loc))))

(defn ancestors'
  "Returns a lazy sequence of all ancestors of location loc, starting
  with loc and proceeding to loc's parent node and on through to the
  root of the tree."
  [loc] (lazy-seq (when loc (cons loc (ancestors' (zip/up loc))))))

(defn ancestry
  "All ancestors of loc, excluding loc itself."
  [loc]
  (seq (rest (ancestors' loc))))

(defn prev-locs
  "Returns a lazy sequence of locations preceding and starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (prev-locs (zip/prev loc))))))

(defn next-locs
  "Returns a lazy sequence of locations following and starting with loc."
  [loc]
  (lazy-seq (when (and loc (not (zip/end? loc)))
              (cons loc (next-locs (zip/next loc))))))

(defn zip-node?
  [v]
  (some-> v meta :zip/branch?))

(defn fixup-apply
  "Calls (pred loc), and then converts the result to the 'appropriate'
  sequence."
  [pred loc]
  (let [rtn (pred loc)]
    (cond
      (zip-node? rtn)   (list rtn)
      (= rtn true)      (list loc)
      (= rtn false)     nil
      (nil? rtn)        nil
      (sequential? rtn) rtn
      :else             (list rtn))))

(defn mapcat-chain
  [loc preds mkpred]
  (reduce
   (fn [prevseq expr]
     (mapcat #(fixup-apply (or (mkpred expr) expr) %) prevseq))
   (list loc)
   preds))

(declare nodes->)

(defn class=
  "Predicate based on the AST node's class."
  [clz]
  (fn [loc] (when (instance? clz (zip/node loc)) (list loc))))

(defn child-class=
  [clz]
  (let [class= (class= clz)]
    (fn [loc]
      (filter #(and (zip/branch? %) (class= %)) (some-> loc children)))))

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
  (mapcat-chain loc preds
                #(cond (keyword? %) (child-class= (keyword->class %))
                       (class? %)   (child-class= %)
                       (string? %)  (text= %)
                       (vector? %)  (seq-test %))))

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
   (child-class= (keyword->class :section))))

(defprotocol AsNode
  (->node [this]))

(def prop->attr
  {"content"           ""
   "entityMap"         ""
   "precededByNewline" "pre-nl"
   "rtd"               ""
   "warnings"          ""})

(defn ast->node
  [ast-node]
  (let [props (.propertyIterator ast-node)
        attrs (loop [attrs {}]
                (if (.next props)
                  (recur
                   (let [k (.getName props)
                         k (not-empty (get prop->attr k k))
                         v (not-empty (str (.getValue props)))]
                     (cond-> attrs (and k v) (assoc (->keyword k) v))))
                  attrs))]
    (cond-> {:tag (class->keyword (class ast-node))}
      (seq attrs) (assoc :attrs attrs))))

(extend-protocol AsNode
  org.sweble.wikitext.parser.nodes.WtIgnored
  (->node [_this] "")

  org.sweble.wikitext.parser.nodes.WtInternalLink
  (->node [this]
    (let [target (->node (. this (getTarget)))]
      {:tag     :link
       :attrs   {:page target}
       :content (if (.hasTitle this)
                  (into [] (map ->node) (.getTitle this))
                  [target])}))

  org.sweble.wikitext.parser.nodes.WtName
  (->node [this]
    (if (.isResolved this) (.getAsString this) ""))

  org.sweble.wikitext.parser.nodes.WtExternalLink
  (->node [this]
    (let [target (->node (. this (getTarget)))]
      {:tag     :url
       :attrs   {:href target}
       :content (if (.hasTitle this)
                  (into [] (map ->node) (.getTitle this))
                  [target])}))

  org.sweble.wikitext.parser.nodes.WtUrl
  (->node [this]
    (str (.getProtocol this) ":" (.getPath this)))

  org.sweble.wikitext.parser.nodes.WtTagExtension
  (->node [this]
    (cond-> {:tag   (->keyword (.getName this))
             :attrs (reduce
                     (fn [m ^org.sweble.wikitext.parser.nodes.WtXmlAttribute xa]
                       (cond-> m
                         (.hasValue xa)
                         (assoc (->keyword (.. xa (getName) (getAsString)))
                                (.getValue xa))))
                     {}
                     (.getXmlAttributes this))}
      (.hasBody this) (assoc :content [(->node (.getBody this))])))

  org.sweble.wikitext.parser.nodes.WtTagExtensionBody
  (->node [this]
    {:tag     :<c
     :content [(. this (getContent))]})
  
  org.sweble.wikitext.parser.nodes.WtTemplate
  (->node [this]
    (let [args (seq (. this (getArgs)))]
      (cond->
          (->(ast->node this)
             (assoc :tag :tpl)
             (assoc-in [:attrs :name] (str/trim (.. this (getName) (getAsString)))))
        args (assoc :content (into [] (map ->node args))))))

  org.sweble.wikitext.parser.nodes.WtTemplateArgument
  (->node [this]
    (cond-> {:tag     :tpl-arg
             :content (into [] (map ->node) (. this (getValue)))}
      (. this (hasName)) (assoc :attrs {:name (->node (. this (getName)))})))

  org.sweble.wikitext.parser.nodes.WtText
  (->node [this]
    (.getContent this))

  org.sweble.wikitext.parser.nodes.WtXmlEntityRef
  (->node [this]
    (.getResolved this))

  org.sweble.wikitext.parser.nodes.WtStringNode
  (->node [this]
    (let [content (.getContent this)]
      (cond-> (ast->node this)
        (not-empty content) (assoc :content [content]))))

  org.sweble.wikitext.parser.nodes.WtInnerNode1
  (->node [this]
    (assoc (ast->node this) :content [(->node (.get this 0))]))

  org.sweble.wikitext.parser.nodes.WtInnerNode2
  (->node [this]
    (assoc (ast->node this) :content [(->node (.get this 0))
                                      (->node (.get this 1))]))

  org.sweble.wikitext.parser.nodes.WtInnerNode3
  (->node [this]
    (assoc (ast->node this) :content [(->node (.get this 0))
                                      (->node (.get this 1))
                                      (->node (.get this 2))]))

  org.sweble.wikitext.parser.nodes.WtContentNode
  (->node [this]
    (cond-> (ast->node this)
      (seq this) (assoc :content (into [] (map ->node) this))))

  org.sweble.wikitext.parser.nodes.WtLeafNode
  (->node [this]
    (ast->node this)))
