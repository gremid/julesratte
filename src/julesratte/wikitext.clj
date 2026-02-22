(ns julesratte.wikitext
  "Wikitext parsing and AST handling."
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.string :as str])
  (:import
   (java.io Writer)
   (org.sweble.wikitext.parser.nodes WtNode WtXmlAttribute)
   (org.sweble.wikitext.parser.utils NonExpandingParser WtRtDataPrinter)))

(def ->keyword
  (memoize csk/->kebab-case-keyword))

(def class->keyword
  (memoize
   (fn [^Class cls]
     (-> cls .getSimpleName
         (str/replace #"^Wt" "")
         (str/replace #"Impl$" "")
         ->keyword))))

(def ^NonExpandingParser parser
  (NonExpandingParser.))

(defn parse ^WtNode
  [^String content]
  (.parseArticle parser content ""))

(defmethod print-method WtNode
  [^WtNode v ^Writer w]
  (WtRtDataPrinter/print w v))

(defprotocol AsNode
  (->node [this]))

(def prop->attr
  {"content"           ""
   "entityMap"         ""
   "precededByNewline" "pre-nl"
   "rtd"               ""
   "warnings"          ""})

(defn ast->node
  [^de.fau.cs.osr.ptk.common.ast.AstNode ast-node]
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
    (let [xml-attrs (->> (.getXmlAttributes this)
                         (filter #(instance? WtXmlAttribute %)))]
      (cond-> {:tag   (->keyword (.getName this))
               :attrs (reduce
                       (fn [m ^WtXmlAttribute xa]
                         (cond-> m
                           (.hasValue xa)
                           (assoc (->keyword (.. xa (getName) (getAsString)))
                                  (.getValue xa))))
                       {} xml-attrs)}
        (.hasBody this) (assoc :content [(->node (.getBody this))]))))

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
