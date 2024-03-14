(ns julesratte.json
  (:require [jsonista.core :as json]
            [clojure.string :as str]))

(defn read-value
  [v]
  (json/read-value v json/keyword-keys-object-mapper))

(defn write-value
  [v]
  (json/write-value-as-string v json/keyword-keys-object-mapper))

(defn json-http-response?
  [{:keys [content-type]}]
  (str/includes? (name content-type) "json"))

(defn parse-http-response
  [response]
  (try
    (cond-> response (json-http-response? response) (update :body read-value))
    (catch Throwable t
      (throw (ex-info "Error parsing JSON response" response t)))))
