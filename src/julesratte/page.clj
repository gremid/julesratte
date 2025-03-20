(ns julesratte.page
  (:require
   [clojure.string :as str]
   [julesratte.client :as client]))

(defn main-namespace?
  [title]
  (not (str/includes? title ":")))

(defn encode-title
  [title]
  (-> title (str/replace " " "_") str/trim))

(def revisions-request-params
  {:prop    "revisions"
   :rvprop  ["user" "timestamp" "content"]
   :rvslots "*"})

(defn extract-revision
  [{:keys [title] [revision] :revisions}]
  {:title     title
   :user      (:user revision)
   :timestamp (:timestamp revision)
   :text      (get-in revision [:slots :main :content])})

(defn extract-revisions
  [{{{:keys [pages]} :query} :body}]
  (map extract-revision pages))

(def extract-revisions-xf
  (comp
   (mapcat extract-revisions)
   (filter (every-pred :title :text))))

(defn request-revisions
  [url params]
  (as-> params $
    (merge revisions-request-params $)
    (client/request-with-params $)
    (assoc $ :url url)
    (client/requests! $)
    (sequence extract-revisions-xf $)))

(defn request-by-title
  [url & titles]
  (request-revisions url {:titles (into #{} (map encode-title) titles)}))

(def max-random-pages-per-request
  50)

(defn request-random
  [url min-n]
  (let [limit-per-req (max 1 (min max-random-pages-per-request min-n))
        max-requests  (Math/ceil (/ min-n limit-per-req))]
    (binding [client/*max-requests* max-requests]
      (request-revisions url
                         {:generator    "random"
                          :grnnamespace "0"
                          :grnlimit     (str limit-per-req)}))))
