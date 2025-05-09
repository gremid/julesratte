(ns julesratte.page
  (:require
   [clojure.string :as str]
   [julesratte.client :as jr]))

(defn main-namespace?
  [title]
  (not (str/includes? title ":")))

(defn encode-title
  [title]
  (-> title (str/replace " " "_") str/trim))

(defn extract-revision
  [{:keys [title] [revision] :revisions}]
  {:title     title
   :user      (:user revision)
   :timestamp (:timestamp revision)
   :text      (get-in revision [:slots :main :content])})

(defn extract-revisions
  [{{{:keys [pages]} :query} :body}]
  (map extract-revision pages))

(defn request-revisions
  [url params num-requests]
  (->> (jr/requests! url
                     (merge {:prop    "revisions"
                             :rvprop  ["user" "timestamp" "content"]
                             :rvslots "*"}
                            params)
                     num-requests)
       (sequence (comp
                  (mapcat extract-revisions)
                  (filter (every-pred :title :text))))))

(defn request-by-title
  [url & titles]
  (request-revisions
   url {:titles (into #{} (map encode-title) titles)} (count titles)))

(defn request-random
  [url min-n]
  (let [limit-per-req (max 1 (min 50 min-n))
        max-requests  (Math/ceil (/ min-n limit-per-req))]
    (request-revisions url
                       {:generator    "random"
                        :grnnamespace "0"
                        :grnlimit     (str limit-per-req)}
                       max-requests)))
