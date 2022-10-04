(ns julesratte.page
  (:require
   [clojure.string :as str]
   [julesratte.client :as client]
   [manifold.stream :as s]))

(defn main-namespace?
  [title]
  (not (str/includes? title ":")))

(defn encode-title
  [title]
  (-> title (str/replace " " "_") str/trim))

(def revisions-request
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
  [req]
  (->> (client/requests (merge revisions-request req))
       (s/transform extract-revisions-xf)))

(defn request-by-title
  [req & titles]
  (request-revisions
   (assoc req :titles (into #{} (map encode-title) titles))))

(def max-random-pages-per-request
  50)

(defn request-random
  [req min-n]
  (let [limit-per-req (max 1 (min max-random-pages-per-request min-n))
        max-requests  (Math/ceil (/ min-n limit-per-req))]
    (request-revisions
     (assoc req
            ::client/max-requests max-requests
            :generator            "random"
            :grnnamespace         "0"
            :grnlimit             (str limit-per-req)))))


