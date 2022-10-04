(ns user
  (:require [taoensso.timbre :as log]))

(defn trace-requests!
  []
  (log/merge-config! {:min-level :trace}))
