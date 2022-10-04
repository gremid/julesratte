(ns julesratte.dump-test
  (:require
   [clojure.test :refer [deftest is]]
   [julesratte.dump :as dump]))

(deftest retrieve-dump
  (with-open [stream     (dump/->input-stream "dewiktionary")
              xml-events (dump/xml-event-reader stream)]
    (let [revisions (dump/parse-revisions xml-events)
          revisions (into [] (take 10) revisions)]
      (is (every? :title revisions)))))
