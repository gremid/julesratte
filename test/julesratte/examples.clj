(ns julesratte.examples
  (:require
   [julesratte.client :as client]
   [julesratte.dump :as dump]
   [julesratte.page :as page]
   [julesratte.wikidata :as wd]))

(->> (page/request-by-title (client/api-endpoint "de.wiktionary.org")
                            "Pfirsich" "Kirsche" "Birnen" "Erdbeeren")
     (into [] (map (juxt :title :user :timestamp (comp #(subs % 0 5) :text)))))

;; => [["Kirsche" "EPIC" "2024-02-01T10:18:54Z" "== Ki"]
;;     ["Pfirsich" "Dr. Karl-Heinz Best" "2023-09-21T09:21:06Z" "== Pf"]
;;     ["Birnen" "BetterkBot" "2017-05-16T16:49:49Z" "== Bi"]
;;     ["Erdbeeren" "DerbethBot" "2022-01-29T09:40:52Z" "== Er"]]


(->> (page/request-random (client/api-endpoint "de.wikipedia.org") 5)
     (into [] (map (juxt :title :user :timestamp))))

;; => [["Bovines Leukämie-Virus" "InternetArchiveBot" "2023-06-18T06:22:54Z"]
;;     ["Jos Murer" "Bph" "2024-02-02T12:56:49Z"]
;;     ["Distrikt Santa Rosa de Quives" "Aka" "2024-01-22T11:54:41Z"]
;;     ["Zinédine Ould Khaled" "Phzh" "2024-02-03T15:15:19Z"]
;;     ["Mōkihinui River" "Du Hugin Skulblaka" "2024-01-29T11:04:59Z"]]

(with-open [stream     (dump/->input-stream "dewiktionary")
            xml-events (dump/xml-event-reader stream)]
  (->> (dump/parse-revisions xml-events)
       (into [] (comp (take 10) (map (juxt :title :username :timestamp))))))

;; => [["MediaWiki:Linktrail" "SteffenB" "2004-06-05T22:14:21Z"]
;;     ["MediaWiki:Mainpage" "Thogo" "2007-11-07T21:10:09Z"]
;;     ["MediaWiki:Aboutpage" "Pajz" "2006-08-23T15:20:06Z"]
;;     ["MediaWiki:Helppage" "Pajz" "2006-05-08T14:21:46Z"]
;;     ["MediaWiki:Wikititlesuffix" "Melancholie" "2005-01-08T04:45:01Z"]
;;     ["MediaWiki:Bugreports" "Elian" "2004-03-27T13:51:39Z"]
;;     ["MediaWiki:Bugreportspage" "Elian" "2004-03-27T13:51:29Z"]
;;     ["MediaWiki:Sitesupport" "Melancholie" "2005-12-28T04:21:16Z"]
;;     ["MediaWiki:Faqpage" "Melancholie" "2004-11-29T22:03:13Z"]
;;     ["MediaWiki:Edithelppage" "Melancholie" "2004-11-29T21:54:46Z"]]

(wd/describe (wd/entity "Wikidata"))

;; => {:xml:lang "en",
;;     :value
;;     "free knowledge graph hosted by Wikimedia and edited by volunteers",
;;     :type "literal"}
