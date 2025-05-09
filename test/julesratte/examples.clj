(ns julesratte.examples
  (:require
   [julesratte.client :as jr]
   [julesratte.dump :as jr.dump]
   [julesratte.page :as jr.page]
   [julesratte.wikibase :as jr.wb]))

(->> (jr.page/request-by-title (jr/api-url "de.wiktionary.org")
                               "Pfirsich" "Kirsche" "Birnen" "Erdbeeren")
     (into [] (map (juxt :title :user :timestamp (comp #(subs % 0 5) :text)))))

;; => [["Kirsche" "Master of Contributions" "2024-11-26T21:04:56Z" "== Ki"]
;;     ["Pfirsich" "Master of Contributions" "2025-01-24T17:51:48Z" "== Pf"]
;;     ["Birnen" "Instance of Bot" "2025-01-25T15:47:50Z" "== Bi"]
;;     ["Erdbeeren" "DerbethBot" "2022-01-29T09:40:52Z" "== Er"]]

(->> (jr.page/request-random (jr/api-url "de.wikipedia.org") 5)
     (into [] (map (juxt :title :user :timestamp))))

;; => [["Konstantin Neven DuMont" "InternetArchiveBot" "2022-03-10T11:33:14Z"]
;;     ["Ottmar Fuchs" "MikerosIBK" "2025-05-02T20:28:04Z"]
;;     ["John Lloyd Dorsey" "APPERbot" "2021-03-01T23:18:23Z"]
;;     ["Liste der Monuments historiques in Ennery" "Balticbuchonia" "2022-10-24T14:56:04Z"]
;;     ["Platygastridae" "Slimguy" "2025-02-14T22:01:08Z"]]

(with-open [stream     (jr.dump/->input-stream "dewiktionary")
            xml-events (jr.dump/xml-event-reader stream)]
  (->> (jr.dump/parse-revisions xml-events)
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

(jr.wb/describe (jr.wb/entity "Peter Hacks"))

;; => "German writer (1928–2003)"
