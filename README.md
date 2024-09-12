# julesratte – a MediaWiki/Wikibase client

<img src="doc/julesratte_small.png"
 alt="Jules Ratte (by Klaus Ensikat)"
 title="Jules Ratte (by Klaus Ensikat)"
 align="right" />

> […] Und ließ die Lösung aller Fragen/ Sich heimlich von der Ratte sagen/ Und schrieb nie Aufsatz noch Diktat/ Ohne die Ratte und ihren Rat. […]

_– Peter Hacks: Jules Ratte. In: ders. Kinderkurzweil. Berlin. 1986._

This library provides **API access to MediaWiki-based sites**, mainly those
comprising the Wikidata/Wikipedia/Wikimedia landscape.

## Features

* Stream processing of [Wikimedia dumps](https://dumps.wikimedia.org/)
* [HTTP client](https://github.com/gnarroway/hato) for the
  [MediaWiki API](https://www.mediawiki.org/wiki/API:Main_page/en), including
  support for continuation requests (paged results) and request throttling for
  proper bot behavior
* Parsing of MediaWiki markup into Clojure data structures via
  [Sweble](https://en.wikipedia.org/wiki/Sweble)
* Access to Wikidata via [Wikidata Query Service](https://query.wikidata.org/)

## Installation

Add the following to your `deps.edn` project file:

```clojure
io.github.gremid/julesratte {:git/sha "..."}
```

## Usage

```clojure
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
```
## Contributors

* Gregor Middell, main author
* Jack Rusher, author of
  [Mundaneum](https://github.com/jackrusher/mundaneum), a thin Clojure
  wrapper around Wikidata, that served as the basis for Wikidata
  support in this library

## License

This project is licensed under the GNU General Public License v3.0.
