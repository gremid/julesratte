# julesratte – a Clojure MediaWiki client

<img src="doc/julesratte_small.png"
 alt="Jules Ratte (by Klaus Ensikat)"
 title="Jules Ratte (by Klaus Ensikat)"
 align="right" />

> […] Und ließ die Lösung aller Fragen/ Sich heimlich von der Ratte sagen/ Und schrieb nie Aufsatz noch Diktat/ Ohne die Ratte und ihren Rat. […]

_– Peter Hacks: Jules Ratte. In: ders. Kinderkurzweil. Berlin. 1986._

This library provides **API access to MediaWiki-based sites**, mainly those
comprising the Wikipedia/Wikimedia landscape.

## Features

* Stream processing of [Wikimedia dumps](https://dumps.wikimedia.org/)
* [Asynchronous HTTP client](https://github.com/gnarroway/hato) for the
  [MediaWiki API](https://www.mediawiki.org/wiki/API:Main_page/en), including
  support for continuation requests (paged results) and request throttling for
  proper bot behavior
* Parsing of MediaWiki markup into Clojure data structures via
  [Sweble](https://en.wikipedia.org/wiki/Sweble)

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
   [manifold.stream :as s]))

(let [pages (page/request-by-title
              (client/base-request "de.wiktionary.org")
              "Pfirsich" "Apfel" "Birnen" "Erdbeeren")]
  (into []
        (map (juxt :title :user :timestamp (comp #(subs % 0 20) :text)))
        (s/stream->seq pages)))
;; => [["Apfel" "UT-Bot" "2022-08-17T10:46:42Z" "{{Siehe auch|[[apfel"]
;;     ["Pfirsich" "LarsvonSpeck" "2022-04-10T14:53:19Z" "== Pfirsich ({{Sprac"]
;;     ["Birnen" "BetterkBot" "2017-05-16T16:49:49Z" "== Birnen ({{Sprache"]
;;     ["Erdbeeren" "DerbethBot" "2022-01-29T09:40:52Z" "== Erdbeeren ({{Spra"]]


(let [pages (page/request-random (client/base-request "de.wikipedia.org") 5)]
  (into []
        (map (juxt :title :user :timestamp (comp #(subs % 0 20) :text)))
        (s/stream->seq pages)))
;; => [["Gruppe (Feuerwehr)"
;;      "2A02:6D40:309E:8101:209A:8BBC:B269:46E6"
;;      "2022-02-16T17:52:51Z"
;;      "Innerhalb der [[Feue"]
;;     ["Badminton Asia Confederation"
;;      "Florentyna"
;;      "2022-07-05T05:50:24Z"
;;      "Die '''Badminton Asi"]
;;     ["Roger Hume" "WKP-Benson" "2022-01-10T11:34:14Z" "'''Roger Hume''' (* "]
;;     ["Tempo (Brauerei)"
;;      "Chemiewikibm"
;;      "2022-07-12T13:10:56Z"
;;      "[[Datei:Makabi beer."]
;;     ["Brzeziński (Adelsgeschlecht)"
;;      "Drc4891"
;;      "2022-08-10T13:28:11Z"
;;      "'''Brzeziński''' (au"]]

(with-open [stream     (dump/->input-stream "dewiktionary")
            xml-events (dump/xml-event-reader stream)]
  (into
   []
   (comp
    (take 10)
    (map (juxt :title :username :timestamp)))
   (dump/parse-revisions xml-events)))
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
```
## License

Copyright 2022 Gregor Middell.

This project is licensed under the GNU General Public License v3.0.
