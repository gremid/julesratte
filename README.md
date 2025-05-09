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
   [julesratte.client :as jr]
   [julesratte.dump :as jr.dump]
   [julesratte.page :as jr.page]
   [julesratte.wikidata :as jr.wd]))

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

(jr.wd/describe (jr.wd/entity "Peter Hacks"))

;; => "German writer (1928–2003)"
```
## Contributors

* Gregor Middell, main author
* Jack Rusher, author of
  [Mundaneum](https://github.com/jackrusher/mundaneum), a thin Clojure
  wrapper around Wikidata, that served as the basis for Wikidata
  support in this library

## License

This project is licensed under the GNU General Public License v3.0.
