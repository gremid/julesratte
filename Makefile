SHELL := /bin/bash

all: src/julesratte/wikidata/properties.json
src/julesratte/wikidata/properties.json:
	clojure -X julesratte.wikidata.properties/download!

all: resources/julesratte/wikidata/lexemes.json.gz
resources/julesratte/wikidata/lexemes.json.gz:
	mkdir -p `dirname $@`
	curl -o $@ https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz


all: resources/julesratte/wiktionary/de.xml
resources/julesratte/wiktionary/de.xml:
	mkdir -p `dirname $@`
	curl https://dumps.wikimedia.org/dewiktionary/latest/dewiktionary-latest-pages-meta-current.xml.bz2 | bzcat >$@

all: resources/julesratte/wiktionary/de.edn
resources/julesratte/wiktionary/de.edn: resources/julesratte/wiktionary/de.xml
	mkdir -p `dirname $@`
	clojure -M -m julesratte.wiktionary.de >$@
