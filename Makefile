SHELL := /bin/bash

all: src/julesratte/wikidata/properties.json
src/julesratte/wikidata/properties.json:
	docker run --rm -it maxlath/wikibase-cli props -e https://query.wikidata.org/sparql >$@

all: resources/julesratte/wikidata/lexemes.json.gz
resources/julesratte/wikidata/lexemes.json.gz:
	curl -o $@ https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.json.gz

all: resources/julesratte/wiktionary/de.edn
resources/julesratte/wiktionary/de.edn:
	mkdir -p `dirname $@`
	clojure -M -m julesratte.wiktionary.de >$@
