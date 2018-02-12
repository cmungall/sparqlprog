# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

#PACKNAME=sparkle
#include ../Makefile.inc

SWIPL = swipl  -L0 -G0 -T0  -p library=prolog
test:
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

bigtest:
	$(SWIPL) -l tests/bigtests.pl -g run_tests,halt

coverage:
	$(SWIPL) -l tests/bigtests.pl -l tests/tests.pl -g "show_coverage(run_tests),halt"

t-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt

# --------------------
# Run SPARQL service inside Docker
# --------------------
BGVERSION = 2.1.4
bg: bg-run pause bg-load

bg-data: examples/load-data/goslim_generic.ttl

examples/load-data:
	mkdir $@
examples/load-data/%.ttl: examples/data/%.ttl.gz examples/load-data 
	gzip -dc $< > $@

bg-run:
	docker run --name blazegraph -d -p 8889:8080 -v $(PWD)/examples/RWStore.properties:/RWStore.properties -v $(PWD)/examples/load-data/:/data lyrasis/blazegraph:$(BGVERSION)


pause:
	sleep 1

bg-load: bg-data
	curl -X POST --data-binary @examples/dataloader.txt   --header 'Content-Type:text/plain'   http://localhost:8889/bigdata/dataloader

bg-stop:
	docker kill blazegraph; docker rm blazegraph

