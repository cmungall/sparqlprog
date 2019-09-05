# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

#PACKNAME=sparkle
#include ../Makefile.inc

SWIPL = swipl -p library=prolog

all: test

check:
install:
clean:

test:
	echo "skipping tests. make realtest for actual tests"

realtest:
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

bigtest:
	$(SWIPL) -l tests/bigtests.pl -g run_tests,halt

coverage:
	$(SWIPL) -l tests/bigtests.pl -l tests/tests.pl -g "show_coverage(run_tests),halt"

t-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt



# --------------------
# Schemas
# --------------------
biopax-level3.owl:
	wget http://www.biopax.org/release/biopax-level3.owl -O $@


# --------------------
# Run pengine
# --------------------

# TODO: get from pack
VERSION = v$(shell swipl -l pack.pl -g "version(V),writeln(V),halt.")

show-version:
	echo $(VERSION)

IM = cmungall/sparqlprog

pe: pe-clean pe-build pe-run

pe-clean:
	docker kill $(IM) || echo not running ;
	docker rm $(IM) || echo not made 

pe-build:
	(test -f tests/out.tmp && rm tests/out.tmp || echo ok) && \
	docker build -t $(IM):$(VERSION) . \
	&& docker tag $(IM):$(VERSION) $(IM):latest


pe-run:
	docker run -p 9083:9083 -e PORT=9083 --name sparqlprog $(IM)

pe-run-bg:
	docker run -p 9083:9083 -e PORT=9083 --name sparqlprog $(IM) &

pe-test: pe-run-bg
	sleep 3 ;\
	python3 ../sparqlprog-python/sparqlprog.py -u http://localhost:9083 -e wd "continent(X)" ;\
	docker kill sparqlprog ;\
	docker rm sparqlprog

pe-publish: pe-build
	@docker push $(IM):$(VERSION) \
	&& docker push $(IM):latest

# First requires: heroku container:login
heroku-new-app:
	heroku create

heroku-deploy:
	heroku container:push web &&\
	heroku logs

# --------------------
# Run pldoc server in Docker
# --------------------

# after running, connect to 
# http://localhost:4000/pldoc/doc/work/prolog/sparqlprog.pl
pldoc-run:
	docker run -v $(PWD)/:/work/ -w /work --name pldoc -p 4000:4000 -e PORT=4000 -it cmungall/sparqlprog swipl -p library=prolog -g [debug] && \
	echo "server started, see http://localhost:4000/pldoc/"

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

