# --------------------
# SIMPLE LOCAL SPARQL TESTS
# --------------------
# This assumes you have a SPARQL endpoint running with the provided sample triples.
# Just do this:
# 
#   make bg
#
# From the top-level directory (docker required). This will
#  1. Start a blazegraph instance
#  2. Load that instance with triples from examples/data/
#
# - The server will be at http://127.0.0.1:8889/bigdata
# - The endpoint will be http://127.0.0.1:8889/bigdata/sparql
# - The sparqlprog default endpoints includes a shortcut 'local' for this URL.
#   We use this here

# ---
# basic query; all subclass axioms
# ---
pl2sparql -s local "subClassOf(X,Y),label(X,XL),label(Y,YL)"

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0040007,http://purl.obolibrary.org/obo/GO_0008150,growth,biological_process
#    http://purl.obolibrary.org/obo/GO_0040011,http://purl.obolibrary.org/obo/GO_0008150,locomotion,biological_process
#    ...

# ---
# compile query; all subclass axioms
# ---
pl2sparql -C "subClassOf(X,Y)"

# Expected results:
#    SELECT ?x ?y WHERE {?x <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?y}

# ---
# direct superclasses of nucleus
# ---
pl2sparql -s local 'label(N,"nucleus"),subClassOf(N,X),label(X,XL)' X-XL

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0043226,organelle

# convenience method: requires pack_install(obo_core) to fetch goslim module
pl2sparql -u obo_core/goslim -s local "nucleus_iri(N),subClassOf(N,X),label(X,XL)" X-XL

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0043226,organelle

# ---
# all superclasses of nucleus
# ---
pl2sparql -u obo_core/goslim -s local "nucleus_iri(N),rdfs_subclass_of(N,X),label(X,XL)" "s(X,XL)"

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0005575,cellular_component
#    http://purl.obolibrary.org/obo/GO_0043226,organelle

# ---
# what is the nucleus part of?
# ---
pl2sparql -u obo_core/goslim -s local "nucleus_iri(N),rdfs_subclass_of(N,Restr),owl_some(Restr,R,Y),label(Y,YL)" "s(Y,YL)"

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0005622,intracellular

# ---
# synonyms
# ---
# this requires pack_install(obo_metadata) to fetch oio
# basic query for all exact synonyms
pl2sparql -u obo_metadata/oio -s local "has_exact_synonym_axiom(C,Syn,A)"

# ---
# synonyms + axiom annotations
# ---
# in ontologies that use the oio modeling approach, synonym axioms may be decorated with
# axiom annotations providing additional info about each synonym assignment, including source (hasDbXref)
pl2sparql -u obo_metadata/oio -s local "has_exact_synonym_axiom(C,Syn,A),has_dbxref(A,X)" "s(C,Syn,X)"

# Expected results:
#    http://purl.obolibrary.org/obo/GO_0007005,mitochondrion organisation,GOC:mah
#    http://purl.obolibrary.org/obo/GO_0016887,adenosine triphosphatase activity,EC:3.6.1.3
#    http://purl.obolibrary.org/obo/GO_0006520,amino acid and derivative metabolism,GOC:curators

