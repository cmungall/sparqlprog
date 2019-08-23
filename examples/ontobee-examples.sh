# All properties in PATO
pq-ontobee  "in_ontology(X,pato,owl:'ObjectProperty')"

# All properties in NCIT, plus labels
pq-ontobee -f prolog  "in_ontology(X,ncit,owl:'ObjectProperty'),label(X,N)" "x(X,N)"

# All properties in NCIT, auto-labels, contract URIs, show domain and range
pq-ontobee -u sparqlprog/ontologies/ncit  -l -f csv  "in_ontology(X,ncit,owl:'ObjectProperty'),owl:domain(X,D),owl:range(X,R)" "x(X,D,R)"

# Same, explicit
pq-ontobee -u sparqlprog/ontologies/ncit  -f csv  "in_ontology(P,ncit,owl:'ObjectProperty'),label(P,PN),owl:domain(P,D),owl:range(P,R),label(D,DN),label(R,RN)" "x(P,PN,D,DN,R,RN)"

# all property usages across ontobee (TODO: select DISTINCT)
pq-ontobee  "rdf(Restr,owl:onProperty,P,G),label(P,PN)" "x(Restr,P,PN,G)"

# similar to above, but instead load RO triples into memory, loop over all OP directly in prolog engine,
# executing separate sparql query for each
pq-ontobee -e -i ro.owl  "owl:objectProperty(P), (ontobee ?? rdf(Restr,owl:onProperty,P,G),label(P,PN))" "x(P,PN,G,Restr)"
pq-ontobee -e -i ro.owl  "owl:objectProperty(P), label(P,PN), (ontobee ?? aggregate_group(count(Restr),[P,G],rdf(Restr,owl:onProperty,P,G),Num))" "x(P,PN,G,Num)"

# also: usages of property by graph
pq-ontobee  "aggregate_group(count(P),[P,G],rdf(_,owl:onProperty,P,G),Num)" 

# number of distinct xrefs per graph
pq-ontobee -f tsv   "aggregate_group(count(distinct(X)),[G],has_dbxref(C,X,G),NumX)"

# exact matches between mondo and other resources
pq-ontobee -u sparqlprog/ontologies/skos -C "in_ontology(X,mondo),has_exact_match(X,Y)"

# all triples with a literal with a trailing whitespace
pq-ontobee  'rdf(C,P,V),is_literal(V),str_ends(str(V)," ")'

# all redundant subclass assertions
pq-ontobee -l  "subClassOf(A,B),subClassOf(B,C),subClassOf(A,C)" 

# class labels that match exact synonyms of other classes
pq-ontobee -l -u obo_metadata/oio  'has_exact_synonym(C1,N),label(C2,N),C1\=C2'

# all labels used more than once

# This causes virtuoso error: Virtuoso 22026 Error SR319: Max row length is exceeded when trying to store a string of 17 chars into a temp col
## pq-ontobee -f tsv  "aggregate_group(count(distinct(C)),[N],(rdf(C,rdfs:label,N1),N is lcase(str(N1))),count(distinct(C))>1,R)" "x(N,R)" 

pq-ontobee -f tsv  "aggregate_group(count(distinct(C)),[N],(rdf(C,rdfs:label,N1),N is str(N1)),count(distinct(C))>1,R)" "x(N,R)" 

# same, exclude obsoletes
pq-ontobee -f tsv   "aggregate_group(count(distinct(C)),[N],(rdf(C,rdfs:label,N1),N is str(N1), \+deprecated(C)),count(C)>1,R)" "x(N,R)" 

# --------
# SEARCH
# --------
# The '/' indicates that all subsequent arguments are to be assembled into a query term, in this case `ontsearch("uberon","limb$",_,_)`
# The ontobee module defines ontsearch/4 as a search within an ontology, using the mapping from the obo id (e.g. 'uberon') to
# the graph in which this is stored in ontobee
pq-ontobee / ontsearch uberon limb$ _ _

# also searches synonyms
pq-ontobee -l / tsearch ^hippocamp _ _
