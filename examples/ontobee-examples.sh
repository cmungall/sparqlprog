# All properties in PATO
pq-ontobee  "in_ontology(X,pato,owl:'ObjectProperty')"

# All properties in NCIT, plus labels
pq-ontobee -f prolog  "in_ontology(X,ncit,owl:'ObjectProperty'),label(X,N)" "x(X,N)"

# All properties in NCIT, auto-labels, contract URIs, show domain and range
pq-ontobee -u sparqlprog/ontologies/ncit  -l -f csv  "in_ontology(X,ncit,owl:'ObjectProperty'),owl:domain(X,D),owl:range(X,R)" "x(X,D,R)"

# Same, explicit
pq-ontobee -u sparqlprog/ontologies/ncit  -f csv  "in_ontology(P,ncit,owl:'ObjectProperty'),label(P,PN),owl:domain(P,D),owl:range(P,R),label(D,DN),label(R,RN)" "x(P,PN,D,DN,R,RN)"

# all property usages across ontobee (TODO: select DISTINCT)
pq-ontobee  "rdf(_,owl:onProperty,P,G),label(P,PN)" "x(P,PN,G)"

# also:
pq-ontobee  "aggregate_group(count(P),[P,G],rdf(_,owl:onProperty,P,G),Num)" 

# all triples with a literal with a trailing whitespace
pq-ontobee  'rdf(C,P,V),is_literal(V),str_ends(str(V)," ")'

# all redundant subclass assertions
pq-ontobee -l  "subClassOf(A,B),subClassOf(B,C),subClassOf(A,C)" 

# class labels that match exact synonyms of other classes
pq-ontobee -l -u obo_metadata/oio  'has_exact_synonym(C1,N),label(C2,N),C1\=C2'

# --------
# SEARCH
# --------
# The '/' indicates that all subsequent arguments are to be assembled into a query term, in this case `ontsearch("uberon","limb$",_,_)`
# The ontobee module defines ontsearch/4 as a search within an ontology, using the mapping from the obo id (e.g. 'uberon') to
# the graph in which this is stored in ontobee
pq-ontobee / ontsearch uberon limb$ _ _

# also searches synonyms
pq-ontobee -l / tsearch ^hippocamp _ _
