# All cells
pq-ubergraph  -l "rdf_closure(C,rdfs:subClassOf,obo:'CL_0000000')"

# All cells and what they are part of
pq-ubergraph  -l "rdf_closure(C,rdfs:subClassOf,obo:'CL_0000000'),part_of(C,Y,ubergraph:nonredundant)"

# All cells in the kidney
pq-ubergraph  -l "subClassOf(C,obo:'CL_0000000'),rdf(C,obo:'BFO_0000050',A),subClassOf(A,obo:'UBERON_0002113')"

# All cells and what they are part of; this time for any class that fits the label
pq-ubergraph  -l "lsearch('^cell$',Cell),rdf_closure(C,rdfs:subClassOf,Cell),part_of(C,Y,ubergraph:nonredundant)" "row(C,Y)"

# All properties, plus labels
pq-ubergraph -f prolog  "rdf(X,rdf:type,owl:'ObjectProperty'),label(X,N)" "x(X,N)"

# All properties, plus labels, and domain and ranges
# not entailed. See https://github.com/NCATS-Tangerine/ubergraph/issues/13
pq-ubergraph  -l -f csv  "rdf(X,rdf:type,owl:'ObjectProperty'),owl:domain(X,D),owl:range(X,R)" "x(X,D,R)"

# all properties used
pq-ubergraph --distinct  "rdf_ontology(S,P,O),label(P,PN)" P

# summary stats for above
pq-ubergraph   "aggregate_group(count(distinct(S)),[P],rdf_ontology(S,P,O),N)" "x(P,N)"

# all triples with a literal with a trailing whitespace
pq-ubergraph  'rdf(C,P,V),is_literal(V),str_ends(str(V)," ")'

# MRCA for pairs of neurons
# note we use egraph_mrca/3 not mrca/3 because we have subclass
# entailments materialized, no need for sparql paths
pq-ubergraph -l "egraph_mrca(obo:'CL_1000379',obo:'CL_0002285',A)"


# --------
# SEARCH
# --------
# The '/' indicates that all subsequent arguments are to be assembled into a query term, in this case `ontsearch("uberon","limb$",_,_)`

# also searches synonyms
pq-ubergraph -l / tsearch ^hippocamp _ _
