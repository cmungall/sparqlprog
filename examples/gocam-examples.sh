# --------------------
# GO-CAM EXAMPLES
# --------------------
# Uses rdf.geneontology.org endpoint (alias 'go')
# The pq-go script is a wrapper for
#     pl2sparql -u sparqlprog/endpoints -u obo_core/goslim -u obo_ro/ro  -s go

# ---
# genes executing kinase activity as a part of signal transduction
# ---
# timeout??
pq-go "kinase_activity(A),part_of(A,P),signal_transduction(P),enabled_by(A,G)"

# ---
# kinase regulating other activity
# ---
pq-go "kinase_activity(A),regulates(A,A2),enabled_by(A,G)"

# ---
# user to model
# ---
pq-go -C "rdf(X,dc:contributor,Y),rdf(X,rdf:type,owl:'Ontology')"

# ---
# search: classes with label ending in morphogenesis
# ---
pq-go 'label(C,L),regex(str(L),"morphogenesis$")' "x(C,L)"

# with flags (case insensitive)
pq-go 'label(C,L),regex(str(L),"morphogenesis$","i")' "x(C,L)"

# same, using owl_util lsearch/4
pq-go 'lsearch("morphogenesis$",C,L,"i")'

# same, using shorthand syntax
pq-go / lsearch morphogenesis$ .. i
