# basic query
pl2sparql -i ./tests/go_nucleus.ttl -e "subClassOf(X,Y)"

# direct superclasses of nucleus
pl2sparql -u obo_core/goslim -i ./tests/go_nucleus.ttl -e "nucleus_iri(N),subClassOf(N,X)"

# all superclasses of nucleus
pl2sparql -u obo_core/goslim -i ./tests/go_nucleus.ttl -e "nucleus_iri(N),rdfs_subclass_of(N,X)"

# all superclasses of nucleus
pl2sparql -u obo_core/goslim -l -i ./tests/go_nucleus.ttl -e "nucleus_iri(N),rdfs_subclass_of(N,Restr),owl_some(Restr,R,Y)"
