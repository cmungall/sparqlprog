# queries that use internal swi triplestore
# no need to run local sparql endpoint, or connect to remote one
# directly loads from examples/data

# basic query
pl2sparql -i ./examples/data/goslim_generic.ttl.gz -e "subClassOf(X,Y)"

# identical query, using void registry
pl2sparql -A examples/void.ttl -i goslim_generic -e "subClassOf(X,Y)"

# direct superclasses of nucleus
pl2sparql -u obo_core/goslim -i ./examples/data/goslim_generic.ttl.gz -e "nucleus_iri(N),subClassOf(N,X)"

# all superclasses of nucleus (including blank nodes representing restrictions)
pl2sparql -u obo_core/goslim -i ./examples/data/goslim_generic.ttl.gz -e "nucleus_iri(N),rdfs_subclass_of(N,X)"

# everything the nucleus is part of
pl2sparql -u obo_core/goslim -l -i ./examples/data/goslim_generic.ttl.gz -e "nucleus_iri(N),rdfs_subclass_of(N,Restr),owl_some(Restr,R,Y)" R-Y

# same as above
pl2sparql -l -i ./examples/data/goslim_generic.ttl.gz -e "lmatch(\"nucleus\",N),rdfs_subclass_of(N,Restr),owl_some(Restr,R,Y)" R-Y
