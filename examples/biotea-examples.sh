# Examples for querying BioTea pubmedgraph
#
# Uses the pq-biotea wrapper that defines the endpoint

# ---
# what does PMID:1 reference?
# ---
pq-pmg  "references(pmid:'1',X)"

# Find all publications that either reference a class, or a descendant of that class.
# E.g., MONDO:0015229 ! Bardet-Biedl syndrome
#
# This is an example of federation: we use the monarch graph to get subclasses of
# the disease of interest, and look these up iteratively on pubmedgraph
#
# For this we use the -e option as the outer query is executed directly in prolog.
# The ??/2 predicate is used to query a remote triplestore.
pq-pmg -e  "(monarch ?? rdfs_subclass_of(C,obo:'MONDO_0015229')),(pmg ?? references(P,C))"

pq-pmg -i mondo -e  "subclass_axiom_validation(A,B,P)"
