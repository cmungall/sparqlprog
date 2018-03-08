
# ---
# what does PMID:1 reference?
# ---
pq-pmg  "references(pmid:'1',X)"

# MONDO:0015229 ! Bardet-Biedl syndrome
pq-pmg -e  "(monarch ?? rdfs_subclass_of(C,obo:'MONDO_0015229')),(pmg ?? references(P,C))"
