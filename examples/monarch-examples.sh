
# phenotypes for subtypes of PBD
pq-mi -f prolog  'label(D,DN),literal_exact_match(DN,"peroxisome biogenesis disorder"),rdfs_subclass_of(D,C),label(C,CN)' "x(C,CN)"

# phenotypes for subtypes of PBD (using equivalence)
pq-mi -f prolog  'label(D,DN),literal_exact_match(DN,"peroxisome biogenesis disorder"),rdfs_subclass_of(D,C),label(C,CN),owl_equivalent_class(C,E),has_phenotype(E,Z)' "x(C,CN,E,Z)"
