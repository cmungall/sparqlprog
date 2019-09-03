# find all diseases and their labels
pq-disgenet -l disease

# all gene-disease-associations
pq-disgenet -f tsv -l "gene_disease_association(A,G,D)"

# all gene-disease-associations with evidence/publication
pq-disgenet -f tsv -l "gene_disease_association(A,G,D),has_evidence(D,E)"

# diseases that have an implicated gene that is also implicated in small lung neoplasm
pq-disgenet -l "disease_pair_by_shared_gene(umls:'C0751688',D2,G)"
