
# all orthologs of an ensembl gene
# taken from https://sparql.omabrowser.org/lode/sparql?
pq-oma "is_orthologous_to(X,Y),encoded_by_ensembl_gene(X,ensembl:'ENSLACG00000002497'),encoded_by_ensembl_gene(Y,YG)"

# federated query
#  see examples/oma_ebi_federated.pl for explanation
pl2sparql -e -d sparqlprog --consult examples/oma_ebi_federated.pl "orthologs_with_coordinates(ensembl:'ENSG00000198840',G2,Tax,B,E,R)"

