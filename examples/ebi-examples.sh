# Show all transcripts of human BRCA2 gene and their coordinates
# https://www.ebi.ac.uk/rdf/services/sparql
pq-ebi "transcribed_from(T,ensembl:'ENSG00000139618'),location(T,L,B,E,R),rdf(T,rdf:type,TType)"

# Get all mouse protein-coding genes on chromosome 11 between location 101,100,523 and 101,190,725 forward strand
pq-ebi -f csv "protein_coding_gene(G),location(G,L,B,E,grcm38:'11'), B >= 101100523,E =< 101190725" G

# sample all samples
pq-ebi -l  sample

# everything about https://www.ebi.ac.uk/biosamples/samples/SAMN02847463
pq-ebi -l  "rdf(biosd:'SAMN02847463',P,Y)"

# sample property-values
pq-ebi -l  "sample_property_value(biosd:'SAMN02847463',P,V)"

# distincty attributes
# (may be too slow...)
pq-ebi  "aggregate(count(distinct(P)),rdf(A,ebi_atlas:propertyType,P),Num)" "x(Num)"

# all distinct attributes (24732)
pq-ebi --distinct "rdf(A,ebi_atlas:propertyType,P)" "x(P)

# federated query
#  see examples/oma_ebi_federated.pl for explanation
pl2sparql -e -d sparqlprog --consult examples/oma_ebi_federated.pl "orthologs_with_coordinates(ensembl:'ENSG00000198840',G2,Tax,B,E,R)"
