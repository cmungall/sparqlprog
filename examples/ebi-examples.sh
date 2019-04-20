# Show all transcripts of human BRCA2 gene and their coordinates
# https://www.ebi.ac.uk/rdf/services/sparql
pq-ebi "transcribed_from(T,ensembl:'ENSG00000139618'),location(T,L,B,E,R),rdf(T,rdf:type,TType)"

# Get all mouse protein-coding genes on chromosome 11 between location 101,100,523 and 101,190,725 forward strand
pq-ebi -f csv "protein_coding_gene(G),location(G,L,B,E,grcm38:'11'), B >= 101100523,E =< 101190725" G

# sample all samples
pq-ebi -l  sample

# everything about https://www.ebi.ac.uk/biosamples/samples/SAMN02847463
pq-ebi -l  "rdf(biosd:'SAMN02847463',P,Y)"

