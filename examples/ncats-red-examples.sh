# all drugs with names
pq-ncats-red   -f tsv "drug(X)" -l -L name

# everything about Imatinib
pq-ncats-red   -f tsv 'name(X,"Imatinib"),rdf(X,R,Y)' -l -L name
