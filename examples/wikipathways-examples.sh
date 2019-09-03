# WikiPathway Examples
# ---
# Adapted from https://www.wikipathways.org/index.php/Help:WikiPathways_Metabolomics

# all pathways
pq-wp pathway/1

# all metabolites
pq-wp metabolite/1

# all metabolite properties
pq-wp --distinct "metabolite(M),rdf(M,P,V)" "x(P)"

# metabolite count
pq-wp  "aggregate(count(distinct(M)),metabolite(M),Num)" "x(Num)"
# 5381

# metabolites part of pathways
pq-wp  "metabolite(M),part_of(M,P),pathway(P)"

# distinct human metabolites
pq-wp --distinct  "metabolite(M),part_of(M,P),organism(P,ncbitaxon:'9606')" "x(M)"

# integrative queries: find all classic metabolic pathways in wikidata using PW ontology
# first do this:
curl -L -s http://purl.obolibrary.org/obo/pw.owl > /tmp/pw.owl
# then we will load this into the in-memory swipl RDF triplestore;
# we will find all inferred subclasses of our pathway, and all wikipathway pathways associated.
# note that we use "-e" to run in prolog evaluation mode, and use ??/2 to explicitly make a sparqlprog query
pq-wp -i /tmp/pw.owl -e "label_of('classic metabolic pathway',Match),rdfs_subclass_of(T,Match),(wikipathways ?? ontology_tag(P,T))" "x(P,T)"
