# --------------------
# UNIPROT EXAMPLES
# --------------------

# ---
# human proteins with disease and interpro
# ---
pq-up  "in_human(P),annotation(P,A),disease_annotation(A),interpro(P,X)"

# ---
# diseases for a specific protein and their annotations
# ---
pq-up "protein_natural_variant_disease(uniprot:'Q15465',A,D),substitution(A,Seq)" "x(A,D,Seq)"

# link from a specific protein to its variant to associated disease and the dbsnp id for the variant
pq-up "protein_natural_variant_disease_dbsnp(uniprot:'Q15465',A,D,X)"

# all xrefs
pq-up "protein_natural_variant_disease_xref(uniprot:'Q15465',A,D,X)"

# all annotations and their types on a specific protein
pq-up  "annotation(uniprot:'Q15465',A),rdf(A,rdf:type,T)"

# all sequence annotation types and their parent classification
pq-up "rdfs_subclass_of(T,up:'Sequence_Annotation'),subClassOf(T,Parent),rdf_is_iri(Parent)"

# GO, KW annotations
pq-up "classified_with(uniprot:'B5MCD5',T)"

# rhea annotations
pq-up "protein_has_catalyzed_reaction(uniprot:'A0A1H0K750',X)"
pq-up "protein_has_catalyzed_reaction(P,rhea:'17641')"

# ---
# FEDERATION
# ---
# 
# we show federated queries in 2 directions, and alternating between two sparql endpoints as base
# 
# 1a. EC->rhea->Protein, uniprot as base
pq-up-rhea "service(rhea,rhea_ec(R,enzyme:'4.1.1.19')),protein_has_catalyzed_reaction(P,R)" 
# 1b. EC->rhea->Protein, rhea as base
pq-rhea-up "rhea_ec(X,enzyme:'4.1.1.19'),service(uniprot,protein_has_catalyzed_reaction(P,X))"

# 2a. Protein->Rhea->EC, rhea as base
pq-rhea-up "service(uniprot,protein_has_catalyzed_reaction(uniprot:'A0A1H0K750',X)),rhea_ec(X,Y)" 
# 2b. Protein->Rhea->EC, uniprot as base
pq-up-rhea "protein_has_catalyzed_reaction(uniprot:'A0A1H0K750',X),service(rhea,rhea_ec(X,Y))"


# same logic but federating via sparqlprog
pq-up-rhea -e  "(uniprot ?? protein_has_catalyzed_reaction(uniprot:'A0A1H0K750',X)),(rhea ?? rhea_ec(X,Y))"
pq-up-rhea -e  "(rhea ?? rhea_ec(X,enzyme:'4.1.1.19')), (uniprot ?? protein_has_catalyzed_reaction(P,X))"

# human RHEA annotations
pq-up "in_reference_proteome(P),rdf(P,up:reviewed,Rev),in_taxon(P,uptaxon:'9606'),protein_has_catalyzed_reaction(P,R)" "row(P,R)"

# proteins associated with >1 disease
pq-up -l -L pref_label  "protein_natural_variant_disease(P,V1,D1),protein_natural_variant_disease(P,V2,D2),D1\=D2"

# count of all proteins
$ pq-up "aggregate(count(P),protein(P),N)" "count(P)"
#262095770


# ---
# distinct databases used in xrefs
# ---
pq-up --distinct "database(X,D)" "x(D)"

# ---
# count of xrefs broken down by database
# ---
pq-up "aggregate_group(count(X),[D],database(X,D),N)"

# ---
# all MGI xrefs
# ---
pq-up "xref(P,X),database(X,updb:'MGI')"

# reviewed subset
pq-up "xref(P,X),database(X,updb:'MGI'),reviewed(P)"

# xrefs to OMIM (this will be both proteins and diseases)
pq-up -l "xref(P,X),database(X,updb:'MIM')"

# ---
# describe a specific mouse protein
# ---
pq-up describe http://purl.uniprot.org/uniprot/Q78E61


# ---
# all in a taxon
# ---
pq-up "protein(P),in_taxon(P,uptaxon:'7955')"

# ---
# all reference proteins in a genome
# ---
pq-up "in_reference_proteome(P),in_taxon(P,uptaxon:'7955')"


# ---
# zebrafish reference proteins with no ZFIN mapping
# ---
pq-up "in_reference_proteome(P),rdf(P,up:reviewed,Rev),in_taxon(P,uptaxon:'7955'),\+xref_in(P,X,'ZFIN')" "row(P,Rev)"

# wormbase genes
pq-up "in_reference_proteome(P),rdf(P,up:reviewed,Rev),in_taxon(P,uptaxon:'6239'),xref_in(P,X,'WormBase')"
pq-up "in_reference_proteome(P),rdf(P,up:reviewed,Rev),xref(P,X),database(X,D),is_wormbase(D)" "row(P,X)"

# all araport
pq-up "in_reference_proteome(P),rdf(P,up:reviewed,Rev),in_taxon(P,Tax),xref_in(P,X,'Araport')"

# https://douroucouli.wordpress.com/2020/08/05/what-is-the-sars-cov-2-molecular-parts-list/#comments
# peptides with descriptions
pq-up "peptide_annotation(Pep),rdf(Pep,rdfs:comment,D),annotation(P,Pep),in_taxon(P,uptaxon:'9606')"
