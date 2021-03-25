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

# https://douroucouli.wordpress.com/2020/08/05/what-is-the-sars-cov-2-molecular-parts-list/#comments
# peptides with descriptions
pq-up "peptide_annotation(Pep),rdf(Pep,rdfs:comment,D),annotation(P,Pep),in_taxon(P,uptaxon:'9606')"
