# --------------------
# UNIPROT EXAMPLES
# --------------------

# ---
# human proteins with disease and interpro
# ---
pq-up  "in_human(P),annotation(P,A),disease_annotation(A),interpro(P,X)


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


# ---
# describe a specific mouse protein
# ---
pq-up describe http://purl.uniprot.org/uniprot/Q78E61
