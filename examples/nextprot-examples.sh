# --------------------
# NEXTPROT EXAMPLES
# --------------------
# Taken from https://snorql.nextprot.org/

# ---
# entries with an isoform that is phosphorylated and in cytoplasm  
# ---
pq-nextprot "isoform(P,F),phosphoprotein(F),cellular_component_term_child_of(F,nextprot_cv:'SL-0086')"
