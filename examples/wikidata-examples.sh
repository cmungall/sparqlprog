
# continents and their labels
pq-wd  'continent(C),label(C,CL),lang(CL)="en"'

# disease-to-gene-to-protein
pq-wd   "disease(D),genetic_association(D,G),encodes(G,P),enlabel(D,DN),enlabel(P,PN)"

# diseases, causes and treatments
pq-wd   "isa_disease(X),has_cause(X,C),treated_by_drug(X,S),enlabel(X,XL),enlabel(C,CL),enlabel(S,SL)"

# civic variants and their types
pq-wd   "civic_id(V,X),instance_of(V,T),enlabel(V,VL),enlabel(T,TL)"

# variant that is PTP for a drug-condition
# See WD example queries
pq-wd   "V='http://www.wikidata.org/entity/Q25100112', civic_id(V,X),positive_therapeutic_predictor_e2s(V,S),medical_condition_treated_s2q(S,C),positive_therapeutic_predictor_s2v(S,Drug),enlabel(Drug,DrugN),enlabel(V,VN),enlabel(C,CN)" "v2d_cond(V,VN,Drug,DrugN,C,CN)"

# power stations and their locations
pq-wd   "power_station_inf(X),coordinate_location(X,Loc),enlabel(X,XL)"

# concepts with an exact match to something in SO
pq-wd  'exact_match(X,URI),str_starts(str(URI),"http://purl.obolibrary.org/obo/SO_")'  

# properties mapped to RO
pq-wd  'ro_id(P,X)'

