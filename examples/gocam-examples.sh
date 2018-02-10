# genes executing kinase activity as a part of signal transduction
pq-go "kinase_activity(A),part_of(A,P),signal_transduction(P),enabled_by(A,G)"

# kinase regulating other activity
pq-go "kinase_activity(A),regulates(A,A2),enabled_by(A,G)"
