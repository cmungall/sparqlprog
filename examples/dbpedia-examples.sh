# DBPedia Examples
# ---
# These all use pq-dbpedia in the bin/ directory, which is a one-line wrapper onto pl2sparql,
# which loads the dbpedia.pl module and sets the endpoint to http://dbpedia.org/sparql/

# ---
# movies directors with children who are musicians
# ---
# this example uses bare rdf/3 predicates
pq-dbpedia  "rdf(F,dbo:director,D),rdf(D,dbo:child,C),rdfs_individual_of(C,dbo:'MusicalArtist')"

# ---
# movies directors with children who are musicians
# ---
# use convenience predicates
pq-dbpedia  "has_director(F,D),child(D,C),musical_artist(C)"

