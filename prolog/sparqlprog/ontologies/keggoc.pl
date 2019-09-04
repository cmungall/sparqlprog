:- module(keggoc,
	  [
	      keggoc_ortholog/2
	  ]).

:- sparql_endpoint(keggoc, 'https://keggoc-rdf.dbcls.jp/sparql/').

:- rdf_register_prefix(orth, 'http://purl.jp/bio/11/orth#').

keggoc_ortholog(G1, G2) :-
    rdf_path(OC, zeroOrMore(orth:member), G1),
    rdf_path(OC, zeroOrMore(orth:member), G2),
    rdf(G1, rdf:type, orth:'Gene'),
    rdf(G2, rdf:type, orth:'Gene').

