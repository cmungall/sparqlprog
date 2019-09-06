:- module(keggoc,
	  [
	      keggoc_ortholog/2
	  ]).

:- sparql_endpoint(keggoc, 'https://www.genome.jp/oc/proxy/sparql').

:- rdf_register_prefix(orth, 'http://purl.org/net/orth#').

keggoc_ortholog(G1, G2) :-
    rdf_path(OC, zeroOrMore(orth:hasHomologous), G1),
    rdf_path(OC, zeroOrMore(orth:hasHomologous), G2),
    rdf(G1, rdf:type, orth:'Gene'),
    rdf(G2, rdf:type, orth:'Gene').

