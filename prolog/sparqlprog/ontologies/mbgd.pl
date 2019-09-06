:- module(mbgd,
	  [
	      complete_genome/1,
	      homologous_member/2,
	      in_mbgd_default/1,
	      mbgd_ortholog/2
	  ]).

:- sparql_endpoint(mbgd, 'https://sparql.nibb.ac.jp/sparql').

:- rdf_register_prefix(orth, 'http://purl.org/net/orth#').
:- rdf_register_prefix(mbgd, 'http://purl.jp/bio/11/mbgd#').
:- rdf_register_prefix(mbgdr, 'http://mbgd.genome.ad.jp/rdf/resource/').

complete_genome(G) :-
    rdf(G, rdf:type, mbgd:'CompleteGenome').

homologous_member(O, G) :-
    rdf_path(O, zeroOrMore(orth:hasHomologous), G),
    rdf(G, rdf:type, orth:'Gene').

in_mbgd_default(OC) :-
    rdf(OC, rdf:type, orth:'OrthologsCluster'),
    rdf(OC, void:inDataset, mbgdr:default).

mbgd_ortholog(G1, G2) :-
    in_mbgd_default(OC),
    homologous_member(OC, G1),
    homologous_member(OC, G2).
