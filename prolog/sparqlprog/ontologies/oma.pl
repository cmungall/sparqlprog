:- module(oma,
          [
           is_orthologous_to/2,
           is_orthologous_to/5,
           organism_in_taxon/2,
           encoded_by/2,
           ensembl_xref/2,
           encoded_by_ensembl_gene/2
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).
:- reexport(library(sparqlprog/ontologies/ebi), [in_taxon/2]).

:- sparql_endpoint( oma, 'https://sparql.omabrowser.org/sparql').

:- rdf_register_prefix(up,'http://purl.uniprot.org/core/').
:- rdf_register_prefix(orth, 'http://purl.org/net/orth#').
:- rdf_register_prefix(lscr, 'http://purl.org/lscr#').

is_orthologous_to(P1,P2) :-
        is_orthologous_to(P1,P2,_,_,_).
is_orthologous_to(P1,P2,N1,N2,C) :-
        rdf(C,rdf:type,orth:'OrthologsCluster'),
        rdf(C,orth:hasHomologousMember,N1),
        rdf(C,orth:hasHomologousMember,N2),
        N1\=N2,
        rdf_path(N1,zeroOrMore(orth:hasHomologousMember),P1),
        rdf_path(N1,zeroOrMore(orth:hasHomologousMember),P2).

encoded_by(P,G) :-
        rdf(P,sio:'010079',G).

ensembl_xref(G,X) :-
        rdf(G,lscr:xrefEnsemblGene,X).

encoded_by_ensembl_gene(P,X) :-
        encoded_by(P,G),ensembl_xref(G,X).

organism_in_taxon(P,T) :-
        rdf(P,orth:organism,O),
        in_taxon(O,T).
