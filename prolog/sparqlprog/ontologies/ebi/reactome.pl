/*

  protein . entityReference => uniprot PURL
  smallMolecule . entityReference => obo:CHEBI
  dna . entityReference => id:ENSG
  rna . entityReference => ENST, mirbase

    */

:- module(reactome,
          [
           equiv_go/2
           ]).

:- use_module(library(sparqlprog/ontologies/biopax3)).
:- use_module(library(sparqlprog/ontologies/ebi)).

:- rdf_register_prefix(reactome,'http://identifiers.org/reactome/').
:- rdf_register_prefix(chebi, 'http://purl.obolibrary.org/obo/CHEBI_').
:- rdf_register_prefix(go, 'http://purl.obolibrary.org/obo/GO_').


% note: this is unidirectional only
equiv_go(E,C) :-
        xref(E,X),
        id(X,Id),
        str_starts(Id,"GO:"),
        bind(replace(Id,"GO:","http://purl.obolibrary.org/obo/GO_"), C).

