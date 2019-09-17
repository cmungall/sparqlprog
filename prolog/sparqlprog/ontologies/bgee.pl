:- module(bgee,
          [
           expressed_in/2,
           expressed_in/4
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).
:- reexport(library(sparqlprog/ontologies/ebi), [in_taxon/2]).

:- sparql_endpoint( bgee, 'http://biosoda.expasy.org:8080/rdf4j-server/repositories/bgeelight').

:- rdf_register_prefix(up,'http://purl.uniprot.org/core/').
:- rdf_register_prefix(genex, 'http://purl.org/genex#').
:- rdf_register_prefix(orth, 'http://purl.org/net/orth#').
:- rdf_register_prefix(obo, 'http://purl.obolibrary.org/obo/').

expressed_in(Seq,Anat) :-
        expressed_in(Seq,Anat,_,_).
expressed_in(Seq,Anat,Expr,Cond) :-
        rdf(Expr,genex:hasSequenceUnit,Seq),
        rdf(Expr,genex:hasExpressionCondition,Cond),
        rdf(Cond,genex:hasAnatomicalEntity,Anat).

% IMPORTED FROM EBI
% note Taxon has purls such as http://purl.uniprot.org/taxonomy/10116
%in_taxon(Cond,Taxon) :-
%        rdf(Cond,obo:'RO_0002162',Taxon).

