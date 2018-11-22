:- module(obo_util,
          [entity_xref_prefix/3]).

:- use_module(library(obo_metadata/oio)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/ontologies/owl), []).
:- use_module(library(sparqlprog/emulate_builtins)).

:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').

%!  entity_xref_prefix(?Cls:atm, ?X:str, ?Pre:str)
%
%   true if Cls has X as an xref, and X has prefix Pre
entity_xref_prefix(C,X,P) :-
        has_dbxref(C,X),
        curie_prefix(X,P).

%!  entity_xref_src(?Cls:atm, ?X:str, ?S:str)
%
%   true if Cls has X as an xref, and axiom annotated with S
entity_xref_src(C,X,S) :-
        entity_xref_src(C,X,_,S).
entity_xref_src(C,X,A,S) :-
        has_dbxref_axiom(C,X,A),
        rdf(A,oio:source,S).

entity_xref_prefix_srcont(C,X,P,S) :-
        entity_xref_prefix(C,X,P),
        entity_xref_src(C,X,SC),
        curie_prefix(SC,S).


%!  curie_prefix(Literal:str, Pre:str)
curie_prefix(Literal,Pre) :-
        str_before(Literal,":",Pre).
