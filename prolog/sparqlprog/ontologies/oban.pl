/*

  
*/

:- module(oban,
          [
           association/4,
           association/5
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(oban,'http://purl.org/oban/').

association(A,S,P,O) :-
        rdf(A,oban:association_has_subject,S),
        rdf(A,oban:association_has_object,O),
        rdf(A,oban:association_has_predicate,P).

association(A,S,P,O,Src) :-
        association(A,S,P,O),
        rdf(A,dc:source,Src).
