/** <module> blazegraph

  convenience predicates for working with blazegraph (formelerly bigdata) triplestores
  
*/

:- module(blazegraph,
          [
           literal_exact_match/2,
           
           literal_match/2,
           literal_match_relevance/3
          ]).
:- use_module(library(sparqlprog)).

:- rdf_register_prefix(bds,'http://www.bigdata.com/rdf/search#').

%! literal_match(?Lit, +Term) is nondet.
%
% wraps bds:search
literal_match(Lit,Term) :-
        rdf(Lit,bds:search,Term).


%! literal_exact_match(?Lit, +Term) is nondet.
%
% wraps bds:search, sets bds:matchExact to true
literal_exact_match(Lit,Term) :-
        rdf(Lit,bds:search,Term),
        rdf(Lit,bds:maxRank,"1"),
        rdf(Lit,bds:matchExact,"true").

%! literal_match_relevance(?Lit, +Term, ?Relevance) is nondet.
%
% searches for literal and scores match by relevance
literal_match_relevance(Lit,Term,Relevance) :-
        rdf(Lit,bds:search,Term),
        rdf(Lit,bds:relevance,Relevance).




    
