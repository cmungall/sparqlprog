/*

  Deprecated - use rdf_matcher now
*/
:- module(nlp_util,
          [rdf_nliteral/3,
           rdf_nliteral/4,
           rdf_nliteral_tr/5,
           shares_literal/6,

           entity_match/6,
           new_entity_match/6]).

%:- use_module(library(tabling)).
%:- use_module(library(memo)).
%:- use_module(library(typedef)).

:- rdf_register_ns(oio, 'http://www.geneontology.org/formats/oboInOwl#').


:- rdf_meta lprop(r).

% TODO: reuse
lprop(rdfs:label).
lprop(oio:hasExactSynonym).
lprop(oio:hasRelatedSynonym).
lprop(oio:hasNarrowSynonym).
lprop(oio:hasBroadSynonym).


%:- table rdf_nliteral_tr/5.

rdf_nliteral(X,P,V,G) :- rdf(X,P,Lit,G), bind(lcase(Lit), V).
rdf_nliteral(X,P,V) :- rdf_nliteral(X,P,V,_).

rdf_nliteral_tr(X,P,V,lcase,G) :- lprop(P),rdf(X,P,Lit,G), bind(lcase(Lit), V).

%:- table shares_literal/6.
shares_literal(A,B,PA,PB,V,F) :-
        rdf_nliteral_tr(A,PA,V,F,_),rdf_nliteral_tr(B,PB,V,F,_),A\=B.

entity_match(A,B,PA,PB,V,F) :-
        shares_literal(A,B,PA,PB,V,F),
        different_prefix(A,B).

new_entity_match(A,B,PA,PB,V,F) :-
        entity_match(A,B,PA,PB,V,F),
        rdf_global_id(PrefixA:_, A),
        rdf_global_id(PrefixB:_, B),
        \+ alt_equiv(A, PrefixB),
        \+ alt_equiv(B, PrefixA).

alt_equiv(A, PrefixB) :-
        rdf_path(A,((owl:equivalentClass)| \(owl:equivalentClass)),B),
        rdf_global_id(PrefixB:_, B).


different_prefix(A,B) :-
        rdf_global_id(PrefixA:_, A),
        rdf_global_id(PrefixB:_, B),
        PrefixA \= PrefixB.

