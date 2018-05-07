:- module(owl_util,
          [owl_some/3,
           owl_all/3,
           owl_equivalent_class/2,
           owl_equivalent_class_asserted/2,
           thing_class/1,
           not_thing_class/1,
           deprecated/1,           
           subclass_of_some/3,
           subclass_cycle/1,
           owl_node_info/4,

           eq_intersection_member/2,
           intersection_member/2,
           rdflist_member/2,

           class_genus/2,
           class_differentia/3,
           
           common_ancestor/3,
           mrca/3,
           common_descendant/3,
           mrcd/3,

           psearch/5,
           psearch/4,
           psearch/3,
           
           lsearch/2,
           lsearch/3,
           lsearch/4,
           lsearch/5
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/ontologies/owl), []).

:- reexport(library(sparqlprog/ontologies/owl), [ label/2, subClassOf/2 ]).

:-op(300,xfy,some).
:-op(300,xfy,all).


thing_class(owl:'Thing').
not_thing_class(X) :- X \= owl:'Thing'.
not_thing_class(X) :- X \= owl:'Thing'.

deprecated(X) :- rdf(X,owl:deprecated,"true"^^xsd:boolean).


owl_equivalent_class(A,B) :- rdf_path(A,zeroOrMore( (owl:equivalentClass)| \(owl:equivalentClass)),B).
owl_equivalent_class_asserted(A,B) :- rdf(A,owl:equivalentClass,B).
owl_equivalent_class_asserted(A,B) :- rdf(B,owl:equivalentClass,A).



subclass_cycle(A) :- rdf_path(A,oneOrMore(rdfs:subClassOf),A).



%! owl_some(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression SomeValuesFrom(Property,Obj)
owl_some(R,P,O) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

subclass_of_some(C,P,O) :-
        owl:subClassOf(C,R),
        owl_some(R,P,O).


%! owl_all(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression AllValuesFrom(Property,Obj)
owl_all(R,P,O) :-
        owl:onProperty(R,P),
        owl:allValuesFrom(R,O).

owl_restriction(R, some(P,O)) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

owl_restriction(R, all(P,O)) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

owl_node_info(S,P,O,E) :-
        rdf(S,P,O),
        bind(S,E).

owl_node_info(S,P,O,Equiv) :-
        owl_equivalent_class(S,Equiv),
        rdf(Equiv,P,O),
        \+ is_blank(O).
owl_node_info(S,P,O,Equiv) :-
        owl_equivalent_class(S,Equiv),
        subclass_of_some(Equiv,P,O).

class_genus(C,G) :-
        eq_intersection_member(C,G),
        \+ is_blank(G).
class_differentia(C,P,Y) :-
        eq_intersection_member(C,R),
        owl_some(R,P,Y).

eq_intersection_member(C,M) :-
        rdf(C,owl:equivalentClass,E),
        intersection_member(E,M).

intersection_member(I,M) :-
        rdf(I,owl:intersectionOf,L),
        rdflist_member(L,M).

rdflist_member(L,M) :-
        rdf_path(L,(zeroOrMore(rdf:rest)/(rdf:first)),M).
        

common_ancestor(X,Y,A) :-
        rdfs_subclass_of(X,A),
        rdfs_subclass_of(Y,A),
        X\=Y.

mrca(X,Y,A) :-
        common_ancestor(X,Y,A),
        \+ ((common_ancestor(X,Y,A2),
             rdf_path(A2,oneOrMore(rdfs:subClassOf),A))).


common_descendant(X,Y,D) :-
        rdfs_subclass_of(D,X),
        rdfs_subclass_of(D,Y),
        X\=Y.

mrcd(X,Y,D) :-
        common_descendant(X,Y,D),
        \+ ((common_descendant(X,Y,D2),
             rdf_path(D,oneOrMore(rdfs:subClassOf),D2))).

% ----------------------------------------
% lexical
% ----------------------------------------

lsearch(P,C,L,F,G) :-   rdf(C,rdfs:label,L,G),regex(str(L),P,F).
lsearch(P,C,L,F) :-   label(C,L),regex(str(L),P,F).
lsearch(P,C,L) :-   label(C,L),regex(str(L),P).
lsearch(P,C) :-   label(C,L),regex(str(L),P).

psearch(P,C,R,L,F) :-   rdf(C,R,L),regex(str(L),P,F).
psearch(P,C,R,L) :-   rdf(C,R,L),regex(str(L),P).
psearch(P,C,R) :-   rdf(C,R,L),regex(str(L),P).

