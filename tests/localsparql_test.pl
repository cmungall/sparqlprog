/**

  tests direct execution on local triplestore

  make bg-run

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(rdf_owl)).

:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/endpoints)).

%:- table ??/2.


count(X,G,N) :-
        (   setof(X,(local ?? G),Xs)
        ->  length(Xs,N)
        ;   N=0).

jaccard(X,Y,N) :-
        count(A,(rdfs_subclass_of(X,A),rdfs_subclass_of(Y,A)),N1),
        count(A,(rdfs_subclass_of(X,A);rdfs_subclass_of(Y,A)),N2),
        N is N1/N2.


:- begin_tests(localsparql_test,
               []).

wl(L) :-
        forall(member(X,L),
               format('~w~n', [X])).

run_test_query(N,X,G,L,L1) :-
        setof(X,(local ?? G),L1),
        format('** ~w~n',[N]),
        wl(L1),
        assertion(L = L1).

test(direct_subclass_of) :-
        run_test_query(direct,X,
                       (   label(C,"nucleus"),
                           rdf(C,rdfs:subClassOf,X)),[_,_],_).

test(inferred_subclass_of) :-
        run_test_query(inferred,X,
                       (   label(C,"nucleus"),
                           rdfs_subclass_of(C,X)),[_,_,_],_).

test(mrca) :-
        run_test_query(mrca_check,t(X,Y,A),
                       (   label(X,"protein complex assembly"),
                           label(Y,"macromolecular complex assembly"),
                           label(A,"cellular component assembly"),
                                 mrca(X,Y,A)),
                       [_],_).

test(jaccard) :-
        forall((local ?? owl:class(X),owl:class(Y)),
               (   jaccard(X,Y,S),
                   format('~w <-> ~w = ~w~n',[X,Y,S]))).


        


:- end_tests(localsparql_test).


