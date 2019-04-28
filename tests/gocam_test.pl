
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/endpoints)).
:- use_module(library(obo_ro/ro)).
:- use_module(library(sparqlprog/ontologies/gocam)).
:- use_module(library(obo_core/goslim)).

:- begin_tests(gocam_test).

test_q(Q,Expected) :-
        findall(Q,(Q),Results),
        maplist(writeln,Results),
        assertion( member(Expected, Results) ).


:- debug(sparqlprog).

/*
k2k(A1,A2,G1C,G2C) :-
        go ?? (
               kinase_activity(A1),
               enabled_by(A1,G1),
               part_of(A1,A2),
               signal_transduction(A2),
               enabled_by(A1,G2),
               rdf(G1,rdf:type,G1C),
               rdf(G2,rdf:type,G2C)
               ).
*/

k2k(A1,A2,G1C,G2C) :-
        go ?? (
               kinase_activity(A1),
               kinase_activity(A2),
               enabled_by(A1,G1),
               rdf(A1,R,A2),
               enabled_by(A1,G2),
               rdf(G1,rdf:type,G1C),
               rdf(G2,rdf:type,G2C)
               ).



test(kinase) :-
        test_q(k2k(_,_,_,_),
               _).


:- end_tests(gocam_test).


