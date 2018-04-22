/* * -*- Mode: Prolog -*- */

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/nlp_util)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl)).

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix('','http://example.org/').

:- begin_tests(nlp_util_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/lexmap_test.ttl').

test(normalize) :-
        forall(rdf_nliteral(X,P,V),
               format('~w ~w ~w~n',[X,P,V])).

test(shar) :-
        G = shares_literal(_A,_B,_PA,_PB,_V,_F),
        forall(G,
               format(':: ~w~n',[G])).

    
:- end_tests(nlp_util_test).
