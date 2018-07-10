/* * -*- Mode: Prolog -*- */

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/nlp_util)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl)).

:- rdf_register_ns(a, 'http://purl.obolibrary.org/obo/A_').
:- rdf_register_ns(b, 'http://purl.obolibrary.org/obo/B_').
:- rdf_register_ns(x, 'http://purl.obolibrary.org/obo/X_').
:- rdf_register_ns(y, 'http://purl.obolibrary.org/obo/Y_').
:- rdf_register_ns(z, 'http://purl.obolibrary.org/obo/Z_').



:- begin_tests(nlp_util_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/lexmap_test.ttl').

test(normalize) :-
        forall(rdf_nliteral(X,P,V),
               format('~w ~w ~w~n',[X,P,V])).

test(shares_literal) :-
        G = shares_literal(_A,_B,_PA,_PB,_V,_F),
        forall(G,
               format(':: ~w~n',[G])).

test(match) :-
        G = entity_match(_A,_B,_PA,_PB,_V,_F),
        forall(G,
               format(':: ~w~n',[G])),
                rdf_global_id(z:'1',Z1),
        rdf_global_id(x:'1',X1),
        rdf_global_id(x:'2',X2),
        rdf_global_id(y:'2',Y2),
        rdf_global_id(z:'1',Z1),
        % case-insensitivity
        assertion( entity_match(X2,Y2,_,_,_,_) ),
        assertion( entity_match(X1,Z1,_,_,_,_) ),
        assertion( entity_match(Z1,X1,_,_,_,_) ).


test(new_match) :-
        G = new_entity_match(_A,_B,_PA,_PB,_V,_F),
        forall(G,
               format(':: ~w~n',[G])),
        rdf_global_id(x:'1',X1),
        rdf_global_id(x:'2',X2),
        rdf_global_id(y:'2',Y2),
        rdf_global_id(z:'1',Z1),
        assertion( new_entity_match(X2,Y2,_,_,_,_) ),
        assertion( \+ new_entity_match(X1,Z1,_,_,_,_) ),
        assertion( \+ new_entity_match(Z1,X1,_,_,_,_) ).

             

    
:- end_tests(nlp_util_test).
