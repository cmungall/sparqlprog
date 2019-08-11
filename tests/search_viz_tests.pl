/**

  tests search on prolog in-memory triples / builtin emulation

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/search_util)).
:- use_module(library(sparqlprog/ontologies/owl)).
:- use_module(library(sparqlprog/owl_search_viz)).

:- begin_tests(search_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).


load_test_file :-
        rdf_load('tests/go_nucleus.ttl'),
        forall(rdf(S,P,O),
               writeln(x(S,P,O))).


test(basic) :-
        owl_search_and_display(nuc, '.', '.', [], info, _).
:- end_tests(search_test).


