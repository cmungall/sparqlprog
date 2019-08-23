/**

  tests direct execution on obo_util in-memory triples

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/obo_util)).
:- use_module(library(sparqlprog/owl_util), []).
:- use_module(library(sparqlprog/labelutils)).

:- rdf_register_prefix(oboInOwl,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix(part_of,'http://purl.obolibrary.org/obo/BFO_0000050').
foo('0').

:- begin_tests(obo_util_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        % load into a test-specific graph due to cache issue
        rdf_load('tests/go_nucleus.ttl',[cache(false),
                                         graph(obo_util)]).


run_test_query(N,X,G,L,L1) :-
        setof(X,G,L1),
        format('** ~w~n',[N]),
        wl(L1),
        assertion(L = L1).

wl(L) :-
        forall((member(X,L),term_labelify(X,L1)),
               writeln(L1)).


test(def) :-
        forall(entity_def_xrefs(E,D,Xs),
               writeln(e(E,D,Xs))).

test(gen) :-
        nl,
        nl,
        gen_obo(current_output,[]).



:- end_tests(obo_util_test).


