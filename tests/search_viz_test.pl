/**

  tests search on prolog in-memory triples / builtin emulation

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/search_util)).
:- use_module(library(sparqlprog/ontologies/owl)).
:- use_module(library(sparqlprog/owl_search_viz)).

:- rdf_register_prefix('GO', 'http://purl.obolibrary.org/obo/GO_').

:- begin_tests(search_viz_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).


load_test_file :-
        rdf_load('tests/go_nucleus.ttl').

test_output(S,Opts,Expecteds) :-
        test_output(S,Opts,Expecteds,[]).
test_output(S,Opts,Expecteds,Unexpecteds) :-
        F='tests/out.tmp',
        format('Search: ~w Opts: ~w~n',[S,Opts]),
        owl_search_and_display(S,[output(F)|Opts]),
        read_file_to_string(F,Str,[]),
        atom_string(A,Str),
        writeln(A),
        forall(member(E,Expecteds),
               assertion(sub_atom(A,_,_,_,E))),
        forall(member(E,Unexpecteds),
               assertion(\+sub_atom(A,_,_,_,E))),
        !.

        


% TODO: compare output to expected

test(basic) :-
        test_output([cell],[],['intracellular membrane-bounded organelle'],[nucleus]).
test(exact) :-
        test_output(['^nucleus$'],[],[nucleus],[cell]).
test(obo) :-
        test_output(['^nucleus$'],
                    [format(obo)],
                    ['name: nucleus',
                     'A membrane-bounded organelle of eukaryotic cells'],
                    ['name: cell']).
test(superclasses) :-
        test_output(['^nucleus$'],[relations([s])],
                    [nucleus, 'intracellular membrane-bounded organelle'],
                    ['0005623']).
test(ancs) :-
        test_output(['^nucleus$'],[relations([s,'part_of'])],
                    [nucleus,
                     '0005623 ! cell',
                     'intracellular membrane-bounded organelle'],
                    []).
test(subclasses) :-
        test_output(['^organelle$'],[relations([s]), extend_lambda(d)],
                    [nucleus, 'intracellular membrane-bounded organelle'],
                    ['0005623']).

test(dot) :-
        (   shell('og2dot.js -h')
        ->  test_output(['^nucleus$'],[format(dot),relations([s])],
                        ['label=cellular_component'],
                        [])
        ;   writeln('Skipping test, as og2dot not installed')).

test(id) :-
        test_output(['GO:0005634'],[search_property(id)],
                    ['GO:0005634', nucleus],
                    [cell]).


                                %owl_search_and_display(nucleus, a, '.', [], obo, _).
xtest(basic2) :-
        owl_search_and_display('^nucleus$', '.', '.', [], obo, _).
    
:- end_tests(search_viz_test).


