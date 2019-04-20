:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module('../prolog/sparql_dcg.pl'). % TODO - move this
:- use_module(test_aux).

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix('','http://example.org/').

:- begin_tests(dcg_test).

test(triple) :-
        phrase(select([S,P,O],
                      rdf(S,P,O),
                      []),
               X),
        writeln(X).


:- end_tests(dcg_test).


