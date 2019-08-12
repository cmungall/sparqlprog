
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/labelutils)).

:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').
:- rdf_register_prefix(oboInOwl,'http://www.geneontology.org/formats/oboInOwl#').


:- begin_tests(labelutils_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        % load into a test-specific graph due to cache issue
        rdf_load('tests/go_nucleus.ttl',[cache(false),
                                         graph(labelutils)]).

test(show) :-
        T=rdf(_,_,_),
        forall(T,
               (   row_labelify(T,X),
                   writeln(X))).

test(labelify) :-
        row_labelify(foo('http://purl.obolibrary.org/obo/GO_0044464',bar),Row),
        assertion(Row = foo('GO:0044464','cell part',bar,'')).
test(labelify_list) :-
        row_labelify(foo(x,['http://purl.obolibrary.org/obo/GO_0044464',y]), Row),
        assertion(Row = foo(x,'','GO:0044464|y','cell part|')).




:- end_tests(labelutils_test).


