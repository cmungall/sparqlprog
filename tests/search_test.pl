/**

  tests search on prolog in-memory triples

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/search_util)).
:- use_module(library(sparqlprog/ontologies/owl)).

:- begin_tests(search_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('tests/go_nucleus.ttl').

test_count(Goal,ExpectedCount) :-
        writeln(t=Goal),
        findall(Goal,Goal,L),
        maplist(writeln,L),
        length(L,Len),
        assertion(Len == ExpectedCount).

test_once(Goal,Test) :-
        findall(Goal,(Goal,Test),L),
        assertion(L = [_|_]).


test(contains) :-
        test_count(lsearch(cell,_,_),7),
        test_once(lsearch(cell,C,_), C='http://purl.obolibrary.org/obo/GO_0005622').

% regex lib does not support
xxxtest(starts) :-
        test_count(lsearch('^cell',_,_),2),
        test_once(lsearch('^cell',C,_), C='http://purl.obolibrary.org/obo/GO_0005622').

test(ends) :-
        test_count(lsearch('cell$',_,_),1),
        test_once(lsearch('cell$',C,_), C='http://purl.obolibrary.org/obo/GO_0005623').

test(wildcard) :-
        test_count(lsearch('cell.*component',_,_),1).

test(synonym) :-
        test_count(tsearch('protoplast',_),2).





:- end_tests(search_test).


