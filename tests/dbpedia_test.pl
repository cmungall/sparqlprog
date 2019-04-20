
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/ontologies/dbpedia)).

:- begin_tests(dbpedia_test).

dbp_artist_child_directed(Artist,Child,Movie) :-
        dbp ?? musical_artist(Artist),has_child(Artist,Child),directed(Child,Movie).

test_q(Q,Expected) :-
        findall(Q,Q,Results),
        maplist(writeln,Results),
        assertion( member(Expected, Results) ).


:- debug(sparqlprog).

test(films_directed_by_musicians_children) :-
        test_q(dbp_artist_child_directed(_,_,_),
               dbp_artist_child_directed('http://dbpedia.org/resource/David_Bowie',
                                     'http://dbpedia.org/resource/Duncan_Jones',
                                     'http://dbpedia.org/resource/Moon_(film)')).


:- end_tests(dbpedia_test).


