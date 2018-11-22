/**

*/

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/dataframe)).

:- begin_tests(dataframe,
               []).

person(p1,joe).
person(p2,fred).
person(p3,sue).

owns(p3,item1).
owns(p3,item2).

address(p1,foo_st,sf).
address(p1,bar_st,boston).
address(p2,bar_st,boston).

dataframe:dataframe(person,
                    [
                     [person=ID,
                      name=Name]-person(ID,Name),
                     [street=Street,
                      city=City]-address(ID,Street,City),
                     [owns=Item]-owns(ID,Item)
                    ],
                    [
                     entity(person),
                     entity(owns)
                     ]).



test(df) :-
        rdf_assert(p1,rdfs:label,"foo"),
        rdf_assert(item1,rdfs:label,"fridge"),
        rdf_assert(item2,rdfs:label,"spork"),
        forall(dataframe_header(person,Hdr),
               writeln(Hdr)),
        forall(dataframe_row(person,Row),
               format('~q.~n',[Row])),
        assertion(dataframe_header(person,[person,'person label',name,street,city,owns,'owns label'])),
        assertion((dataframe_row(person,Row),
                   Row=[p1,foo,joe,'bar_st|foo_st','boston|sf',_,_])),
        dataframe_to_csv(person,[]).



  

:- end_tests(dataframe).


