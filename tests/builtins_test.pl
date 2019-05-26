
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/emulate_builtins)).

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix('','http://example.org/').

:- begin_tests(bultins_test).



% string test functions should accept as input: str, atom, lang-literal, str-literal
test(str_starts1) :- assertion(str_starts("abc","ab")).
test(str_starts2) :- assertion(str_starts("abc"@en,"ab")).
test(str_starts3) :- assertion(str_starts("abc"^^xsd:string,"ab")).
test(str_starts4) :- assertion(str_starts(abc,"ab")).
test(str_starts5) :- assertion(str_starts(abc,ab)).
test(str_starts6) :- assertion(str_starts("abc",ab)).

test(str_ends) :- assertion(str_ends("abc","bc")).

test(str_replace) :- assertion(str_replace("abc","ab","x","xc")).
test(str_replace_eval) :-
        seval(str_replace("ABC DEF GHI"," ","X"),S),
        assertion(S == "ABCXDEFXGHI").

test(reflexive) :- bind("x",X),
        assertion( X=="x").
xxtest(nested) :-
        bind(lcase("X"), X),
        assertion( X=="x").
test(nested) :- bind(lcase(ucase(lcase("X"))), X),
        assertion( X=="x").

test(lcase1) :- assertion(lcase("ABC","abc")).
test(lcase2) :-
        assertion(lcase("ABC",X)),
        assertion(X="abc").
test(lcase_eval) :-
        seval(lcase("AbC"),S),
        assertion(S == "abc").

test(seval1) :-
        assertion( seval(append([a,b],[c,d]),[a,b,c,d]) ).
test(seval_nested) :-
        seval(lcase(str_replace("ABC DEF GHI"," ","X")),S),
        assertion(S == "abcxdefxghi").

test(arith1) :-
        assertion( seval(1+2,3) ).
test(arith_nested) :-
        assertion( seval(1+(2*3),7) ).

test(arith_types) :-
        One="1"^^xsd:integer,
        Two="2",
        assertion( seval(One+Two,3) ).

test(agg) :-
        assertion( seval(count(append([a,b],[c])), 3) ).
test(max) :-
        assertion( seval(agg_max(append([1,2],[3,5])), 5) ).

test(agg_group) :-
        forall(aggregate_group(max(D),[C],entity_value(C,D),R),
               format('Max for ~w is ~w~n',[C,R])),
        findall(C-R,
                aggregate_group(max(D),[C],entity_value(C,D),R),
                L),
        true,
        assertion(L=[_,_,_]),
        assertion(member(a-3,L)),
        assertion(member(b-2,L)),
        assertion(member(c-4,L)).
/*
test(agg_group2) :-
        findall(sum(P,R),
                aggregate_group(sum(Sal),[P],sal(P,J,Sal),R),
                L),
        maplist(writeln,L),
        assertion(L=[_,_,_]).
*/

test(intersects) :-
        X=[a,b,c,d],
        Y=[c,d,e,f],
        assertion( seval(intersection(X,Y), [c,d]) ).
test(n_intersects) :-
        X=[a,b,c,d],
        Y=[c,d,e,f],
        assertion( seval(count(intersection(X,Y)) / 2, 1) ).

test(jac) :-
        X=[a,b,c,d],
        Y=[c,d,e,f,g,h],
        assertion( (seval(count(intersection(X,Y)) / count(union(X,Y)), N),
                    N=0.25)
                   ).

entity_value(a,1).
entity_value(a,2).
entity_value(a,3).
entity_value(b,1).
entity_value(b,2).
entity_value(c,4).

sal(p1,j1,100).
sal(p1,j2,200).

sal(p2,j1,200).
sal(p2,j2,400).

sal(p3,j3,800).





        

    
:- end_tests(bultins_test).
