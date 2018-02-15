% * -*- Mode: Prolog -*- */

/*
https://www.w3.org/TR/sparql11-query/#expressions
*/
:- module(emulate_builtins,
          [
           ensure_atom/2,
           lcase/2,
           str_starts/2,
           str_ends/2,
           str_before/3,

           str_replace/4,

           count/2,
           group_concat/3,

           optional/1,
           rdf_path/3,
           rdf_path/4,
           
           seval/2
           ]).

:- use_module(library(semweb/rdf11)).

/*
:- use_module(library(regex)).

% TODO
regex_str(R,X) :-
        regex_str(R,X,'').
regex_str(R,X,Flag) :-
        X = S^^_,
        atom_string(S,A),
        A =~ R/Flag.
*/

/* ----------
 https://www.w3.org/TR/sparql11-query/#func-strings

 all string functions take as arguments either:

   - prolog string
   - prolog atom (converted to strong)
   - prolog rdf11 literal: either String@Lang or String^^xsd:string
   
*/

lcase(S,V) :-
        ensure_atom(S,S1),
        downcase_atom(S1,V1),
        ensure_string(V1,V).


%! str_starts(+S:strlike, +Sub:strlike) is semidet
str_starts(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(Sub1,_,S1).

%! str_ends(+S:strlike, +Sub:strlike) is semidet
str_ends(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(_,Sub1,S1).

%! str_before(+S:strlike, +Sep:strlike,  ?Sub:strlike) is det
str_before(S,Sep,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat([Sub1|_],Sep1,S1),
        (   var(Sub)
        ->  atom_string(Sub1,Sub)
        ;   ensure_atom(Sub,Sub1)).

%! str_replace(+S:strlike, +Match:strlike, +Replace:strlike,  ?NewStr:strlike) is det
str_replace(S,In,Out,NewS) :-
        ensure_atom(S,S1),
        ensure_atom(In,In1),
        ensure_atom(Out,Out1),
        atomic_list_concat(Toks,In1,S1),
        atomic_list_concat(Toks,Out1,NewS1),
        ensure_string(NewS1,NewS).

% for compat
count(L,N) :- length(L,N).
max(L,N) :- aggregate(max(X),member(X,L),N).


% aggregates TODO
group_concat(L, Sep, V) :-
        maplist(ensure_atom,L,L1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat(L1,Sep1,V1),
        ensure_string(V1,V).

%:- rdf_meta rdf_path(r,r,r).
%:- rdf_meta rdf_path(r,r,r,g).

rdf_path(A,P,B) :-  rdf_path(A,P,B,_).

rdf_path(A,(\P),B,G) :-  rdf_path(B,P,A,G).
rdf_path(A,inverseOf(P),B,G) :-  rdf_path(B,P,A,G).
rdf_path(A,(P|Q),B,G) :-  (rdf_path(A,P,B,G) ;rdf_path(A,Q,B,G)).
rdf_path(A,(P/Q),B,G) :-  rdf_path(A,P,Z,G), rdf_path(Z,Q,B,G).

rdf_path(A,zeroOrMore(_),A,_).
rdf_path(A,zeroOrMore(P),B,G) :-  rdf_path(A,oneOrMore(P),B,G).

rdf_path(A,oneOrMore(P),B,G) :-  rdf_path(A,P,B,G).
rdf_path(A,oneOrMore(P),B,G) :-  rdfx(A,P,Z,G),rdf_path(Z,oneOrMore(P),B,G).
rdf_path(A,P,B,G) :- rdfx(A,P,B,G).

rdfx(A,P,B,G) :-
        ground(P),
        rdf_global_id(P,Px),
        atomic(Px),
        rdf(A,Px,B,G).


optional(G) :- G,!.
optional(_).





% convert strlike to atom
ensure_atom(S@_, A) :- !, atom_string(A,S).
ensure_atom(S^^_, A) :- !, atom_string(A,S).
ensure_atom(A, A) :- atom(A), !.
ensure_atom(S, A) :- string(S), !, atom_string(A,S).

ensure_string(S,S) :- string(S),!.
ensure_string(A,S) :- atom(A), !, atom_string(A,S).
ensure_string(A,_) :- throw(ensure_string(A)).


ensure_number(N, N) :- number(N),!.
ensure_number(S^^_, N) :- !, atom_string(A,S), atom_number(A,N). % todo: fail if incorrect datatype?
ensure_number(S, N) :- string(S), !, atom_string(A,S), atom_number(A,N).
ensure_number(A, N) :- atom(A), !, atom_number(A,N).


%! seval(+FunctionTerm,?ReturnValue)
%
% evaluates a function term
seval(L,L2) :-
        is_list(L),
        !,
        maplist(seval,L,L2).

seval(V, V) :- var(V),!.
seval(T, T) :- T = _@_, !.
seval(T, T) :- T = _^^_, !.
seval(Term, Ret) :-
        compound(Term),
        !,
        Term =.. [P|Args],
        maplist(seval,Args,EArgs),
        (   ArithTerm =.. [P|EArgs],
            current_arithmetic_function(ArithTerm),
            maplist(ensure_number,EArgs,NArgs),
            ArithTerm2 =.. [P|NArgs],
            arithmetic_expression_value(ArithTerm2,Ret)
        ->  true
        ;   append(EArgs,[Ret],GoalArgs),
            Goal =.. [P|GoalArgs],
            Goal).
seval(X,X).

