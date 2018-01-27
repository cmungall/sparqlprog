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
           str_before/3
           ]).

:- use_module(library(semweb/rdf11)).

/* ----------
 https://www.w3.org/TR/sparql11-query/#func-strings

 all string functions take as arguments either:

   - prolog string
   - prolog atom (converted to strong)
   - prolog rdf11 literal: either String@Lang or String^^xsd:string
   
*/

lcase(S,Sdn) :-
        ensure_atom(S,S1),
        downcase_atom(S1,Sdn).


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

str_replace(S,In,Out,NewS) :-
        ensure_atom(S,S1),
        ensure_atom(In,In1),
        ensure_atom(Out,Out1),
        atom_concat(Toks,In1,S1),
        atom_concat(Toks,Out1,NewS).


% convert strlike to atom
ensure_atom(S@_, A) :- !, atom_string(A,S).
ensure_atom(S^^_, A) :- !, atom_string(A,S).
ensure_atom(A, A) :- atom(A), !.
ensure_atom(S, A) :- string(S), !, atom_string(A,S).


seval(Term,Ret) :-
        compound(Term),
        !,
        Term =.. [P|Args],
        maplist(seval,Args,Args2),
        append(Args2,[Ret],GoalArgs),
        Goal =.. [P|GoalArgs],
        Goal.
seval(X,X).

