% * -*- Mode: Prolog -*- */

/*
https://www.w3.org/TR/sparql11-query/#expressions
*/
:- module(emulate_builtins,
          [
           str_starts/2,
           str_ends/2,
           str_before/3
           ]).

:- use_module(library(semweb/rdf11)).

% ----------
% https://www.w3.org/TR/sparql11-query/#func-strings
% ----------

str_starts(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(Sub1,_,S1).

str_ends(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(_,Sub1,S1).

str_before(S,Sep,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat([Sub1|_],Sep1,S1),
        (   var(Sub)
        ->  atom_string(Sub1,Sub)
        ;   ensure_atom(Sub,Sub1)).


ensure_atom(S@_, A) :- !, atom_string(A,S).
ensure_atom(S^^_, A) :- !, atom_string(A,S).
ensure_atom(A, A) :- atom(A), !.
ensure_atom(S, A) :- string(S), !, atom_string(A,S).

