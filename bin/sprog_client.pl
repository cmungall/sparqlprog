#!/usr/bin/env swipl

:- use_module(library(pengines)).

main :-
    pengine_create([
        server('http://127.0.0.1:9083'),
        src_text("
            t(X):- '??'(go, rdf(X,rdf:type,owl:'ObjectProperty')).
            t(X) :- q(X).
            t(X) :- tobjprop(X).
            q(X) :- p(X).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle, []).


handle(create(ID, _)) :-
    pengine_ask(ID, p(_), []).
%    pengine_ask(ID, (go ?? rdf(X,rdf:type,owl:'ObjectProperty')), []).
handle(success(_ID, [X], false)) :-
    writeln(X).
handle(success(ID, [X], true)) :-
    writeln(X),
    pengine_next(ID, []).
