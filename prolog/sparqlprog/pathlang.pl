:- module(pathlang,
          []).

prim_goal(Pred, X, Y, Goal) :-
        var(Pred),
        !,
        Goal = rdf(X, PredIRI, Y).
prim_goal(inverse(Pred), X, Y, Goal) :-
        !,
        prim_goal(Pred, Y, X, Goal).
prim_goal(Pred, X, Y, Goal) :-
        rdf_global_id(Pred,PredIRI),
        rdf_is_iri(PredIRI),
        !,
        Goal = rdf(X, PredIRI, Y).
prim_goal(Pred, X, Y, Goal) :-
        !,
        Goal =.. [Pred, X, Y].
                   
eval( A -> B, Goal ) :-
        % assume head term is an object
        eval( A, B, Goal ).

eval( A, B -> C, (Goal, Goal2) ) :-
        % e.g. A, has_friend -> lives_in ==> has_friend(A, Out), lives_in(Out, ...)
        \+ compound(B),
        !,
        prim_goal(B, A, Out, Goal),
        eval(Out, C, Goal2).

eval( A, B -> C, (Goal, Goal2) ) :-
        % e.g. A, has_friend(Friend) -> lives_in ==> has_friend(A, Friend), lives_in(Friend, ...)        
        compound(B),
        !,
        B =.. [Pred, Out],
        prim_goal(Pred, A, Out, Goal),
        eval(Out, C, Goal2).

eval( A, (B,C), (Goal, Goal2) ) :-
        !,
        eval(A, B, Goal),
        eval(A, C, Goal2).

eval( A, (B | C), (Goal ; Goal2) ) :-
        !,
        eval(A, B, Goal),
        eval(A, C, Goal2).


        