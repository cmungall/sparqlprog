% * -*- Mode: Prolog -*- */

/** <module> emulate builtins

  This module allows you to write programs that can be executed on a triplestore or directly on the in-memory
  SWI-Prolog rdf database.

  Sparqlprog defines predicates such as rdf_path/3 and str_starts/2. These are usually compiled down to SPARQL queries.
  This module provides prolog implementations for these predicates using predicates such as rdf/3

  Many of the predicates here take an argument strlike - this can be either an atom or a string.
  
*/

:- module(emulate_builtins,
          [
           ensure_atom/2,
           ensure_string/2,
           ensure_atoms/2,
           lcase/2,
           regex/2,
           regex/3,
           str_starts/2,
           str_ends/2,
           str_before/3,
           str_after/3,

           str_replace/4,
           concat/3,
           concat/4,

           agg_max/2,
           count/2,
           group_concat/3,
           aggregate_group/4,

           iri_prefix_curie/3,
           iri_prefix/2,
           iri_curie/2,
           curie_prefix/2,

           optional/1,
           rdf_path/3,
           rdf_path/4,

           if/4,
           
           bind/2,
           seval/2,
           eval_to_atom/2
           ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(pcre)).


/*
https://www.w3.org/TR/sparql11-query/#expressions

*/


/* ----------
 https://www.w3.org/TR/sparql11-query/#func-strings

 all string functions take as arguments either:

   - prolog string
   - prolog atom (converted to strong)
   - prolog rdf11 literal: either String@Lang or String^^xsd:string
   
*/

%! lcase(+S:strlike, ?V:str)
%
%
%
lcase(S,V) :-
        ensure_atom(S,S1),
        downcase_atom(S1,V1),
        ensure_string(V1,V).
%! ucase(+S:strlike, ?V:str)
ucase(S,V) :-
        ensure_atom(S,S1),
        upcase_atom(S1,V1),
        ensure_string(V1,V).

% 17.4.2.5 str
str(X,V) :-
        ensure_atom(X,A),
        atom_string(A,V).

% 17.4.2.6 lang
lang(_^^L,L).

%17.4.2.7 datatype
datatype(_^^D, D) :- !.
datatype(_ @ _, xsd:string) :- !.
datatype(_, xsd:string) :- !.


% 17.4.2.8 iri
iri(X,V) :-
        ensure_atom(X,A),
        atom_string(A,V).
uri(X,V) :- iri(X,V).



%! regex(?String, +Pattern, +Flag) is nondet.
%! regex(?String, +Pattern) is nondet.
% 
%    equivalent to REGEX in SPARQL
% 
%    corresponds to re_match/2 in SPARQL
% 
regex(S,P) :-
        regex(S,P,"").

regex(S,P,Flag) :-
        eval_to_string(S,S1),
        eval_to_string(P,P1),
        eval_to_atom(Flag,Flag1),
        re_match(P1/Flag1, S1).


%! eval_to_atom(?X, ?A) is nondet.
%
%   evaluates expression X ensuring A is an atom
%
eval_to_atom(X,A) :-
        seval(X,Y),
        ensure_atom(Y,A).

eval_to_string(X,A) :-
        seval(X,Y),
        ensure_string(Y,A).


%! str_starts(+S:strlike, +Sub:strlike) is semidet.
%
%
%
str_starts(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(Sub1,_,S1).

%! str_ends(+S:strlike, +Sub:strlike) is semidet.
%
%
%
str_ends(S,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sub,Sub1),
        atom_concat(_,Sub1,S1).

%! str_before(+S:strlike, +Sep:strlike,  ?Sub:strlike) is det.
%
%
%
str_before(S,Sep,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat([Sub1|_],Sep1,S1),
        (   var(Sub)
        ->  atom_string(Sub1,Sub)
        ;   ensure_atom(Sub,Sub1)).

%! str_after(+S:strlike, +Sep:strlike,  ?Sub:strlike) is det.
%
%
%
str_after(S,Sep,Sub) :-
        ensure_atom(S,S1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat([_|Toks],Sep1,S1),
        atomic_list_concat(Toks,Sep1,Sub1),
        ensure_atom(Sub,Sub1).

%! str_replace(+S:strlike, +Match:strlike, +Replace:strlike,  ?NewStr:strlike) is det.
%
%
%
str_replace(S,In,Out,NewS) :-
        ensure_atom(S,S1),
        ensure_atom(In,In1),
        ensure_atom(Out,Out1),
        atomic_list_concat(Toks,In1,S1),
        atomic_list_concat(Toks,Out1,NewS1),
        ensure_string(NewS1,NewS).


%! concat(?S1, ?S2, ?S) is nondet.
%
%    equivalent to CONCAT in SPARQL
%
%
concat(S1,S2,S) :-
        concatl([S1,S2],S).

%! concat(+S1, +S2, +S3, ?S) is nondet.
%
%    equivalent to CONCAT in SPARQL
%
concat(S1,S2,S3,S) :-
        concatl([S1,S2,S3],S).


concatl(L,S) :-
        maplist(ensure_atom,L,LA),
        concat_atom(LA,SA),
        ensure_string(SA,S).        

% for compat

%! count(?L, ?N) is nondet.
%
%
%
count(L,N) :- length(L,N).

%! agg_max(?L, ?N) is nondet.
%
%
%
agg_max(L,N) :- aggregate(max(X),member(X,L),N).



%! group_concat(?L, ?Sep, ?V) is nondet.
%
%
%
group_concat(L, Sep, V) :-
        maplist(ensure_atom,L,L1),
        ensure_atom(Sep,Sep1),
        atomic_list_concat(L1,Sep1,V1),
        ensure_string(V1,V).

:- module_transparent(aggregate_group/4).
%! aggregate_group(+AggExpression, +GroupBys:list, +Goal, ?Val) is det.
%
%   perform an aggregate query
%
%    equivalent to GROUP BY queries in SPARQL
%
%    maps to aggregate/3 in prolog
%
aggregate_group(AggExpr, _GroupBys, Goal, Val) :-
        aggregate(AggExpr, Goal, Val).

        

/*

%! aggregate_group(?AggExpr, ?[Witness], ?Goal, ?RWitness-RVal) is nondet.
aggregate_group(AggExpr, [Witness], Goal, RWitness-RVal) :-
        AggExpr =.. [Pred, Val],
        GTerm =.. [Pred, Witness, Val],
        RTerm =.. [Pred, RWitness, RVal],
        aggregate(GTerm,Goal,RTerm).
*/

%:- rdf_meta rdf_path(r,r,r).
%:- rdf_meta rdf_path(r,r,r,g).

%! rdf_path(?S, +Path, ?O, ?G) is nondet.
%! rdf_path(?S, +Path, ?O) is nondet.
%
%  Evaluate an rdf path expression in terms of rdf/3.
%                                
%  See https://www.w3.org/TR/sparql11-query/#propertypaths
%                                
%  ==                              
%  Path = Pred OR \Path OR P|Q OR P\Q OR zeroOrMore(Path) OR oneOrMore(Path) OR inverseOf(Path)
%  ==                              

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

% true if there is a triple A-P-B in G
% where P is either a URI or an atom representation of a CURIE
rdfx(A,P,B,G) :-
        ground(P),
        rdf_global_id(P,Px),
        atomic(Px),
        rdf(A,Px,B,G).

:- rdf_meta iri_prefix_curie(r,o,o).
iri_prefix_curie(IRI, Prefix, CURIE) :-
        ground(IRI),
        !,
        rdf_global_id(Prefix:Local, IRI),
        concat_atom([Prefix,Local],':',CURIE).
iri_prefix_curie(IRI, Prefix, CURIE) :-
        ground(CURIE),
        !,
        concat_atom([Prefix,Local],':',CURIE),
        rdf_global_id(Prefix:Local, IRI).
iri_curie(IRI, CURIE) :-
        iri_prefix_curie(IRI, _, CURIE).
iri_prefix(IRI, Prefix) :-
        iri_prefix_curie(IRI, Prefix, _).
curie_prefix(CURIE, Prefix) :-
        str_before(CURIE,":",Prefix).




%! optional(?G) is det.
%
%   call G once, succeed if G fails
%
%   equivalent to OPTIONAL in SPARQL
%
:- module_transparent(optional/1).
optional(G) :- G,!.
optional(_).

entailed(G,AssertedG) :-
        entailed(G,AssertedG,_,[]).
entailed(G,AssertedG,Goals,_Opts) :-
        G =.. [P|Args],
        map_args_to_equiv(Args,Args2,Goals),
        % TODO: query optimization
        all_succeed(Goals),
        AssertedG =.. [P|Args2],
        AssertedG.

map_args_to_equiv([],[],[]).
map_args_to_equiv([A|L],[A2|L],[G|GL]) :-
        G=owl_equivalent_class(A,A2),
        map_args_to_equiv(L,L,GL).

all_succeed([]).
all_succeed([G|L]) :-
        G,
        all_succeed(L).


        


%! ensure_atom(?Str:strlike, ?Atom:atom) is det
%
% convert a string, literal, or any strlike entity to an atom
ensure_atom(S@_, A) :- !, atom_string(A,S).

ensure_atom(S^^_, A) :- !, string(S), atom_string(A,S).

ensure_atom(A, A) :- atom(A), !.

ensure_atom(S, A) :- string(S), !, atom_string(A,S).

%! ensure_string(?A, ?Str:string) is det
%
% convert an entity to its string representation
ensure_string(S,S) :- string(S),!.

ensure_string(A,S) :- atom(A), !, atom_string(A,S).

ensure_string(A,_) :- throw(ensure_string(A)).


%! ensure_atoms(?Input:list, ?Atoms:list) is det.
%
%
%
ensure_atoms(L, L2) :- maplist([A,B]>>ensure_atom(A,B),L,L2).


ensure_number(N, N) :- number(N),!.
ensure_number(S^^_, N) :- !, atom_string(A,S), atom_number(A,N). % todo: fail if incorrect datatype?
ensure_number(literal(type(_,S)), N) :- !, atom_string(A,S), atom_number(A,N). % todo: fail if incorrect datatype?
ensure_number(S, N) :- string(S), !, atom_string(A,S), atom_number(A,N).
ensure_number(A, N) :- atom(A), !, atom_number(A,N).

% 17.4.1.1 bound
bound(Expr) :- nonvar(Expr).

% 17.4.1.2 IF

%! if(?E1, ?E2, ?E3, ?R) is nondet.
%
%
%
if(E1, E2, E3, R) :-
        (   E1
        ->  bind(E2, R)
        ;   bind(E3, R)).

% 17.4.1.3 COALESCE
coalesce([H|_],R) :-
        bind(H, R),
        nonvar(R),
        !.
coalesce([_|T],R) :-
        coalesce(T,R).

%! exists(E) is semidet.
exists(E) :- \+ \+ E.

% 17.4.1.8 sameTerm : use ==

% 17.4.1.9 IN : use member/2

% 17.4.2.1 isIRI : use rdf_is_iri

%! bind(?ReturnValue,+FunctionTerm)
%
% evaluates a function term.
% The function term can be a:
%  - SPARQL builtin (e.g. coalesce, regex)
%  - An arithmetic term (e.g. 1 + (2 / X) )
%  - An abritrary prolog goal term in which:
%      - the final argument N is treated as result/output
%      - the arguments 1..( N-1) are treated as inputs
%      - inputs should be ground
%      - determinism or semi-determinism assumed when inputs are ground
bind(Expr,Result) :-
        seval(Expr, Result).


%! seval(+FunctionTerm,?ReturnValue)
%
%    evaluates a function term
%  
%    this is the same as bind/2 with args reversed.
:- module_transparent(seval/2).

seval(L,L2) :-
        is_list(L),
        !,
        maplist(seval,L,L2).


seval(V, V) :- var(V),!.

seval(T, T) :- T = _@_, !.

seval(T, T) :- T = _^^_, !.

seval(T, T) :- T = literal(_), !.

seval(T, T) :- is_aggregate_goal(T),!.

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

is_aggregate_goal(aggregate_group(_,_,_,_)).
is_aggregate_goal(aggregate(_,_,_)).

