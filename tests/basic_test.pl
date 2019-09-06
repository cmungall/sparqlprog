
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/test_aux)).
:- use_module(library(sparqlprog)).

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix('','http://example.org/').

:- begin_tests(basic_test).

test(clause) :-
        % TODO: possible bug in 7.6.x swi?
        % this may not be visible depending on order of execution
        assertion(text_aux:clause(my_unary_pred(_X),_Body,_Ref)).


test_select(Q,ExpectedSPARQL) :-
        test_select(Q,[],ExpectedSPARQL).
test_select(Q,Opts,ExpectedSPARQL) :-
        create_sparql_select(Q,Q,SPARQL,Opts),
        format(' ~q ==> ~w~n',[ Q, SPARQL ]),
        assertion( SPARQL = ExpectedSPARQL ).

test_construct(H,Q,ExpectedSPARQL) :-
        create_sparql_construct(H,Q,SPARQL,[]),
        format(' ~q :- ~q ==> ~w~n',[ H, Q, SPARQL ]),
        assertion( SPARQL = ExpectedSPARQL ).


% todo: Test succeeded with choicepoint
test(bindings) :-
        Q=rdf(A,B,C),
        create_sparql_select(Q,Q,SPARQL,[bindings([a=A,b=B,c=C])]),
        assertion( SPARQL = "SELECT ?a ?b ?c WHERE {?a ?b ?c}" ).

test(simple_select) :-
        nl,
        test_select(rdf(_,_,_),"SELECT ?v0 ?v1 ?v2 WHERE {?v0 ?v1 ?v2}"),
        test_select(rdf(_,_,_,_),_),
        test_select(rdf(_,_,_,'':g1),_),
        true.

test(select_subset) :-
        nl,
        create_sparql_select(rdf(A,B,C),rdf(A,B,C),SPARQL1,[]),
        create_sparql_select(foo(A,B,C),rdf(A,B,C),SPARQL2,[]),
        assertion( SPARQL1 == SPARQL2 ),
        create_sparql_select(A,rdf(A,_,_),SPARQL3,[]),
        assertion( SPARQL3 = "SELECT ?v0 WHERE {?v0 ?v1 ?v2}" ),
        true.



test(conj) :-
        test_select( (rdf(S,'':p1,O),
                      rdf(S,'':p2,O)),
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://example.org/p1> ?v1 . ?v0 <http://example.org/p2> ?v1}").

test(disj) :-
        test_select( (   rdf(S,'':p1,O)
                     ;   rdf(S,'':p2,O)),
                     "SELECT ?v0 ?v1 WHERE {{?v0 <http://example.org/p1> ?v1} UNION {?v0 <http://example.org/p2> ?v1}}").

test(negation) :-
        test_select( (rdf(S,'':p1,O),
                      \+ rdf(S,'':p2,O)),
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://example.org/p1> ?v1 . FILTER NOT EXISTS {?v0 <http://example.org/p2> ?v1}}").

test(optional) :-
        test_select( (rdf(S,'':p1,_O),
                      optional(rdf(S,'':p2,_Z))),
                     "SELECT ?v0 ?v1 ?v2 WHERE {?v0 <http://example.org/p1> ?v1 . OPTIONAL {?v0 <http://example.org/p2> ?v2}}").

test(str_eq) :-
        test_select( (rdf(_S,rdfs:label,Label),
                      Label=="foo"),
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . FILTER (?v1 = \"foo\")}").


test(str_eq2) :-
        test_select( rdf(_S,rdfs:label,"foo"),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . FILTER (?v1 = \"foo\")}").

%test(str_eq3) :-
%        test_select( (label(S,Label),
%                      Label=="foo"),
%                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . FILTER (?v1 = \"foo\")}").

test(lang_literal_eq) :-
        test_select( rdf(_S,rdfs:label,"foo"@en),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> \"foo\"@en}").

test(escape_quotes) :-
        test_select( rdf(_S,rdfs:label," \"\" "@en),
                      "SELECT ?v0 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> \" \\\"\\\" \"@en}").


test(str_starts) :-
        test_select( (rdf(_S,rdfs:label,Label),
                      str_starts(Label,"foo")),
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . FILTER (strStarts(?v1,\"foo\"))}").

test(construct) :-
        test_construct(rdf(O,P,S),rdf(S,P,O),_).

test(expand) :-
        test_select( my_unary_pred(_X),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/c1>}").

test(recursive_expand_cycle) :-
        \+ catch(test_select( recursive_subclass_of(_,'':c1), _),
                 _,
                 fail).

% 
test(expand_with_unit_clause) :-
        test_select( (unify_with_iri(C),  % unit clause
                      rdf(_,rdf:type,C)),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://x.org>}").

test(equals) :-
        test_select( (rdf(A,'':p1,X),
                      rdf(B,'':p1,X),
                      A==B),
                     "SELECT ?v0 ?v1 ?v2 WHERE {?v0 <http://example.org/p1> ?v1 . ?v2 <http://example.org/p1> ?v1 . FILTER (?v0 = ?v2)}").

test(not_equals) :-
        test_select( (rdf(A,'':p1,X),
                      rdf(B,'':p1,X),
                      A\=B),
                     "SELECT ?v0 ?v1 ?v2 WHERE {?v0 <http://example.org/p1> ?v1 . ?v2 <http://example.org/p1> ?v1 . FILTER (?v0 != ?v2)}").


test(expand_multi) :-
        test_select( a_or_b(_X),
                     "SELECT ?v0 WHERE {{?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/a>} UNION {?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/b>}}").

test(inject_labels) :-
        test_select( rdf(_A,rdf:type,'':c1),
                     [inject_labels(true)],
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/c1> . OPTIONAL {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1}}").


test(rdf_path) :-
        test_select( rdf(_,zeroOrMore(rdfs:subClassOf),'':c1),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#subClassOf>* <http://example.org/c1>}").


test(rdf_path2) :-
        test_select( rdf_path(_,zeroOrMore(rdfs:subClassOf),'':c1),
                     "SELECT ?v0 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#subClassOf>* <http://example.org/c1>}").

test(nofilter) :-
        test_select( (rdf(_X,rdfs:label,L),
                      rdf(L,'http://www.bigdata.com/rdf/search#search',"foo")),
                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . ?v1 <http://www.bigdata.com/rdf/search#search> \"foo\"}").

test(arith) :-
        test_select( (rdf(_X,'':v,V),
                      _V2 is V/2),
                     "SELECT ?v0 ?v1 ?v2 WHERE {?v0 <http://example.org/v> ?v1 . BIND( (?v1 / 2) AS ?v2 )}").

% rdf11 preds
test(substring) :-
        test_select( ({substring(L,foo)},rdf(_,rdfs:label,L)),
                     "SELECT ?v0 ?v1 WHERE {FILTER (contains(?v0,\"foo\")) . ?v1 <http://www.w3.org/2000/01/rdf-schema#label> ?v0}").
test(like1) :-
        test_select( ({like(L,'foo*')},rdf(_,rdfs:label,L)),
                     "SELECT ?v0 ?v1 WHERE {FILTER (regex(?v0,\"^foo.*\",\"i\")) . ?v1 <http://www.w3.org/2000/01/rdf-schema#label> ?v0}").



%:- debug(sparqlprog).

%xxxtest(refl) :-
%        test_select( refl(_,_),
%                     "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#label> ?v1 . ?v1 <http://www.bigdata.com/rdf/search#search> \"foo\"}").

test(agg_max) :-
        create_sparql_select(MaxVal,
                             aggregate(max(Val),rdf(_,'':v,Val),MaxVal),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT max(?v1) AS ?v0 WHERE {?v2 <http://example.org/v> ?v1}" ).

test(agg_count) :-
        create_sparql_select(Count,
                             aggregate(count(Val),rdf(_,'':v,Val),Count),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT COUNT(?v1) AS ?v0 WHERE {?v2 <http://example.org/v> ?v1}" ).

test(agg_group1) :-
        create_sparql_select(_,
                             aggregate_group(count(D),[C],rdf(C,rdfs:subClassOf,D),_N),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT (COUNT(?v1) AS ?v3) ?v2 WHERE {?v2 <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?v1} GROUP BY ?v2").
test(agg_group2) :-
        create_sparql_select(_,
                             aggregate_group(count(D),[C,P],rdf(C,P,D),_N),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT (COUNT(?v1) AS ?v4) ?v2 ?v3 WHERE {?v2 ?v3 ?v1} GROUP BY ?v2 ?v3").

test(agg_group_having) :-
        create_sparql_select(_,
                             aggregate_group(count(D),[C,P],rdf(C,P,D),count(D)>2,_N),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT (COUNT(?v1) AS ?v4) ?v2 ?v3 WHERE {?v2 ?v3 ?v1} GROUP BY ?v2 ?v3 HAVING (COUNT(?v1) > 2)").

test(agg_group_having2) :-
        create_sparql_select(_,
                             aggregate_group(count(D),[P,group_concat(C," ")],rdf(C,P,D),count(D)>2,_N),
                             SPARQL,
                             []),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT (COUNT(?v1) AS ?v4) ?v2 GROUP_CONCAT(?v3 ;  SEPARATOR = \" \") WHERE {?v3 ?v2 ?v1} GROUP BY ?v2 GROUP_CONCAT(?v3 ;  SEPARATOR = \" \") HAVING (COUNT(?v1) > 2)").
test(rule_expansion) :-
        create_sparql_select(_,
                             foo(_,_),
                             SPARQL,
                             [rule( (foo(A,B) :- rdf(A,rdfs:label,B)))]),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT ?v0 WHERE {?v1 <http://www.w3.org/2000/01/rdf-schema#label> ?v2}").

test(recursive_rule_expansion) :-
        create_sparql_select(_,
                             cls(_),
                             SPARQL,
                             [rule( (mytype(A,B) :- rdf(A,rdf:type,B))),
                              rule( (cls(A) :- mytype(A,owl:'Class')) )
                             ]),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT ?v0 WHERE {?v1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class>}").

test(recursive_rule_expansion_prog) :-
        create_sparql_select(_,
                             cls(_),
                             SPARQL,
                             [rule('mytype(A,B) :- rdf(A,rdf:type,B).'),
                              rule('cls(A) :- mytype(A,owl:\'Class\').')
                             ]),
        format(' Query ==> ~w~n',[ SPARQL ]),
        assertion( SPARQL = "SELECT ?v0 WHERE {?v1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class>}").




test(rdf_predicate) :-
        test_select( rdf_predicate(_),
                     "SELECT ?v0 WHERE {SELECT DISTINCT ?v0 WHERE {[] ?v0 []}}").

test(member) :-
        test_select( member(_A,[1,2,3]),
                     "SELECT ?v0 WHERE {VALUES ?v0 {1 2 3 }}").
test(member2) :-
        test_select( member([_A,_B],[["a",1],["b",2],["c",3]]),
                     "SELECT ?v0 ?v1 WHERE {VALUES (?v0 ?v1) {(\"a\" 1) (\"b\" 2) (\"c\" 3) }}").


test(strlang) :-
        test_select( (X is strlang("x","en")),
                     "SELECT ?v0 WHERE {BIND( STRLANG("x","en") AS ?v0 )}").
test(strlang2) :-
        test_select( (X is strlang(literal('x'),"en")),
                     "SELECT ?v0 WHERE {BIND( STRLANG("x","en") AS ?v0 )}").



% TODO
test(disj) :-
        test_select( is_mammal(_X),
                     "SELECT ?v0 WHERE {{?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/cat>} UNION {?v0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/dog>}}").

:- end_tests(basic_test).


