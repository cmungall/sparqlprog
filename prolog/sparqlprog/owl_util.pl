:- module(owl_util,
          [enlabel_of/2,
           label_of/3,
           
           owl_some/3,
           owl_all/3,
           owl_equivalent_class/2,
           owl_equivalent_class_asserted/2,
           owl_equivalent_class_asserted_symm/2,
           thing_class/1,
           not_thing_class/1,
           deprecated/1,           
           subclass_of_some/3,
           subclass_cycle/1,
           owl_node_info/4,

           eq_intersection_member/2,
           intersection_member/2,
           rdflist_member/2,

           class_genus/2,
           class_differentia/3,

           owl_edge/3,
           owl_edge/4,
           owl_subgraph/4,

           quads_objects/2,
           quads_dict/2,

           assert_named_individuals/0,
           assert_named_individuals_forall/0,

           ensure_uri/2,
           ensure_curie/2,
           
           common_ancestor/3,
           mrca/3,
           common_descendant/3,
           mrcd/3,

           simj_by_subclass/3,
           simj_by_subclass/5
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/ontologies/owl), []).

:- reexport(library(sparqlprog/ontologies/owl), [ label/2, subClassOf/2 ]).

:-op(300,xfy,some).
:-op(300,xfy,all).

enlabel_of(Label,X) :- label_of(Label,X,en).

label_of(Label,X,Lang) :- rdf(X,rdfs:label,Label@Lang).
label_of(Label,X,_) :- rdf(X,rdfs:label,Label^^xsd:string).
%label_of(Label,X,Lang) :- rdf(X,rdfs:label,Lit), (Lit == Label@Lang ; Lit == Label^^xsd:string).



thing_class(owl:'Thing').
not_thing_class(X) :- X \= owl:'Thing'.
not_thing_class(X) :- X \= owl:'Thing'.

deprecated(X) :- rdf(X,owl:deprecated,"true"^^xsd:boolean).


owl_equivalent_class(A,B) :- rdf_path(A,zeroOrMore( (owl:equivalentClass)| \(owl:equivalentClass)),B).
owl_equivalent_class_asserted(A,B) :- rdf(A,owl:equivalentClass,B).
owl_equivalent_class_asserted(A,B) :- rdf(B,owl:equivalentClass,A).

owl_equivalent_class_asserted_symm(A,B) :-
        (   rdf(A,owl:equivalentClass,B)
        ;    rdf(B,owl:equivalentClass,A)).



subclass_cycle(A) :- rdf_path(A,oneOrMore(rdfs:subClassOf),A).


    
%! owl_some(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression SomeValuesFrom(Property,Obj)
:- rdf_meta owl_some(r,r,r).
owl_some(R,P,O) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

%! subclass_of_some(?Cls, ?Property, ?Obj) is nondet.
%
% true if Cls is a subclass of the expression SomeValuesFrom(Property,Obj)
:- rdf_meta subclass_of_some(r,r,r).
subclass_of_some(C,P,O) :-
        owl:subClassOf(C,R),
        owl_some(R,P,O).


%! owl_all(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression AllValuesFrom(Property,Obj)
owl_all(R,P,O) :-
        owl:onProperty(R,P),
        owl:allValuesFrom(R,O).

owl_restriction(R, some(P,O)) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

owl_restriction(R, all(P,O)) :-
        owl:onProperty(R,P),
        owl:someValuesFrom(R,O).

owl_node_info(S,P,O,E) :-
        rdf(S,P,O),
        bind(S,E).

owl_node_info(S,P,O,Equiv) :-
        owl_equivalent_class(S,Equiv),
        rdf(Equiv,P,O),
        \+ is_blank(O).
owl_node_info(S,P,O,Equiv) :-
        owl_equivalent_class(S,Equiv),
        subclass_of_some(Equiv,P,O).

class_genus(C,G) :-
        eq_intersection_member(C,G),
        \+ is_blank(G).
class_differentia(C,P,Y) :-
        eq_intersection_member(C,R),
        owl_some(R,P,Y).

eq_intersection_member(C,M) :-
        rdf(C,owl:equivalentClass,E),
        intersection_member(E,M).

intersection_member(I,M) :-
        rdf(I,owl:intersectionOf,L),
        rdflist_member(L,M).

% use rdfs_member/2 ?
rdflist_member(L,M) :-
        rdf_path(L,(zeroOrMore(rdf:rest)/(rdf:first)),M).



common_ancestor(X,Y,A) :-
        rdfs_subclass_of(X,A),
        rdfs_subclass_of(Y,A),
        X\=Y.

mrca(X,Y,A) :-
        common_ancestor(X,Y,A),
        \+ ((common_ancestor(X,Y,A2),
             rdf_path(A2,oneOrMore(rdfs:subClassOf),A))).

% NOTE: some of these may move to a utility library
common_descendant(X,Y,D) :-
        rdfs_subclass_of(D,X),
        rdfs_subclass_of(D,Y),
        X\=Y.

mrcd(X,Y,D) :-
        common_descendant(X,Y,D),
        \+ ((common_descendant(X,Y,D2),
             rdf_path(D,oneOrMore(rdfs:subClassOf),D2))).

:- rdf_meta owl_edge(r,r,r,o).
:- rdf_meta owl_edge(r,r,r).

%! owl_edge(?S,?P,?O,?G) is nondet.
%! owl_edge(?S,?P,?O) is nondet.
%
% An edge in an existential graph
owl_edge(S,P,O) :-
        owl_edge(S,P,O,_).

owl_edge(S,P,O,G) :-
        rdf(S,rdfs:subClassOf,R,G),
        owl_some(R,P,O),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(S,rdfs:subClassOf,O,G) :-
        rdf(S,rdfs:subClassOf,O,G),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(S,P,O,G) :-
        rdf(S,P,O,G),
        rdf_is_iri(O),
        rdf(S,rdf:type,owl:'NamedIndividual'),
        rdf(O,rdf:type,owl:'NamedIndividual').
owl_edge(S,rdf:type,O,G) :-
        rdf(S,rdf:type,O,G),
        rdf_is_iri(O),
        rdf(S,rdf:type,owl:'NamedIndividual'),
        \+ rdf_global_id(owl:_,O).


owl_edge_query(Preds,S,^(P),O,G) :-
        nonvar(Preds),
        member(^(P),Preds),
        % inverse
        owl_edge(O,P,S,G).
owl_edge_query(Preds,S,P,O,G) :-
        owl_edge(S,P,O,G),
        opt_member(P,Preds).


inferred_named_individual(I,G) :-
        rdf(I,rdf:type,C,G),
        rdf_is_iri(C),
        \+ rdf_global_id(owl:_,C),
        \+ rdf_global_id(rdfs:_,C).

assert_named_individuals :-
        inferred_named_individual(I,G),
        rdf_assert(I,rdf:type,owl:'NamedIndividual',G),
        debug(owl,'NI: ~w ~w',[I,G]),
        fail.
assert_named_individuals.

assert_named_individuals_forall :-
        rdf_node_graph(I,G),
        rdf_assert(I,rdf:type,owl:'NamedIndividual',G),
        debug(owl,'NI: ~w ~w',[I,G]),
        fail.
assert_named_individuals_forall.

rdf_node_graph(I,G) :- rdf(I,_,_,G).
rdf_node_graph(I,G) :- rdf(_,_,I,G), rdf_is_iri(I), \+ rdf_global_id(owl:'NamedIndividual',I).
        

%! node_ancestor_graph_edge(?Node,?S,?P,?O,?G) is nondet.
%
%  true if S is a reflexive superclass ancestor of Node,
%  and SPO is a triple in G
node_ancestor_graph_edge(Node,S,P,O,G) :-
        rdfs_subclass_of(Node,S),
        rdf(S,P,O,G).

%! owl_subgraph(+Nodes:list, +Preds:list, ?Quads:list, +Opts:list) is det.
%
%  traverses owl edge graph starting from a predefined set of nodes
owl_subgraph(Nodes,Preds,Quads,Opts) :-
        owl_subgraph(Nodes,Preds,[],Quads,[],Opts).

%! owl_subgraph(+Nodes:list, +Preds:list, +Quads:list, ?FinalQuads:list, +Visited:list, +Opts:list) is det.
owl_subgraph([],_,Quads,NQuads,_,_) :-
        maplist(normalize_quad,Quads,NQuads).
owl_subgraph([Node|Nodes],Preds,Quads,FinalQuads,Visited,Opts) :-
        \+ member(Node,Visited),
        debug(owl_graph,'Query node: ~w; PredQ=~w',[Node,Preds]),
        setof(rdf(Node,P,O,G),(owl_edge_query(Preds,Node,P,O,G)),NewQuads),
        debug(owl_graph,'  Quads: ~w',[NewQuads]),
        !,
        setof(O,S^P^G^member(rdf(S,P,O,G),NewQuads),NewNodes),
        ord_union(Quads,NewQuads,AccQuads),
        append(Nodes,NewNodes,NextNodes),
        owl_subgraph(NextNodes,Preds,AccQuads,FinalQuads,[Node|Visited],Opts).
owl_subgraph([Root|Nodes],Preds,Quads,FinalQuads,Visited,Opts) :-
        owl_subgraph(Nodes,Preds,[root(Root)|Quads],FinalQuads,Visited,Opts).

normalize_quad(rdf(S,^(P),O,G),rdf(O,P,S,G)) :- !.
normalize_quad(X,X).


opt_member(_,L) :- var(L),!.
opt_member(X,L) :- member(X,L).

quads_object(Quads,E) :-
        member(rdf(S,P,O,_),Quads),
        (   E=S
        ;   E=P
        ;   E=O).
quads_object(Quads,N) :-
        member(root(N), Quads).

quads_objects(Quads,Objs) :-
        setof(Obj,quads_object(Quads,Obj),Objs).


% transforms quads (triples + graph arg) to obograph JSON dict
quads_dict(Quads, Dict) :-
        setof(Obj,quads_object(Quads,Obj),Objs),
        maplist(owl_object_dict, Objs,  Nodes),
        findall(Q,(member(Q,Quads),Q=rdf(_,_,_,_)),RealQuads),
        maplist(quad_dict, RealQuads,  Edges),
        Dict = graph{nodes:Nodes, edges:Edges}.

quad_dict(rdf(S,P,O,_), Dict) :-
        ensure_curie(S, Sx),
        ensure_curie(P, Px),
        ensure_curie_or_atom(O, Ox),
        Dict = edge{sub:Sx, pred:Px, obj:Ox}. 
owl_object_dict(C, Dict) :-
        ensure_curie(C, Id),
        (   label(C,N1)
        ->  true
        ;   N1=C),
        rdf_node_type(C,Type1),
        upcase_atom(Type1,Type),
        ensure_curie_or_atom(N1, N),
        findall(PV, object_property_value(C,_,_,PV),PVs),
        Meta = meta{ basicPropertyValues: PVs },
        Dict = node{id:Id, type:Type, lbl:N, meta:Meta}.

object_property_value(C,P,V,pv{pred:P,val:V}) :-
        rdf(C,P1,V1),
        ensure_curie(P1,P),
        rdf_is_literal(V1),
        literal_atom(V1,V),
        debug(owl,'~w PV = ~w ~q',[C,P,V]),
        \+ compound(V).

literal_atom(V@_,V).
literal_atom(V^^_,V).

        

rdf_node_type(X,class) :- is_class(X),!.
rdf_node_type(X,property) :- is_property(X),!.
rdf_node_type(X,instance) :- is_instance(X),!.
rdf_node_type(_,unknown).


is_class(X) :- \+ \+ rdf(_,rdf:type,X).
is_class(X) :- rdf(X,rdf:type,owl:'Class').
is_property(X) :- rdf(X,rdf:type,owl:'ObjectProperty').
is_property(X) :- rdf(X,rdf:type,owl:'DataProperty').
is_property(X) :- rdf(X,rdf:type,owl:'TransitiveProperty').
is_property(X) :- rdf(X,rdf:type,owl:'AnnotationProperty').
is_property(X) :- rdf(X,rdf:type,rdfs:'Property').
is_property(X) :- \+ \+ rdf(_,X,_).
is_instance(X) :- rdf(X,rdf:type,C), \+ rdf_global_id(owl:_,C), \+ rdf_global_id(rdfs:_,C).




ensure_curie_or_atom(R ^^ _, R) :- !.
ensure_curie_or_atom(R @ _, R) :- !.
ensure_curie_or_atom(R, Id) :-
        ensure_curie(R, Id).

%! ensure_uri(+Uri, ?CurieOrUriTerm) is det
%
%  translates URI to a CurieOrUriTerm
ensure_curie(In, Id) :-
        rdf_global_id(In, Uri),
        rdf_global_id(Id1, Uri),
        sformat(Id,'~w',[Id1]).

%! ensure_uri(+CurieOrUriTerm, ?Uri) is det
%
%  translates CurieOrUriTerm to a URI.
%  CurieOrUriTerm is either:
%   - a Uri atom
%   - a Pre:Post CURIE term
%   - an atom of the form 'Pre:Post'
ensure_uri(Pre:Post, Uri) :-
        !,
        rdf_global_id(Pre:Post,Uri).
ensure_uri(Id, Uri) :-
        concat_atom([Pre,Post],:,Id),
        rdf_current_prefix(Pre,_),
        !,
        rdf_global_id(Pre:Post,Uri).
ensure_uri(X, Uri) :-
        rdf_global_id(X,Uri).


% TODO: move to other module
simj_by_subclass(C1,C2,S) :-
        simj_by_subclass(C1,C2,S,_,_).
simj_by_subclass(C1,C2,S,N1,N2) :-
        owl:class(C1),
        owl:class(C2),
        aggregate(count,X,(rdfs_subclass_of(X,C1) , rdfs_subclass_of(X,C2)),N1),
        aggregate(count,X,(rdfs_subclass_of(X,C1) ; rdfs_subclass_of(X,C2)),N2),
        N2 > 0,
        S is N1/N2.

        
