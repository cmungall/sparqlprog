:- module(owl_util,
          [is_en/1,
           enlabel_of/2,
           label_of/2,
           label_of/3,

           literal_atom/2,

           declare_shacl_prefixes/0,

           triple_axiom/2,
           triple_axiom/4,
           triple_axiom_annotation/3,
           triple_axiom_annotation/5,
           triple_axiom_annotations/4,
           triple_property_axiom_annotations/5,
           axiom_annotation/3,

           owl_some/3,
           owl_all/3,
           owl_equivalent_class/2,
           owl_equivalent_class_asserted/2,
           owl_equivalent_class_asserted_symm/2,
           owl_equivalent_property_asserted_symm/2,
           thing_class/1,
           not_thing_class/1,
           deprecated/1,           
           subclass_of_some/3,
           subclass_cycle/1,
           owl_node_info/4,
           bnode_signature/2,
           
           eq_intersection_member/2,
           intersection_member/2,
           rdflist_member/2,

           class_genus/2,
           class_differentia/3,

           owl_edge/3,
           owl_edge/4,
           owl_subgraph/4,
           owl_edge_ancestor/2,
           owl_edge_ancestor/3,
           
           extract_subontology/3,

           quads_objects/2,
           quads_dict/2,

           assert_named_individuals/0,
           assert_named_individuals_forall/0,

           ensure_uri/2,
           ensure_curie/2,
           subsumed_prefix_namespace/4,
           
           common_ancestor/3,
           mrca/3,
           common_descendant/3,
           mrcd/3,
           egraph_common_ancestor/3,
           egraph_mrca/3,

           simj_by_subclass/3,
           simj_by_subclass/5,

           owl_assert_axiom/1,
           owl_assert_axiom/2,
           owl_assert_axiom/3,
           owl_assert_axiom_with_anns/3,
           owl_assert_axiom_with_anns/4
          ]).

/** <module> OWL ontology wrapper

This module provides predicates for working with OWL ontologies. Although OWL ontologies can be accessed directly via rdf/3 triples, this can be quite a low level means of access, especially for ontologies employing constructs that map to multiple triples, including:

  * owl_some/3 and subclass_of_some/3 for existential restrictions (e.g. finger SubClassOf part-of SOME hand)
  * axiom_annotation/3

## Note on use outside sparqlprog

Although this is distributed with sparqlprog, it can be used directly
in conjunction with an in-memory triplestore.
  
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/ontologies/owl), []).

:- reexport(library(sparqlprog/ontologies/owl), [ label/2, subClassOf/2 ]).

:-op(300,xfy,some).
:-op(300,xfy,all).

is_en(X) :- rdf_where(lang_matches(X,"en")).


enlabel_of(Label,X) :- label_of(Label,X,en).


%! label_of(?Label, ?X) is nondet.
%
%
%
label_of(Label,X) :- label_of(Label,X,_).

%! label_of(?Label, ?X, ?Lang) is nondet.
%
%
%
label_of(Label,X,Lang) :- rdf(X,rdfs:label,Label@Lang).
label_of(Label,X,_) :- rdf(X,rdfs:label,Label^^xsd:string).
%label_of(Label,X,Lang) :- rdf(X,rdfs:label,Lit), (Lit == Label@Lang ; Lit == Label^^xsd:string).

declare_shacl_prefixes :-
        rdf(X,'http://www.w3.org/ns/shacl#prefix',Prefix1),
        rdf(X,'http://www.w3.org/ns/shacl#namespace', NS1),
        emulate_builtins:ensure_atom(Prefix1,Prefix),
        emulate_builtins:ensure_atom(NS1,NS),
        rdf_register_prefix(Prefix, NS),
        debug(owl_util,'Registered ~w ~w',[Prefix,NS]),
        fail.
declare_shacl_prefixes.


%! triple_axiom(T,A) is nondet.
%
%  as triple_axiom/4, first argument is triple(I,P1,J)
triple_axiom(rdf(I,P,J),A) :-
        triple_axiom(I,P,J,A).

%! triple_axiom(?I, ?P, ?J, ?A) is nondet.
%
%
%
triple_axiom(I,P,J,A) :-
        rdf(A,owl:annotatedSource,I),
        rdf(A,owl:annotatedProperty,P),
        rdf(A,owl:annotatedTarget,J).

:- rdf_meta triple_axiom_annotation(r,r,o).

%! triple_axiom_annotation(?T, ?P, ?V) is nondet.
%
%  as triple_axiom_annotation/5, first argument is triple(I,P1,J)
% 
triple_axiom_annotation(T,P,V) :-
        triple_axiom(T,A),
        axiom_annotation(A,P,V).

:- rdf_meta triple_axiom_annotation(r,r,o,r,o).

%! triple_axiom_annotation(?I, ?P1, ?J, ?P, ?V) is nondet.
%
%
%
triple_axiom_annotation(I,P1,J,P,V) :-
        triple_axiom(I,P1,J,A),
        axiom_annotation(A,P,V).

:- rdf_meta triple_axiom_annotations(r,r,o,-).

%! triple_axiom_annotations(?I, ?P, ?J, ?L) is nondet.
%
%
%
triple_axiom_annotations(I,P,J,L) :-
        rdf(I,P,J),
        findall(P1-V1,triple_axiom_annotation(I,P,J,P1,V1),L).

:- rdf_meta triple_property_axiom_annotations(r,r,o,r,-).
%! triple_property_axiom_annotations(?I, ?P, ?J, ?P1, ?L:list) is nondet.
%
%  for a triple IPJ, yield all axiom annotation values
%  for annotation property P1
%
%
triple_property_axiom_annotations(I,P,J,P1,L) :-
        rdf(I,P,J),
        findall(V1,triple_axiom_annotation(I,P,J,P1,V1),L).
        

%! axiom_annotation(?Axiom, ?Property, ?Value) is nondet.
%
%    Axiom is always a blank node
%
%    See https://www.w3.org/TR/owl2-primer/#Annotating_Axioms_and_Entities
%
axiom_annotation(A,P,V) :-
        rdf(A,P,V),
        rdf(A,rdf:type,owl:'Axiom'),
        P \= 'http://www.w3.org/2002/07/owl#annotatedProperty',
        P \= 'http://www.w3.org/2002/07/owl#annotatedSource',
        P \= 'http://www.w3.org/2002/07/owl#annotatedTarget',
        \+ ((P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
             V='http://www.w3.org/2002/07/owl#Axiom')).



thing_class(owl:'Thing').

%! not_thing_class(?X) is nondet.
%
%   true unless X is owl:Thing
%
not_thing_class(X) :- X \= owl:'Thing'.


%! deprecated(?X) is nondet.
%
%    true if X has a owl:deprecated axiom with value true
%
deprecated(X) :- rdf(X,owl:deprecated,"true"^^xsd:boolean).



%! owl_equivalent_class(?A, ?B) is nondet.
%
%   inferred equivalent class between A and B, exploiting transitivity and symmetry
%
owl_equivalent_class(A,B) :- rdf_path(A,zeroOrMore( (owl:equivalentClass)| \(owl:equivalentClass)),B).

%! owl_equivalent_class_asserted(?A, ?B) is nondet.
%
%   only holds if the assertion is in the direction from A to B
%
owl_equivalent_class_asserted(A,B) :- rdf(A,owl:equivalentClass,B).
%owl_equivalent_class_asserted(A,B) :- rdf(B,owl:equivalentClass,A).


%! owl_equivalent_class_asserted_symm(?A, ?B) is nondet.
%
%   inferred equivalent class between A and B, exploiting symmetry
%
owl_equivalent_class_asserted_symm(A,B) :-
        (   rdf(A,owl:equivalentClass,B)
        ;    rdf(B,owl:equivalentClass,A)).

%! owl_equivalent_property_asserted_symm(?A, ?B) is nondet.
%
%   inferred equivalent property between A and B, exploiting symmetry
%
owl_equivalent_property_asserted_symm(A,B) :-
        (   rdf(A,owl:equivalentProperty,B)
        ;    rdf(B,owl:equivalentProperty,A)).




%! subclass_cycle(?A) is nondet.
%
%   true if there is a path between A and A following one or more subClassOf links
%
subclass_cycle(A) :- rdf_path(A,oneOrMore(rdfs:subClassOf),A).


%! bnode_signature(?N, ?X) is nondet.
%
%    true if X is in the signature of the construct defined by blank node N
%
bnode_signature(N,X) :-
        rdf(N,_,X),
        \+ rdf_is_bnode(X).
bnode_signature(N,X) :-
        rdf(N,_,Y),
        rdf_is_bnode(Y),
        bnode_signature(Y,X).



    
%! owl_some(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is a blank node representing OWL expression SomeValuesFrom(Property,Obj)
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


%! owl_node_info(+S, ?P, ?O, ?E) is nondet.
%
%  find asserted or inferred triples for S
%
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


%! class_genus(?C, ?G) is nondet.
%
%    true if C EquivalentTo .... and .... and G and ...
%
class_genus(C,G) :-
        eq_intersection_member(C,G),
        \+ rdf_is_bnode(G).

%! class_differentia(?C, ?P, ?Y) is nondet.
%
%    true if C EquivalentTo .... and .... and (P some Y) and ...
%
class_differentia(C,P,Y) :-
        eq_intersection_member(C,R),
        owl_some(R,P,Y).


%! eq_intersection_member(?C, ?M) is nondet.
%
%    true if C EquivalentTo .... and .... and M and ...
%
eq_intersection_member(C,M) :-
        rdf(C,owl:equivalentClass,E),
        intersection_member(E,M).

%! intersection_member(?I, ?M) is nondet.
%
%    true if I is a blank node representing an intersection, and M is a member of the list
%
intersection_member(I,M) :-
        rdf(I,owl:intersectionOf,L),
        rdflist_member(L,M).

%! rdflist_member(?L, ?M) is nondet.
%
%   see also rdfs_member/2 
%
%   this is an alternate implementation that makes the expansion to an rdf list explicit
%
rdflist_member(L,M) :-
        rdf_path(L,(zeroOrMore(rdf:rest)/(rdf:first)),M).






%! common_ancestor(?X, ?Y, ?A) is nondet.
%
%
%  MAY MOVE TO ANOTHER MODULE
common_ancestor(X,Y,A) :-
        rdfs_subclass_of(X,A),
        rdfs_subclass_of(Y,A),
        X\=Y.


%! mrca(?X, ?Y, ?A) is nondet.
%
%   most recent common ancestor
%
%  MAY MOVE TO ANOTHER MODULE
mrca(X,Y,A) :-
        common_ancestor(X,Y,A),
        \+ ((common_ancestor(X,Y,A2),
             rdf_path(A2,oneOrMore(rdfs:subClassOf),A))).

% NOTE: some of these may move to a utility library

%! common_descendant(?X, ?Y, ?D) is nondet.
%
%
%  MAY MOVE TO ANOTHER MODULE
common_descendant(X,Y,D) :-
        rdfs_subclass_of(D,X),
        rdfs_subclass_of(D,Y),
        X\=Y.


%! mrcd(?X, ?Y, ?D) is nondet.
%
%
%  MAY MOVE TO ANOTHER MODULE
mrcd(X,Y,D) :-
        common_descendant(X,Y,D),
        \+ ((common_descendant(X,Y,D2),
             rdf_path(D,oneOrMore(rdfs:subClassOf),D2))).


%! egraph_common_ancestor(?X, ?Y, ?A) is nondet.
%
%
%  version of common_ancestor/3 for graphs that have entailments materialized (egraphs)
%
%  MAY MOVE TO ANOTHER MODULE
egraph_common_ancestor(X,Y,A) :-
        subClassOf(X,A),
        subClassOf(Y,A),
        X\=Y.


%! egraph_mrca(?X, ?Y, ?A) is nondet.
%
%
%  version of mrca/3 for graphs that have entailments materialized (egraphs)
%
%
%  MAY MOVE TO ANOTHER MODULE
egraph_mrca(X,Y,A) :-
        egraph_common_ancestor(X,Y,A),
        \+ ((egraph_common_ancestor(X,Y,A2),
             A2\=A,
             subClassOf(A2,A))).


:- rdf_meta owl_edge(r,r,r,o).
:- rdf_meta owl_edge(r,r,r).

%! owl_edge(?S,?P,?O,?G) is nondet.
%! owl_edge(?S,?P,?O) is nondet.
%
%     An edge in an existential graph
%
%  Either: S SubClassOf O
%      Or: S SubClassOf P some O
%      Or: S EquivalentTo O
%      Or: S type O
owl_edge(S,P,O) :-
        owl_edge(S,P,O,_).


owl_edge(S,P,O,G) :-
        % S sub (P some O)
        rdf(S,rdfs:subClassOf,R,G),
        owl_some(R,P,O),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(S,rdfs:subClassOf,O,G) :-
        % S sub O
        rdf(S,rdfs:subClassOf,O,G),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(S,owl:equivalentClass,O,G) :-
        % S = O
        rdf(S,owl:equivalentClass,O,G),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(O,owl:equivalentClass,S,G) :-
        % O = S (symmetric form)
        rdf(S,owl:equivalentClass,O,G),
        \+ rdf_is_bnode(O),
        \+ rdf_is_bnode(S).
owl_edge(S,P,O,G) :-
        % triple / object property assertion
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

owl_edge_ancestor(S,A) :-
        owl_edge_ancestor(S,A,_).
owl_edge_ancestor(S,A,Preds) :-
        rdf_subject(S),
        owl_subgraph([S],Preds,QL,[]),
        member(rdf(_,_,A,_),QL).



%! extract_subontology(?Objs, ?G, ?Opts) is nondet.
%
%
%
extract_subontology(Objs, G, Opts) :-
        findall(T,(member(Obj,Objs),owl_object_triple(Obj,T,Objs,Opts)),Ts),
        forall(member(rdf(S,P,O),Ts),
               rdf_assert(S,P,O,G)).

owl_object_triple(Obj,T,_Objs,_Opts) :-
        T=rdf(Obj,rdf:type,_),
        T.
owl_object_triple(Obj,T,Objs,_Opts) :-
        T=rdf(Obj,_P,O),
        T,
        (   rdf_is_iri(O)
        ->  member(O,Objs)
        ;   true).

owl_object_triple(Obj,T,Objs,_Opts) :-
        T1=rdf(Obj,_,Z),
        T1,
        rdf_is_bnode(Z),
        \+ \+ (rdf(Z,_,O),member(O,Objs)),
        (   T=T1
        ;   T=rdf(Z,_,O),
            T,
            rdf_is_iri(O)).

        

opt_member(_,L) :- var(L),!.
opt_member(X,L) :- member(X,L).

quads_object(Quads,E) :-
        member(rdf(S,P,O,_),Quads),
        (   E=S
        ;   E=P
        ;   E=O).
quads_object(Quads,N) :-
        member(root(N), Quads).


%! quads_objects(?Quads, ?Objs) is nondet.
%
%
%
quads_objects(Quads,Objs) :-
        setof(Obj,quads_object(Quads,Obj),Objs).


% transforms quads (triples + graph arg) to obograph JSON dict
% TODO: move this

%! quads_dict(?Quads, ?Dict) is nondet.
%
%   generates a OBO JSON object from a set of triples or quads
%
%  Quads = [rdf(S,P,O,G), ...]
%
quads_dict(Quads, Dict) :-
        setof(Obj,quads_object(Quads,Obj),Objs),
        maplist(owl_object_dict, Objs,  Nodes),
        findall(Q,(member(Q,Quads),Q=rdf(_,_,_,_)),RealQuads),
        maplist(quad_dict, RealQuads,  Edges),
        Dict = graph{nodes:Nodes, edges:Edges}.

quad_dict(rdf(S,P,O,_), Dict) :-
        ensure_curie(S, Sx),
        ensure_curie(P, Px1),
        ensure_curie_or_atom(O, Ox),
        (   Px1='rdfs:subClassOf'
        ->  Px=is_a
        ;   Px=Px1),
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
        findall(xref{val: Xref}, (rdf(C,'http://www.geneontology.org/formats/oboInOwl#hasDbXref',X1),ensure_atom(X1,Xref)), Xrefs),
        Meta = meta{ basicPropertyValues: PVs, xrefs: Xrefs },
        Dict = node{id:Id, type:Type, lbl:N, meta:Meta}.

object_property_value(C,P,V,pv{pred:P,val:V}) :-
        rdf(C,P1,V1),
        ensure_curie(P1,P),
        rdf_is_literal(V1),
        literal_atom(V1,V),
        debug(owl,'~w PV = ~w ~q',[C,P,V]),
        \+ compound(V).

literal_atom(V@_,V) :- !.
literal_atom(V^^_,V) :- !.
literal_atom(literal(type(_,V)),V) :- !.
literal_atom(literal(lang(_,V)),V) :- !.
literal_atom(literal(V),V) :- !.
literal_atom(V,V) :- !.

        

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

%! ensure_curie(+Uri, ?CurieOrUriTerm) is det
%
%  translates URI to a CurieOrUriTerm

ensure_curie(In, Id) :-
        rdf_global_id(In, Uri),
        rdf_global_id(Id1, Uri),
        format(atom(Id2),'~w',[Id1]),
        strip_trailing_colon(Id2,Id).

strip_trailing_colon(A,B) :-        atom_concat(B,':',A),!.
strip_trailing_colon(A,A).


%! subsumed_prefix_namespace(?Pre, ?NS, ?Pre2, ?NS2) is nondet.
%
%
%
subsumed_prefix_namespace(Pre, NS, Pre2, NS2) :-
        rdf_current_prefix(Pre, NS),
        rdf_current_prefix(Pre2, NS2),
        NS \= NS2,
        atom_concat(NS,_,NS2).

        

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

%! simj_by_subclass(?C1, ?C2, ?S) is nondet.
%
%
%
simj_by_subclass(C1,C2,S) :-
        simj_by_subclass(C1,C2,S,_,_).

%! simj_by_subclass(?C1, ?C2, ?S, ?N1, ?N2) is nondet.
%
%
%
simj_by_subclass(C1,C2,S,N1,N2) :-
        owl:class(C1),
        owl:class(C2),
        aggregate(count,X,(rdfs_subclass_of(X,C1) , rdfs_subclass_of(X,C2)),N1),
        aggregate(count,X,(rdfs_subclass_of(X,C1) ; rdfs_subclass_of(X,C2)),N2),
        N2 > 0,
        S is N1/N2.

        
                                % === WRITING ====

%! owl_assert_axiom(+Axiom, ?MainTriple, +Graph:iri) is det.
%! owl_assert_axiom(+Axiom, +Graph:iri) is det.
%! owl_assert_axiom(+Axiom) is det.
%
%  asserts an axiom
%  
owl_assert_axiom(Axiom) :-
        rdf_default_graph(G),
        owl_assert_axiom(Axiom,G).

owl_assert_axiom(Axiom,G) :-
        owl_assert_axiom(Axiom,_,G).
owl_assert_axiom(equivalentTo(A,B),rdf(A,owl:equivalentClass,B),G) :-
        !,
        debug(def, 'Saving ~w = ~w to ~w',[A,B,G]),
        owl_assert_expression(A,Ax,G),
        owl_assert_expression(B,Bx,G),
        rdf_assert(Ax,owl:equivalentClass,Bx,G).
owl_assert_axiom(subClassOf(A,B),rdf(A,rdfs:subClassOf,B),G) :-
        !,
        owl_assert_expression(A,Ax,G),
        owl_assert_expression(B,Bx,G),
        rdf_assert(Ax,rdfs:subClassOf,Bx,G).
owl_assert_axiom(T,T,G) :-
        % fallback
        T=rdf(S,P,O),
        !,
        rdf_assert(S,P,O,G).

%! owl_assert_axiom_with_anns(+Axiom, ?MainTriple, +Graph:iri, +Annotations:list) is det.
%! owl_assert_axiom_with_anns(+Axiom, +Graph:iri, +Annotations:list) is det.
%
%  Annotations = [annotation(P1,V1), ...]
%  
owl_assert_axiom_with_anns(Axiom,G,Anns) :-
        owl_assert_axiom_with_anns(Axiom,_,G,Anns).
owl_assert_axiom_with_anns(Axiom,T,G,Anns) :-
        owl_assert_axiom(Axiom,T,G),
        T=rdf(S,P,O),
        owl_assert_expression(P,Px,G),
        owl_assert_expression(S,Sx,G),
        owl_assert_expression(O,Ox,G),
        rdf_create_bnode(AxiomNode),
        debug(def, 'Saving AxiomAnnotation ~w = ~w -> ~w // ~w,~w,~w',[AxiomNode,AP,V,S,Px,O]),
        rdf_assert(AxiomNode,owl:annotatedSource,Sx,G),
        rdf_assert(AxiomNode,owl:annotatedProperty,Px,G),
        rdf_assert(AxiomNode,owl:annotatedTarget,Ox,G),
        rdf_assert(AxiomNode,rdf:type,owl:'Axiom',G),
        forall(member(annotation(AP,V),Anns),
               rdf_assert(AxiomNode,AP,V,G)),
        !.
owl_assert_axiom_with_anns(Axiom,T,G,Anns) :-
        throw(error(could_not_owl_assert(Axiom,T,G,Anns))).



% TODO: axiom annotation
owl_assert_expression(and(Xs),Node,G) :-
        !,
        rdf_create_bnode(Node),
        debug(xdef,'Made IXN bnode: ~w',[Node]),
        rdf_assert(Node,rdf:type,owl:'Class',G),
        maplist({G}/[In,Out]>>owl_assert_expression(In,Out,G),Xs,IxnNodesPL),
        debug(xdef, 'AND list = ~w',[IxnNodesPL]),
        rdf_assert_list(IxnNodesPL,IxnNode,G),
        rdf_assert(Node,owl:intersectionOf,IxnNode,G).
owl_assert_expression(some(P,V),Node,G) :-
        !,
        rdf_create_bnode(Node),
        owl_assert_expression(P,Px,G),
        owl_assert_expression(V,Vx,G),
        rdf_assert(Node,rdf:type,owl:'Restriction',G),
        rdf_assert(Node,owl:onProperty,Px,G),
        rdf_assert(Node,owl:someValuesFrom,Vx,G).
owl_assert_expression(Node,Node,_) :-
        Node = _^^_,
        !.
owl_assert_expression(Node,Node,_) :-
        Node = _@_,
        !.
owl_assert_expression(P:X,Node,_) :-
        rdf_global_id(P:X,Node),
        !.
owl_assert_expression(Node,Node,_) :-
        atomic(Node),
        !.
