:- module(ontobee,
          [
           in_graph/2,
           typed_in_graph/3,
           in_ontology/2,
           in_ontology/3,

           graph_ontology/2,

           searchall/4,
           ontsearch/4,

           compare_via_xref/1,
           compare_via_xref/6,
           ontobee_edge/3
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(obomerged,'http://purl.obolibrary.org/obo/merged/').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').

:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').



in_graph(X,G) :-
        typed_in_graph(X,G,_).
typed_in_graph(X,G,T) :-
        rdf(X,rdf:type,T,G).


:- srule(in_ontology,[entity:iri, ontology:iri],
         'Entity is declared to be of some type in Ontology').
in_ontology(X,O) :-
        in_ontology(X,O,_).

:- srule(in_ontology,[entity:iri, ontology:iri, type:iri],
         'Entity is declared to be of type T in Ontology').
in_ontology(X,O,T) :-
        typed_in_graph(X,G,T),
        graph_ontology(G,O).

:- srule(graph_ontology,[graph:iri, ontology:iri], 'maps an ontobee graph IRI to an ontology name (e.g. hp, go, uberon, ncit)').
graph_ontology(G,O) :-
        G == uri(concat('http://purl.obolibrary.org/obo/merged/',ucase(O))).    


ontsearch(O,P,C,L) :- graph_ontology(G,O),rdf(C,rdfs:label,L,G),regex(str(L),P).
searchall(G,P,C,L) :- rdf(C,rdfs:label,L,G),regex(str(L),P).



curie_uri_graph(X,URI,G) :-
        concat_atom([DB,Local],':',X),
        DB\=http,
        atom_concat('http://purl.obolibrary.org/obo/merged/',DB,G),
        concat_atom(['http://purl.obolibrary.org/obo/',DB,'_',Local],URI).

% UBERON:0002228 ! rib
compare_via_xref(C) :-
        compare_via_xref(C,_R,_P,_CX,_XG,_Status).

compare_via_xref(C_id,R,P,CX,XG,Status) :-
        ensure_uri(C_id,C),
        format('# Testing: ~w~n',[C]),
        ontobee_edge(C,R,P),
        ontobee_label(P,PN),
        format('# Edge: ~w ~w ~w ~w~n',[C,R,P,PN]),
        ontobee_has_dbxref(C,CX),
        format('# C xref: ~w~n',[CX]),
        curie_uri_graph(CX,CXU,XG),
        format('# Checking if present in : ~w~n',[XG]),
        find_and_compare_edge(C,R,P,CXU,XG, Status),
        format('# Status: Checking for relationship ~w ~w "~w" in ~w, Status:  ~w~n',[R,P,PN,XG,Status]).



find_and_compare_edge(_C,R,P,CXU,XG, Status) :-
        ontobee_has_dbxref(P,PX),
        curie_uri_graph(PX,PXU,XG),
        !,
        test_for_edge(R,CXU, PXU, Status).
find_and_compare_edge(_,_,P,_,XG, no_equiv_to(P,XG)).

test_for_edge(RU,CXU, PXU, analogous_edge(Same)) :-
        ontobee_edge(CXU,RXU,PXU),
        !,
        is_same_rel(RU,RXU,Same).
xxxxxtest_for_edge(RU,CXU, PXU, indirect_subclass(Same)) :-
        service(ontobee,rdfs_subclass_of(CXU,PXU)),
        !,
        is_same_rel(RU,rdfs:subClassOf,Same).
test_for_edge(_,_, _, no_relationship_between_equivs) :- !.
        
is_same_rel(RU,RXU,Same) :-
        (   RU=RXU
        ->  Same=same_rel
        ;   Same=diff_rel).
        
        
ontobee_edge(C,R,P) :-
        service(ontobee,subclass_of_some(C,R,P)).
ontobee_edge(C,subClassOf,P) :-
%        service(ontobee,((subClassOf(C,P),\+rdf_bnode(P)))).
%        service(ontobee,(subClassOf(C,P),\+is_blank(P))).
        service(ontobee,(subClassOf(C,P))).

ontobee_has_dbxref(C,CX):-
        service(ontobee,has_dbxref(C,CX1)),
        literal_atom(CX1,CX).
ontobee_label(C,N):-
        service(ontobee,rdf(C,rdfs:label,N1)),
        !,
        literal_atom(N1,N).
ontobee_label(_,'').

        
        
        
        
        
