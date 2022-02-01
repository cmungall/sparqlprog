:- module(ubergraph,
          [
           rdf_ontology/3,
           rdf_closure/3,
           rdf_nr/3,
           rdf_redundant/3,

           category_direct/2,
           category_inferred/2,
           metatype/2,

           anc/3,

           simj/3,
           simj/4,
           simj_e/3,
           simj_e/4,
           ancestor_intersection/3,
           ancestor_intersection/4,
           ancestor_union/3,
           ancestor_union/4,

           underlaps/3,
           underlaps_under/4,
           underlaps_in_subset/4,
           relational_disjoint_in_subset/3,
           relational_disjoint_in_leaf_subset/3,
           is_a_or_part_of/2,
           subclass_leaf/1,

           class_pair/2,
           ancestor_reflexive/3,
           tbl_ancestor_reflexive/3,
           ancestor_proper/3,

           pair_has_child/6,
           has_child/3,
           direct_edge/3,
           disjoint_over/3,
           not_disjoint_over/4,
        
           tbl_disjoint_over/3,
           tbl_not_disjoint_over/4,
           tbl_pair_has_child/6,

           pair_descendants_test_disjointness/6,

           subclass_count/2,

           is_conjugate_acid_of/2,
           is_conjugate_base_of/2,
           conjugate_non_reciprocal1/2,
           conjugate_non_reciprocal2/2,
           conjugate_inconsistency1/6,
           conjugate_inconsistency2/6,
           conjugate_inconsistency/7,
           chebi_uniprot_name/3,
           
           taxon/1,
           never_in_taxon/2,
           inf_never_in_taxon_via_1/3,
           inf_never_in_taxon_via_2/3,
           inf_never_in_taxon_via_1/5,
           inf_never_in_taxon_via_2/5,
           conservative_inf_never_in_taxon_via/2,
           inf_never_in_taxon_via/3,
           inf_never_in_taxon_via/5,
           taxon_propagating_property/1
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(ubergraph,'http://reasoner.renci.org/').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(biolink, 'https://w3id.org/biolink/vocab/').
:- rdf_register_prefix('NCBITaxon', 'http://purl.obolibrary.org/obo/NCBITaxon_').
:- rdf_register_prefix('UBERON', 'http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('CHEBI', 'http://purl.obolibrary.org/obo/CHEBI_').

    
rdf_ontology(S,P,O):- rdf(S,P,O,ubergraph:ontology).
rdf_closure(S,P,O):- rdf(S,P,O,ubergraph:'ontology/closure').
rdf_nr(S,P,O):- rdf(S,P,O,ubergraph:nonredundant).
rdf_redundant(S,P,O):- rdf(S,P,O,ubergraph:redundant).

category_direct(X,C) :- rdf_nr(X,biolink:category,C).
category_inferred(X,C) :- rdf(X,biolink:category,C).

count_chemical_entity(Num) :- aggregate(count(distinct(X)),category_inferred(X,biolink:'ChemicalEntity'),Num).

    
%% metatype(?Cls,?MetaCls) is nondet.
%
%  e.g. metatype(C,biolink:'Disease')
metatype(C,MC) :-
        rdf(MC,skos:exactMatch,M),
        subClassOf(C,M).

%% anc(?Cls,+Rel,?Parent) is nondet
%
%  reflexive ancestor over Rel
%  note that ubergraph does not materialize reflexive
anc(C,R,P) :- rdf_path(C,R|rdfs:subClassOf,P).


ancestor_intersection(C1,C2,Num) :-
        aggregate(count(distinct(P)),(subClassOf(C1,P),subClassOf(C2,P)),Num).
ancestor_union(C1,C2,Num) :-
        aggregate(count(distinct(P)),(subClassOf(C1,P);subClassOf(C2,P)),Num).

ancestor_intersection(C1,C2,R,Num) :-
        aggregate(count(distinct(P)),(anc(C1,R,P),anc(C2,R,P)),Num).
ancestor_union(C1,C2,R,Num) :-
        aggregate(count(distinct(P)),(anc(C1,R,P);anc(C2,R,P)),Num).

%% simj(+C1,C2,?S)
%% simj(+C1,C2,+R,?S)
%
% Jaccard similarity (entirely in SPARQL)
%
simj(C1,C2,S) :-
        ancestor_intersection(C1,C2,AI),
        ancestor_union(C1,C2,AU),
        S is AI/AU.
simj(C1,C2,R,S) :-
        ancestor_intersection(C1,C2,R,AI),
        ancestor_union(C1,C2,R,AU),
        S is AI/AU.

%% simj_e(+C1,C2,?S)
%% simj_e(+C1,C2,+R,?S)
%
% Jaccard similarity (mixed sparql/prolog)
%
% If R is specified then ancestor is calculated over this relation as well as subclass
%
% inner goals are evaluated using 2 sparql calls
simj_e(C1,C2,S) :-
        ??(ubergraph, ancestor_intersection(C1,C2,AI)),
        ??(ubergraph, ancestor_union(C1,C2,AU)),
        bind(AI/AU,S).
simj_e(C1,C2,R,S) :-
        ??(ubergraph, ancestor_intersection(C1,C2,R,AI)),
        ??(ubergraph, ancestor_union(C1,C2,R,AU)),
        bind(AI/AU,S).

underlaps(C1,C2,X) :-
        C1 @< C2,
        is_a_or_part_of(X,C1),
        is_a_or_part_of(X,C2).


% E.g. UBERON:0000062 ! organ
underlaps_under(T,C1,C2,X) :-
        subClassOf(C1,T),
        subClassOf(C2,T),
        \+ subClassOf(C1,C2),
        \+ subClassOf(C2,C1),
        underlaps(C1,C2,X).

underlaps_in_subset(S,C1,C2,X) :-
        rdf(C1,'http://www.geneontology.org/formats/oboInOwl#inSubset',S),
        rdf(C2,'http://www.geneontology.org/formats/oboInOwl#inSubset',S),
        \+ subClassOf(C1,C2),
        \+ subClassOf(C2,C1),
        underlaps(C1,C2,X).

relational_disjoint_in_subset(S,C1,C2) :-
        rdf(C1,'http://www.geneontology.org/formats/oboInOwl#inSubset',S),
        rdf(C2,'http://www.geneontology.org/formats/oboInOwl#inSubset',S),
        C1 @< C2,
        \+ subClassOf(C1,C2),
        \+ subClassOf(C2,C1),
        \+ underlaps(C1,C2,_).

relational_disjoint_in_leaf_subset(S,C1,C2) :-
        relational_disjoint_in_subset(S,C1,C2),
        subclass_leaf(C1),
        subclass_leaf(C2).

subclass_leaf(C) :-
        \+ ((subClassOf(X,C),
             X\=C)).

:- table pair_descendants_test_disjointness/6.
pair_descendants_test_disjointness(P1,P2,R,C1,C2,true) :-
        tbl_pair_has_child(P1,P2,R,C1,C2,_),
        writeln(x(R,C1,C2)),
        \+ tbl_not_disjoint_over(R,C1,C2,_),
        format('~q.~n',[disjoint(R,C1,C2)]).
pair_descendants_test_disjointness(P1,P2,R,C1,C2,IsDisjoint) :-
        tbl_pair_has_child(P1,P2,R,Z1,Z2,_),
        writeln(y(R,Z1,Z2)),
        \+ \+ tbl_not_disjoint_over(R,Z1,Z2,_),
        writeln(z(Z1,Z2)),
        pair_descendants_test_disjointness(Z1,Z2,R,C1,C2,IsDisjoint).
pair_descendants_test_disjointness(P1,P2,R,C1,C2,IsDisjoint) :-
        tbl_ancestor_reflexive(P1,R,P2),
        tbl_pair_has_child(P1,P2,R,Z1,Z2,2),
        writeln(w2(R,Z1,Z2)),
        writeln(w2z(R,Z1,Z2)),
        pair_descendants_test_disjointness(Z1,Z2,R,C1,C2,IsDisjoint).
pair_descendants_test_disjointness(P1,P2,R,C1,C2,IsDisjoint) :-
        tbl_ancestor_reflexive(P2,R,P1),
        tbl_pair_has_child(P1,P2,R,Z1,Z2,1),
        writeln(w1(R,Z1,Z2)),
        writeln(w1z(R,Z1,Z2)),
        pair_descendants_test_disjointness(Z1,Z2,R,C1,C2,IsDisjoint).


:- table tbl_pair_has_child/6.
tbl_pair_has_child(P1,P2,R,C1,C2,Side) :-
        ubergraph ??
        pair_has_child(P1,P2,R,C1,C2,Side).


:- table tbl_disjoint_over/3.
tbl_disjoint_over(R,C1,C2) :-
        ubergraph ??
        disjoint_over(R,C1,C2).

:- table tbl_not_disjoint_over/4.
tbl_not_disjoint_over(R,C1,C2,X) :-
        ubergraph ??
        not_disjoint_over(R,C1,C2,X).

:- table tbl_ancestor_reflexive/3.
tbl_ancestor_reflexive(X,R,Y) :-
        ubergraph ??
        ancestor_reflexive(X,R,Y).

% a pair has a child that extends one side of the pair,
% but does not extend a side that is already subsumed
pair_has_child(P1,X,R,C1,X,1) :-        has_child(P1,R,C1), \+ ancestor_proper(P1,R,X).
pair_has_child(X,P2,R,X,C2,2) :-        has_child(P2,R,C2), \+ ancestor_proper(P2,R,X).

has_child(P,R,C) :-     direct_edge(C,R,P).
has_child(P,_,C) :-     direct_edge(C,rdfs:subClassOf,P).

class_pair(C1,C2) :-
        owl:class(C1),
        owl:class(C2).


disjoint_over(R,C1,C2) :-
        \+ not_disjoint_over(R,C1,C2,_).
not_disjoint_over(R,C1,C2,X) :-
        ancestor_reflexive(X,R,C1),
        ancestor_reflexive(X,R,C2).

ancestor_proper(X,R,Y) :-
        ancestor_reflexive(X,R,Y),
        X \= Y.
ancestor_reflexive(X,_,Y) :-
        rdf(X,rdfs:subClassOf,Y).
ancestor_reflexive(X,R,Y) :-
        rdf(X,R,Y).




direct_edge(X,R,Y) :-
        rdf(X,R,Y,ubergraph:nonredundant).
normalized_information_content(C,N) :-
        rdf(C,ubergraph:'vocab/normalizedInformationContent',N).
subclass_count(C,N) :-
        rdf(C,ubergraph:'vocab/subClassCount',N).


%% --
%% chebi
%% --

%is_conjugate_base_of(C1,C2) :- rdf_nr(C1,'http://purl.obolibrary.org/obo/chebi#is_conjugate_base_of',C2).
%is_conjugate_acid_of(C1,C2) :- rdf_nr(C1,'http://purl.obolibrary.org/obo/chebi#is_conjugate_acid_of',C2).
is_conjugate_base_of(C1,C2) :- rdf_path(C1,oneOrMore('http://purl.obolibrary.org/obo/chebi#is_conjugate_base_of'),C2, ubergraph:nonredundant).
is_conjugate_acid_of(C1,C2) :- rdf_path(C1,oneOrMore('http://purl.obolibrary.org/obo/chebi#is_conjugate_acid_of'),C2, ubergraph:nonredundant).


% WRONG: this walks up is-a
%is_conjugate_base_of(C1,C2) :- rdf(C1,'http://purl.obolibrary.org/obo/chebi#is_conjugate_base_of',C2).
%is_conjugate_acid_of(C1,C2) :- rdf(C1,'http://purl.obolibrary.org/obo/chebi#is_conjugate_acid_of',C2).

conjugate_non_reciprocal1(C1,C2) :-
        is_conjugate_base_of(C1,C2),
        \+ is_conjugate_acid_of(C2,C1).
conjugate_non_reciprocal2(C1,C2) :-
        is_conjugate_acid_of(C1,C2),
        \+ is_conjugate_base_of(C2,C1).

/*
conjugate_inconsistency1(C1,C2,P1,P2) :-
        is_conjugate_base_of(C1,C2),
        rdf(C1,rdfs:subClassOf,P1),
        is_conjugate_base_of(P1,P2),
        \+ rdf(C2,rdfs:subClassOf,P2).
*/

/*

pq-ubergraph --header bioname,bioname_parent,base,acid,base_parent,acid_parent,inconsistency,null,null,base_n,acid_n,base_parent_n,acid_parent_n,type_n -f tsv conjugate_inconsistency -l  

  right_incomplete:
  
                                 7.3 form
  
  Bp <--[is_conjugate_base_of]-- Ap
  ^                              ?
  |                              ?
  |                              ?
  B  <--[is_conjugate_base_of]-- A

  left_incomplete:
  
  7.3 form
  
  Bp <--[is_conjugate_base_of]-- Ap
  ?                              ^
  ?                              |
  ?                              |
  B  <--[is_conjugate_base_of]-- A

*/

conjugate_inconsistency1(C2N,P2N,C1,C2,P1,P2) :-
        is_conjugate_base_of(C1,C2),
        chebi_uniprot_name(C2,_,C2N),
        rdf(C1,rdfs:subClassOf,P1),
        is_conjugate_base_of(P1,P2),
        chebi_uniprot_name(P2,_,P2N),
        \+ rdf(C2,rdfs:subClassOf,P2).
conjugate_inconsistency2(C1N,P1N,C1,C2,P1,P2) :-
        is_conjugate_base_of(C1,C2),
        chebi_uniprot_name(C1,_,C1N),
        rdf(C2,rdfs:subClassOf,P2),
        is_conjugate_base_of(P1,P2),
        chebi_uniprot_name(P1,_,P1N),
        \+ rdf(C1,rdfs:subClassOf,P1).

conjugate_inconsistency(N,PN,C1,C2,P1,P2,right_incomplete) :-
        conjugate_inconsistency1(N,PN,C1,C2,P1,P2).
conjugate_inconsistency(N,PN,C1,C2,P1,P2,left_incomplete) :-
        conjugate_inconsistency2(N,PN,C1,C2,P1,P2).



chebi_uniprot_name(C,P,N) :-
        triple_axiom_annotation(C,P,N,'http://www.geneontology.org/formats/oboInOwl#hasDbXref',AV),
        str(AV) = "UniProt".
        

%% --
%% taxon constraints
%% --

% NCBITaxon:4930 ! Saccharomyces
% UBERON:0000955 ! brain

taxon(T) :-     subClassOf(T,'NCBITaxon':'1').

never_in_taxon(C,T) :- rdf(C,'http://purl.obolibrary.org/obo/RO_0002161',T).

inf_never_in_taxon_via_1(C,P,T) :-
        inf_never_in_taxon_via_1(C,P,T,_,_).
inf_never_in_taxon_via_1(C,P,T,C1,T1) :-
        rdf(C,P,C1),
        in_taxon(C1,T1),
        \+ subClassOf(T,T1).
inf_never_in_taxon_via_2(C,P,T) :-
        inf_never_in_taxon_via_2(C,P,T,_,_).
inf_never_in_taxon_via_2(C,P,T,C1,T1) :-
        rdf(C,P,C1),
        never_in_taxon(C1,T1),
        subClassOf(T,T1).

inf_never_in_taxon_via(C,P,T) :- inf_never_in_taxon_via_1(C,P,T).
inf_never_in_taxon_via(C,P,T) :- inf_never_in_taxon_via_2(C,P,T).

inf_never_in_taxon_via(C,P,T,C1,T1) :- inf_never_in_taxon_via_1(C,P,T,C1,T1).
inf_never_in_taxon_via(C,P,T,C1,T1) :- inf_never_in_taxon_via_2(C,P,T,C1,T1).

conservative_inf_never_in_taxon_via(C,T) :-
        inf_never_in_taxon_via(C,P,T),
        taxon_propagating_property(P).

taxon_propagating_property(rdfs:subClassOf).
taxon_propagating_property(obo:'BFO_0000066').
taxon_propagating_property(obo:'BFO_0000051').
taxon_propagating_property(obo:'BFO_0000050').

is_a_or_part_of(X,Y) :- rdf(X,rdfs:subClassOf,Y).
is_a_or_part_of(X,Y) :- rdf(X,obo:'BFO_0000050',Y).


