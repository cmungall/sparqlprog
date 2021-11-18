:- module(ubergraph,
          [
           rdf_ontology/3,
           rdf_closure/3,
           rdf_nr/3,
           rdf_redundant/3,

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
           is_a_or_part_of/2,

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

    
rdf_ontology(S,P,O):- rdf(S,P,O,ubergraph:ontology).
rdf_closure(S,P,O):- rdf(S,P,O,ubergraph:'ontology/closure').
rdf_nr(S,P,O):- rdf(S,P,O,ubergraph:nonredundant).
rdf_redundant(S,P,O):- rdf(S,P,O,ubergraph:redundant).

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
