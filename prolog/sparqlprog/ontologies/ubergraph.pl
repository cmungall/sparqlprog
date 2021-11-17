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

           taxon/1,
           never_in_taxon/2,
           not_in_taxon_1/3,
           not_in_taxon_2/3,
           conservative_not_in_taxon/2,
           not_in_taxon/3,
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

%% --
%% taxon constraints
%% --

% NCBITaxon:4930 ! Saccharomyces
% UBERON:0000955 ! brain

taxon(T) :-     subClassOf(T,'NCBITaxon':'1').

never_in_taxon(C,T) :- rdf(C,'http://purl.obolibrary.org/obo/RO_0002161',T).

not_in_taxon_1(C,P,T) :-
        rdf(C,P,C1),
        in_taxon(C1,T1),
        \+ subClassOf(T,T1).
not_in_taxon_2(C,P,T) :-
        rdf(C,P,C1),
        never_in_taxon(C1,T1),
        subClassOf(T,T1).

not_in_taxon(C,P,T) :- not_in_taxon_1(C,P,T).
not_in_taxon(C,P,T) :- not_in_taxon_2(C,P,T).

conservative_not_in_taxon(C,T) :-
        not_in_taxon(C,P,T),
        taxon_propagating_property(P).

taxon_propagating_property(rdfs:subClassOf).
taxon_propagating_property(obo:'BFO_0000066').
taxon_propagating_property(obo:'BFO_0000051').
taxon_propagating_property(obo:'BFO_0000050').





        

