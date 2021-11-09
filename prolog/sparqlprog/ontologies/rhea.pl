:- module(rhea,
          [
           rhea_status/2,
           is_approved/1,
           is_transport/1,
           reaction/1,
           nondirectional_reaction/1,
           directional_reaction/1,
           bidirectional_reaction/1,
           directional_form/2,
           bidirectional_form/2,
           reaction_all_forms/4,
           directional_forms/3,
           reaction_form/3,
           rhea_xref/2,
           rhea_ec/2,
           rhea_xref_conflated/2,
           rhea_xref_skos/3,

           reaction_side/2,
           side_contains/2,

           reaction_participant/2,
           participant_compound/2,
           compound_chebi/2,
           participant_chebi/2,
           reaction_chebi_participant/2,

           rhea_equation/2
           
           ]).

:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(rh, 'http://rdf.rhea-db.org/').
:- rdf_register_prefix(chebi, 'http://purl.obolibrary.org/obo/CHEBI_').

:- rdf_meta status(r,r).
rhea_status(A,B) :- rdf(A, rh:status, B).

:- rdf_meta is_approved(r).
is_approved(A) :- rhea_status(A,rh:'Approved').

is_transport(R) :- rdf(R,rh:isTransport,"true"^^xsd:boolean).

:- rdf_meta reaction(r).
:- rdf_meta nondirectional_reaction(r).
:- rdf_meta directional_reaction(r).
:- rdf_meta bidirectional_reaction(r).
reaction(R) :- nondirectional_reaction(R) ; directional_reaction(R) ; bidirectional_reaction(R).
nondirectional_reaction(R) :- rdf(R,rdfs:subClassOf,rh:'Reaction').
directional_reaction(R) :- rdf(R,rdfs:subClassOf,rh:'DirectionalReaction').
bidirectional_reaction(R) :- rdf(R,rdfs:subClassOf,rh:'BidirectionalReaction').

directional_form(R,DR) :- rdf(R,rh:directionalReaction,DR).
bidirectional_form(R,DR) :- rdf(R,rh:bidirectionalReaction,DR).

reaction_all_forms(R,D1,D2,BD) :- directional_form(R,D1),directional_form(R,D2),D1@<D2,bidirectional_form(R,BD).

directional_forms(R,D1,D2) :- rdf(R,rh:directionalReaction,D1),rdf(R,rh:directionalReaction,D2),D1@<D2.

%! reaction_form(Parent,Relation,Reaction) is det.
reaction_form(R,nondirectional,R) :- reaction(R).
reaction_form(P,directional,R) :- directional_form(P,R).
reaction_form(P,bidirectional,R) :- bidirectional_form(P,R).



rhea_ec(R,X) :- rdf(R,rh:ec,X).
rhea_xref(R,X) :- rdf(R,rdfs:seeAlso,X).

rhea_xref_conflated(R,X) :-
        (   nondirectional_reaction(R),rhea_xref(R,X))
        ;    
        (   directional_form(R,R2),rhea_xref(R2,X))
        ;    
        (   bidirectional_form(R,R2),rhea_xref(R2,X)).

rhea_xref_skos(R,Pred,X) :-
        rhea_xref_conflated(R,X),
        member([Prefix,Pred],
               [
                ['http://identifiers.org/reactome/',skos:broadMatch],
                ['http://identifiers.org/biocyc/METACYC:',skos:exactmatch],
                ['http://identifiers.org/biocyc/ECOCYC:',skos:exactmatch],
                ['http://identifiers.org/kegg.reaction/',skos:exactmatch]
               ]),
        str_starts(str(X),str(Prefix)).

%% rhea_products(R, S)
%
%   only asserted for directional_reaction
rhea_products(R, S) :- rdf(R,rh:products,S).

%% rhea_substrates(R, S)
%
%   only asserted for directional_reaction
rhea_substrates(R, S) :- rdf(R,rh:substrates,S).

%% rhea_substratesOrProducts(R, S)
%
%   only asserted for bidirectional_reaction
rhea_substratesOrProducts(R, S) :- rdf(R,rh:substratesOrProducts,S).


reaction_side(R,S) :- rdf(R,rh:side,S).
side_contains(S,P) :- rdf(S,rh:contains,P).
reaction_participant(R,P) :- reaction_side(R,S),side_contains(S,P).

participant_compound(P,C) :- rdf(P,rh:compound,C).
compound_chebi(C,X) :- rdf(C,rh:chebi,X).
participant_chebi(P,X) :- participant_compound(P,C),compound_chebi(C,X).

reaction_chebi_participant(R,X) :- reaction_participant(R,P),participant_chebi(P,X).

rhea_name(R,N) :- rdf(R,rh:name,N).
rhea_accession(R,A) :- rdf(R,rh:accession,A). % CHEBI CURIE, or POLYMER:nn

rhea_equation(R,E) :- rdf(R,rh:equation,E).

