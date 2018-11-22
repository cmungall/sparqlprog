:- module(rhea,
          [
           rhea_status/2,
           is_approved/1,
           is_transport/1,
           reaction/1,
           rhea_xref/2,

           reaction_side/2,
           side_contains/2,

           reaction_participant/2,
           participant_compound/2,
           compound_chebi/2,
           participant_chebi/2,
           reaction_chebi_participant/2
           
           ]).

:- use_module(library(semweb/rdf11)).

%:- use_module(library(sparqlprog/ontologies/faldo)).

:- rdf_register_prefix(rh, 'http://rdf.rhea-db.org/').
:- rdf_register_prefix(chebi, 'http://purl.obolibrary.org/obo/CHEBI_').

:- rdf_meta status(r,r).
rhea_status(A,B) :- rdf(A, rh:status, B).

:- rdf_meta is_approved(r).
is_approved(A) :- rhea_status(A,rh:'Approved').

is_transport(R) :- rdf(R,rh:isTransport,"true"^^xsd:boolean).

:- rdf_meta reaction(r).
reaction(R) :- rdf(R,rdfs:subClassOf,rh:'Reaction').
directional_reaction(R) :- rdf(R,rdfs:subClassOf,rh:'DirectionalReaction').
bidirectional_reaction(R) :- rdf(R,rdfs:subClassOf,rh:'BidirectionalReaction').

directional_form(R,DR) :- rdf(R,rh:directionalReaction,DR).
bidirectional_form(R,DR) :- rdf(R,rh:bidirectionalReaction,DR).

rhea_xref(R,X) :- rdf(R,rdfs:seeAlso,X).

rhea_product(R, S) :- rdf(R,rh:products,S).
rhea_substrate(R, S) :- rdf(R,rh:substrate,S).


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

