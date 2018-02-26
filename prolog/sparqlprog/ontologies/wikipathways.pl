:- module(wikipathways,
          [
           interaction/1,
           identifier/2,
           pathway_part_of/2,
           has_participant/2,

           pairwise_interaction/2
           ]).

:- sparql_endpoint( wikipathways, 'http://sparql.wikipathways.org').

:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').
:- rdf_register_prefix(wp,'http://vocabularies.wikipathways.org/wp#').

interaction(X) :- rdf(X,rdf:type,wp:'Interaction').
identifier(P,Id) :- rdf(P,dcterms:identifier,Id).
pathway_part_of(P,W) :- rdf(P,dcterms:isPartOf,W).
has_participant(P,X) :- rdf(P,wp:participants,X).

pairwise_interaction(A,B) :-
        interaction(I),
        has_participant(I,A),
        has_participant(I,B).
