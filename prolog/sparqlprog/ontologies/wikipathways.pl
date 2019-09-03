/** <module> wrapper for wikipathways endpoint

See https://www.wikipathways.org/index.php/Portal:Semantic_Web

  https://www.wikipathways.org/index.php/Help:WikiPathways_Sparql_queries

  https://www.wikipathways.org/index.php/Help:WikiPathways_Metabolomics
  
*/

:- module(wikipathways,
          [
           interaction/1,
           pathway/1,
           metabolite/1,
           identifier/2,
           part_of/2,
           has_participant/2,
           organism/2,
           ontology_tag/2,

           pairwise_interaction/2,
           pairwise_interaction/3,

           pathway_version/2
           ]).

:- use_module(library(sparqlprog/owl_types)).
:- use_module(library(typedef)).

:- sparql_endpoint( wikipathways, 'http://sparql.wikipathways.org').

:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').
:- rdf_register_prefix(wp,'http://vocabularies.wikipathways.org/wp#').
:- rdf_register_prefix(pav,'http://purl.org/pav/').
:- rdf_register_prefix(ncbitaxon,'http://purl.obolibrary.org/obo/NCBITaxon_').
:- rdf_register_prefix(pw,'http://purl.obolibrary.org/obo/PW_').

:- type wp_pathway ---> atomic_iri.
:- type wp_interaction ---> atomic_iri.


interaction(X) :- rdf(X,rdf:type,wp:'Interaction').
pathway(X) :- rdf(X,rdf:type,wp:'Pathway').
metabolite(X) :- rdf(X,rdf:type,wp:'Metabolite').
identifier(P,Id) :- rdf(P,dcterms:identifier,Id).
part_of(P,W) :- rdf(P,dcterms:isPartOf,W).
has_participant(P,X) :- rdf(P,wp:participants,X).
organism(P,O) :- rdf(P,wp:organism,O).
ontology_tag(P,O) :- rdf(P,wp:ontologyTag,O).

%! pairwise_interaction(?P1,?P2,?I) is nondet.
%! pairwise_interaction(?P1,?P2) is nondet.
pairwise_interaction(P1,P2) :-
        pairwise_interaction(P1,P2,_).
pairwise_interaction(P1,P2,I) :-
        interaction(I),
        has_participant(I,P1),
        has_participant(I,P2).

%! pathway_version(+P,?V) is nondet.
%
% note: this is in https://www.wikipathways.org/index.php/Help:WikiPathways_Metabolomics
% but does not seem to be used.
pathway_version(P,V) :-
        rdf(P,pav:version,V).
