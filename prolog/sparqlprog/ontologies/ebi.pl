:- module(ebi,
          [

           sample/1,
           sample_attribute/2,
           attribute_property_value/3,
           attribute_property_value/4,
           attribute_property_value/5,
           sample_biocharacteristic/2,
           sample_property_value/3,
           sample_property_value/4,
           sample_depth/2,
           sample_environment_feature/2,
           
           identifier/2,
           description/2,
           direct_mapping/2,
           see_also/2,
           alt_label/2,
           exon/1,
           protein_coding_gene/1,
           paralogous_to/2,
           orthologous_to/2,
           homologous_to/2,
           in_taxon/2,
           transcribed_from/2,
           translates_to/2,
           has_part/2,

           feature_in_range/4,
           mouse_ortho_by_range/5,
           has_mouse_ortholog/2
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).
:- use_module(library(sparqlprog/ontologies/sequence_feature), []).

:- sparql_endpoint( ebi, 'https://www.ebi.ac.uk/rdf/services/sparql').
%:- sparql_endpoint( ebi, ['https://www.ebi.ac.uk/rdf/services/sparql', 'https://integbio.jp/rdf/mirror/ebi/sparql']).

% samples
:- rdf_register_prefix(biosd_terms, 'http://rdf.ebi.ac.uk/terms/biosd/').
:- rdf_register_prefix(biosd, 'http://rdf.ebi.ac.uk/resource/biosamples/sample/').


:- rdf_register_prefix(ebi_atlas, 'http://rdf.ebi.ac.uk/terms/atlas/').

:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(ensembl,'http://rdf.ebi.ac.uk/resource/ensembl/').
:- rdf_register_prefix(ensembl_protein,'http://rdf.ebi.ac.uk/resource/ensembl.protein/').
:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').
:- rdf_register_prefix(so, 'http://purl.obolibrary.org/obo/SO_').
:- rdf_register_prefix(ro, 'http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(taxon, 'http://identifiers.org/taxonomy/').
:- rdf_register_prefix(human, 'http://identifiers.org/taxonomy/9606').

% Note: this needs updating every ensemnl release...    
:- rdf_register_prefix(grch38, 'http://rdf.ebi.ac.uk/resource/ensembl/92/homo_sapiens/GRCh38/').
:- rdf_register_prefix(grcm38, 'http://rdf.ebi.ac.uk/resource/ensembl/92/mus_musculus/GRCm38/').


sample(A) :- rdf(A,rdf:type,biosd_terms:'Sample').

%! sample_biocharacteristic(?S : ebi_biosample, ?A : ebi_attribute_node) is nondet.
sample_biocharacteristic(S,A) :-
        rdf(S,biosd_terms:'has-bio-characteristic',A).



%! sample_attribute(?S : ebi_biosample, ?A : ebi_attribute_node) is nondet.
sample_attribute(S,A) :-
        rdf(S,biosd_terms:'has-sample-attribute',A).

%! attribute_property_value(?A : ebi_attribute_node, ?P : ebi_biosample_property, ?V : ebi_biosample_value) is nondet.
%
attribute_property_value(A, P, V) :-
        rdf(A,ebi_atlas:propertyType,P),
        rdf(A,ebi_atlas:propertyValue,V).
attribute_property_value(A, P, V, T) :-
        rdf(A,rdf:type,T),
        rdf(A,ebi_atlas:propertyType,P),
        rdf(A,ebi_atlas:propertyValue,V).
attribute_property_value(A, P, V, T, G) :-
        rdf(A,rdf:type,T, G),
        rdf(A,ebi_atlas:propertyType,P),
        rdf(A,ebi_atlas:propertyValue,V).

%! sample_property_value(?S : ebi_biosample, ?P : ebi_biosample_property, ?V : ebi_biosample_value) is nondet.
%
sample_property_value(S, P, V) :-
        sample_attribute(S, A),
        attribute_property_value(A,P,V).
sample_property_value(S, P, V, T) :-
        sample_attribute(S, A),
        attribute_property_value(A,P,V, T).

sample_depth(S,V) :-
        sample_property_value(S, "depth"^^xsd:string, V).
sample_environment_feature(S,V) :-
        sample_property_value(S,  "environment feature"^^xsd:string, V).

% B seems to always be identifiers.org
see_also(A,B) :- rdf(A,rdfs:seeAlso,B).

alt_label(A,B) :- rdf(A,skos:altLabel,B).

in_taxon(A,B) :- rdf(A,ro:'0002162',B).

identifier(A,X) :- rdf(A,dcterms:identifier,X).
description(A,X) :- rdf(A,dcterms:description,X).
translates_to(T,P) :- rdf(T,so:translates_to,P).  % e.g. T to G
transcribed_from(T,G) :- rdf(T,so:transcribed_from,G).  % e.g. T to G
has_part(T,E) :- rdf(T,so:has_part,E).                  % e.g. T to E

direct_mapping(A,B) :- rdf(A,'http://rdf.ebi.ac.uk/terms/ensembl/DIRECT',B).

protein_coding_gene(G) :- rdf(G,rdf:type,so:'0001217').
exon(X) :- rdf(X,rdf:type,so:'0000147').
transcript(X) :- rdf(X,rdf:type,so:'0000234').

homologous_to(A,B) :- orthologous_to(A,B).
homologous_to(A,B) :- paralogous_to(A,B).

orthologous_to(A,B) :- rdf(A,sio:'000558',B).
paralogous_to(A,B) :- rdf(A,'http://semanticscience.org/resource/SIO:000630',B).  % EBI uses incorrect PURL

foo1('0').

feature_in_range(Chr,B,E,F) :-
        location(F,FB,FE,Chr),
        FB >= B,
        FE =< E.

% mostly for demo:
mouse_ortho_by_range(Chr,B,E,G,H) :-
        protein_coding_gene(G), % SO
        feature_in_range(Chr,B,E,G),
        orthologous_to(G,H),
        in_taxon(H,taxon:'10090').

foo2('0').

has_mouse_ortholog(HumanGene, MouseGene) :- 
        orthologous_to(MouseGene, HumanGene),
        in_taxon(MouseGene,taxon:'10090').


