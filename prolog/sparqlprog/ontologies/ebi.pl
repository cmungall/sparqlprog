:- module(ebi,
          [

           sample/1,
           sample_attribute_node/2,
           
           identifier/2,
           description/2,
           see_also/2,
           alt_label/2,
           protein_coding_gene/1,
           paralogous_to/2,
           orthologous_to/2,
           homologous_to/2,
           in_taxon/2,
           transcribed_from/2
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).

:- sparql_endpoint( ebi, 'https://www.ebi.ac.uk/rdf/services/sparql').

% samples
:- rdf_register_prefix(biosd_terms, 'http://rdf.ebi.ac.uk/terms/biosd/').
:- rdf_register_prefix(biosd, 'http://rdf.ebi.ac.uk/resource/biosamples/sample/').


:- rdf_register_prefix(ebi_atlas, 'http://rdf.ebi.ac.uk/terms/atlas/').

:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(ensembl,'http://rdf.ebi.ac.uk/resource/ensembl/').
:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').
:- rdf_register_prefix(so, 'http://purl.obolibrary.org/obo/SO_').
:- rdf_register_prefix(ro, 'http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(taxon, 'http://identifiers.org/taxonomy/').
:- rdf_register_prefix(human, 'http://identifiers.org/taxonomy/9606').

:- rdf_register_prefix(grch38, 'http://rdf.ebi.ac.uk/resource/ensembl/90/homo_sapiens/GRCh38/').
:- rdf_register_prefix(grcm38, 'http://rdf.ebi.ac.uk/resource/ensembl/90/mus_musculus/GRCm38/').


sample(A) :- rdf(A,rdf:type,biosd_terms:'Sample').
sample_attribute_node(S,A) :-
        rdf(S,biosd_terms:'has-sample-attribute',A).

%sample_


% B seems to always be identifiers.org
see_also(A,B) :- rdf(A,rdfs:seeAlso,B).

alt_label(A,B) :- rdf(A,skos:altLabel,B).

in_taxon(A,B) :- rdf(A,ro:'0002162',B).

identifier(A,X) :- rdf(A,dcterms:identifier,X).
description(A,X) :- rdf(A,dcterms:description,X).
transcribed_from(T,G) :- rdf(T,so:transcribed_from,G).

protein_coding_gene(G) :- rdf(G,rdf:type,so:'0001217').

homologous_to(A,B) :- orthologous_to(A,B).
homologous_to(A,B) :- paralogous_to(A,B).

orthologous_to(A,B) :- rdf(A,sio:'000558',B).
paralogous_to(A,B) :- rdf(A,'http://semanticscience.org/resource/SIO:000630',B).  % EBI uses incorrect PURL

