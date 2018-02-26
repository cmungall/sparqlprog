:- module(ebi,
          [
           protein_coding_gene/1,
           identifier/2,
           homologous_to/2,
           in_taxon/2,
           transcribed_from/2
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).

:- sparql_endpoint( ebi, 'https://www.ebi.ac.uk/rdf/services/sparql').

:- rdf_register_prefix(ensembl,'http://rdf.ebi.ac.uk/resource/ensembl/').
:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').
:- rdf_register_prefix(so, 'http://purl.obolibrary.org/obo/SO_').
:- rdf_register_prefix(ro, 'http://purl.obolibrary.org/obo/RO_').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(taxon, 'http://identifiers.org/taxonomy/').
:- rdf_register_prefix(human, 'http://identifiers.org/taxonomy/9606').

:- rdf_register_prefix(grch38, 'http://rdf.ebi.ac.uk/resource/ensembl/90/homo_sapiens/GRCh38/').
:- rdf_register_prefix(grcm38, 'http://rdf.ebi.ac.uk/resource/ensembl/90/mus_musculus/GRCm38/').


homologous_to(A,B) :- rdf(A,sio:'000558',B).
in_taxon(A,B) :- rdf(A,ro:'0002162',B).

identifier(A,X) :- rdf(A,dcterms:identifier,X).
transcribed_from(T,G) :- rdf(T,so:transcribed_from,G).

protein_coding_gene(G) :- rdf(G,rdf:type,so:'0001217').

