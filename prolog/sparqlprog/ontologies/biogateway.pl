:- module(biogateway,
          [

           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).

:- sparql_endpoint( biogateway, 'https://biogw-db.nt.ntnu.no:4333/sparql' ).

:- rdf_register_prefix(involved_in_regulation_of, 'http://purl.obolibrary.org/obo/RO_0002428').
:- rdf_register_prefix(involved_in_of, 'http://purl.obolibrary.org/obo/RO_0002311').
:- rdf_register_prefix(molecularly_interacts_with, 'http://purl.obolibrary.org/obo/RO_0002436').
:- rdf_register_prefix(encodes, 'http://semanticscience.org/resource/SIO_010078').


:- rdf_register_prefix(id, 'http://identifiers.org/').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(blmod, 'http://w3id.org/biolink/vocab/').
:- rdf_register_prefix(d2s, 'https://w3id.org/data2services/data/').


