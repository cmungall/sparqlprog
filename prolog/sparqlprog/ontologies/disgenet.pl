/*
  
*/

:- module(disgenet,
          [
           refers_to/2,
           gene/1,
           disease/1,

           assoc/1,
           gda/1,
           gda/3
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( disgenet, 'http://rdf.disgenet.org/sparql/').

:- rdf_register_prefix(disgenet,'http://rdf.disgenet.org/').
:- rdf_register_prefix(disgenet_gda,'http://rdf.disgenet.org/resource/gda/').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(ncitevs, 'http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#').
:- rdf_register_prefix(umls, 'http://linkedlifedata.com/resource/umls/id/').
:- rdf_register_prefix(ncbigene, 'http://identifiers.org/ncbigene/').


refers_to(A,B) :- rdf(A,sio:'000628',B).

disease(D) :- rdf(D,rdf:type,ncitevs:'C7057').
gene(G) :- rdf(G,rdf:type,ncitevs:'C16612').
assoc(A) :- rdfs_individual_of(A,sio:'000897').
gda(A) :- rdfs_individual_of(A,sio:'001121').
gda(A,G,D) :- gda(A),refers_to(A,G),gene(G),refers_to(A,D),disease(D).






