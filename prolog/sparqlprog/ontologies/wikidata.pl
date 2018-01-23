/*

expose a subclass of dbpedia for demo purposes

  For complete ontology use rdfs2pl

  
  
*/

:- module(dbpedia,
          [
           continent/1,
           country/1,
           city/1,

           cancer/1,
           
           exact_match/2
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( wd, 'http://query.wikidata.org/sparql').

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
%:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms').
:- rdf_register_prefix(wikipathways,'http://vocabularies.wikipathways.org/wp#').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(so,'http://purl.obolibrary.org/obo/SO_').

% https://www.mediawiki.org/wiki/Wikibase/Indexing/RDF_Dump_Format#Prefixes_used
:- rdf_register_prefix(wd,'http://www.wikidata.org/entity/').
:- rdf_register_prefix(wdt,'http://www.wikidata.org/prop/direct/').
:- rdf_register_prefix(instance_of,'http://www.wikidata.org/prop/direct/P31').

% --------------------
% classes
% --------------------

% geography
continent(I) :- rdf(I,instance_of:'',wd:'Q5107').
country(I) :- rdf(I,instance_of:'',wd:'Q6256').
city(I) :- rdf(I,instance_of:'',wd:'Q515').

% disease
cancer(I) :- rdf(I,instance_of:'',wd:'Q12078').

% --------------------
% predicates
% --------------------

% meta
subproperty_of(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P1647',O).
exact_match(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P2888',O).

% info
author(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P50',O).

% geo
coordinate_location(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P625',O).


/*

anything matching anything in SO
  
pl2sparql -u sparqlprog/ontologies/wikidata -v -s wd  'exact_match(X,URI),str_starts(str(URI),"http://purl.obolibrary.org/obo/SO_")'  

  
*/
