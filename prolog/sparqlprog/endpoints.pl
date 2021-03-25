/** <module> sparql endpoint registry

  This module is provider for convenience. It provides default names for standard endpoint URLs,
  with a bias towards life-science endpoints.
  
*/

:- module(endpoints,
          []).
:- use_module(library(sparqlprog)).

%:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
:- sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').
:- sparql_endpoint( wd, 'http://query.wikidata.org/sparql').
:- sparql_endpoint( go, 'http://rdf.geneontology.org/sparql').
:- sparql_endpoint( godev, 'http://rdf-dev.geneontology.org/sparql').
:- sparql_endpoint( uniprot, 'http://sparql.uniprot.org/sparql').
:- sparql_endpoint( dbpedia, 'http://dbpedia.org/sparql/').
:- sparql_endpoint( ontobee, 'http://sparql.hegroup.org/sparql').
:- sparql_endpoint( nextprot, 'https://api.nextprot.org/sparql').
:- sparql_endpoint( disgenet, 'http://rdf.disgenet.org/sparql/').
:- sparql_endpoint( wikipathways, 'http://sparql.wikipathways.org').
:- sparql_endpoint( biotea, 'http://biotea.linkeddata.es/sparql').
:- sparql_endpoint( rhea, 'http://sparql.rhea-db.org/sparql').
:- sparql_endpoint( ubergraph, 'https://stars-app.renci.org/ubergraph/sparql').
:- sparql_endpoint( local, 'http://127.0.0.1:8921/bigdata/sparql').
:- sparql_endpoint( local2, 'http://127.0.0.1:8922/bigdata/sparql').
    
