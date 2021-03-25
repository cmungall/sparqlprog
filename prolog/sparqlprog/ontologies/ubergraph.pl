:- module(ubergraph,
          [
           rdf_ontology/3,
           rdf_closure/3,
           rdf_nr/3,
           rdf_redundant/3
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(ubergraph,'http://reasoner.renci.org/').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').

    
rdf_ontology(S,P,O):- rdf(S,P,O,ubergraph:ontology).
rdf_closure(S,P,O):- rdf(S,P,O,ubergraph:'ontology/closure').
rdf_nr(S,P,O):- rdf(S,P,O,ubergraph:nonredundant).
rdf_redundant(S,P,O):- rdf(S,P,O,ubergraph:redundant).


