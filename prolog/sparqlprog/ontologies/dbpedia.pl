/*

expose a subclass of dbpedia for demo purposes

  For complete ontology use rdfs2pl

  
  
*/

:- module(dbpedia,
          [person/1,
           musical_artist/1,
           photographer/1,
           disease/1,

           has_child/2,
           child_of/2,
           descendant_of/2,
           has_director/2,
           directed/2,
           band_member/2,
           has_name/2]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
:- rdf_register_prefix(dbr,'http://dbpedia.org/resource/').

person(Person) :- rdf(Person,rdf:type,foaf:'Person').
has_name(S,L) :- rdf(S,foaf:'Name',L).

has_director(S,O) :- rdf(S,dbont:director,O).
directed(S,O) :- rdf(O,dbont:director,S).
has_child(S,O) :- rdf(S,dbont:child,O).
child_of(S,O) :- rdf(O,dbont:child,S).
descendant_of(S,O) :- rdf_path(O,oneOrMore(dbont:child),S).

band_member(S,O) :- rdf(S,dbont:bandMember,O).


related_to(S,O) :- has_child(S,O).
related_to(S,O) :- has_child(O,S).

grandchild_of(S,O) :- child_of(S,Z),child_of(Z,O).




photographer(X) :- rdf(X,rdf:type,dbont:'Photographer').
musical_artist(X) :- rdf(X,rdf:type,dbont:'MusicalArtist').

disease(X) :- rdf(X,rdf:type,dbont:'Disease').
