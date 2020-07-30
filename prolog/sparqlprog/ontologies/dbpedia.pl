/* wrappers for dbpedia

Note that this only wraps a small subset of dbpedia, for demo purposes.

For complete ontology use rdfs2pl

  
  
*/

:- module(dbpedia,
          [person/1,
           musical_artist/1,
           band/1,
           photographer/1,
           disease/1,

           related_to/2,
           has_child/2,
           child_of/2,
           descendant_of/2,
           has_director/2,
           directed/2,
           has_genre/2,
           band_member/2,
           has_name/2,

           city/1,
           country/1,
           sport_competition_result/1]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
:- rdf_register_prefix(dbr,'http://dbpedia.org/resource/').

person(Person) :- rdf(Person,rdf:type,foaf:'Person').

%! has_name(?S, ?L) is nondet.
%
%   binds foaf:Name
%
has_name(S,L) :- rdf(S,foaf:'Name',L).

%! has_genre(?S, ?G) is nondet.
%
%   binds dbont:genre
%
has_genre(S,G) :- rdf(S,dbont:genre,G).


%! has_director(?S, ?O) is nondet.
%
%
%   binds dbont:director
%
has_director(S,O) :- rdf(S,dbont:director,O).

%! directed(?S, ?O) is nondet.
%
%
%   binds dbont:directed
%
directed(S,O) :- rdf(O,dbont:director,S).

%! has_child(?S, ?O) is nondet.
%
%
%
has_child(S,O) :- rdf(S,dbont:child,O).

%! child_of(?S, ?O) is nondet.
%
%
%
child_of(S,O) :- rdf(O,dbont:child,S).

%! descendant_of(?S, ?O) is nondet.
%
%    true if S can be connected to O via one or more has_child/2 relationships
%
descendant_of(S,O) :- rdf_path(O,oneOrMore(dbont:child),S).


%! band_member(?S, ?O) is nondet.
%
%
%
band_member(S,O) :- rdf(S,dbont:bandMember,O).

related_to(S,O) :- related_to(S,O,_).
related_to(S,O,has_child) :- has_child(S,O).
related_to(S,O,has_parent) :- has_child(O,S).

grandchild_of(S,O) :- child_of(S,Z),child_of(Z,O).





%! band(?X) is nondet.
%
%
%
band(X) :- rdf(X,rdf:type,dbont:'Band').

%! photographer(?X) is nondet.
%
%
%
photographer(X) :- rdf(X,rdf:type,dbont:'Photographer').

%! musical_artist(?X) is nondet.
%
%
%
musical_artist(X) :- rdf(X,rdf:type,dbont:'MusicalArtist').


%! disease(?X) is nondet.
%
%
%
disease(X) :- rdf(X,rdf:type,dbont:'Disease').

city(X) :- rdf(X,rdf:type,dbont:'City').
country(X) :- rdf(X,rdf:type,dbont:'Country').
sport_competition_result(X) :- rdf(X,rdf:type,dbont:'SportCompetitionResult').

             
