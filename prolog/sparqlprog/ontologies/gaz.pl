:- module(gaz,
          [
           geolocated_in/2,
           shared_location/3,
           nge/1
           ]).


%geolocated_in(X,Y) :- subclass_of_some(X,'http://purl.obolibrary.org/obo/gaz#located_in',Y).
geolocated_in(X,Y) :- rdf(X,'http://purl.obolibrary.org/obo/RO_0001025',Y).
shared_location(X,Y,Z) :- geolocated_in(X,Y),geolocated_in(X,Z),Y\=Z.

nge(X) :- rdf(X,rdf:type,'http://purl.obolibrary.org/obo/ENVO_00000009').

