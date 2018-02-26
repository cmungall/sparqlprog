:- module(faldo,
          [

           location/2,
           location/5,
           begin/2,
           end/2,

           begin_coord/3,
           end_coord/3,
           position/2,
           reference/2
           ]).

:- use_module(faldo).

:- rdf_register_prefix(faldo,'http://biohackathon.org/resource/faldo#').

location(F,L) :- rdf(F,faldo:location,L).
location(F,L,B,E,R) :- rdf(F,faldo:location,L),begin(L,PB),position(PB,B),reference(PB,R),end(L,PE),position(PE,E),reference(PE,R).

begin(L,P):- rdf(L,faldo:begin,P).
end(L,P):- rdf(L,faldo:end,P).

begin_coord(L,C,R):- begin(L,P),position(P,C),reference(P,R).
end_coord(L,C,R):- end(L,P),position(P,C),reference(P,R).

position(L,P):- rdf(L,faldo:position,P).
reference(L,R):- rdf(L,faldo:reference,R).

