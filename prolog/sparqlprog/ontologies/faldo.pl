:- module(faldo,
          [

           location/2,
           location/5,
           stranded_location/6,
           begin/2,
           end/2,

           begin_coord/3,
           end_coord/3,
           position/2,
           reference/2
           ]).


/** <module> wrapper for FALDO genome interval vocabulary

[FALDO](https://github.com/OBF/FALDO) is a vocabulary for specifying
locations of sequence features along sequence intervals (DNA, RNA, or
protein sequences). This module provides convenient wrappers for this
vocabulary.
  
*/

:- use_module(faldo).

:- rdf_register_prefix(faldo,'http://biohackathon.org/resource/faldo#').


%! location(?F, ?L) is nondet.
%
%    L is the location of feature F
%
location(F,L) :- rdf(F,faldo:location,L).

%! location(?F, ?L, ?B, ?E, ?R) is nondet.
%
%    feature F has location L, which has starts at B, ends at E, on reference R
%
%     assumes feature is not split across references
location(F,L,B,E,R) :- rdf(F,faldo:location,L),begin(L,PB),position(PB,B),reference(PB,R),end(L,PE),position(PE,E),reference(PE,R).


%stranded_location(F,L,B,E,R,Str) :- location(F,L,B,E,R), bind(if(B<E, 1, -1), Str).

stranded_location(F,L,B,E,R,Str) :- location(F,L,B,E,R), B < E, bind(1,Str).
stranded_location(F,L,E,B,R,Str) :- location(F,L,B,E,R), B > E, bind(-1,Str).
stranded_location(F,L,B,B,R,Str) :- location(F,L,B,B,R), bind(0,Str).



%! begin(?L, ?P) is nondet.
%
%    location L starts at position P
%
begin(L,P):- rdf(L,faldo:begin,P).

%! end(?L, ?P) is nondet.
%
%    location L ends at position P
%
end(L,P):- rdf(L,faldo:end,P).


%! begin_coord(?L, ?C, ?R) is nondet.
%
%    location L has start coordinate value C on reference R
%
begin_coord(L,C,R):- begin(L,P),position(P,C),reference(P,R).

%! end_coord(?L, ?C, ?R) is nondet.
%
%    location L has end coordinate value C on reference R
%
end_coord(L,C,R):- end(L,P),position(P,C),reference(P,R).


%! position(?L, ?P) is nondet.
%
%    location L has position P
%
position(L,P):- rdf(L,faldo:position,P).

%! reference(?L, ?R) is nondet.
%
%    location L has reference R
%
reference(L,R):- rdf(L,faldo:reference,R).


