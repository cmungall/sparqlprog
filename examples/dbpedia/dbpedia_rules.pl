:- use_module(library(tabling)).
:- use_module(library(sparqlprog/emulate_builtins)).


has_shared_band_member(B1,B2,A) :-
        rdf(A,dbo:associatedBand,B1),
        rdf(A,dbo:associatedBand,B2),
        B1\=B2.

has_shared_genre(B1,B2,A) :-
        rdf(B1,dbo:genre,A),
        rdf(B2,dbo:genre,A),
        B1\=B2.

genre_pair(G1,G2,A) :-
        rdf(A,dbo:genre,G1),
        rdf(A,dbo:genre,G2),
        G1\=G2.

get_all_genres(Band,L) :-
        service_query_all(dbpedia,G,rdf(Band,dbo:genre,G),L).


band(B) :- rdf(B,rdf:type,dbont:'Band').

:- table get_num_bands/1.
get_num_bands(Count) :-
        ??(dbpedia,num_bands(Count)).
num_bands(Count) :-
        aggregate(count(distinct(B)),band(B),Count).


genre_num_bands(G,Count) :-
        aggregate_group(count(distinct(B)),[G],(rdf(B,dbont:genre,G),band(B)),Count).

%:- table get_genre_num_bands/2.
get_genre_num_bands(G,Count) :-
        ??(dbpedia,genre_num_bands(G,Count)).

genre_ic(G,IC) :-
        get_genre_num_bands(G,Count),
        debug(dbpedia,'|bands| in ~w = ~w',[G,Count]),
        get_num_bands(Total),
        debug(dbpedia,'Total bands = ~w',[Total]),
        seval(-log(Count/Total)/log(2), IC).

% Example: pair_genre_ic(dbr:'Metallica', dbr:'Megadeth', IC)
pair_genre_ic(A,B,SumIC) :-
        get_all_genres(A,SA),
        ??(dbpedia,(band(B),has_shared_genre(A,B,_))),
        get_all_genres(B,SB),
        ord_intersection(SA,SB,I),
        debug(dbpedia,'~w vs ~w :: INTERSECTION(~w + ~w) = ~w',[A,B,SA,SB,I]),
        aggregate(sum(IC),G^(member(G,I),genre_ic(G,IC)),SumIC).

        
        
similarity_by_genre(A,B,Sim) :-
        get_all_genres(A,SA),
        get_all_genres(B,SB),
        jaccard(SA,SB,Sim).

jaccard(SA,SB,Sim) :-
        ord_intersection(SA,SB,I),
        ord_union(SA,SB,U),
        length(I,NI),
        length(U,NU),
        Sim is NI/NU.
