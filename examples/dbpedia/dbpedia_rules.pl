:- use_module(library(tabling)).
:- use_module(library(sparqlprog/emulate_builtins)).

%! band(?B) is nondet
%
%    true if B is a band
band(B) :- rdf(B,rdf:type,dbont:'Band').

%! has_shared_band_member(?B1, ?B2, ?SharedMember) is nondet
%
%    links bands by members in common. E.g. Ronnie James Dio in both Rainbow and Black Sabbath
%
%    true if SharedMember is a member of both B1 and B2 (and B1 and B2 are distinct)
%
has_shared_band_member(B1,B2,A) :-
        rdf(A,dbont:associatedBand,B1),
        rdf(A,dbont:associatedBand,B2),
        B1\=B2.

%! has_shared_band_genre(?B1, ?B2, ?SharedGenre) is nondet
%
%    links bands by shared genre common.
%
%    true if SharedGenre is a genre of both B1 and B2 (and B1 and B2 are distinct)
%
has_shared_genre(B1,B2,A) :-
        rdf(B1,dbont:genre,A),
        rdf(B2,dbont:genre,A),
        B1\=B2.

genre_pair(G1,G2,A) :-
        rdf(A,dbont:genre,G1),
        rdf(A,dbont:genre,G2),
        G1\=G2.


        
%! similarity_by_genre(?BandA, ?BandB, ?SumIC)
%
%    calculates the jaccard similarity between two entities based on genres in common
%
%    the entities should be of similar types (e.g. two bands, or two books)
%
%    ==
%    | genres(A) /\ genres(B) | / | genres(A) \/ genres(B) | 
%    ==
%
%    if no genres are in common, then this should equal 0
%    if all genres are in common, then this should equal 1
%
%    note this does not take into account how *meaningful* it is for a genre to be shared;
%    e.g. sharing the common genre 'pop' counts as much as a rarer genre like 'psytrance'.
%    see further on for IC-based metrics.
%
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

get_all_genres(Entity,L) :-
        service_query_all(dbpedia,G,rdf(Entity,dbont:genre,G),L).



:- table get_num_bands/1.
%! get_num_bands(?Count) is det
%
%    unifies Count with the total number of bands in the database
%
%    note this is tabled (cached) so that repeated calls do not invoke new SPARQL queries
%
get_num_bands(Count) :-
        ??(dbpedia,num_bands(Count)).
num_bands(Count) :-
        aggregate(count(distinct(B)),band(B),Count).



%! get_genre_num_bands(?Genre,?Count) is nondet.
%! get_genre_num_bands(+Genre,?Count) is det.
%
%    unifies Count with the total number of bands that are categorized as Genre
%
%
%%%%:- table get_genre_num_bands/2.
get_genre_num_bands(G,Count) :-
        ??(dbpedia,genre_num_bands(G,Count)).

genre_num_bands(G,Count) :-
        aggregate_group(count(distinct(B)),[G],(rdf(B,dbont:genre,G),band(B)),Count).

        

%! pair_genre_sum_ic(?BandA, ?BandB, ?SumIC)
%
%   for a pair of bands, SumIC is the sum of the ICs of the genres shared in common.
%
%
% Example: =pair_genre_ic(dbr:'Metallica', dbr:'Megadeth', IC)=
pair_genre_sum_ic(A,B,SumIC) :-
        get_all_genres(A,SA),
        ??(dbpedia,(band(B),has_shared_genre(A,B,_))),
        get_all_genres(B,SB),
        ord_intersection(SA,SB,I),
        debug(dbpedia,'~w vs ~w :: INTERSECTION(~w + ~w) = ~w',[A,B,SA,SB,I]),
        aggregate(sum(IC),G^(member(G,I),genre_ic(G,IC)),SumIC).

%! genre_ic(?Genre, ?InformationContent:float) is nondet.
%
%    gets the IC of a particular genre. The higher the IC, the rarer and more 'surprising' or information-rich it is.
%
%    for example, many bands are pop, so this would have a low IC. Progressive sludge metal is relatively rare and would have a high IC
%
%    ==
%    InformationContent = -log2( Pr(Genre) )
%    ==
%
genre_ic(G,IC) :-
        get_genre_num_bands(G,Count),
        debug(dbpedia,'|bands| in ~w = ~w',[G,Count]),
        get_num_bands(Total),
        debug(dbpedia,'Total bands = ~w',[Total]),
        seval(-log(Count/Total)/log(2), IC).

