has_shared_band_member(B1,B2,A) :-
        rdf(A,dbo:associatedBand,B1),
        rdf(A,dbo:associatedBand,B2),
        B1\=B2.

has_shared_genre(G1,G2,A) :-
        rdf(A,dbo:genre,B1),
        rdf(A,dbo:genre,B2),
        B1\=B2.
