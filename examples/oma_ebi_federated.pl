
:- use_module(library(sparqlprog/ontologies/oma)).
:- use_module(library(sparqlprog/ontologies/ebi),[]).
:- use_module(library(sparqlprog/ontologies/faldo)).

orthologs_with_coordinates(G1,G2,B,E,R) :-
        (   oma ?? (is_orthologous_to(P1,P2),
                    organism_in_taxon(P2,'http://purl.uniprot.org/taxonomy/9606'),
                    encoded_by_ensembl_gene(P1,G1),
                    encoded_by_ensembl_gene(P2,G2)
                    )),
        (   ebi ?? location(G2,B,E,R)).

