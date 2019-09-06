
/*

  Example:
  
pl2sparql -e --consult examples/oma_ebi_federated.pl "orthologs_with_coordinates(ensembl:'ENSG00000198840',G2,Tax,E,R)"

  We pass the -e option to force execution within the prolog environment (rather attempting to compile the function)

  Within the prolog environment, the ?? predicate will explicitly invoke sparqlprog on an explicit endpoint

    */

:- use_module(library(sparqlprog/ontologies/oma)).
:- use_module(library(sparqlprog/ontologies/ebi),[]).
:- use_module(library(sparqlprog/ontologies/faldo)).

%! orthologs_with_coordinates(?Gene1, ?Gene2, ?Taxon, ?Begin:int, End:int, ?Ref) :-
orthologs_with_coordinates(G1,G2,T,B,E,R) :-
        (   oma ?? (is_orthologous_to(P1,P2),
                    organism_in_taxon(P2,T),
                    encoded_by_ensembl_gene(P1,G1),
                    encoded_by_ensembl_gene(P2,G2)
                    )),
        (   ebi ?? location(G2,B,E,R)).

