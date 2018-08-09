:- module(ncit,
          [
           gene_associated_with_disease/2,
           gene_associated_with_disease_inf/3,

           disease_has_finding_svf/2
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( ncit, 'https://stars-app.renci.org/ncitgraph/sparql').

:- rdf_register_prefix('NCIT','http://purl.obolibrary.org/obo/NCIT_').

gene_associated_with_disease(G,D) :- rdf(G,'NCIT':'R38',D).
gene_associated_with_disease_inf(G,D,X) :- rdf(G,'NCIT':'R38',X),rdfs_subclass_of(X,D).

disease_has_finding_svf(D,F) :- subclass_of_some(D,'NCIT':'R108',F).
    
