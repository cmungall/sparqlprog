/*

  
*/

:- module(gocam,
          [
           gene_to_function/2,
           gene_to_process/2,
           gene_to_component/2,
           gene_to_go/2,

           gene_to_gene/3
           
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(obo_ro/ro)).
:- use_module(library(obo_core/goslim)).


:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(go,'http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix(bds,'http://www.bigdata.com/rdf/search#').

gene_to_go(GC,C) :-  gene_to_process(GC,C).
gene_to_go(GC,C) :-  gene_to_component(GC,C).
gene_to_go(GC,C) :-  gene_to_function(GC,C).

gene_to_process(GC,PC) :-
        ro:enabled_by(F,G),
        ro:part_of(F,P),
        rdf(G,rdf:type,GC),
        rdf(P,rdf:type,PC).

gene_to_component(GC,CC) :-
        ro:enabled_by(F,G),
        ro:occurs_in(F,C),
        rdf(G,rdf:type,GC),
        rdf(C,rdf:type,CC).

gene_to_function(GC,FC) :-
        ro:enabled_by(F,G),
        rdf(G,rdf:type,GC),
        rdf(F,rdf:type,FC).

gene_to_gene(G1C,R,G2C) :-
        ro:enabled_by(F1,G1),
        rdf(F1,R,F2),
        ro:enabled_by(F2,G2),
        rdf(G1,rdf:type,G1C),
        rdf(G2,rdf:type,G2C).
