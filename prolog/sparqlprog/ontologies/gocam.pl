/*

  
*/

:- module(gocam,
          [
           model/1,
           model_title/2,
           model_state/2,
           production_model/1,
           model_gene/2,
           
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
:- rdf_register_prefix(lego,'http://geneontology.org/lego/').
:- rdf_register_prefix(prov,'http://www.w3.org/ns/prov#').


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

% TODO - restrict
model(M) :- rdf(M,rdf:type,owl:'Ontology').

    
model_title(M,T) :- model(M),rdf(M,dc:title,T).
model_state(M,X) :- model(M),rdf(M,lego:modelstate,X).
production_model(M) :- model_state(M,"production").

model_gene(M,GC) :-
        ro:enabled_by(_,G,M),
        rdf(G,rdf:type,GC).

        

