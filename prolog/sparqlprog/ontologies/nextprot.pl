/*
  
*/

:- module(nextprot,
          [

           keyword/2,
           keyword_term/2,

           isoform/2,
           gene/2,
           child_of/2,
           term_child_of/2,
           
           cellular_component/2,
           cellular_component_term_child_of/2,

           expression/2,
           expression_term_child_of/2,
           expression_term_child_of/3,
           evidence_expression_level/2,
           
           topology/2,
           topology_transmembrane/2,

           phosphoprotein/1,

           ensembl_localid/2,

           protein_phosphorylated_in_cytoplasm/1,
           high_expression_in_brain/1

           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( nextprot, 'https://api.nextprot.org/sparql').

:- rdf_register_prefix(nextprot,'http://nextprot.org/rdf#').
:- rdf_register_prefix(nextprot_cv,'http://nextprot.org/rdf/terminology/').

keyword(G,K) :- rdf(G,nextprot:keyword,K).
keyword_term(G,T) :-
        rdf(G,nextprot:keyword,K),
        rdf(K,nextprot:term,T).

isoform(A,B) :- rdf(A,nextprot:isoform,B).
gene(A,B) :- rdf(A,nextprot:gene,B).

child_of(A,B) :- rdf(A,nextprot:childOf,B).
term_child_of(T,B) :- rdf(T,nextprot:term,A), rdf(A,nextprot:childOf,B).

cellular_component(P,C) :- rdf(P,nextprot:cellularComponent,C).
cellular_component_term_child_of(P,C) :- cellular_component(P,Z),term_child_of(Z,C).

expression(P,E) :- rdf(P,nextprot:expression,E).
expression_term_child_of(P,T,E) :- expression(P,E),term_child_of(E,T).
expression_term_child_of(P,T) :- expression_term_child_of(P,T,_).

evidence_expression_level(E,L) :- rdf(E,nextprot:evidence,Ev), rdf(Ev,nextprot:expressionLevel,L).

topology(P,S) :- rdf(P,nextprot:topology,S).

topology_transmembrane(P,S) :- topology(P,S),rdf(S,rdf:type,nextprot:'TransmembraneRegion').


phosphoprotein(P) :- keyword_term(P,nextprot_cv:'KW-0597').

% ----------------------------------------
% IDs
% ----------------------------------------
ensembl_localid(G,Id) :- bind(replace(str(G),"http://nextprot.org/rdf/gene/",""), Id).


% ----------------------------------------
% Example queries
% ----------------------------------------

protein_phosphorylated_in_cytoplasm(P) :-
        isoform(P,F),
        phosphoprotein(F),
        cellular_component_term_child_of(F,nextprot_cv:'SL-0086').

high_expression_in_brain(P) :-
        isoform(P,F),
        expression(F,E),
        term_child_of(E,nextprot_cv:'TS-0095'),
        evidence_expression_level(E,nextprot:'High').





