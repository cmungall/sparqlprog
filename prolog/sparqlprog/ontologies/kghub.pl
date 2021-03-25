:- module(kghub,
          [
           category/2,
           '^'/2,

           edge/5,
           edge/6,
           edge_property_value/3,
           provided_by/2,
           provided_by/4,

           relation_count/2,
           category_count/2,
           chained_provider/3,
           
           category_provider_count/3,
           category_node_property_count/3,
           category_relation_count/3,
           category_link_count/3,
           category_link_count/4,
           category_link_count/5,
           chained_edge_provider_count/3,
           edge_provider_count/2,
           provided_by_count/2,
           relation_provider_count/3
           %edge_property_count/2
           ]).

%:- use_module(library(biolink_model)).
%:- reexport([library(biolink_model)]).


:- rdf_register_prefix(biolink, 'https://w3id.org/biolink/vocab/').
:- rdf_register_prefix(uniprotkb, 'http://identifiers.org/uniprot/').
:- rdf_register_prefix(tmp, 'https://www.example.org/UNKNOWN/').
:- sparql_endpoint( kgcovid19, 'http://kg-hub-rdf.berkeleybop.io/blazegraph/sparql').

category(I,C) :- rdf(I,biolink:category,C).

capitalize(W,W2) :-
        atom_chars(W,[C|W1]),
        upcase_atom(C,C2),
        atom_chars(W2,[C2|W1]).
        

'^'(I,C) :-
        pre((
             kghub:capitalize(C,Cx),
             rdf_global_id(biolink:Cx,URI)
             )),
        category(I,URI).

edge(I,R,J,P,V) :-
        edge(_,I,R,J,P,V).
edge(A,I,R,J,P,V) :-
        rdf(A,biolink:subject,I),
        rdf(A,biolink:relation,R),
        rdf(A,biolink:object,J),
        rdf(A,P,V).

edge_property_value(A,P,V) :-
        rdf(A,biolink:subject,_),
        rdf(A,P,V).

provided_by(A,X) :-
        rdf(A,biolink:provided_by,X).
provided_by(I,R,J,X) :-
        rdf(A,biolink:subject,I),
        rdf(A,biolink:relation,R),
        rdf(A,biolink:object,J),
        rdf(A,biolink:provided_by,X).


chained_provider(Src1,Src2,JoinNode) :-
        rdf(A1,biolink:object,JoinNode),
        rdf(A2,biolink:subject,JoinNode),
        rdf(A1,biolink:provided_by,Src1),
        rdf(A2,biolink:provided_by,Src2).
        

relation_count(P,N) :- aggregate_group(count(*),[P],rdf(_,P,_),N).
category_count(C,N) :- aggregate_group(count(I),[C],category(I,C),N).
category_provider_count(C,S,N) :- aggregate_group(count(I),[C,S],(category(I,C),provided_by(I,S)),N).
category_relation_count(C,P,N) :- aggregate_group(count(*),[C,P],(category(I,C),rdf(I,P,_)),N).
category_node_property_count(C,P,N) :- aggregate_group(count(*),[C,P],(category(I,C),rdf(I,P,O),is_literal(O)),N).
category_link_count(C1,C2,N) :- aggregate_group(count(*),[C1,C2],(category(I1,C1),rdf(I1,_P,I2),category(I2,C2)),N).
category_link_count(C1,R,C2,N) :- aggregate_group(count(*),[C1,R,C2],(category(I1,C1),rdf(I1,R,I2),category(I2,C2)),N).
category_link_count(C1,R,C2,Src,N) :- aggregate_group(count(*),[C1,R,C2],(category(I1,C1),rdf(I1,R,I2),category(I2,C2),provided_by(I1,R,I2,Src)),N).
chained_edge_provider_count(Src1,Src2,N) :- aggregate_group(count(*),[Src1,Src2],chained_provider(Src1,Src2,_),N).
provided_by_count(P,N) :- aggregate_group(count(*),[P],provided_by(_,P),N).
edge_provider_count(S,N) :- aggregate_group(count(*),[],(provided_by(_I1,_R,_I2,S)),N). % TODO
relation_provider_count(R,S,N) :- aggregate_group(count(A),[R],(rdf(A,biolink:relation,R),rdf(A,biolink:provided_by,S)),N).

%edge_property_count(S,N) :- aggregate_group(count(*),[],(provided_by(_I1,_R,_I2,S)),N). % TODO
%relation_count(P,N) :- aggregate_group(count(*),[P],rdf(_,P,_),N).
