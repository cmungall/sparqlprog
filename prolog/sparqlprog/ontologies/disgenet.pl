/** <module> wrapper for disgenet endpoint

*/

:- module(disgenet,
          [
           gene/1,
           disease/1,
           
           refers_to/2,
           has_evidence/2,
           
           association/1,
           gene_disease_association/1,
           gene_disease_association/3,
           gene_disease_association/4,

           disease_pair_by_shared_gene/3
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- use_module(library(sparqlprog/owl_types)).
:- use_module(library(typedef)).

:- type disgenet_protein ---> atomic_iri.
:- type disgenet_annotation ---> atomic_iri.
:- type disgenet_gene ---> atomic_iri.
:- type disgenet_publication ---> atomic_iri.

:- sparql_endpoint( disgenet, 'http://rdf.disgenet.org/sparql/').

:- rdf_register_prefix(disgenet,'http://rdf.disgenet.org/').
:- rdf_register_prefix(disgenet_gene_disease_association,'http://rdf.disgenet.org/resource/gene_disease_association/').
:- rdf_register_prefix(sio, 'http://semanticscience.org/resource/SIO_').
:- rdf_register_prefix(ncitevs, 'http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#').
:- rdf_register_prefix(umls, 'http://linkedlifedata.com/resource/umls/id/').
:- rdf_register_prefix(ncbigene, 'http://identifiers.org/ncbigene/').


%! refers_to(?A : disgenet_annotation, ?X : atomic_iri) is nondet.
refers_to(A,B) :- rdf(A,sio:'000628',X).

%! has_evidence(?A : disgenet_annotation ,?E : disgenet_publication) is nondet.
has_evidence(A,B) :- rdf(A,sio:'000772',B).

%! disease(?D : disgenet_disease) is nondet.
disease(D) :- rdf(D,rdf:type,ncitevs:'C7057').

%! gene(?G : disgenet_gene) is nondet.
gene(G) :- rdf(G,rdf:type,ncitevs:'C16612').

%! association(?A : disgenet_annotation) is nondet.
assoc(A) :- rdfs_individual_of(A,sio:'000897').

%! gene_disease_association(?A : disgenet_annotation) is nondet.
%! gene_disease_association(?A : disgenet_annotation, ?G : disgenet_gene, ?D : disgenet_disease) is nondet.
%! gene_disease_association(?A : disgenet_annotation, ?G : disgenet_gene, ?D : disgenet_disease, ?P) is nondet.
%
%  gene G is associated with disease D via evidence A
gene_disease_association(A) :- rdfs_individual_of(A,sio:'001121').
gene_disease_association(A,G,D) :- gene_disease_association(A),refers_to(A,G),gene(G),refers_to(A,D),disease(D).
gene_disease_association(A,G,D,P) :- gene_disease_association(A),refers_to(A,G),gene(G),refers_to(A,D),disease(D),has_evidence(A,P).

%! disease_pair_by_shared_gene(?D1 : disgenet_disease, ?D2 : disgenet_disease, ?G : disgenet_gene) is nondet.
%
%  both D1 and D2 are associated with the same gene G
disease_pair_by_shared_gene(D1,D2,G) :- gene_disease_association(_,G,D1),gene_disease_association(_,G,D2).






