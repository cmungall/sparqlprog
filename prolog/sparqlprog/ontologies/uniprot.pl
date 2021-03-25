:- module(uniprot,
          [
           protein/1,
           has_annotation_type/3,
           disease_annotation/1,
           has_disease_annotation/2,
           natural_variant_annotation/1,
           has_natural_variant_annotation/2,
           protein_natural_variant_disease/3,
           protein_natural_variant_disease_xref/4,
           protein_natural_variant_disease_dbsnp/4,
           is_dbsnp/1,

           has_ptm_annotation/2,

           modification_annotation/1,
           peptide_annotation/1,
           transmembrane_annotation/1,
           has_transmembrane_annotation/2,

           mnemonic/2,
           encoded_by/2,

           annotation_range/4,
           protein_annotation_range/5,
           protein_begin/3,

           in_taxon/2,
           in_human/1,

           reviewed/1,
           annotation/2,
           database/2,

           substitution/2,

           xref/2,
           xref_interpro/2,
           xref_panther/2,
           xref_pro/2,
           is_interpro/1,
           is_panther/1,
           is_pro/1,
           has_full_name/2,

           pref_label/2,
           recommended_name/2
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).
:- use_module(library(sparqlprog)).

:- use_module(library(sparqlprog/owl_types)).
:- use_module(library(typedef)).

:- rdf_register_prefix(up,'http://purl.uniprot.org/core/').
:- rdf_register_prefix(updb,'http://purl.uniprot.org/database/').
:- rdf_register_prefix(uniprot,'http://purl.uniprot.org/uniprot/').
:- rdf_register_prefix(uniprot_annotation,'http://purl.uniprot.org/annotation/').
:- rdf_register_prefix(uptaxon,'http://purl.uniprot.org/taxonomy/').
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(embl_csd, 'http://purl.uniprot.org/embl-cds/').

:- sparql_endpoint( uniprot, 'http://sparql.uniprot.org/sparql').


:- type uniprot_protein ---> atomic_iri.
:- type uniprot_annotation ---> atomic_iri.
:- type uniprot_disease_annotation ---> uniprot_annotation.
:- type uniprot_variant_annotation ---> uniprot_annotation.
:- type uniprot_xref ---> atomic_iri.
:- type uniprot_term ---> atomic_iri.
:- type uniprot_sequence_string ---> string ^^ xsd_type.

%! protein(?P : uniprot_protein) is nondet.
%
%  true if P is a protein entry in uniprot
%
protein(C) :- rdf(C,rdf:type,up:'Protein').

%! has_annotation_type(?P : uniprot_protein, ?A : uniprot_annotation, ?T : owl_class) is nondet.
has_annotation_type(P,A,T) :- annotation(P,A),rdf(A,rdf:type,T).

%! disease_annotation(?A : uniprot_disease_annotation) is nondet.
%
%  true if A is a disease annotation in uniprot
%
disease_annotation(A) :- rdf(A,rdf:type,up:'Disease_Annotation').

%! has_disease_annotation(?P : uniprot_protein, ?A : uniprot_disease_annotation) is nondet.
%
%  protein P links to disease annotation A
%
has_disease_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Disease_Annotation').


has_ptm_annotation(P,A) :-
        annotation(P,A),
        rdf(A,rdf:type,up:'PTM_Annotation').


%! natural_variant_annotation(?A : uniprot_annotation) is nondet.
%
%  A is a natural variant annotation
%
natural_variant_annotation(A) :- rdf(A,rdf:type,up:'Natural_Variant_Annotation').

%! has_natural_variant_annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
%
%  protein P links to natural variant annotation A
%
has_natural_variant_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation').


%! protein_natural_variant_disease(?P : uniprot_protein, ?V : uniprot_variant_annotation, ?D) is nondet.
%
%  protein P links to disease D via natural variant annotation A 
%
protein_natural_variant_disease(P,A,D) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D).


%! protein_natural_variant_disease_xref(?P : uniprot_protein, ?A : uniprot_annotation, ?D, ?X : uniprot_xref) is nondet.
%
%  protein P links to disease D via natural variant annotation A with xref (e.g. dbSNP) X
%
protein_natural_variant_disease_xref(P,A,D,X) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D),rdf(A,rdfs:seeAlso,X).

%! protein_natural_variant_disease_dbsnp(?P : uniprot_protein, ?A : uniprot_annotation, ?D, ?X : uniprot_xref) is nondet.
%
%  protein P links to disease D via natural variant annotation A with dbSNP xref X
%
protein_natural_variant_disease_dbsnp(P,A,D,X) :- protein_natural_variant_disease_xref(P,A,D,X), is_dbsnp(X).


%! is_dbsnp(?X : uniprot_xref) is nondet.
is_dbsnp(X) :- rdf(X,up:database,updb:dbSNP).

%! modification_annotation(?A : uniprot_annotation) is nondet.
modification_annotation(A) :- rdfs_individual_of(A,up:'Modification_Annotation').


%! peptid_annotation(?A : peptide_annotation) is nondet.
peptide_annotation(A) :- rdfs_individual_of(A,up:'Peptide_Annotation').


%! transmembrane_annotation(?A : uniprot_annotation) is nondet.
transmembrane_annotation(A) :- rdf(A,rdf:type,up:'Transmembrane_Annotation').

%! has_transmembrane_annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
has_transmembrane_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Transmembrane_Annotation').



%! mnemonic(?C, ?N) is nondet.
mnemonic(C,N) :- rdf(C,up:mnemonic,N).

%! encoded_by(?P : uniprot_protein, ?G) is nondet.
encoded_by(P,G) :- rdf(P,up:encodedBy,G).


%! recommended_name(?P : uniprot_protein, ?N) is nondet.
recommended_name(P,N) :- rdf(P,up:recommendedName,N).

%! has_full_name(?P : uniprot_protein, ?X) is nondet.
has_full_name(P,X) :- rdf(P,up:recommendedName,N), rdf(N,up:fullName,X).

% for genes only

%! pref_label(?E, ?N) is nondet.
pref_label(E,N) :- rdf(E,skos:prefLabel,N).


%! in_taxon(?P : uniprot_protein, ?T) is nondet.
in_taxon(P,T) :- rdf(P,up:organism,T).


%! annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
annotation(P,A) :- rdf(P,up:annotation,A).

%! database(?X : uniprot_xref, ?D) is nondet.
database(X,D) :- rdf(X,up:database,D).

    
%! protein_annotation_range(?P : uniprot_protein, ?A, ?B, ?E, ?R) is nondet.
%
%  
%
protein_annotation_range(P,A,B,E,R) :-
        annotation(P,A),
        annotation_range(A,B,E,R).


%! annotation_range(?P : uniprot_annotation, ?B, ?E, ?R) is nondet.
annotation_range(P,B,E,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R),
        end_coord(I,E,R).

%! protein_begin(?P : uniprot_protein, ?B, ?R) is nondet.
protein_begin(P,B,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R).



%! classified_with(P : uniprot_protein, T : uniprot_term) is nondet.
classified_with(P,T) :- rdf(P,up:classifiedWith,T).


%! substitution(?A : uniprot_annotation, ?S : uniprot_sequence_string) is nondet.
%
%  annotation A is associated with a substitution S
%
substitution(A,S) :- rdf(A,up:substitution,S).



%! xref(?P : uniprot_protein, ?X : uniprot_xref) is nondet.
%
%  P has xref X
%
xref(P,X) :- rdf(P,rdfs:seeAlso,X).

%! xref_interpro(?P : uniprot_protein, ?X) is nondet.
%
%  P has xref X where X is in interpro
%
xref_interpro(P,X) :- xref(P,X),is_interpro(X).

%! xref_panther(?P : uniprot_protein, ?X) is nondet.
%
%  P has xref X is in panther
%
xref_panther(P,X) :- xref(P,X),is_panther(X).

%! xref_pro(?P : uniprot_protein, ?X : uniprot_xref) is nondet.
%
%  P has xref X where X is in uniprot
%
xref_pro(P,X) :- xref(P,X),is_pro(X).




%! is_interpro(?X : uniprot_xref) is nondet.
%
%  X is an xref in the UniProt database
%
is_interpro(X) :- database(X,updb:'InterPro').

%! is_panther(?X : uniprot_xref) is nondet.
%
%  X is an xref in the PANTHER database
%
is_panther(X) :- database(X,updb:'PANTHER').

%! is_pro(?X : uniprot_xref) is nondet.
%
%  X is an xref in the PRO database
%
is_pro(X) :- database(X,updb:'PRO').




%! in_human(?P : uniprot_protein) is nondet.
%
%  convenience predicate, true if P is a human protein
%
in_human(P) :- rdf(P,up:organism,uptaxon:'9606').


%! reviewed(?P : uniprot_protein) is nondet.
%
%  P is a protein with review status of true
%
reviewed(P) :- rdf(P,up:reviewed,true^^xsd:boolean).


/*

  in_human(P),annotation(P,A),ann_begin(A,B,R),substitution(A,\"F\")

  
  */




