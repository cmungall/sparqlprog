:- module(uniprot,
          [
           protein/1,
           disease_annotation/1,
           has_disease_annotation/2,
           natural_variant_annotation/1,
           has_natural_variant_annotation/2,
           protein_natural_variant_disease/3,
           protein_natural_variant_disease_xref/4,
           protein_natural_variant_disease_dbsnp/4,
           is_dbsnp/1,
           
           transmembrane_annotation/1,
           has_transmembrane_annotation/2,

           mnemonic/2,
           encoded_by/2,

           ann_begin/3,

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
:- rdf_register_prefix(uptaxon,'http://purl.uniprot.org/taxonomy/').
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(embl_csd, 'http://purl.uniprot.org/embl-cds/').

:- sparql_endpoint( uniprot, 'http://sparql.uniprot.org/sparql').


:- type protein_iri ---> atomic_iri.

protein(C) :- rdf(C,rdf:type,up:'Protein').

disease_annotation(A) :- rdf(A,rdf:type,up:'Disease_Annotation').
has_disease_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Disease_Annotation').

natural_variant_annotation(A) :- rdf(A,rdf:type,up:'Natural_Variant_Annotation').
has_natural_variant_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation').

protein_natural_variant_disease(P,A,D) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D).

protein_natural_variant_disease_xref(P,A,D,X) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D),rdf(A,rdfs:seeAlso,X).
protein_natural_variant_disease_dbsnp(P,A,D,X) :- protein_natural_variant_disease_xref(P,A,D,X), is_dbsnp(X).

is_dbsnp(X) :- rdf(X,up:database,updb:dbSNP).


transmembrane_annotation(A) :- rdf(A,rdf:type,up:'Transmembrane_Annotation').
has_transmembrane_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Transmembrane_Annotation').


mnemonic(C,N) :- rdf(C,up:mnemonic,N).
encoded_by(P,G) :- rdf(P,up:encodedBy,G).

recommended_name(P,N) :- rdf(P,up:recommendedName,N).
has_full_name(P,X) :- rdf(P,up:recommendedName,N), rdf(N,up:fullName,X).

% for genes only
pref_label(E,N) :- rdf(E,skos:prefLabel,N).

in_taxon(P,T) :- rdf(P,up:organism,T).

annotation(P,A) :- rdf(P,up:annotation,A).
database(X,D) :- rdf(X,up:database,D).

    
ann_range(P,B,E,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R),
        end_coord(I,E,R).
ann_begin(P,B,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R).

substitution(A,S) :- rdf(A,up:substitution,S).

xref(P,X) :- rdf(P,rdfs:seeAlso,X).

:- srule(xref_interpro,[protein:uniprotPurl, xref:interproPurl ],
         'Maps protein to domain').

/*

  too slow
  
xref_interpro(P,D) :- xref(P,D),str_starts(str(D),'http://purl.uniprot.org/interpro/').
xref_panther(P,F) :- xref(P,F),str_starts(str(F),'http://purl.uniprot.org/panther/').
xref_pro(P,X) :- xref(P,X),str_starts(str(X),'http://purl.obolibrary.org/obo/PR_').
*/

xref_interpro(P,X) :- xref(P,X),is_interpro(X).
xref_panther(P,X) :- xref(P,X),is_panther(X).
xref_pro(P,X) :- xref(P,X),is_pro(X).

is_interpro(X) :- database(X,updb:'InterPro').
is_panther(X) :- database(X,updb:'PANTHER').
is_pro(X) :- database(X,updb:'PRO').




% convenience
in_human(P) :- rdf(P,up:organism,uptaxon:'9606').

reviewed(P) :- rdf(P,up:reviewed,true^^xsd:boolean).


/*

  in_human(P),annotation(P,A),ann_begin(A,B,R),substitution(A,\"F\")

  
  */




