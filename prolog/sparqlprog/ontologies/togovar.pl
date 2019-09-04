:- module(togovar,
          [
           snv/1,
           reference_allele/2,
           alternative_allele/2,
           snv_interpreted_condition/2,
           snv_with_clinvar/5,
           frequency/1,
           dataset/2,
           snv_frequency/3,
           snv_japanese_frequency/5
          ]).

:- use_module(library(semweb/rdf11)).

:- use_module(library(sparqlprog/ontologies/faldo)).

%:- sparql_endpoint(togovar, 'https://togovar.biosciencedbc.jp/sparql').
:- sparql_endpoint(togovar, 'https://togovar.l5dev.jp/sparql').

:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(togovar, 'http://togovar.biosciencedbc.jp/variant/').
:- rdf_register_prefix(tgvo, 'http://togovar.biosciencedbc.jp/ontology/').
:- rdf_register_prefix(obo, 'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(m2r, 'http://med2rdf.org/ontology/med2rdf#').
:- rdf_register_prefix(hco, 'http://identifiers.org/hco/').

%! snv(?V) is nondet
%
%   V is a single nucleotide variant (SNV)
snv(V) :- rdf(V,rdf:type,obo:'SO_0001483').

%! reference_allele(?V,?A) is nondet
%
%   single nucleotide variant V has reference allele A
reference_allele(V,A) :- snv(V),rdf(V,m2r:reference_allele,A).

%! alternative_allele(?V,?A) is nondet
%
%   single nucleotide variant V has alternative allele A
alternative_allele(V,A) :- snv(V),rdf(V,m2r:alternative_allele,A).

%! snv_interpreted_condition(?V,?C) is nondet
%
%   single nucleotide variant V has ClinVar's interpreted condition C
snv_interpreted_condition(V,C) :- rdf(V,tgvo:hasInterpretedCondition,C).


%! snv_with_clinvar(?V,?C) is nondet
%
%   single nucleotide variant V located in genomic region between
%   B and E of reference sequence R has ClinVar's interpreted condition C
%
%   (example)
%   pl2sparql  -u sparqlprog/ontologies/togovar -u sparqlprog/ontologies/faldo -s togovar "snv_with_clinvar(V,198000000,199000000,'http://identifiers.org/hco/1#GRCh37',CL)" "h(V,CL)"
%
snv_with_clinvar(V,B,E,R,CL) :- snv(V),location(V,P),position(P,C),reference(P,R),C >= B,C =< E,snv_interpreted_condition(V,CL).

%! frequency(F) is nondet
%
%   F is instance of tgvo:Frequency 
frequency(F) :- rdf(F,rdf:type,tgvo:'Frequency').

%! dataset(D) is nondet
%
%   single nucleotide variant V is included in dataset D 
dataset(V,D) :- rdf(V,tgvo:hasFrequency,N),rdf(N,rdfs:label,D).

%! snv_frequency(?V,?D,?F) is nondet
%
%   single nucleotide variant V in dataset D has frequency F 
snv_frequency(V,D,F) :- rdf(V,tgvo:hasFrequency,N),frequency(N),rdf(N,rdfs:label,D),rdf(N,tgvo:frequency,F).

%! snv_japanese_frequency(?V,?RA,?AA,?D,?F) is nondet
%
%   single nucleotide variant V with reference allele RA and 
%   alternative allele AA in dataset D has frequency F
%   (example)
%   pl2sparql -u sparqlprog/ontologies/togovar -s togovar "snv_japanese_frequency(togovar:tgv1006196,"T","C",D,F)" "h(D,F)"
%
snv_japanese_frequency(V,RA,AA,D,F) :- snv(V),reference_allele(V,RA),alternative_allele(V,AA),snv_frequency(V,D,F).


