:- module(jcm,
          [
           microbial_strain/1,
           microbial_strain_id/2,
           species_name/2,
           qualified_species_name/2,
           microbial_strain_sample/2,
           sample_environment/3,
           sample_environment/4,
           microbial_strain_sample_environment/2,
           microbial_strain_sample_environment/3,
           microbial_strain_sample_environment/4
          ]).

:- use_module(library(semweb/rdf11)).

%:- use_module(library(sparqlprog/ontologies/faldo)).

:- sparql_endpoint(rdfportal, 'https://integbio.jp/rdf/sparql').
%:- sparql_endpoint(local, 'http://localhost').

:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(mccv, 'http://purl.jp/bio/10/mccv#').
:- rdf_register_prefix(obo, 'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(jcms, 'http://metadb.riken.jp/db/rikenbrc_jcm_microbe/').

%! microbial_strain(?S) is nondet
%
%   S is a microbial strain stocked in a biological resource center
microbial_strain(S) :- rdf(S,rdf:type,mccv:'MCCV_000001').

%! microbial_strain_id(?S) is nondet
%
%   microbial strain S has entry identifier I
microbial_strain_id(S,I) :- microbial_strain(S),rdf(S,dcterms:identifier,I).

%! species_name(?S,?N) is nondet
%
%   microbial strain S has species name N
species_name(S,N) :- microbial_strain(S),rdf(S,mccv:'MCCV_000011',N).

%! qualified_species_name(?S) is nondet
%
%   microbial strain S has qualified species name N
qualified_species_name(S,N) :- microbial_strain(S),rdf(S,mccv:'MCCV_000010',N).

%! microbial_strain_sample(?S,?SM) is nondet
%
%   microbial strain S was isolated from sampled SM
microbial_strain_sample(S,SM) :- microbial_strain(S),rdf(S,mccv:'MCCV_000028',SM).

%! sample_environment(?S,?SM,?E) is nondet
%
%   sample S from which microbial strain S was isolated was sapmled from
%   palce with environmental feature E
sample_environment(S,SM,E) :- microbial_strain(S),microbial_strain_sample(S,SM),rdf(SM,mccv:'MCCV_000072',P),rdf(P,mccv:'MCCV_000071',E).
sample_environment(S,SM,E,L) :- microbial_strain(S),microbial_strain_sample(S,SM),rdf(SM,mccv:'MCCV_000072',P),rdf(P,mccv:'MCCV_000071',E),rdf(E,rdfs:label,L).

%! microbial_strain_sample_environment(?S,?E) is nondet
%
%   microbial strain S inhabits place with environmetal feature E
microbial_strain_sample_environment(S,E) :- microbial_strain(S),microbial_strain_sample(S,SM),sample_environment(S,SM,E).

microbial_strain_sample_environment(S,N,E) :- microbial_strain(S),species_name(S,N),microbial_strain_sample(S,SM),sample_environment(S,SM,E).

microbial_strain_sample_environment(S,N,E,L) :- microbial_strain(S),species_name(S,N),microbial_strain_sample(S,SM),sample_environment(S,SM,E),rdf(E,rdfs:label,L).

