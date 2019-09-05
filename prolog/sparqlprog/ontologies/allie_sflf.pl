:- module(allie_sflf,
 [
    cluster_member/2,
    long_form_representation/2,
    long_form/2,
    short_form/2,
    long_form_list/4
 ]).

:- sparql_endpoint(allie, 'https://data.allie.dbcls.jp/sparql').

:- rdf_register_prefix(allie, 'http://purl.org/allie/ontology/201108#').

cluster_member(A, B) :- rdf_path(A, ( (allie:contains) / (allie:hasMemberOf) ), B).
long_form_representation(A, B) :- rdf(A, allie:hasLongFormRepresentationOf, B).
long_form(A, B) :- rdf(A, allie:hasLongFormOf, B).
short_form(A, B) :- rdf(A, allie:hasShortFormOf, B).

long_form_list(S, L1, L2, LANG) :-
  cluster_member(A, B),
  short_form(B, F),
  label_of(S, F, "en"),
  optional(
    (long_form_representation(A, D),
    rdf(D,rdfs:label,L1),lang(L1)=LANG)),
  optional(
    (long_form(B, E),
    rdf(E,rdfs:label,L2),lang(L2)=LANG)).
