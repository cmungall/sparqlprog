/** <module> wrapper that provides an obo-format like view over an OWL ontology

  More details on obo format:
  
  http://owlcollab.github.io/oboformat/doc/obo-syntax.html

  In particular, this module provides:

   - A graph-like view over an ontology via is_a/2 and relationship/3
   - the obo synonym model: entity_synonym_scope/3
   - the obo xref model: entity_xref/2 is re-exported from obo_metadata/oio; additional convenience preds such as entity_xref_src/4
  
  
*/

:- module(obo,
          [
           is_a/2,
           relationship/3,
           is_dangling/1,
           synprop_scope/2,
           entity_synonym_scope/2,
           entity_synonym_scope/3,
           entity_synonym_scope_type/4,
           entity_synonym_scope_xref/4,
           entity_xref_src/3,
           entity_xref_src/4,
           entity_xref_prefix/3
           ]).

:- use_module(library(obo_metadata/oio)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/owl_util)).

:- reexport(library(obo_metadata/oio), [has_dbxref/2]).

:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_ns(def,'http://purl.obolibrary.org/obo/IAO_0000115').

synprop_scope('http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym','RELATED').
synprop_scope('http://www.geneontology.org/formats/oboInOwl#hasNarrowSynonym','NARROW').
synprop_scope('http://www.geneontology.org/formats/oboInOwl#hasBroadSynonym','BROAD').
synprop_scope('http://www.geneontology.org/formats/oboInOwl#hasExactSynonym','EXACT').


%! is_a(?A, ?B) is nondet.
%
%  subClassOf between two names classes
%
is_a(A,B) :-
        rdf(A,rdfs:subClassOf,B),
        rdf_is_iri(B),
        rdf_is_iri(A).
is_a(A,B) :-
        rdf(A,rdfs:subPropertyOf,B),
        rdf_is_iri(B),
        rdf_is_iri(A).

%! relationship(?E, ?R, ?O) is nondet.
%
%  true if E subClassOf R some O
%
relationship(E,R,O) :-
        rdf(E,rdfs:subClassOf,Res),
        owl_some(Res,R,O),
        \+ rdf_is_bnode(O).


%! is_dangling(?E) is semidet.
%
%  true if E has no label
%
is_dangling(E) :-
        \+ label(E,_).



%! entity_synonym_scope(?E, ?V) is nondet.
%! entity_synonym_scope(?E, ?V, ?Scope) is nondet.
%
%  true if V is a synonym for entity E, with a Scope of either: exact, broad, narrow, related
%
entity_synonym_scope(E,V,Scope) :-
        synprop_scope(P,Scope),
        rdf(E,P,V).
entity_synonym_scope(E,V) :-
        entity_synonym_scope(E,V,_).


%! entity_synonym_scope_type(?E, ?V, ?Scope, ?Type) is nondet.
%
%  As entity_synonym_scope/3 but also include the synonym type
%
entity_synonym_scope_type(E,V,Scope,Type) :-
        synprop_scope(P,Scope),
        triple_axiom_annotation(E,P,V,oio:hasSynonymType,Type).


%! entity_synonym_scope_xref(?E, ?V, ?Scope, ?Xref) is nondet.
%
%  As entity_synonym_scope/3 but also include the xref provenance for the synonym
%
entity_synonym_scope_xref(E,V,Scope,Xref) :-
        synprop_scope(P,Scope),
        triple_axiom_annotation(E,P,V,oio:hasDbXref,Xref).



%!  entity_xref_src(?Cls:atm, ?X:str, ?S:str)
%
%   true if Cls has X as an xref, and axiom annotated with S
entity_xref_src(C,X) :-
        entity_xref_src(C,X,_).
entity_xref_src(C,X,S) :-
        entity_xref_src(C,X,_,S).
entity_xref_src(C,X,A,S) :-
        has_dbxref_axiom(C,X,A),
        rdf(A,oio:source,S).

%!  entity_xref_prefix(?Cls:atm, ?X:str, ?Pre:str)
%
%   true if Cls has X as an xref, and X has prefix Pre
%
:- rdf_meta entity_xref_prefix(r,o,o).
entity_xref_prefix(C,X,P) :-
        has_dbxref(C,X),
        curie_prefix(X,P).

%! one_to_one_xref(?E,X?,?S) is nondet
one_to_one_xref(E,X,S) :-
        entity_xref_idspace(E,X,S),
        \+ ((entity_xref_idspace(E,X2,S),
             X2\=X)),
        \+ ((entity_xref_idspace(E2,X,S),
             E2\=E)).

non_one_to_one_xref(E,X,S) :-
        entity_xref_idspace(E,X,S),
        \+ one_to_one_xref(E,X,S).

one_to_many_xref(E,X,S) :-
        one_to_many_xref(E,X,S,_).
one_to_many_xref(E,X,S,X2) :-
        entity_xref_idspace(E,X,S),
        entity_xref_idspace(E,X2,S),
        X2\=X.

many_to_one_xref(E,X,S) :-
        many_to_one_xref(E,X,S,_).
many_to_one_xref(E,X,S,E2) :-
        entity_xref_idspace(E,X,S),
        entity_xref_idspace(E2,X,S),
        E2\=E.

many_to_many_xref(E,X,S,E2,X2) :-
        many_to_one_xref(E,X,S,X2),
        one_to_many_xref(E,X,S,E2).

many_to_many_xref(E,X,S) :-
        entity_xref_idspace(E,X,S),
        \+ \+ many_to_one_xref(E,X,S),
        \+ \+ one_to_many_xref(E,X,S).



