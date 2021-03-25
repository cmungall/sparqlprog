:- module(search_util,
          [
           % must be exposed so it can be expanded in query rewrites
           label_or_synonym_pred_hook/1,

           lmatch/2,
           
           psearch/5,
           psearch/4,
           psearch/3,

           tsearch/6,
           tsearch/5,
           tsearch/4,
           tsearch/3,
           tsearch/2,
           
           lsearch/2,
           lsearch/3,
           lsearch/4,
           lsearch/5
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/owl_util)).
%:- use_module(library(sparqlprog/ontologies/owl), [label/2]).
:- use_module(library(sparqlprog/emulate_builtins)).

%! lmatch(+Pattern:atom, ?Object) is nondet
%
%
% Uses swi-prolog rdf11 'like' matching:
%  - case insensitive
%  - '*' is wildcard
%  - exact match unless wildcards specified
%
% NOTE: this will NOT do a conversion to STR in SPARQL; e.g 
% SELECT ?x WHERE {FILTER (regex(?v0,"^cell$","i")) . ?x <http://www.w3.org/2000/01/rdf-schema#label> ?v0}
lmatch(P,C) :-   {like(L,P)},rdf(C,rdfs:label,L).


%! lsearch(+Pattern, ?Object, ?Label, +Flags:str, ?Graph) is nondet.
%
% search for objects with an rdfs:label matching Pattern
%
% NOTE: this will do a conversion to STR in SPARQL; e.g 
% SELECT ?x WHERE {?x <http://www.w3.org/2000/01/rdf-schema#label> ?v0 . FILTER (regex(STR(?v0),"^cell$","i"))}
lsearch(P,C,L,F,G) :-   rdf(C,rdfs:label,L,G),regex(str(L),P,F).
lsearch(P,C,L,F) :-   label(C,L),regex(str(L),P,F).
lsearch(P,C,L) :-   label(C,L),regex(str(L),P,"i").
lsearch(P,C) :-   label(C,L),regex(str(L),P,"i").

%! lsearch(+Pattern, ?Object, ?Pred, ?Label, +Flags:str) is nondet.
%
% search for objects with any property matching Pattern
psearch(P,C,R,L,F) :-   rdf(C,R,L),regex(str(L),P,F).
psearch(P,C,R,L) :-   rdf(C,R,L),regex(str(L),P,"i").
psearch(P,C,R) :-   rdf(C,R,L),regex(str(L),P,"i").

%! tsearch(+Pattern, ?Object, ?Label, +Flags:str, ?Graph) is nondet.
%
% term search: find objects with an rdfs:label or synonym-like property matching Pattern
tsearch(P,C,Pred,L,F,G) :-   label_or_synonym_pred_hook(Pred), rdf(C,Pred,L,G),regex(str(L),P,F).
tsearch(P,C,Pred,L,F) :-   tsearch(P,C,Pred,L,F,_).
tsearch(P,C,Pred,L) :-   tsearch(P,C,Pred,L,"i",_).
tsearch(P,C,Pred) :-   tsearch(P,C,Pred,_,"i",_).
tsearch(P,C) :-   tsearch(P,C,_,_,"i",_).

:- rdf_meta label_or_synonym_pred_hook(r).
:- multifile label_or_synonym_pred_hook/1.
label_or_synonym_pred_hook(rdfs:label).
label_or_synonym_pred_hook('http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym').
label_or_synonym_pred_hook('http://www.geneontology.org/formats/oboInOwl#hasNarrowSynonym').
label_or_synonym_pred_hook('http://www.geneontology.org/formats/oboInOwl#hasBroadSynonym').
label_or_synonym_pred_hook('http://www.geneontology.org/formats/oboInOwl#hasExactSynonym').

