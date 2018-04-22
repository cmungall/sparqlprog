:- module(nlp_util,
          [rdf_nliteral/3,
           rdf_nliteral/4,
           rdf_nliteral_tr/5,
           shares_literal/6]).

:- rdf_register_ns(oio, 'http://www.geneontology.org/formats/oboInOwl#').


:- rdf_meta lprop(r).

lprop(rdfs:label).
lprop(oio:hasExactSynonym).
lprop(oio:hasRelatedSynonym).
lprop(oio:hasNarrowSynonym).
lprop(oio:hasBroadSynonym).


rdf_nliteral(X,P,V,G) :- rdf(X,P,Lit,G), bind(lcase(Lit), V).
rdf_nliteral(X,P,V) :- rdf_nliteral(X,P,V,_).


rdf_nliteral_tr(X,P,V,lcase,G) :- lprop(P),rdf(X,P,Lit,G), bind(lcase(Lit), V).

shares_literal(A,B,PA,PB,V,F) :-
        rdf_nliteral_tr(A,PA,V,F,_),rdf_nliteral_tr(B,PB,V,F,_),A\=B.




