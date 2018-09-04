:- module(biotea,
          [
           src_term/2,
           ann_src_term/5,
           coannotation/6
           ]).

:- rdf_register_prefix(ncitevs, 'http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#').
:- rdf_register_prefix(snomed, 'http://purl.bioontology.org/ontology/SNOMEDCT/').

:- rdf_register_prefix(biotea,'https://biotea.github.io/biotea-ontololgy#').
:- rdf_register_prefix(oa,'http://www.w3.org/ns/oa#').
:- rdf_register_prefix(pmc,'http://linkingdata.io/pmcdoc/pmc/').

ann_src_term(A, Src, Para, TermUri, Label) :-
        rdf(A, oa:hasTarget, Para),
        rdf(Para, oa:hasSource, Src),
        rdf(A, oa:hasBody, Text),
        rdf(Text, rdf:value, Label),
        rdf(A, oa:hasBody, TermUri),
        \+ rdf(TermUri, rdf:type, oa:'TextualBody').



src_term(Src, TermUri) :-
        ann_src_term(_, Src, _, TermUri).

% e.g. `coannotation(pmc:'3875424',pmc:'3933681',_,_,_,_)`
coannotation(P1,P2,TermUri,Label,F1,F2) :-
        ann_src_term(A1,P1,_,TermUri,Label),
        ann_src_term(A2,P2,_,TermUri,_),
        rdf(A1,biotea:tf,F1),
        rdf(A2,biotea:tf,F2).

        
        