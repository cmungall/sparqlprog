/*

expose a subclass of dbpedia for demo purposes

  For complete ontology use rdfs2pl

  
  
*/

:- module(wikidata,
          [
           %continent/1,
           %country/1,
           %city/1,

           enlabel/2,
           exact_match/2
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( wd, 'http://query.wikidata.org/sparql').

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
%:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms').
:- rdf_register_prefix(wikipathways,'http://vocabularies.wikipathways.org/wp#').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(so,'http://purl.obolibrary.org/obo/SO_').

% https://www.mediawiki.org/wiki/Wikibase/Indexing/RDF_Dump_Format#Prefixes_used
:- rdf_register_prefix(wd,'http://www.wikidata.org/entity/').
:- rdf_register_prefix(wdt,'http://www.wikidata.org/prop/direct/').
:- rdf_register_prefix(instance_of,'http://www.wikidata.org/prop/direct/P31').

user:term_expansion(pname_wid(P,Id),
                    [(   Head :- Body),
                     (:- initialization(export(P/2), now))
                     ]) :-
        Head =.. [P,S,O],
        upcase_atom(Id,Frag),
        atom_concat('http://www.wikidata.org/prop/direct/',Frag,Px),
        Body = rdf(S,Px,O).

user:term_expansion(cname_wid(C,Id),
                    [Rule,
                     RuleInf,
                     RuleIsa,
                     (:- initialization(export(InfC/1), now)),
                     (:- initialization(export(SubC/1), now)),
                     (:- initialization(export(C/1), now))
                     ]) :-
        upcase_atom(Id,Frag),
        atom_concat('http://www.wikidata.org/entity/',Frag,Cx),
        
        Head =.. [C,I],
        Body = rdf(I,'http://www.wikidata.org/prop/direct/P31',Cx),
        Rule = (Head :- Body),
        
        atom_concat(C,'_inf',InfC),
        Head2 =.. [InfC,I],
        Body2 = rdf(I,('http://www.wikidata.org/prop/direct/P31'/zeroOrMore('http://www.wikidata.org/prop/direct/P279')),Cx),
        RuleInf = (Head2 :- Body2),
        
        atom_concat('isa_',C,SubC),
        Head3 =.. [SubC,I],
        Body3 = rdf(I,zeroOrMore('http://www.wikidata.org/prop/direct/P279'),Cx),
        RuleIsa = (Head3 :- Body3).


enlabel(E,N) :- label(E,N),lang(N)="en".

% --------------------
% classes
% --------------------

% geography
%continent(I) :- rdf(I,instance_of:'',wd:'Q5107').
%country(I) :- rdf(I,instance_of:'',wd:'Q6256').
%city(I) :- rdf(I,instance_of:'',wd:'Q515').

% disease
%cancer(I) :- rdf(I,instance_of:'',wd:'Q12078').

:- initialization(export(cancer/1), now).

% --------------------
% predicates
% --------------------

% meta
subproperty_of(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P1647',O).
exact_match(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P2888',O).

% info
%author(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P50',O).

% geo
%coordinate_location(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P625',O).



% PROPS

% meta
pname_wid(instance_of, p31).
pname_wid(subclass_of, p279).

% general
pname_wid(author, p50).

% geo
pname_wid(coordinate_location, p625).

% bio
pname_wid(hp_id, p3841).
pname_wid(envo_id, p3859).
pname_wid(doid_id, p699).
pname_wid(chebi_id, p683).
pname_wid(uniprot_id, p352).
pname_wid(ncbigene_id, p351).
pname_wid(ipr_id, p2926).
pname_wid(civic_id, p3329).

pname_wid(encodes, p688).
pname_wid(genetic_association, p2293).
pname_wid(treated_by_drug, p2176).
pname_wid(symptoms, p780).
pname_wid(pathogen_transmission_process, p1060).
pname_wid(has_cause, p828).
pname_wid(positive_prognostic_predictor, p3358).
pname_wid(biological_variant_of, p3358).

% CLASSES

% geo
cname_wid(continent, q5107).
cname_wid(country, q6256).
cname_wid(city, q515).

% bio
cname_wid(cancer, q12078).
cname_wid(disease, q12136).
cname_wid(infectious_disease, q18123741).

% random
cname_wid(power_station, q159719).

