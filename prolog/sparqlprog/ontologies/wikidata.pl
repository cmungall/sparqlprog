/*

  @Deprecated

    now in its own pack: https://www.swi-prolog.org/pack/list?p=sparqlprog_wikidata
  
expose a subclass of dbpedia for demo purposes

See examples/wikidata-examples.sh

  For complete ontology use rdfs2pl

Note: this module uses macros to generate predicates. For every pname_wid/2 and cname_wid/2, predicates will be generated 
  
*/

:- module(wikidata,
          [

           property_constraint_pv/4,
           
           var_drug_condition/4,
           
           enlabel/2,
           enlabelp/2,
           enlabel_any/2,
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
:- rdf_register_prefix(wbont,'http://wikiba.se/ontology#').

:- rdf_register_prefix(instance_of,'http://www.wikidata.org/prop/direct/P31').

user:term_expansion(pname_wid(P,Id),
                    [(   Head :- Body),
                     (   Head_trans :- Body_trans),
                     (   Head_s :- Body_s),
                     (   Head_ps :- Body_ps),
                     (   Head_q :- Body_q),
                     (   Head_iri :- true),
                     (   Head_eiri :- true),
                     (   :- initialization(export(P_trans/2), now)),
                     (   :- initialization(export(P_s/2), now)),
                     (   :- initialization(export(P_ps/2), now)),
                     (   :- initialization(export(P_q/2), now)),
                     (   :- initialization(export(P_iri/1), now)),
                     (   :- initialization(export(P_eiri/1), now)),
                     (   :- initialization(export(P/2), now))
                    ]) :-

        % e.g. p9 ==> P9
        upcase_atom(Id,Frag),
        
        
        % Truthy assertions about the data, links entity to value directly
        % wd:Q2  wdt:P9 <http://acme.com/> ==> P9(Q2,"...")
        Head =.. [P,S,O],
        atom_concat('http://www.wikidata.org/prop/direct/',Frag,Px),
        Body = rdf(S,Px,O),

        atom_concat(P,'_transitive',P_trans),
        Head_trans =.. [P_trans,S,O],
        Body_trans = rdf_path(S,zeroOrMore(Px),O),
        
        
        % p: Links entity to statement
        % wd:Q2 p:P9 wds:Q2-82a6e009 ==> P9_statement(Q2,wds:....)
        atom_concat(P,'_e2s',P_s),
        Head_s =.. [P_s,S,O],
        atom_concat('http://www.wikidata.org/prop/',Frag,Px_s),
        Body_s = rdf(S,Px_s,O),

        atom_concat(P,'_iri',P_iri),
        Head_iri =.. [P_iri,Px],

        atom_concat('http://www.wikidata.org/entity/',Frag,Pe),        
        atom_concat(P,'_eiri',P_eiri),
        Head_eiri =.. [P_eiri,Pe],
        
        % ps: Links value from statement
        % wds:Q3-24bf3704-4c5d-083a-9b59-1881f82b6b37 ps:P8 "-13000000000-01-01T00:00:00Z"^^xsd:dateTime
        atom_concat(P,'_s2v',P_ps),
        Head_ps =.. [P_ps,S,O],
        atom_concat('http://www.wikidata.org/prop/statement/',Frag,Px_ps),
        Body_ps = rdf(S,Px_ps,O),
        
        % pq: Links qualifier from statement node
        % wds:Q3-24bf3704-4c5d-083a-9b59-1881f82b6b37 pq:P8 "-13000000000-01-01T00:00:00Z"^^xsd:dateTime
        % => P8_q(wds:..., "..."^^...)
        atom_concat(P,'_s2q',P_q),
        Head_q =.. [P_q,S,O],
        atom_concat('http://www.wikidata.org/prop/qualifier/',Frag,Px_q),
        Body_q = rdf(S,Px_q,O).

        

user:term_expansion(cname_wid(C,Id),
                    [Rule,
                     RuleInf,
                     RuleIsa,
                     (   Head_iri :- true),
                     (:- initialization(export(InfC/1), now)),
                     (:- initialization(export(SubC/1), now)),
                     (:- initialization(export(C_iri/1), now)),
                     (:- initialization(export(C/1), now))
                     ]) :-
        upcase_atom(Id,Frag),
        atom_concat('http://www.wikidata.org/entity/',Frag,Cx),
        
        Head =.. [C,I],
        Body = rdf(I,'http://www.wikidata.org/prop/direct/P31',Cx),
        Rule = (Head :- Body),

        atom_concat(C,'_iri',C_iri),
        Head_iri =.. [C_iri,Cx],
        
        atom_concat(C,'_inf',InfC),
        Head2 =.. [InfC,I],
        Body2 = rdf(I,('http://www.wikidata.org/prop/direct/P31'/zeroOrMore('http://www.wikidata.org/prop/direct/P279')),Cx),
        RuleInf = (Head2 :- Body2),
        
        atom_concat('isa_',C,SubC),
        Head3 =.. [SubC,I],
        Body3 = rdf(I,zeroOrMore('http://www.wikidata.org/prop/direct/P279'),Cx),
        RuleIsa = (Head3 :- Body3).


enlabel(E,N) :- label(E,N),lang(N)="en".
enlabelp(E,N) :- rdf(X,wbont:directClaim,E),enlabel(X,N).
enlabel_any(E,N) :- enlabel(E,N).
enlabel_any(E,N) :- enlabelp(E,N).



% --------------------
% classes
% --------------------

% geography

% disease
%cancer(I) :- rdf(I,instance_of:'',wd:'Q12078').
%:- initialization(export(cancer/1), now).

% --------------------
% predicates
% --------------------

% meta
subproperty_of(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P1647',O).
%exact_match(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P2888',O).

% info
%author(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P50',O).

% geo
%coordinate_location(S,O) :- rdf(S,'http://www.wikidata.org/prop/direct/P625',O).



% PROPS

% meta
pname_wid(instance_of, p31).
pname_wid(subclass_of, p279).
pname_wid(equivalent_property, p1628).
pname_wid(property_constraint, p2302).

property_constraint_pv(P,C,PP,V) :-
        property_constraint_e2s(P,S),
        rdf(S,PP,V),
        property_constraint_s2v(S,C).

% general
pname_wid(author, p50).
pname_wid(exact_match, p2888).

% geo
pname_wid(coordinate_location, p625).

% bio
% IDs
pname_wid(hp_id, p3841).
pname_wid(envo_id, p3859).
pname_wid(doid_id, p699).
pname_wid(chebi_id, p683).
pname_wid(uniprot_id, p352).
pname_wid(ncbigene_id, p351).
pname_wid(ipr_id, p2926).
pname_wid(civic_id, p3329).
pname_wid(ro_id, p3590).
pname_wid(mesh_id, p486).
pname_wid(go_id, p686).
pname_wid(ncbitaxon_id, p685).
pname_wid(uberon_id, p1554).
pname_wid(umls_id, p2892).

% rels
pname_wid(encodes, p688).
pname_wid(genetic_association, p2293).
pname_wid(treated_by_drug, p2176).
pname_wid(symptoms, p780).
pname_wid(pathogen_transmission_process, p1060).
pname_wid(has_cause, p828).
pname_wid(biological_variant_of, p3433).
pname_wid(has_part, p527).

% https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples#Get_known_variants_reported_in_CIViC_database_(Q27612411)_of_genes_reported_in_a_Wikipathways_pathway:_Bladder_Cancer_(Q30230812)
pname_wid(positive_therapeutic_predictor, p3354).
pname_wid(negative_therapeutic_predictor, p3355).
pname_wid(positive_diagnostic_predictor, p3356).
pname_wid(negative_diagnostic_predictor, p3357).
pname_wid(positive_prognostic_predictor, p3358).
pname_wid(negative_prognostic_predictor, p3359).
pname_wid(medical_condition_treated, p2175).

% beacon
pname_wid(physically_interacts_with, p129).
pname_wid(location, p276).
pname_wid(manifestation_of, p1557).
pname_wid(part_of, p361).
pname_wid(followed_by, p156).
pname_wid(product_or_material_produced, p1056).
pname_wid(uses, p2283).
pname_wid(has_effect, p1542).
pname_wid(drug_used_for_treatment, p2176).
pname_wid(found_in_taxon, p703).
pname_wid(ortholog, p684).
pname_wid(biological_process, p682).
pname_wid(cell_component, p681).
pname_wid(molecular_function, p680).
pname_wid(has_quality, p1552).
pname_wid(regulates, p128).


    
% CLASSES

% geo
cname_wid(geographic_entity, q27096213).
cname_wid(continent, q5107).
cname_wid(country, q6256).
cname_wid(city, q515).
pname_wid(population, p1082).
pname_wid(part_of_continent, p30).


% bio
cname_wid(cancer, q12078).
cname_wid(disease, q12136).
cname_wid(infectious_disease, q18123741).

cname_wid(chemical_compound, q11173).
cname_wid(chemical_element, q11344).
cname_wid(drug, q12140).

cname_wid(symptom, q169872).
cname_wid(medical_finding, q639907).
cname_wid(trait, q1211967).
cname_wid(pathway, q4915012).
cname_wid(macromolecular_complex, qQ22325163).
cname_wid(gene, qQ7187).
cname_wid(gene_product, qQ424689).
cname_wid(sequence_variant, qQ15304597).

cname_wid(therapy, q179661).
cname_wid(medical_procedure, qQ796194).


% random
cname_wid(power_station, q159719).

% TODO
nary(ptp_var_drug_condition, positive_therapeutic_predictor, medical_condition_treated).



var_drug_condition(V,D,C,positive_therapeutic_predictor) :-
        positive_therapeutic_predictor_e2s(V,S),
        medical_condition_treated_s2q(S,C),
        positive_therapeutic_predictor_s2v(S,D).



