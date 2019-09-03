/*

  
*/

:- module(monarch,
          [

           protein_coding_gene/1,
           human_gene/1,
           disease_to_gene/2,
    %is_marker_for/2,
           association/4,
           association/5,
           clinvar_to_rs_number/2,
           dbsnp_to_rs_number/2,
           has_phenotype_association/3,
           has_phenotype_association/4,
           has_phenotype_freq/3,

           phenotype_rel/3,
           disease_to_phenotype_SD/2,
           disease_to_phenotype_EE/4,
           disease_to_phenotype_EE/2,

           disease_to_phenotype_SX/4
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/ontologies/oban)).

%:- sparql_endpoint( monarch, 'http://rdf.monarchinitiative.org/sparql').
:- sparql_endpoint( monarch, 'https://translator.ncats.io/monarch-blazegraph/namespace/kb/sparql/').


:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_prefix(oban,'http://purl.org/oban/').
:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(mondo,'http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix(so,'http://purl.obolibrary.org/obo/SO_').
:- rdf_register_prefix(bds,'http://www.bigdata.com/rdf/search#').
:- rdf_register_prefix(monarch,'https://monarchinitiative.org/').

% TODO
:- rdf_register_prefix(mgi,'http://www.informatics.jax.org/accession/MGI:').
:- rdf_register_prefix(orphanet,'http://www.orpha.net/ORDO/Orphanet_').
:- rdf_register_prefix(clinvar,'http://www.ncbi.nlm.nih.gov/clinvar/').

id_uri(ID,URI) :-
        concat_atom([Pre,Frag],':',ID),
        concat_atom(['http://purl.obolibrary.org/obo/',Pre,'_',Frag],URI).

% Each property declared with pname_id generates a binary predicate
user:term_expansion(pname_id(P,Id),
                    [(   Head :- Body),
                     (:- initialization(export(P/2), now))
                     ]) :-
        Head =.. [P,S,O],
        id_uri(Id,Px),
        Body = rdf(S,Px,O).

% Each class name declared with cname_id generates
%  - A unary predicate cname(Inst)
%  - A unary predicate cname_inf(Ins) [inferred]
%  - A unary predicate isa_cname(C) [inferred]
user:term_expansion(cname_id(C,Id),
                    [Rule,
                     RuleInf,
                     RuleIsa,
                     (:- initialization(export(InfC/1), now)),
                     (:- initialization(export(SubC/1), now)),
                     (:- initialization(export(C/1), now))
                     ]) :-
        id_uri(Id,Cx),
        
        Head =.. [C,I],
        Body = rdf(I,rdf:type,Cx),
        Rule = (Head :- Body),
        
        atom_concat(C,'_inf',InfC),
        Head2 =.. [InfC,I],
        Body2 = rdfs_individual_of(I,Cx),
        RuleInf = (Head2 :- Body2),
        
        atom_concat('isa_',C,SubC),
        Head3 =.. [SubC,I],
        Body3 = rdfs_subclass_of(I,Cx),
        RuleIsa = (Head3 :- Body3).



has_phenotype_association(A,S,O) :-
        has_phenotype_iri(P),
        association(A,S,P,O).

has_phenotype_association(A,S,O,Src) :-
        has_phenotype_association(A,S,O),
        rdf(A,dc:source,Src).

has_phenotype_freq(X,P,F) :-
        has_phenotype_association(X,P,A),
        rdf(A,monarch:frequencyOfPhenotype,F).


%! disease_to_phenotype_EE(?D,?P) is nondet
%
% true if D has P, 
% and where the assertion may be anything equivalent to D
% or anything equivalent to P
disease_to_phenotype_EE(D,P) :-
        disease_to_phenotype_EE(D,P,_,_).
disease_to_phenotype_EE(D,P,Dx,Px) :-
        owl_equivalent_class(D,Dx),
        has_phenotype(Dx,Px),
        owl_equivalent_class(Px,P).

%! disease_to_phenotype_ED(?D,?P) is nondet
%
% true if D has P, 
% and where the assertion may be anything equivalent to D
% and directly to P
disease_to_phenotype_ED(D,P) :-
        owl_equivalent_class(D,Dx),
        has_phenotype(Dx,P).

%! disease_to_phenotype_ED(?D,?P) is nondet
%
% true if D has P, 
% and where the assertion may be any subclass of D
% and directly to P
disease_to_phenotype_SD(D,P) :-
        rdfs_subclass_of(Ds,D),
        owl_equivalent_class(Ds,Dx),
        has_phenotype(Dx,P).

disease_to_phenotype_SX(D,P,Dx,Px) :-
        rdfs_subclass_of(Ds,D),
        owl_equivalent_class(Ds,Dx),
        has_phenotype(Dx,Px),
        owl_equivalent_class(Px,P).


d2v(D,V) :- is_marker_for(V,D).
disease_to_gene(D,G) :- is_marker_for(V,D),rdf(V,obo:'GENO_0000418',G).



protein_coding_gene(G) :- rdfs_subclass_of(G,obo:'SO_0001217').
human_gene(G) :-     protein_coding_gene(G), in_taxon(G,obo:'NCBITaxon_9606').


phenotype_rel(Ph,Pr,Y) :-
        rdf(Ph,owl:equivalentClass,EP),
        has_part_iri(HasPart),
        owl_some(EP,HasPart,I),
        intersection_member(I,M),
        owl_some(M,Pr,Y).


clinvar_to_rs_number(V,Id) :-
        % see https://github.com/monarch-initiative/dipper/issues/822
        rdf(V,rdf:type,obo:'GENO_0000030'),
        rdf(V,oio:hasdbxref,X),
        dbsnp_to_rs_number(X,Id).

dbsnp_to_rs_number(X,Id) :-
        % CHANGE THIS: see https://github.com/monarch-initiative/dipper/issues/822
        replace(str(X),"http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=","",Id).
dbsnp_to_curie(X,Id) :-
        replace(str(X),"http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=","dbSNP:",Id).

        
        
% in RO        
%pname_id(has_phenotype, 'RO:0002200').


% cut -f1,2 biolink-model.tsv | grep : | perl -npe 's@\|SIO:\d+@@' | tbl2p -p cname_id

cname_id('individual_organism', 'SIO:010000').
cname_id('disease', 'MONDO:0000001').
cname_id('phenotypic_feature', 'UPHENO:0000001').
cname_id('confidence_level', 'CIO:0000028').
cname_id('evidence_type', 'ECO:0000000').
cname_id('publication', 'IAO:0000311').
cname_id('chemical_substance', 'SIO:010004').
cname_id('genomic_entity', 'SO:0000110').
cname_id('genome', 'SO:0001026').
cname_id('transcript', 'SO:0000673').
cname_id('exon', 'SO:0000147').
cname_id('coding_sequence', 'SO:0000316').
cname_id('gene', 'SO:0000704').
cname_id('protein', 'PR:000000001').
cname_id('RNA_product', 'CHEBI:33697').
cname_id('microRNA', 'SO:0000276').
cname_id('macromolecular_complex', 'GO:0032991').
cname_id('gene_family', 'NCIT:C20130').
cname_id('zygosity', 'GENO:0000133').
cname_id('sequence_variant', 'GENO:0000512').
cname_id('drug_exposure', 'ECTO:0000509').
cname_id('treatment', 'OGMS:0000090').
cname_id('molecular_activity', 'GO:0003674').
cname_id('biological_process', 'GO:0008150').
cname_id('cellular_component', 'GO:0005575').
cname_id('cell', 'GO:0005623').

/*


*/
