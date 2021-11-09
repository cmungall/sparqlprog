:- module(uniprot,
          [
           protein/1,
           in_reference_proteome/1,
           has_annotation_type/3,
           disease_annotation/1,
           has_disease_annotation/2,
           natural_variant_annotation/1,
           has_natural_variant_annotation/2,
           protein_natural_variant_disease/3,
           protein_natural_variant_disease_xref/4,
           protein_natural_variant_disease_dbsnp/4,
           is_dbsnp/1,

           has_ptm_annotation/2,

           modification_annotation/1,
           peptide_annotation/1,
           transmembrane_annotation/1,
           has_transmembrane_annotation/2,

           classified_with/2,
           classified_with_go/2,
           classified_with_go_mf/2,
           classified_with_go_catalysis/2,
           has_catalytic_activity/2,
           protein_has_enzyme_class/2,
           protein_has_catalyzed_reaction/2,
           enzyme_class/2,
           catalyzed_reaction/2,

           mnemonic/2,
           encoded_by/2,

           annotation_range/4,
           protein_annotation_range/5,
           protein_begin/3,

           in_taxon/2,
           in_human/1,

           reviewed/1,
           annotation/2,
           database/2,

           substitution/2,

           xref/2,
           xref_intersection_count/3,
           xref_union_count/3,
           xref_in/3,
           xref_interpro/2,
           xref_panther/2,
           xref_pro/2,
           xref_araport/2,
           xref_tair/2,
           is_interpro/1,
           is_panther/1,
           is_pro/1,
           is_organism_database/1,
           is_organism_database/2,
           is_xenbase/1,
           is_wormbase/1,
           has_full_name/2,

           pref_label/2,
           recommended_name/2,

           rhea_count/2,

           uniprot_class/1,
           predicate_summary/5
           ]).

:- use_module(library(sparqlprog/ontologies/faldo)).
:- use_module(library(sparqlprog)).

:- use_module(library(sparqlprog/owl_types)).
:- use_module(library(typedef)).

:- rdf_register_prefix(up,'http://purl.uniprot.org/core/').
:- rdf_register_prefix(updb,'http://purl.uniprot.org/database/').
:- rdf_register_prefix(uniprot,'http://purl.uniprot.org/uniprot/').
:- rdf_register_prefix(uniprotkw,'http://purl.uniprot.org/keywords/').
:- rdf_register_prefix(uniprot_annotation,'http://purl.uniprot.org/annotation/').
:- rdf_register_prefix(uptaxon,'http://purl.uniprot.org/taxonomy/').
:- rdf_register_prefix(interpro,'http://purl.uniprot.org/interpro/').
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(embl_cds, 'http://purl.uniprot.org/embl-cds/').
:- rdf_register_prefix(rhea, 'http://rdf.rhea-db.org/').
:- rdf_register_prefix(enzyme, 'http://purl.uniprot.org/enzyme/').
:- rdf_register_prefix('GO', 'http://purl.obolibrary.org/obo/GO_').

:- sparql_endpoint( uniprot, 'http://sparql.uniprot.org/sparql').

:- type uniprot_protein ---> atomic_iri.
:- type uniprot_annotation ---> atomic_iri.
:- type uniprot_disease_annotation ---> uniprot_annotation.
:- type uniprot_variant_annotation ---> uniprot_annotation.
:- type uniprot_xref ---> atomic_iri.
:- type uniprot_term ---> atomic_iri.
:- type uniprot_sequence_string ---> string ^^ xsd_type.

uniprot_class(C) :-
        rdf(C,rdf:type,owl:'Class'),
        str_starts(str(C),'http://purl.uniprot.org/core/').

predicate_summary(P,ST,OT,Num,1) :-
        ??(uniprot,
           rdf(P,rdf:type,owl:'ObjectProperty')),
        format('# ~w',[P]),
        ??(uniprot,
           uniprot_class(ST)),
        ??(uniprot,
           exists(rdf(_,rdf:type,ST))),
        format('# ~w',[ST]),
        ??(uniprot,
           exists( (rdf(X,rdf:type,ST),rdf(X,P,_)))),
        format('#f ~w',[P]),
        ??(uniprot,
           aggregate_group(count(O),
                           [OT],
                           (   rdf(S,rdf:type,ST),rdf(S,P,O),rdf(O,rdf:type,OT)),
                           Num)),
        format('# ~w',[P/Num]).




%! protein(?P : uniprot_protein) is nondet.
%
%  true if P is a protein entry in uniprot
%
protein(C) :- rdf(C,rdf:type,up:'Protein').

in_reference_proteome(C) :- rdf(C,up:classifiedWith,uniprotkw:'1185').


%! has_annotation_type(?P : uniprot_protein, ?A : uniprot_annotation, ?T : owl_class) is nondet.
has_annotation_type(P,A,T) :- annotation(P,A),rdf(A,rdf:type,T).

%! disease_annotation(?A : uniprot_disease_annotation) is nondet.
%
%  true if A is a disease annotation in uniprot
%
disease_annotation(A) :- rdf(A,rdf:type,up:'Disease_Annotation').

%! has_disease_annotation(?P : uniprot_protein, ?A : uniprot_disease_annotation) is nondet.
%
%  protein P links to disease annotation A
%
has_disease_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Disease_Annotation').


has_ptm_annotation(P,A) :-
        annotation(P,A),
        rdf(A,rdf:type,up:'PTM_Annotation').


%! natural_variant_annotation(?A : uniprot_annotation) is nondet.
%
%  A is a natural variant annotation
%
natural_variant_annotation(A) :- rdf(A,rdf:type,up:'Natural_Variant_Annotation').

%! has_natural_variant_annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
%
%  protein P links to natural variant annotation A
%
has_natural_variant_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation').


%! protein_natural_variant_disease(?P : uniprot_protein, ?V : uniprot_variant_annotation, ?D) is nondet.
%
%  protein P links to disease D via natural variant annotation A 
%
protein_natural_variant_disease(P,A,D) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D).


%! protein_natural_variant_disease_xref(?P : uniprot_protein, ?A : uniprot_annotation, ?D, ?X : uniprot_xref) is nondet.
%
%  protein P links to disease D via natural variant annotation A with xref (e.g. dbSNP) X
%
protein_natural_variant_disease_xref(P,A,D,X) :- annotation(P,A),rdf(A,rdf:type,up:'Natural_Variant_Annotation'),rdf(A,skos:related,D),rdf(A,rdfs:seeAlso,X).

%! protein_natural_variant_disease_dbsnp(?P : uniprot_protein, ?A : uniprot_annotation, ?D, ?X : uniprot_xref) is nondet.
%
%  protein P links to disease D via natural variant annotation A with dbSNP xref X
%
protein_natural_variant_disease_dbsnp(P,A,D,X) :- protein_natural_variant_disease_xref(P,A,D,X), is_dbsnp(X).


%! modification_annotation(?A : uniprot_annotation) is nondet.
modification_annotation(A) :- rdfs_individual_of(A,up:'Modification_Annotation').


%! peptid_annotation(?A : peptide_annotation) is nondet.
peptide_annotation(A) :- rdfs_individual_of(A,up:'Peptide_Annotation').


%! transmembrane_annotation(?A : uniprot_annotation) is nondet.
transmembrane_annotation(A) :- rdf(A,rdf:type,up:'Transmembrane_Annotation').

%! has_transmembrane_annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
has_transmembrane_annotation(P,A) :- annotation(P,A),rdf(A,rdf:type,up:'Transmembrane_Annotation').

%! has_catalytic_activity(?P : uniprot_protein, ?CA : catalytic_activity) is nondet.
has_catalytic_activity(P,CA) :- annotation(P,Ann),rdf(Ann,up:catalyticActivity,CA).


catalyzed_reaction(CA,R) :- rdf(CA,up:catalyzedReaction,R).
enzyme_class(CA,R) :- rdf(CA,up:enzymeClass,R).

% rhea
protein_has_catalyzed_reaction(P,R) :- has_catalytic_activity(P,A),catalyzed_reaction(A,R).
protein_has_enzyme_class(P,R) :- has_catalytic_activity(P,A),enzyme_class(A,R).



%! mnemonic(?C, ?N) is nondet.
mnemonic(C,N) :- rdf(C,up:mnemonic,N).

%! encoded_by(?P : uniprot_protein, ?G) is nondet.
encoded_by(P,G) :- rdf(P,up:encodedBy,G).


%! recommended_name(?P : uniprot_protein, ?N) is nondet.
recommended_name(P,N) :- rdf(P,up:recommendedName,N).

%! has_full_name(?P : uniprot_protein, ?X) is nondet.
has_full_name(P,X) :- rdf(P,up:recommendedName,N), rdf(N,up:fullName,X).

% for genes only

%! pref_label(?E, ?N) is nondet.
pref_label(E,N) :- rdf(E,skos:prefLabel,N).


%! in_taxon(?P : uniprot_protein, ?T) is nondet.
in_taxon(P,T) :- rdf(P,up:organism,T).


%! annotation(?P : uniprot_protein, ?A : uniprot_annotation) is nondet.
annotation(P,A) :- rdf(P,up:annotation,A).

%! database(?X : uniprot_xref, ?D) is nondet.
database(X,D) :- rdf(X,up:database,D).

    
%! protein_annotation_range(?P : uniprot_protein, ?A, ?B, ?E, ?R) is nondet.
%
%  
%
protein_annotation_range(P,A,B,E,R) :-
        annotation(P,A),
        annotation_range(A,B,E,R).


%! annotation_range(?P : uniprot_annotation, ?B, ?E, ?R) is nondet.
annotation_range(P,B,E,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R),
        end_coord(I,E,R).

%! protein_begin(?P : uniprot_protein, ?B, ?R) is nondet.
protein_begin(P,B,R) :-
        rdf(P,up:range,I),
        begin_coord(I,B,R).



%! classified_with(P : uniprot_protein, T : uniprot_term) is nondet.
classified_with(P,T) :- rdf(P,up:classifiedWith,T).

classified_with_go(P,T) :- rdf(P,up:classifiedWith,T),str_starts(str(T),"http://purl.obolibrary.org/obo/GO_").

% slow/timeouts
classified_with_go_mf(P,T) :- rdf(P,up:classifiedWith,T),rdf(T,zeroOrMore(rdfs:subClassOf),'GO':'0003674').

% move to other module? bit of a hack
classified_with_go_catalysis(P,T) :- rdf(P,up:classifiedWith,T),rdf(T,'http://purl.obolibrary.org/obo/IAO_0000115',Def),str_starts(Def,'Catalysis of the reaction:').


% R is a faldo range
alt_sequence_annotation(PF,A) :-
        rdf(PF,up:modification,A),
        rdf(A,rdf:type,up:'Alternative_Sequence_Annotation').
isoform_region(PF,R) :-
        alt_sequence_annotation(PF,A),
        rdf(A,up:range,R).

        



%! substitution(?A : uniprot_annotation, ?S : uniprot_sequence_string) is nondet.
%
%  annotation A is associated with a substitution S
%
substitution(A,S) :- rdf(A,up:substitution,S).



%! xref(?P : uniprot_protein, ?X : uniprot_xref) is nondet.
%
%  P has xref X
%
xref(P,X) :- rdf(P,rdfs:seeAlso,X).



xref_in(P,X,DB) :- xref(P,X),database(X,updb:DB).


xref_intersection_count(X1,X2,N) :-
        aggregate(count(distinct(P)),(xref(P,X1),xref(P,X2)),N).
xref_union_count(X1,X2,N) :-
        aggregate(count(distinct(P)),(xref(P,X1);xref(P,X2)),N).






%! in_human(?P : uniprot_protein) is nondet.
%
%  convenience predicate, true if P is a human protein
%
in_human(P) :- rdf(P,up:organism,uptaxon:'9606').


%! reviewed(?P : uniprot_protein) is nondet.
%
%  P is a protein with review status of true
%
reviewed(P) :- rdf(P,up:reviewed,true^^xsd:boolean).


rhea_count(R,Num) :-
        aggregate(count(P),protein_has_catalyzed_reaction(P,R),Num).


/*

  in_human(P),annotation(P,A),ann_begin(A,B,R),substitution(A,\"F\")

  
  */

xref_organism_database(P,X) :-
        xref(P,X),database(X,D), is_organism_database(D).
is_organism_database(X) :-
        is_organism_database(X,_).
is_organism_database(X,D) :-
        database(X,D),
        member(D,
               [
                updb:'HGNC',
                updb:'BioCyc',
                updb:'MGI',
                updb:'RGD',
                updb:'SGD',
                updb:'FlyBase',
                updb:'PomBase',
                updb:'dictyBase',
                updb:'TAIR',
                updb:'ZFIN',
                updb:'WormBase',
                updb:'Xenbase'
               ]).




is_ensembl(X) :- database(X,updb:'Ensembl').
is_ensemblbacteria(X) :- database(X,updb:'EnsemblBacteria').
is_ensemblfungi(X) :- database(X,updb:'EnsemblFungi').
is_ensemblplants(X) :- database(X,updb:'EnsemblPlants').
is_pdb(X) :- database(X,updb:'PDB').
is_abcd(X) :- database(X,updb:'ABCD').
is_allergome(X) :- database(X,updb:'Allergome').
is_antibodypedia(X) :- database(X,updb:'Antibodypedia').
is_arachnoserver(X) :- database(X,updb:'ArachnoServer').
is_araport(X) :- database(X,updb:'Araport').
is_bmrb(X) :- database(X,updb:'BMRB').
is_brenda(X) :- database(X,updb:'BRENDA').
is_bgee(X) :- database(X,updb:'Bgee').
is_bindingdb(X) :- database(X,updb:'BindingDB').
is_biocyc(X) :- database(X,updb:'BioCyc').
is_biogrid(X) :- database(X,updb:'BioGRID').
is_biogridorcs(X) :- database(X,updb:'BioGRID-ORCS').
is_biomuta(X) :- database(X,updb:'BioMuta').
is_cazy(X) :- database(X,updb:'CAZy').
is_ccds(X) :- database(X,updb:'CCDS').
is_cdd(X) :- database(X,updb:'CDD').
is_cgd(X) :- database(X,updb:'CGD').
is_clae(X) :- database(X,updb:'CLAE').
is_compluyeast2dpage(X) :- database(X,updb:'COMPLUYEAST-2DPAGE').
is_corum(X) :- database(X,updb:'CORUM').
is_cptac(X) :- database(X,updb:'CPTAC').
is_cptc(X) :- database(X,updb:'CPTC').
is_ctd(X) :- database(X,updb:'CTD').
is_carbonyldb(X) :- database(X,updb:'CarbonylDB').
is_chembl(X) :- database(X,updb:'ChEMBL').
is_chitars(X) :- database(X,updb:'ChiTaRS').
is_collectf(X) :- database(X,updb:'CollecTF').
is_complexportal(X) :- database(X,updb:'ComplexPortal').
is_conoserver(X) :- database(X,updb:'ConoServer').
is_depod(X) :- database(X,updb:'DEPOD').
is_dip(X) :- database(X,updb:'DIP').
is_dmdm(X) :- database(X,updb:'DMDM').
is_dnasu(X) :- database(X,updb:'DNASU').
is_dosaccobs2dpage(X) :- database(X,updb:'DOSAC-COBS-2DPAGE').
is_disgenet(X) :- database(X,updb:'DisGeNET').
is_disprot(X) :- database(X,updb:'DisProt').
is_drugbank(X) :- database(X,updb:'DrugBank').
is_drugcentral(X) :- database(X,updb:'DrugCentral').
is_elm(X) :- database(X,updb:'ELM').
is_embl(X) :- database(X,updb:'EMBL').
is_epd(X) :- database(X,updb:'EPD').
is_esther(X) :- database(X,updb:'ESTHER').
is_echobase(X) :- database(X,updb:'EchoBASE').
is_ensemblmetazoa(X) :- database(X,updb:'EnsemblMetazoa').
is_ensemblprotists(X) :- database(X,updb:'EnsemblProtists').
is_evolutionarytrace(X) :- database(X,updb:'EvolutionaryTrace').
is_expressionatlas(X) :- database(X,updb:'ExpressionAtlas').
is_flybase(X) :- database(X,updb:'FlyBase').
is_gene3d(X) :- database(X,updb:'Gene3D').
is_genecards(X) :- database(X,updb:'GeneCards').
is_genedb(X) :- database(X,updb:'GeneDB').
is_geneid(X) :- database(X,updb:'GeneID').
is_genereviews(X) :- database(X,updb:'GeneReviews').
is_genetree(X) :- database(X,updb:'GeneTree').
is_genewiki(X) :- database(X,updb:'GeneWiki').
is_genevisible(X) :- database(X,updb:'Genevisible').
is_genomernai(X) :- database(X,updb:'GenomeRNAi').
is_glyconnect(X) :- database(X,updb:'GlyConnect').
is_glygen(X) :- database(X,updb:'GlyGen').
is_gramene(X) :- database(X,updb:'Gramene').
is_guidetopharmacology(X) :- database(X,updb:'GuidetoPHARMACOLOGY').
is_hamap(X) :- database(X,updb:'HAMAP').
is_hgnc(X) :- database(X,updb:'HGNC').
is_hogenom(X) :- database(X,updb:'HOGENOM').
is_hpa(X) :- database(X,updb:'HPA').
is_ideal(X) :- database(X,updb:'IDEAL').
is_imgt_genedb(X) :- database(X,updb:'IMGT_GENE-DB').
is_inparanoid(X) :- database(X,updb:'InParanoid').
is_intact(X) :- database(X,updb:'IntAct').
is_interpro(X) :- database(X,updb:'InterPro').
is_kegg(X) :- database(X,updb:'KEGG').
is_legiolist(X) :- database(X,updb:'LegioList').
is_leproma(X) :- database(X,updb:'Leproma').
is_merops(X) :- database(X,updb:'MEROPS').
is_mgi(X) :- database(X,updb:'MGI').
is_mim(X) :- database(X,updb:'MIM').
is_mint(X) :- database(X,updb:'MINT').
is_maizegdb(X) :- database(X,updb:'MaizeGDB').
is_malacards(X) :- database(X,updb:'MalaCards').
is_massive(X) :- database(X,updb:'MassIVE').
is_maxqb(X) :- database(X,updb:'MaxQB').
is_metosite(X) :- database(X,updb:'MetOSite').
is_moondb(X) :- database(X,updb:'MoonDB').
is_moonprot(X) :- database(X,updb:'MoonProt').
is_niagads(X) :- database(X,updb:'NIAGADS').
is_ogp(X) :- database(X,updb:'OGP').
is_oma(X) :- database(X,updb:'OMA').
is_opentargets(X) :- database(X,updb:'OpenTargets').
is_orphanet(X) :- database(X,updb:'Orphanet').
is_orthodb(X) :- database(X,updb:'OrthoDB').
is_panther(X) :- database(X,updb:'PANTHER').
is_patric(X) :- database(X,updb:'PATRIC').
is_pcddb(X) :- database(X,updb:'PCDDB').
is_pdbsum(X) :- database(X,updb:'PDBsum').
is_phibase(X) :- database(X,updb:'PHI-base').
is_pir(X) :- database(X,updb:'PIR').
is_pirsf(X) :- database(X,updb:'PIRSF').
is_pride(X) :- database(X,updb:'PRIDE').
is_prints(X) :- database(X,updb:'PRINTS').
is_pro(X) :- database(X,updb:'PRO').
is_prosite(X) :- database(X,updb:'PROSITE').
is_pathwaycommons(X) :- database(X,updb:'PathwayCommons').
is_paxdb(X) :- database(X,updb:'PaxDb').
is_peptideatlas(X) :- database(X,updb:'PeptideAtlas').
is_peroxibase(X) :- database(X,updb:'PeroxiBase').
is_pfam(X) :- database(X,updb:'Pfam').
is_pharmgkb(X) :- database(X,updb:'PharmGKB').
is_pharos(X) :- database(X,updb:'Pharos').
is_phosphositeplus(X) :- database(X,updb:'PhosphoSitePlus').
is_phylomedb(X) :- database(X,updb:'PhylomeDB').
is_plantreactome(X) :- database(X,updb:'PlantReactome').
is_pombase(X) :- database(X,updb:'PomBase').
is_promex(X) :- database(X,updb:'ProMEX').
is_proteomicsdb(X) :- database(X,updb:'ProteomicsDB').
is_pseudocap(X) :- database(X,updb:'PseudoCAP').
is_rebase(X) :- database(X,updb:'REBASE').
is_reproduction2dpage(X) :- database(X,updb:'REPRODUCTION-2DPAGE').
is_rgd(X) :- database(X,updb:'RGD').
is_rnact(X) :- database(X,updb:'RNAct').
is_reactome(X) :- database(X,updb:'Reactome').
is_refseq(X) :- database(X,updb:'RefSeq').
is_sabiork(X) :- database(X,updb:'SABIO-RK').
is_sasbdb(X) :- database(X,updb:'SASBDB').
is_sfld(X) :- database(X,updb:'SFLD').
is_sgd(X) :- database(X,updb:'SGD').
is_signor(X) :- database(X,updb:'SIGNOR').
is_smart(X) :- database(X,updb:'SMART').
is_smr(X) :- database(X,updb:'SMR').
is_string(X) :- database(X,updb:'STRING').
is_supfam(X) :- database(X,updb:'SUPFAM').
is_swiss2dpage(X) :- database(X,updb:'SWISS-2DPAGE').
is_signalink(X) :- database(X,updb:'SignaLink').
is_swisslipids(X) :- database(X,updb:'SwissLipids').
is_swisspalm(X) :- database(X,updb:'SwissPalm').
is_tair(X) :- database(X,updb:'TAIR').
is_tcdb(X) :- database(X,updb:'TCDB').
is_tigrfams(X) :- database(X,updb:'TIGRFAMs').
is_topdownproteomics(X) :- database(X,updb:'TopDownProteomics').
is_treefam(X) :- database(X,updb:'TreeFam').
is_tuberculist(X) :- database(X,updb:'TubercuList').
is_ucd2dpage(X) :- database(X,updb:'UCD-2DPAGE').
is_ucsc(X) :- database(X,updb:'UCSC').
is_unilectin(X) :- database(X,updb:'UniLectin').
is_veupathdb(X) :- database(X,updb:'VEuPathDB').
is_vgnc(X) :- database(X,updb:'VGNC').
is_wbparasite(X) :- database(X,updb:'WBParaSite').
is_world2dpage(X) :- database(X,updb:'World-2DPAGE').
is_wormbase(X) :- database(X,updb:'WormBase').
is_xenbase(X) :- database(X,updb:'Xenbase').
is_zfin(X) :- database(X,updb:'ZFIN').
is_dbsnp(X) :- database(X,updb:'dbSNP').
is_dictybase(X) :- database(X,updb:'dictyBase').
is_eggnog(X) :- database(X,updb:'eggNOG').
is_euhcvdb(X) :- database(X,updb:'euHCVdb').
is_iptmnet(X) :- database(X,updb:'iPTMnet').
is_jpost(X) :- database(X,updb:'jPOST').
is_nextprot(X) :- database(X,updb:'neXtProt').
is_po(X) :- database(X,updb:'PO').
is_mesh(X) :- database(X,updb:'MeSH').
is_medgen(X) :- database(X,updb:'MedGen').
is_go(X) :- database(X,updb:'go').

%%%%

xref_ensembl(P,X) :- xref(P,X),database(X,updb:'Ensembl').
xref_ensemblbacteria(P,X) :- xref(P,X),database(X,updb:'EnsemblBacteria').
xref_ensemblfungi(P,X) :- xref(P,X),database(X,updb:'EnsemblFungi').
xref_ensemblplants(P,X) :- xref(P,X),database(X,updb:'EnsemblPlants').
xref_pdb(P,X) :- xref(P,X),database(X,updb:'PDB').
xref_abcd(P,X) :- xref(P,X),database(X,updb:'ABCD').
xref_allergome(P,X) :- xref(P,X),database(X,updb:'Allergome').
xref_antibodypedia(P,X) :- xref(P,X),database(X,updb:'Antibodypedia').
xref_arachnoserver(P,X) :- xref(P,X),database(X,updb:'ArachnoServer').
xref_araport(P,X) :- xref(P,X),database(X,updb:'Araport').
xref_bmrb(P,X) :- xref(P,X),database(X,updb:'BMRB').
xref_brenda(P,X) :- xref(P,X),database(X,updb:'BRENDA').
xref_bgee(P,X) :- xref(P,X),database(X,updb:'Bgee').
xref_bindingdb(P,X) :- xref(P,X),database(X,updb:'BindingDB').
xref_biocyc(P,X) :- xref(P,X),database(X,updb:'BioCyc').
xref_biogrid(P,X) :- xref(P,X),database(X,updb:'BioGRID').
xref_biogridorcs(P,X) :- xref(P,X),database(X,updb:'BioGRID-ORCS').
xref_biomuta(P,X) :- xref(P,X),database(X,updb:'BioMuta').
xref_cazy(P,X) :- xref(P,X),database(X,updb:'CAZy').
xref_ccds(P,X) :- xref(P,X),database(X,updb:'CCDS').
xref_cdd(P,X) :- xref(P,X),database(X,updb:'CDD').
xref_cgd(P,X) :- xref(P,X),database(X,updb:'CGD').
xref_clae(P,X) :- xref(P,X),database(X,updb:'CLAE').
xref_compluyeast2dpage(P,X) :- xref(P,X),database(X,updb:'COMPLUYEAST-2DPAGE').
xref_corum(P,X) :- xref(P,X),database(X,updb:'CORUM').
xref_cptac(P,X) :- xref(P,X),database(X,updb:'CPTAC').
xref_cptc(P,X) :- xref(P,X),database(X,updb:'CPTC').
xref_ctd(P,X) :- xref(P,X),database(X,updb:'CTD').
xref_carbonyldb(P,X) :- xref(P,X),database(X,updb:'CarbonylDB').
xref_chembl(P,X) :- xref(P,X),database(X,updb:'ChEMBL').
xref_chitars(P,X) :- xref(P,X),database(X,updb:'ChiTaRS').
xref_collectf(P,X) :- xref(P,X),database(X,updb:'CollecTF').
xref_complexportal(P,X) :- xref(P,X),database(X,updb:'ComplexPortal').
xref_conoserver(P,X) :- xref(P,X),database(X,updb:'ConoServer').
xref_depod(P,X) :- xref(P,X),database(X,updb:'DEPOD').
xref_dip(P,X) :- xref(P,X),database(X,updb:'DIP').
xref_dmdm(P,X) :- xref(P,X),database(X,updb:'DMDM').
xref_dnasu(P,X) :- xref(P,X),database(X,updb:'DNASU').
xref_dosaccobs2dpage(P,X) :- xref(P,X),database(X,updb:'DOSAC-COBS-2DPAGE').
xref_disgenet(P,X) :- xref(P,X),database(X,updb:'DisGeNET').
xref_disprot(P,X) :- xref(P,X),database(X,updb:'DisProt').
xref_drugbank(P,X) :- xref(P,X),database(X,updb:'DrugBank').
xref_drugcentral(P,X) :- xref(P,X),database(X,updb:'DrugCentral').
xref_elm(P,X) :- xref(P,X),database(X,updb:'ELM').
xref_embl(P,X) :- xref(P,X),database(X,updb:'EMBL').
xref_epd(P,X) :- xref(P,X),database(X,updb:'EPD').
xref_esther(P,X) :- xref(P,X),database(X,updb:'ESTHER').
xref_echobase(P,X) :- xref(P,X),database(X,updb:'EchoBASE').
xref_ensemblmetazoa(P,X) :- xref(P,X),database(X,updb:'EnsemblMetazoa').
xref_ensemblprotists(P,X) :- xref(P,X),database(X,updb:'EnsemblProtists').
xref_evolutionarytrace(P,X) :- xref(P,X),database(X,updb:'EvolutionaryTrace').
xref_expressionatlas(P,X) :- xref(P,X),database(X,updb:'ExpressionAtlas').
xref_flybase(P,X) :- xref(P,X),database(X,updb:'FlyBase').
xref_gene3d(P,X) :- xref(P,X),database(X,updb:'Gene3D').
xref_genecards(P,X) :- xref(P,X),database(X,updb:'GeneCards').
xref_genedb(P,X) :- xref(P,X),database(X,updb:'GeneDB').
xref_geneid(P,X) :- xref(P,X),database(X,updb:'GeneID').
xref_genereviews(P,X) :- xref(P,X),database(X,updb:'GeneReviews').
xref_genetree(P,X) :- xref(P,X),database(X,updb:'GeneTree').
xref_genewiki(P,X) :- xref(P,X),database(X,updb:'GeneWiki').
xref_genevisible(P,X) :- xref(P,X),database(X,updb:'Genevisible').
xref_genomernai(P,X) :- xref(P,X),database(X,updb:'GenomeRNAi').
xref_glyconnect(P,X) :- xref(P,X),database(X,updb:'GlyConnect').
xref_glygen(P,X) :- xref(P,X),database(X,updb:'GlyGen').
xref_gramene(P,X) :- xref(P,X),database(X,updb:'Gramene').
xref_guidetopharmacology(P,X) :- xref(P,X),database(X,updb:'GuidetoPHARMACOLOGY').
xref_hamap(P,X) :- xref(P,X),database(X,updb:'HAMAP').
xref_hgnc(P,X) :- xref(P,X),database(X,updb:'HGNC').
xref_hogenom(P,X) :- xref(P,X),database(X,updb:'HOGENOM').
xref_hpa(P,X) :- xref(P,X),database(X,updb:'HPA').
xref_ideal(P,X) :- xref(P,X),database(X,updb:'IDEAL').
xref_imgt_genedb(P,X) :- xref(P,X),database(X,updb:'IMGT_GENE-DB').
xref_inparanoid(P,X) :- xref(P,X),database(X,updb:'InParanoid').
xref_intact(P,X) :- xref(P,X),database(X,updb:'IntAct').
xref_interpro(P,X) :- xref(P,X),database(X,updb:'InterPro').
xref_kegg(P,X) :- xref(P,X),database(X,updb:'KEGG').
xref_legiolist(P,X) :- xref(P,X),database(X,updb:'LegioList').
xref_leproma(P,X) :- xref(P,X),database(X,updb:'Leproma').
xref_merops(P,X) :- xref(P,X),database(X,updb:'MEROPS').
xref_mgi(P,X) :- xref(P,X),database(X,updb:'MGI').
xref_mim(P,X) :- xref(P,X),database(X,updb:'MIM').
xref_mint(P,X) :- xref(P,X),database(X,updb:'MINT').
xref_maizegdb(P,X) :- xref(P,X),database(X,updb:'MaizeGDB').
xref_malacards(P,X) :- xref(P,X),database(X,updb:'MalaCards').
xref_massive(P,X) :- xref(P,X),database(X,updb:'MassIVE').
xref_maxqb(P,X) :- xref(P,X),database(X,updb:'MaxQB').
xref_metosite(P,X) :- xref(P,X),database(X,updb:'MetOSite').
xref_moondb(P,X) :- xref(P,X),database(X,updb:'MoonDB').
xref_moonprot(P,X) :- xref(P,X),database(X,updb:'MoonProt').
xref_niagads(P,X) :- xref(P,X),database(X,updb:'NIAGADS').
xref_ogp(P,X) :- xref(P,X),database(X,updb:'OGP').
xref_oma(P,X) :- xref(P,X),database(X,updb:'OMA').
xref_opentargets(P,X) :- xref(P,X),database(X,updb:'OpenTargets').
xref_orphanet(P,X) :- xref(P,X),database(X,updb:'Orphanet').
xref_orthodb(P,X) :- xref(P,X),database(X,updb:'OrthoDB').
xref_panther(P,X) :- xref(P,X),database(X,updb:'PANTHER').
xref_patric(P,X) :- xref(P,X),database(X,updb:'PATRIC').
xref_pcddb(P,X) :- xref(P,X),database(X,updb:'PCDDB').
xref_pdbsum(P,X) :- xref(P,X),database(X,updb:'PDBsum').
xref_phibase(P,X) :- xref(P,X),database(X,updb:'PHI-base').
xref_pir(P,X) :- xref(P,X),database(X,updb:'PIR').
xref_pirsf(P,X) :- xref(P,X),database(X,updb:'PIRSF').
xref_pride(P,X) :- xref(P,X),database(X,updb:'PRIDE').
xref_prints(P,X) :- xref(P,X),database(X,updb:'PRINTS').
xref_pro(P,X) :- xref(P,X),database(X,updb:'PRO').
xref_prosite(P,X) :- xref(P,X),database(X,updb:'PROSITE').
xref_pathwaycommons(P,X) :- xref(P,X),database(X,updb:'PathwayCommons').
xref_paxdb(P,X) :- xref(P,X),database(X,updb:'PaxDb').
xref_peptideatlas(P,X) :- xref(P,X),database(X,updb:'PeptideAtlas').
xref_peroxibase(P,X) :- xref(P,X),database(X,updb:'PeroxiBase').
xref_pfam(P,X) :- xref(P,X),database(X,updb:'Pfam').
xref_pharmgkb(P,X) :- xref(P,X),database(X,updb:'PharmGKB').
xref_pharos(P,X) :- xref(P,X),database(X,updb:'Pharos').
xref_phosphositeplus(P,X) :- xref(P,X),database(X,updb:'PhosphoSitePlus').
xref_phylomedb(P,X) :- xref(P,X),database(X,updb:'PhylomeDB').
xref_plantreactome(P,X) :- xref(P,X),database(X,updb:'PlantReactome').
xref_pombase(P,X) :- xref(P,X),database(X,updb:'PomBase').
xref_promex(P,X) :- xref(P,X),database(X,updb:'ProMEX').
xref_proteomicsdb(P,X) :- xref(P,X),database(X,updb:'ProteomicsDB').
xref_pseudocap(P,X) :- xref(P,X),database(X,updb:'PseudoCAP').
xref_rebase(P,X) :- xref(P,X),database(X,updb:'REBASE').
xref_reproduction2dpage(P,X) :- xref(P,X),database(X,updb:'REPRODUCTION-2DPAGE').
xref_rgd(P,X) :- xref(P,X),database(X,updb:'RGD').
xref_rnact(P,X) :- xref(P,X),database(X,updb:'RNAct').
xref_reactome(P,X) :- xref(P,X),database(X,updb:'Reactome').
xref_refseq(P,X) :- xref(P,X),database(X,updb:'RefSeq').
xref_sabiork(P,X) :- xref(P,X),database(X,updb:'SABIO-RK').
xref_sasbdb(P,X) :- xref(P,X),database(X,updb:'SASBDB').
xref_sfld(P,X) :- xref(P,X),database(X,updb:'SFLD').
xref_sgd(P,X) :- xref(P,X),database(X,updb:'SGD').
xref_signor(P,X) :- xref(P,X),database(X,updb:'SIGNOR').
xref_smart(P,X) :- xref(P,X),database(X,updb:'SMART').
xref_smr(P,X) :- xref(P,X),database(X,updb:'SMR').
xref_string(P,X) :- xref(P,X),database(X,updb:'STRING').
xref_supfam(P,X) :- xref(P,X),database(X,updb:'SUPFAM').
xref_swiss2dpage(P,X) :- xref(P,X),database(X,updb:'SWISS-2DPAGE').
xref_signalink(P,X) :- xref(P,X),database(X,updb:'SignaLink').
xref_swisslipids(P,X) :- xref(P,X),database(X,updb:'SwissLipids').
xref_swisspalm(P,X) :- xref(P,X),database(X,updb:'SwissPalm').
xref_tair(P,X) :- xref(P,X),database(X,updb:'TAIR').
xref_tcdb(P,X) :- xref(P,X),database(X,updb:'TCDB').
xref_tigrfams(P,X) :- xref(P,X),database(X,updb:'TIGRFAMs').
xref_topdownproteomics(P,X) :- xref(P,X),database(X,updb:'TopDownProteomics').
xref_treefam(P,X) :- xref(P,X),database(X,updb:'TreeFam').
xref_tuberculist(P,X) :- xref(P,X),database(X,updb:'TubercuList').
xref_ucd2dpage(P,X) :- xref(P,X),database(X,updb:'UCD-2DPAGE').
xref_ucsc(P,X) :- xref(P,X),database(X,updb:'UCSC').
xref_unilectin(P,X) :- xref(P,X),database(X,updb:'UniLectin').
xref_veupathdb(P,X) :- xref(P,X),database(X,updb:'VEuPathDB').
xref_vgnc(P,X) :- xref(P,X),database(X,updb:'VGNC').
xref_wbparasite(P,X) :- xref(P,X),database(X,updb:'WBParaSite').
xref_world2dpage(P,X) :- xref(P,X),database(X,updb:'World-2DPAGE').
xref_wormbase(P,X) :- xref(P,X),database(X,updb:'WormBase').
xref_xenbase(P,X) :- xref(P,X),database(X,updb:'Xenbase').
xref_zfin(P,X) :- xref(P,X),database(X,updb:'ZFIN').
xref_dbsnp(P,X) :- xref(P,X),database(X,updb:'dbSNP').
xref_dictybase(P,X) :- xref(P,X),database(X,updb:'dictyBase').
xref_eggnog(P,X) :- xref(P,X),database(X,updb:'eggNOG').
xref_euhcvdb(P,X) :- xref(P,X),database(X,updb:'euHCVdb').
xref_iptmnet(P,X) :- xref(P,X),database(X,updb:'iPTMnet').
xref_jpost(P,X) :- xref(P,X),database(X,updb:'jPOST').
xref_nextprot(P,X) :- xref(P,X),database(X,updb:'neXtProt').
xref_po(P,X) :- xref(P,X),database(X,updb:'PO').
xref_mesh(P,X) :- xref(P,X),database(X,updb:'MeSH').
xref_medgen(P,X) :- xref(P,X),database(X,updb:'MedGen').
xref_go(P,X) :- xref(P,X),database(X,updb:'go').


