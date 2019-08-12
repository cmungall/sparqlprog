/** <module> chembl

  See https://www.ebi.ac.uk/rdf/documentation/chembl/
*/
:- module(chembl,
          [
           molecule/1,
           target/1,
           source/1,
           activity/1,
           uniprotref/1,

           class_level/2,
           has_target_descendant/2,
           has_molecule/2,
           has_assay/2,
           has_target_component/2,
           target_cmpt_xref/2
           
           ]).

:- use_module(ebi).

:- rdf_register_prefix(cco, 'http://rdf.ebi.ac.uk/terms/chembl#').
:- rdf_register_prefix(chembl_molecule, 'http://rdf.ebi.ac.uk/resource/chembl/molecule/').
:- rdf_register_prefix(chembl_source, 'http://rdf.ebi.ac.uk/resource/chembl/source/').
:- rdf_register_prefix(chembl_target, 'http://rdf.ebi.ac.uk/resource/chembl/target/').
:- rdf_register_prefix(chembl_protclass, 'http://rdf.ebi.ac.uk/resource/chembl/protclass/').
:- rdf_register_prefix(chembl_assay, 'http://rdf.ebi.ac.uk/resource/chembl/assay/').

molecule(X) :- rdfs_individual_of(X,cco:'Substance').
target(X) :- rdfs_individual_of(X,cco:'Target').
source(X) :- rdfs_individual_of(X,cco:'Source').
activity(X) :- rdfs_individual_of(X,cco:'Activity').

uniprotref(X) :- rdf(X,rdf:type,cco:'UniprotRef').

class_level(P,L) :- rdf(P,cco:classLevel,L).
has_target_descendant(C,P) :- rdf(C,cco:hasTargetDescendant,P).
has_molecule(A,M) :- rdf(A,cco:hasMolecule,M).
has_assay(A,T) :- rdf(A,cco:hasAssay,T).
has_target_component(T,C) :- rdf(T,cco:hasTargetComponent,C).
target_cmpt_xref(C,X) :- rdf(C,cco:targetCmptXref,X).

has_child_molecule(P,C) :- rdf(P,cco:hasChildMolecule,C).
has_parent_molecule(C,P) :- rdf(C,cco:hasParentMolecule,P).
