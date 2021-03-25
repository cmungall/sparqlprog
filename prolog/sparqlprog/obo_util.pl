:- module(obo_util,
          [gen_obo/2,
           gen_stanza/3,
           gen_header/3,

           entity_def_xrefs/3
          ]).

:- use_module(library(obo_metadata/oio)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/obo)).
:- use_module(library(sparqlprog/emulate_builtins)).

entity_obotype(E,T) :- entity_obotype(E,T,_).
entity_obotype(E,'Term',G) :- rdf(E,rdf:type,owl:'Class',G), rdf_is_iri(E).
entity_obotype(E,'Typedef',G) :- rdf(E,rdf:type,owl:'ObjectProperty',G).

ensure_curie_wrap(U,X) :-
        ensure_curie(U,X1),
        (   atom_concat('http://purl.obolibrary.org/obo/',Frag,X1),
            concat_atom([Pre,Post],'_',Frag)
        ->  concat_atom([Pre,Post],':',X)
        ;   X=X1).


        
ensure_id(U,Id) :-
        ensure_curie_wrap(U,X),
        (   atom_concat(Id,':',X)
        ->  true
        ;   Id=X).


entity_def_xrefs(E,D,Xs) :-
        triple_property_axiom_annotations(E,def:'',D1,oio:hasDbXref,Xs1),
        ensure_atom(D1,D),
        ensure_atoms(Xs1,Xs).


syntype_uri_to_id(V1,V) :-
        concat_atom([_,V],'#',V1),
        !.
syntype_uri_to_id(V,V).

entity_xref_xrefs(E,X,QVs) :-
        triple_axiom_annotations(E,oio:hasDbXref,X1,QVs),
        ensure_atom(X1,X).


entity_xref_prefix_srcont(C,X,P,S) :-
        entity_xref_prefix(C,X,P),
        entity_xref_src(C,X,SC),
        curie_prefix(SC,S).

entity_synonym_scope_type_xrefs(E,V,Scope,Type,Xrefs) :-
        synprop_scope(P,Scope),
        triple_axiom_annotations(E,P,V1,Anns),
        ensure_atom(V1,V),
        convlist(['http://www.geneontology.org/formats/oboInOwl#hasDbXref'-Y1,
                  Y]>>ensure_atom(Y1,Y),
                 Anns,Xrefs),
        (   member('http://www.geneontology.org/formats/oboInOwl#hasSynonymType'-Type1,Anns)
        ->  syntype_uri_to_id(Type1,Type)
        ;   Type='').

subset_uri_to_id(V1,V) :-
        % envo has some subset tags as strings
        rdf_is_literal(V1),
        !,
        ensure_atom(V1,V).
subset_uri_to_id(V1,V) :-
        concat_atom([_,V],'#',V1),
        !.
subset_uri_to_id(V,V).


entity_subset_id(E,V) :-
        rdf(E,oio:inSubset,V1),
        subset_uri_to_id(V1,V).

        

%!  curie_prefix(Literal:str, Pre:str)
%curie_prefix(Literal,Pre) :-
%        str_before(Literal,":",Pre).

entity_nameatom(E,N) :-
        label(E,N1),
        ensure_atom(N1,N).


gen_obo(S,Opts) :-
        gen_obo(S,_,Opts).

gen_obo(S,G,Opts) :-
        forall(gen_header(S,G,Opts),true),
        format(S,'~n',[]),
        setof(E,T^entity_obotype(E,T,G),Es),
        forall((member(E,Es),\+is_dangling(E)),
               gen_stanza(S,E,G,Opts)).



gen_header(S,G,Opts) :-
        forall(gen_header1(S,G,Opts),true).

gen_header1(S,G,_) :-
        rdf_graph(G),
        ensure_id(G,Id),
        format(S,'ontology: ~w~n',Id).
gen_header1(S,_G,_) :-
        rdf(V1,rdfs:subPropertyOf,oio:'SubsetProperty'),
        subset_uri_to_id(V1,V),
        format(S,'subsetdef: ~w "~w"~n',[V,V]).

gen_stanza(S,E,Opts) :-
        gen_stanza(S,E,_G,Opts).

gen_stanza(S,E,G,Opts) :-
        entity_obotype(E,T),
        ensure_id(E,Id),
        format(S,'[~w]~n',[T]),
        format(S,'id: ~w~n',[Id]),
        forall(gen_tag(S,E,G,Opts),true),
        format(S,'~n',[]),
        !.
gen_stanza(_S,E,G,_Opts) :-
        format(user_error,'Cannot write: ~w ~w~n',[E,G]).



gen_tag(S,E,_,_) :-
        entity_nameatom(E,N),
        format(S,'name: ~w~n',[N]).
gen_tag(S,E,_,_) :-
        entity_def_xrefs(E,N,Xrefs),
        escq(N,N1),
        serialize_xrefs(Xrefs,X),
        format(S,'def: "~w" ~w~n',[N1,X]).

gen_tag(S,E,_,_) :-
        entity_subset_id(E,X),
        format(S,'subset: ~w~n',[X]).

gen_tag(S,E,_,_) :-
        entity_synonym_scope_type_xrefs(E,V1,Scope,Type,Xrefs),
        serialize_xrefs(Xrefs,X),
        escq(V1,V),
        format(S,'synonym: "~w" ~w ~w ~w~n',[V,Scope,Type,X]).

gen_tag(S,E,_,_) :-
        entity_xref_xrefs(E,X,_PVs),
        format(S,'xref: ~w~n',[X]).

gen_tag(S,E,_,_) :-
        is_a(E,X),
        tv(S,is_a,[X]).

gen_tag(S,E,_,_) :-
        relationship(E,R,O),
        tv(S,relationship,[R,O]).

gen_tag(S,E,_,_) :-
        class_genus(E,G),
        \+ \+ class_differentia(E,_,_),
        tv(S,intersection_of,[G]),
        forall(class_differentia(E,P,Y),
               tv(S,intersection_of,[P,Y])).

gen_tag(S,E,_,_) :-
        rdf(E,rdf:type,owl:'TransitiveProperty'),
        tv(S,is_transitive,[true]).

tv(S,T,Vs) :-
        format(S,'~w:',[T]),
        forall((member(V,Vs),ensure_id(V,Id)),
               format(S,' ~w',[Id])),
        findall(N,(member(V,Vs),entity_nameatom(V,N)),
                Ns),
        (   Ns=[]
        ->  true
        ;   format(S,' !',[]),
            forall(member(N,Ns),
                   format(S,' ~w',[N]))),
        nl(S).


serialize_xrefs(Xs,A) :-
        concat_atom(Xs,', ',A1),
        concat_atom(['[',A1,']'],A).



escq(A,C) :-
        concat_atom(Xs,'"',A),
        concat_atom(Xs,'\\"',B),
        concat_atom(Xs2,'\n',B),
        concat_atom(Xs2,' ',C).
