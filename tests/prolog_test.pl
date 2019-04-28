/**

  tests direct execution on prolog in-memory triples

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl)).

% run docker
%:- sparql_endpoint( local, 'http://127.0.0.1:8889/bigdata/sparql').

:- begin_tests(prolog_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        % load into a test-specific graph due to cache issue
        rdf_load('tests/go_nucleus.ttl',[cache(false),
                                         graph(prolog)]).


run_test_query(N,X,G,L,L1) :-
        setof(X,G,L1),
        format('** ~w~n',[N]),
        wl(L1),
        assertion(L = L1).

test(direct_subclass_of) :-
        label(C,"nucleus"),
        setof(D,subClassOf(C,D),Ds),
        assertion(Ds = [Parent]),
        assertion(label(Parent, "intracellular membrane-bounded organelle")).

cls_label_ends_with(C,M) :-
        class(C),label(C,Label),str_ends(Label,M).
cls_label_starts_with(C,M) :-
        class(C),label(C,Label),str_starts(Label,M).

test(str_starts) :-
        setof(C,cls_label_starts_with(C,"cell"),Cs),
        assertion(Cs = [_,_,_]),
        assertion( (member(C,Cs),label(C,"cell")) ),
        assertion( (member(C,Cs),label(C,"cell part")) ).

test(str_ends) :-
        setof(C,cls_label_ends_with(C,"organelle"),Cs),
        assertion(Cs = [_,_,_,_]),
        assertion( (member(C,Cs),label(C,"organelle")) ),
        assertion( (member(C,Cs),label(C,"intracellular membrane-bounded organelle")) ).

has_dbxref(I,J):-rdf(I,'http://www.geneontology.org/formats/oboInOwl#hasDbXref',J).

has_dbxref_with_prefix(C,X,P) :- has_dbxref(C,X),str_before(X,":",P).
has_dbxref_with_prefix(C,P) :- has_dbxref_with_prefix(C,_,P).

test(str_before) :-
        % test with argument bound
        setof(C,has_dbxref_with_prefix(C,"Wikipedia"),Cs),
        assertion(Cs = [_,_,_,_]),
        member(C,Cs),
        label(C,"organelle"),
        % test with argument not-bound
        setof(P,has_dbxref_with_prefix(C,P),Ps),
        assertion( Ps = ["NIF_Subcellular","Wikipedia"] ).

wl(L) :-
        forall((member(X,L),optional(label(X,N))),
               format('~w ~w~n', [X,N])).


%test(z) :-
%        label(C,"intracellular"),
%        run_test_query(foo,D,rdf(_,_,_),_,_).


test(path) :-
        label(C,"intracellular"),
        run_test_query(onePlus,D,rdf_path(C,oneOrMore(rdfs:subClassOf),D),[_,_,_],_),
        run_test_query(zeroPlus,D,rdf_path(C,zeroOrMore(rdfs:subClassOf),D),[_,_,_,_],Lz),
        assertion(member(C,Lz)),
        label(C2,"intracellular part"),
        run_test_query(inv,D,rdf_path(C2,inverseOf(rdfs:subClassOf),D),[_],_),
        run_test_query(or,D,rdf_path(C2,(rdfs:subClassOf|inverseOf(rdfs:subClassOf)),D),[_,_,_],_),
        run_test_query(all,D,rdf_path(C2,zeroOrMore((rdfs:subClassOf)|(\(rdfs:subClassOf))),D),[_,_,_,_],_),
        true.


:- end_tests(prolog_test).


