/**

  tests direct execution on owl_util in-memory triples

*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(sparqlprog/ontologies/owl)).

:- rdf_register_prefix(oboInOwl,'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').
:- rdf_register_prefix(obo,'http://purl.obolibrary.org/obo/').
:- rdf_register_prefix(part_of,'http://purl.obolibrary.org/obo/BFO_0000050').
foo('0').

:- begin_tests(owl_util_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        % load into a test-specific graph due to cache issue
        rdf_load('tests/go_nucleus.ttl',[cache(false),
                                         graph(owl_util)]).


run_test_query(N,X,G,L,L1) :-
        setof(X,G,L1),
        format('** ~w~n',[N]),
        wl(L1),
        assertion(L = L1).

wl(L) :-
        forall((member(X,L),term_labelify(X,L1)),
               writeln(L1)).

test(curie) :-
        URI='http://purl.obolibrary.org/obo/BFO_123',
        setof(Pre:Post,rdf_global_id(Pre:Post,URI),L),
        assertion( L=['BFO':'123'] ),
        ensure_curie(URI, CURIE),
        assertion( CURIE = 'BFO:123' ),
        rdf_global_id(part_of:'',PART_OF),
        ensure_curie(PART_OF, CURIE2),
        assertion( CURIE2 = 'part_of' ).

test(prefix) :-
        G=subsumed_prefix_namespace(_,_,_,_),
        setof(G,G,Gs),
        assertion(Gs=[_,_,_]),
        assertion( member(subsumed_prefix_namespace(obo,_,'BFO',_),Gs) ).





test(owl_edge) :-
        rdf_global_id(part_of:'',PART_OF),
        label_of('intracellular part',INTRACELLULAR_PART),
        label_of('intracellular',INTRACELLULAR),
        label_of('cell part',CELL_PART),
        label_of('cell',CELL),
        rdf_global_id(oboInOwl:hasExactSynonym,SYN),
        run_test_query(edge,
                       part_of(S,O),
                       owl_edge(S,PART_OF,O),[part_of(INTRACELLULAR_PART,INTRACELLULAR),
                                              part_of(CELL_PART,CELL)],_),
        run_test_query(axiom_annotations,
                       a(P,V),
                       triple_axiom_annotation(CELL_PART,SYN,_,P,V),
                       [a('http://www.geneontology.org/formats/oboInOwl#hasDbXref',_)],
                       _).



:- end_tests(owl_util_test).


