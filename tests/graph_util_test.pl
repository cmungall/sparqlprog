
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/graph_util)).
:- use_module(library(sparqlprog/search_util)).

:- use_module(library(http/json)).

:- rdf_register_prefix('GO','http://purl.obolibrary.org/obo/GO_').
:- rdf_register_prefix('BFO','http://purl.obolibrary.org/obo/BFO_').


:- begin_tests(graph_util_test,
               [setup(load_test_file),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file :-
        rdf_load('examples/data/goslim_generic.ttl.gz').

test(basic) :-
        Es=[edge(a,b,is_a), edge(b,c,is_a)],
        edges_to_dict(Es,Dict),
        nl,
        json_write_dict(current_output,Dict),
        edges_to_dotfile(Es,'/tmp/basic.dot'),
        !.

test(subgraph) :-
        lmatch('cell',C1),
        %lmatch('golgi apparatus',C2),
        extract_subgraph([C1],Edges,[direction(down)]),
        edges_to_dict(Edges,Dict),
        json_write_dict(current_output,Dict),
        edges_to_dotfile(Edges,'/tmp/subgraph.dot'),
        edges_to_imagefile(Edges,'/tmp/subgraph.png', png),
        !.


        


:- end_tests(graph_util_test).


