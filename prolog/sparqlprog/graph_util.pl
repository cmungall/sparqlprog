:- module(graph_util,
          [
           extract_subgraph/3,
           
           edges_to_dict/2,
           edges_to_dotfile/2,

           edges_to_imagefile/2,
           edges_to_imagefile/3
          ]).

:- use_module(library(http/json)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/emulate_builtins)).

my_maplist(L1,P,L2) :-
        G =.. [P,In,Out],
        maplist([In,Out]>>G, L1, L2).

%! extract_subgraph(+Nodes:list, ?Edges:list, +Opts:list) is det
%
%   extracts a subgraph starting from seed nodes in Nodes
%
% Options:
%
%   - direction: up (default) or down
%
extract_subgraph(Nodes, Edges, Opts) :-
        extract_subgraph(Nodes, Edges, [], Opts).

extract_subgraph([], [], _, _).
extract_subgraph([N|Nodes], Edges, Visited, Opts) :-
        memberchk(N,Visited),
        !,
        extract_subgraph(Nodes, Edges, Visited, Opts).
extract_subgraph([N|Nodes], Edges, Visited, Opts) :-
        extend_node(N,Edges1,Opts),
        collect_nodes(Edges1,Nodes1),
        append(Nodes1,Nodes,Nodes2),
        extract_subgraph(Nodes2, Edges2, [N|Visited], Opts),
        append(Edges1, Edges2, Edges).

extend_node(N,Edges,Opts) :-
        memberchk(direction(down),Opts),
        !,
        findall(edge(S,N,P,[graph=G]),
                owl_edge(S,P,N,G),
                Edges).
extend_node(N,Edges,Opts) :-
        findall(edge(N,O,P,[graph=G]),
                owl_edge(N,P,O,G),
                Edges).

        

shorten(URI,Id) :-  rdf_global_id(Pre:Local, URI),concat_atom([Pre,Local],:,Id),!.
shorten(X,X).



        

edges_to_dict(Edges, doc{graphs:[G]}) :-
        collect_nodes(Edges,Nodes),
        my_maplist(Nodes,node_obj,NodeObjs),
        my_maplist(Edges,edge_obj,EdgeObjs),
        G = nodelist{nodes: NodeObjs, edges: EdgeObjs}.

edges_to_dotfile(Edges,DF) :-
        edges_to_dotfile(Edges,DF,[]).
edges_to_dotfile(Edges,DF,Opts) :-
        edges_to_dict(Edges,D),
        tmp_file(obograph,GF),
        open(GF,write,Stream,[]),
        json_write_dict(Stream,D),
        close(Stream),
        findall(A,
                (   member(Opt=Val,Opts),
                    sformat(A,'~w ~w',[Opt,Val])),
                OptsAtoms),
        concat_atom(OptsAtoms,OptsAtom),
        sformat(Cmd,'og2dot.js ~w -o ~w ~w',[OptsAtom,DF,GF]),
        shell(Cmd).

edges_to_imagefile(Edges,F) :-
        edges_to_imagefile(Edges,F, png).
edges_to_imagefile(Edges,F, Fmt) :-
        edges_to_dotfile(Edges,F,['-t'=Fmt]).
        

node_obj(N, Obj) :-
        findall(A,node_attr(N,A),Attrs),
        (   Attrs=[]
        ->  true
        ;   true),
        shorten(N,Nx),
        Obj = node{id:Nx}.put(Attrs).

:- rdf_meta node_attr(r,-).
node_attr(rdfs:subClassOf, lbl='is_a').
node_attr(N, lbl=Label) :-
        rdf(N, rdfs:label, Label1),
        eval_to_atom(str(Label1),Label).


        

edge_obj(edge(S,O,P), Dict) :- edge_obj(edge(S,O,P,[]), Dict).
edge_obj(edge(S,O,P,Attrs), Dict) :-
        shorten(S,Sx),
        shorten(O,Ox),
        shorten(P,Px),
        Dict1 = edge{sub:Sx, obj:Ox, pred:Px},
        (   Attrs = []
        ->  Dict = Dict1
        ;   Meta=meta{}.put(Attrs),
            Dict = Dict1.put([meta=Meta])).


edge_node(edge(S,O,P,_),N) :- edge_node(edge(S,O,P),N).
edge_node(edge(X,_,_),X).
edge_node(edge(_,X,_),X).
edge_node(edge(_,_,X),X).


collect_nodes(Edges,Nodes) :-
        setof(N,Edge^(member(Edge,Edges),edge_node(Edge,N)),Nodes),
        !.
collect_nodes(_,[]).

