:- module(dataframe,
          [dataframe_header/2,
           dataframe_header/3,
           dataframe_row/2,
           dataframe_row/3,
           dataframe_row/4,

           dataframe_to_csv/2,
           dataframe_to_csv/3]).

:- use_module(library(semweb/rdf11)).

/**

  Example

  dataframe(foo,
             [id=ID,
              name=Name]-person(ID,Name),
             [street=Street,
              city=City]-address(ID,Street,City),
             ...],
            [description('person and location report'),
             entity(city)]).

  TODO

  frame('person location report') <<
    foo(id=ID :: entity, name=Name) where person(ID,Name),
    foo(street=Street, city=City :: entity) where address(ID,Street,City).


*/

:- multifile dataframe/2.
:- multifile dataframe/3.

dataframe_specs_opts(Name,Specs,Opts) :- dataframe(Name,Specs,Opts).
dataframe_specs_opts(Name,Specs,[]) :- dataframe(Name,Specs).

:- module_transparent(dataframe_header/2).
:- module_transparent(dataframe_header/3).
dataframe_header(Name,Header) :-
        dataframe_header(Name,Header,[]).
dataframe_header(Name,Header,Opts) :-
        dataframe_specs_opts(Name,Specs,SpecOpts),
        findall(K,
                (   member(Sub-_,Specs),
                    member(K=_,Sub)),
                Header1),
        inject_labels_to_header(Header1,Header,SpecOpts,Opts).

inject_labels_to_header([],[],_,_).
inject_labels_to_header([K|L],[K,KN|L2],SpecOpts,Opts) :-
        member(entity(K),SpecOpts),
        !,
        atom_concat(K,' label',KN),
        inject_labels_to_header(L,L2,SpecOpts,Opts).
inject_labels_to_header([K|L],[K|L2],SpecOpts,Opts) :-
        inject_labels_to_header(L,L2,SpecOpts,Opts).



:- module_transparent(dataframe_row/2).
:- module_transparent(dataframe_row/3).
:- module_transparent(dataframe_row/4).
dataframe_row(Name,Row) :-
        dataframe_row(Name,Row,[]).
dataframe_row(Name,Row,Opts) :-
        dataframe_row(Name,Row,Opts,[]).
dataframe_row(Name,Row,Opts,SpecOpts) :-
        dataframe_specs_opts(Name,[Spec|Specs],SpecOpts),
        spec_bindings_goal(Spec,Bs,G),
        dataframe_header(Name,Header,Opts),
        call_wrap(G,SpecOpts),
        maplist([_=V,V]>>true,Bs,Row1),
        apply_specs(Specs,Row2),
        append(Row1,Row2,Row3),
        flatten_row(Row3,Row,Header,[Spec|Specs],SpecOpts,Opts).

:- module_transparent(call_wrap/2).
call_wrap(G,Opts) :-
        member(endpoint(Endpoint),Opts),
        !,
        (   Endpoint ?? G).
call_wrap(G,_) :-
        G.

:- module_transparent(apply_specs/2).
apply_specs([],[]).
apply_specs([Spec|Specs],Row) :-
        spec_bindings_goal(Spec,Bs,G),
        (   setof(Bs,G,BsSet)
        ->  true
        ;   BsSet=[]),
        maplist({BsSet}/[Var=_,Vals] >> findall(X,(member(Bs1,BsSet),member(Var=X,Bs1)),Vals),Bs,Row1),
        append(Row1,Row2,Row),
        apply_specs(Specs,Row2).

:- module_transparent(spec_bindings_goal/3).
spec_bindings_goal(Bs-G,Bs,G).



flatten_row([],[],_,_,_,_).
flatten_row([V|Row],Row2,Keys,Specs,SpecOpts,Opts) :-
        Keys=[K|_],
        select(entity(K),SpecOpts,SpecOpts2),
        !,
        get_labels(V,Ns,Opts),
        contract_uris(V,V2,Opts),
        flatten_row([V2,Ns|Row],Row2,Keys,Specs,SpecOpts2,Opts).
flatten_row([V|Row],[V2|Row2],Keys,Specs,SpecOpts,Opts) :-
        Keys=[K|Keys2],
        select(iri(K),SpecOpts,SpecOpts2),
        !,
        contract_uris(V,V1,Opts),
        serialize_value(V1,V2,Opts),
        flatten_row(Row,Row2,Keys2,Specs,SpecOpts2,Opts).
flatten_row([V|Row],[V2|Row2],[_K|Keys],Specs,SpecOpts,Opts) :-
        serialize_value(V,V2,Opts),
        flatten_row(Row,Row2,Keys,Specs,SpecOpts,Opts).

% todo: hook
get_labels(L,Vs,Opts) :-
        is_list(L),
        !,
        maplist({Opts}/[X,X2]>>get_labels(X,X2,Opts),L,L2),
        flatten(L2,Vs).
get_labels(X,V,_Opts) :-
        rdf_is_iri(X),
        rdf(X,rdfs:label,V),
        !.
get_labels(_,'',_).

contract_uris(L,L2,Opts) :- is_list(L),!,maplist({Opts}/[A,B]>>contract_uri(A,B,Opts),L,L2).
contract_uris(A,B,Opts) :- contract_uri(A,B,Opts).
contract_uri(A,B,_) :- rdf_is_iri(A),rdf_global_id(Pre:Local,A),!,concat_atom([Pre,Local],':',B).
contract_uri(A,A,_).


serialize_value(V,'_',_) :-
        var(V),
        !.
serialize_value(L,V,Opts) :-
        is_list(L),
        !,
        maplist({Opts}/[X,X2]>>serialize_value(X,X2,Opts),L,L2),
        option(internal_sep(Sep),Opts,'|'),
        concat_atom(L2,Sep,V).
serialize_value(S@_,V,_) :-
        !,
        atom_string(V,S).
serialize_value(S^^_,V,_) :-
        !,
        atom_string(V,S).
serialize_value(literal(type(_,V)),V,_) :-
        !.
serialize_value(literal(lang(_,V)),V,_) :-
        !.
serialize_value(literal(V),V,_) :-
        !.
serialize_value(S,V,_) :-
        string(S),
        !,
        atom_string(V,S).
serialize_value(X,V,_) :-
        sformat(S,'~w',[X]),
        string_to_atom(S,V).


:- module_transparent(dataframe_to_csv/2).
:- module_transparent(dataframe_to_csv/3).
dataframe_to_csv(Name, Opts) :-
        Stream = current_output,
        dataframe_to_csv(Name, Stream, Opts).
dataframe_to_csv(Name, Stream, Opts) :-
        dataframe_header(Name, Header, Opts),
        option(separator(Sep), Opts, 0'\t),
        HTerm =.. [row|Header],
        csv_write_stream(Stream, [HTerm], [separator(Sep)]),
        dataframe_specs_opts(Name,_Specs,SpecOpts),
        (   member(sort(_),SpecOpts)
        ->  findall(Row,dataframe_row(Name, Row, Opts, SpecOpts),Rows),
            % TODO sort by key
            sort(Rows,RowsSorted),
            forall((member(Row,RowsSorted), RTerm =.. [row|Row]),
                   csv_write_stream(Stream, [RTerm], [separator(Sep)]))
        ;   forall((dataframe_row(Name, Row, Opts, SpecOpts), RTerm =.. [row|Row]),
                   csv_write_stream(Stream, [RTerm], [separator(Sep)]))),
        !.
dataframe_to_csv(Name, Stream, Opts) :-
        throw(exception(no_dataframe(Name, Stream, Opts))).


               
               
        

        
