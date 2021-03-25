:- module(owl_search_viz,
          [
           searchviz/1,
           searchviz/2,

           owl_search_and_display/2
           %owl_search_and_display/6,
           %owl_search_and_display/7
           ]).

/** <module> search and visualize results
  
  Convenience wrapper that combines search_util and owl_edge/4 from owl_util

  The main predicate is owl_search_and_display/2


  
  requires og2dot - https://www.npmjs.com/package/obographviz
*/

:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/search_util)).
:- use_module(library(sparqlprog/emulate_builtins)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(http/json)).
:- use_module(library(regex)).


%% searchviz(+Term, +Preds) is semidet
%% searchviz(+Term) is semidet
%
% performs search on Term using lsearch/2 and 
% draws results
searchviz(Term, Preds) :-
        setof(Obj,lsearch(Term,Obj),Objs),
        owl_subgraph(Objs, Preds, Quads, []),
        quads_dict(Quads, Dict),
        write_json_tmp(Dict, OgFile),
                                %write_json_tmp(stylemap{highlightIds: Objs}, StyleFile),
        atom_json_term(Style,stylemap{highlightIds: Objs}, []),
        style_file_args(StyleFileArgs),
        sformat(Cmd,'og2dot.js ~w -S \'~w\' -t png ~w',[StyleFileArgs,Style, OgFile]),
        shell(Cmd).

searchviz(Term) :-
        searchviz(Term, _).

% Default locations for style files
style_file('obograph-style.json').
style_file('style.json').
style_file('conf/obograph-style.json').
style_file('conf/style.json').
style_file('~/.obograph-style.json').

style_file_args(A) :-
        style_file(File),
        expand_file_name(File,Files),
        member(F1,Files),
        exists_file(F1),
        sformat(A,'-s ~w',[F1]),
        !.
style_file_args('').



write_json_tmp(Dict,File) :-
        atom_json_dict(JsonAtom, Dict, []),
        tmp_file(foo, File),
        open(File,write,IO,[]),
        format(IO,'~w',[JsonAtom]),
        debug(viz, 'j=~w', [JsonAtom]),
        close(IO).

owl_search_and_display([SearchTerm], Opts) :-
        concat_atom([Pred,ST2],'=',SearchTerm),
        % e.g. id=GO:123456
        !,
        owl_search_and_display([ST2], [search_property(Pred)|Opts]).

%! owl_search_and_display(+SearchTerms:list, +Opts:list) is semidet.
%
%    perform a search over all SearchTerms and display
%
%    SearchTerms:
%
%    Each term in the list is an atom that is a regex used to search.
%    Search is determined by search_property. By default this is `label`, to
%    search using rdfs:label
%
%    In place of a regex, a prolog query term can be used, e.g
%    'q//label_of(shape,R),rdfs_subclass_of(Q,R)' to get all subclasses of 'shape'
%
%    Opts:
%
%     - search_property(Prop)
%       one of: label, id, synonym, all
%       only the first letter needs to be specified (l, i, s, a)
%       default: label
%     - relations(Rels)
%       list of object properties
%       `s` is a shorthand for rdfs:subClassOf
%     - output(File)
%     - format(Fmt)
%       one of: disp, obo, viz, png, dot, ids
%     - extend_lambda(File)
%
%  Examples:
%   - `owl_search_and_display([nucleus],[])` show any terms text matching '.*nucleus.*'
%   - `owl_search_and_display(['^nucleus$'],[relations([s])])` exact match, show superclasses
%   - `owl_search_and_display(['GO:0005634'],[search_property(id)])` search by ID
%
owl_search_and_display(SearchTerms, Opts) :-
        debug(search, 'Opts  = ~q',[Opts]),
        option(search_property(P),Opts,l),
        option(extend_lambda(ExtendAtom),Opts,''),
        option(relations(RelAtom),Opts,''),
        option(format(OutFmt),Opts,info),
        normalize_predterm(P,P2),
        normalize_rels(RelAtom, Rels),
        debug(search, 'Rels(~q)  = ~q',[RelAtom, Rels]),
        findall(Obj,(member(T,SearchTerms),
                     search_to_objs(T, P2, Objs1, Opts),
                     member(Obj,Objs1)),
                Objs),
        debug(search, 'Search(~q) / ~q = ~q',[SearchTerms, P2, Objs]),
        concat_atom(PostTerms,',',ExtendAtom),
        findall(Obj2,(member(PostTerm,PostTerms),
                      normalize_extension_lambda(PostTerm, Lambda),
                      member(Obj,Objs),
                      call_lambda(Lambda,Obj,Obj2)),
                ObjsX),
        debug(search, 'PP(~q) = ~q',[PostTerms, ObjsX]),
        append(Objs,ObjsX,SeedObjs),
        debug(search, 'SG(~q)',[Rels]),
        owl_subgraph(SeedObjs, Rels, Quads, []),
        debug(search, 'SeedObjs = ~q',[SeedObjs]),
        (   option(output(OutFile),Opts)
        ->  true
        ;   OutFile=_),
        display_quads(Objs, Quads, OutFmt, OutFile, Opts).


% turn atomic search term into complex term if certain syntax is used
normalize_searchterm(X,Y/i) :-
        % if search term is =V, turn into exact regex
        atom(X),atom_concat('=',Term,X),concat_atom(['^',Term,'$'],Y).
normalize_searchterm(X,q(Q,Term)) :-
        % if search term is prolog query
        % e.g. subclasses of shape: "q//label_of(shape,R),rdfs_subclass_of(Q,R)"
        atom(X),atom_concat('q//',TermA,X),
        !,
        atom_to_term(TermA,Term,Bindings),
        member('Q'=Q,Bindings).
normalize_searchterm(X,X) :- X = _/_, !.
normalize_searchterm(X,X/i).
  
predterm(i,id).
predterm(l,label).
predterm(s,synonym).
predterm(a,all).
predterm('X',match_anything).

normalize_predterm(S,X) :- predterm(S,X),!.
normalize_predterm(X,X).

normalize_rels('.',_) :- !.
normalize_rels(L,L2) :- is_list(L), !, maplist(normalize_relterm,L,L2).
normalize_rels(X,L) :- concat_atom(L1,',',X),maplist(normalize_relterm,L1,L).

normalize_relterm(X,^(P)) :- atom_concat('^',P1,X),!,normalize_relterm(P1,P).
normalize_relterm(X,P) :- normalize_rel(X,P1),ensure_uri(P1,P).

normalize_rel(s,rdfs:subClassOf) :- !.
normalize_rel(e,owl:equivalentClass) :- !.
normalize_rel(t,rdf:type) :- !.
normalize_rel(N,R) :- \+ \+ lmatch(N,R), !, lmatch(N,R).
normalize_rel(N,R) :- concat_atom(L,'_',N), L=[_,_|_], concat_atom(L,' ',N1),!,normalize_rel(N1,R).
normalize_rel(X,X).

% @Deprecated
/*
search_and_display1(SearchTerm, PredTerm, PostTerm, Rels, DispTerm, OutFile, Opts) :-
        search_to_objs(SearchTerm, PredTerm, Objs, Opts),
        debug(search, 'Search(~q) / ~q = ~q',[SearchTerm, PredTerm, Objs]),
        findall(Obj2,(member(Obj,Objs),
                      normalize_extension_lambda(PostTerm, Lambda),
                      call_lambda(Lambda,Obj,Obj2)),
                ObjsX),
        debug(search, 'PP(~q) = ~q',[PostTerm, ObjsX]),
        append(Objs,ObjsX,SeedObjs),
        owl_subgraph(SeedObjs, Rels, Quads, []),
        display_quads(Objs, Quads, DispTerm, OutFile, Opts).
*/
% TODO
%normalize_extension_lambda(_, X,X,_).

normalize_extension_lambda(a, rdfs_subclass_of).
normalize_extension_lambda(d, [In,Out]>>rdfs_subclass_of(Out,In)).
normalize_extension_lambda(p, [In,Out]>>rdf(In,rdfs:subClassOf,Out)).
normalize_extension_lambda(c, [In,Out]>>rdf(Out,rdfs:subClassOf,In)).
normalize_extension_lambda(i, [In,Out]>>owl_edge(Out,_,In)).
normalize_extension_lambda(o, [In,Out]>>owl_edge(In,_,Out)).
normalize_extension_lambda(_,_) :- fail.

call_lambda([In,Out]>>G,In,Out) :- !, G.
call_lambda(P,In,Out) :- atomic(P),!, G =.. [P,In,Out], G.


%! search_to_objs(+SearchTerm, +PredTerm, ?Objs:list, +Opts:list) is det.
%
%    given a SearchTerm and search predicate, find matching objects
%
%    SearchTerm = BaseSearchTerm / Flag
%
search_to_objs(SearchTerm, PredTerm, Objs, Opts) :-
        % normalize and redo search
        normalize_searchterm(SearchTerm,SearchTerm1),
        setof(Obj, search_to_obj(SearchTerm1, PredTerm, Obj, Opts), Objs),
        !.
search_to_objs(SearchTerm, PredTerm, [], _) :-
        debug(info, 'No matches for ~q ~q',[SearchTerm, PredTerm]).
 
search_to_obj(q(T,Query), _, Obj, _Opts) :-
        !,
        setof(T,Query,Objs),
        member(Obj,Objs).
search_to_obj(SearchTerm/_, id, Obj, _Opts) :-
        % if the term is already an entity, use it
        ensure_uri(SearchTerm, Obj),
        rdf_subject(Obj),
        !.
search_to_obj(SearchTerm/_, id, Obj, _Opts) :-
        % as above, but assume OBO expansion
        (   Sep=':' ; Sep='_'),
        concat_atom([Pre,Post],Sep,SearchTerm),
        concat_atom(['http://purl.obolibrary.org/obo/',Pre,'_',Post],Obj),
        rdf_subject(Obj),
        !.
search_to_obj(SearchTerm/FlagStr, id, Obj, _Opts) :-
        % slash indicates regex search; if search by id, do exact
        !,
        rdf_subject(Obj),
        regex(str(Obj),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, all, Obj, _Opts) :-
        % regex over all
        !,
        rdf(Obj,_,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, label, Obj, _Opts) :-
        % regex over label
        !,
        rdf(Obj,rdfs:label,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, synonym, Obj, _Opts) :-
        %regex over syn
        !,
        label_or_synonym_pred_hook(Pred),
        rdf(Obj,Pred,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, Pred, Obj, _Opts) :-
        !,
        rdf(Obj,Pred,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(_, match_anything, Obj, _Opts) :-
        rdf(Obj,_,_).

gv_fmt(svg).
gv_fmt(png).

magic_tell(F) :-
        var(F),
        !.
magic_tell(F) :- tell(F).

opt_open_stream(F,user_output) :-
        var(F),
        !.
opt_open_stream(F,S) :-
        open(F,write,S,[]),
        !.

create_style_json_term(FocusObjs, Style) :-
        maplist(ensure_curie, FocusObjs, IDs),
        atom_json_term(Style,stylemap{highlightIds: IDs}, []).


display_quads(Objs, Quads, Fmt, OutFile, _Opts) :-
        gv_fmt(Fmt),
        !,
        quads_dict(Quads, Dict),
        write_json_tmp(Dict, OgFile),
        atom_json_term(Style,stylemap{highlightIds: Objs}, []),
        style_file_args(StyleFileArgs),
        sformat(Cmd,'og2dot.js ~w -S \'~w\' -t ~w -o ~w ~w',[StyleFileArgs,Style, Fmt,OutFile,OgFile]),
        debug(gv,'cmd: ~w',[Cmd]),
        shell(Cmd).


display_quads(Objs, Quads, viz, _, _Opts) :-
        !,
        quads_dict(Quads, Dict),
        write_json_tmp(Dict, OgFile),
        create_style_json_term(Objs, Style),
        %atom_json_term(Style,stylemap{highlightIds: Objs}, []),
        style_file_args(StyleFileArgs),
        sformat(Cmd,'og2dot.js ~w -S \'~w\' -t png ~w',[StyleFileArgs,Style, OgFile]),
        debug(gv,'cmd: ~w',[Cmd]),
        shell(Cmd).
display_quads(Objs, Quads, dot, F, _Opts) :-
        !,
        quads_dict(Quads, Dict),
        write_json_tmp(Dict, OgFile),
        atom_json_term(Style,stylemap{highlightIds: Objs}, []),
        style_file_args(StyleFileArgs),
        (   nonvar(F)
        ->  sformat(Cmd,'og2dot.js ~w -S \'~w\' -o ~w ~w',[StyleFileArgs,Style, F, OgFile])
        ;   sformat(Cmd,'og2dot.js ~w -S \'~w\' ~w',[StyleFileArgs,Style, OgFile])),
        shell(Cmd).
display_quads(_, Quads, json, Dest, _Opts) :-
        !,
        quads_dict(Quads, Dict),
        atom_json_dict(JsonAtom, Dict, []),
        write_to(JsonAtom, Dest).
display_quads(_, Quads, ids, _, _Opts) :-
        !,
        quads_objects(Quads, Objs),
        maplist(writeln, Objs).
display_quads(_, Quads, info, F, Opts) :-
        !,
        magic_tell(F),
        quads_objects(Quads, Objs),
        forall(member(Obj, Objs),
               display_obj(Obj, Opts)),
        told.
display_quads(_, Quads, obo, F, Opts) :-
        !,
        opt_open_stream(F,S),
        quads_objects(Quads, Objs),
        ensure_loaded(library(sparqlprog/obo_util)),
        gen_header(S,_,Opts),
        nl(S),
        forall(member(Obj, Objs),
               gen_stanza(S,Obj,Opts)),
        close(S).

display_quads(_, Quads, rdf, File, Opts) :-
        !,
        quads_objects(Quads, Objs),
        ensure_loaded(library(semweb/turtle)),
        G=x,
        extract_subontology(Objs,G,Opts),
        rdf_save_turtle(File,[graph(G)]).


display_obj(Uri, Opts) :-
        (   option(expand_uris(IsExp), Opts),
            ground(IsExp),
            IsExp=true
        ->  Id=Uri
        ;   ensure_curie(Uri, Id)),
        format('~w !',[Id]),
        forall((rdf(Uri,rdfs:label,Label),ensure_atom(Label,A)),
               format(' ~w',[A])),
        nl.


display_obo_stanza(Uri, Opts) :-
        gen_stanza(user_output,Uri,Opts).

write_to(Atom, File) :-
        var(File),
        !,
        write(Atom).
write_to(Atom, stream(S)) :-
        !,
        format(S,Atom,[]).
write_to(Atom, F) :-
        !,
        open(F, write, S, []),
        format(S,Atom,[]),
        close(S).




        

       
