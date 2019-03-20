/**
  search and visualize results
  
  Convenience wrapper that combines search_util and owl_graph/4 from owl_util

  requires og2dot
*/
:- module(owl_search_viz,
          [
           searchviz/1,
           searchviz/2,

           owl_search_and_display/6,
           owl_search_and_display/7
           ]).

:- use_module(library(sparqlprog/search_util)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(http/json)).

searchviz(Term) :-
        searchviz(Term, _).

%% searchviz(+Term, +Preds) is semidet
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

%! owl_search_and_display(+SearchTerm, +PredTerm, +PostTerm, +Rels, +DispTerm, +OutFile, +Opts:list) is det
%
%  SearchTerm = SearchAtom / FlagAtom
%               SearchAtom = regex
%
%    a regular expression used to search for literals. E.g. '^limb$'/i (exact match, case insensitive)
%    
%
%  PredTerm = id | label | synonym | all
%
%    predicate used to connect subject to literal. 'label' will search against rdfs:label.
%    id will search against the subject IRI *OR* it's curiefied form.
%    synonym will search against label or synonyms (default oboInOwl vocabulary).
%    all will search against all predicates
%
%  Rels = RelList | RelListAtom
%         RelListAtom = Rel [',',Rel]*
%         Rel = CURIE | URI | RelLabel
%
%    comma separated relations used to extend out subgraph. See owl_subgraph/6.
%    Use 's' for shorthand for subClassOf, t for type.
%    URI or CURIE (e.g. BFO:0000050) or label (e.g. 'part of') can be used.
%
%  PostTerm = a | d | c | p
%
%    post process initial nodes returned from search
%
owl_search_and_display(SearchTerm, PredTerm, PostTerm, Rels, DispTerm, OutFile) :-
        owl_search_and_display(SearchTerm, PredTerm, PostTerm, Rels, DispTerm, OutFile, []).
owl_search_and_display(SearchTerm, PredTerm, PostTerm, Rels, DispTerm, OutFile, Opts) :-
        normalize_searchterm(SearchTerm, SearchTerm1),
        normalize_predterm(PredTerm, PredTerm1),
        normalize_rels(Rels, Rels1),
        search_and_display1(SearchTerm1, PredTerm1, PostTerm, Rels1, DispTerm, OutFile, Opts).

normalize_searchterm(X,X) :- X = _/_, !.
normalize_searchterm(X,X/i).

predterm(i,id).
predterm(l,label).
predterm(s,synonym).
predterm(a,all).

normalize_predterm(S,X) :- predterm(S,X),!.
normalize_predterm(X,X).

normalize_rels('.',_) :- !.
normalize_rels(L,L) :- is_list(L), !.
normalize_rels(X,L) :- concat_atom(L1,',',X),maplist(normalize_rel,L1,L2),maplist(ensure_uri,L2,L).


normalize_rel(s,rdfs:subClassOf) :- !.
normalize_rel(t,rdf:type) :- !.
normalize_rel(N,R) :- \+ \+ lmatch(N,R), !, lmatch(N,R).
normalize_rel(X,X).

search_and_display1(SearchTerm, PredTerm, PostTerm, Rels, DispTerm, OutFile, Opts) :-
        search_to_objs(SearchTerm, PredTerm, Objs, Opts),
        debug(search, 'Search(~q) = ~q',[SearchTerm, Objs]),
        findall(Obj2,(member(Obj,Objs),
                      postprocess(PostTerm, Lambda),
                      call_lambda(Lambda,Obj,Obj2)),
                Objs2),
        debug(search, 'PP(~q) = ~q',[PostTerm, Objs2]),
        append(Objs,Objs2,Objs3),
        owl_subgraph(Objs3, Rels, Quads, []),
        display_quads(Objs3, Quads, DispTerm, OutFile, Opts).

% TODO
%postprocess(_, X,X,_).

postprocess(a, rdfs_subclass_of).
postprocess(d, [In,Out]>>rdfs_subclass_of(Out,In)).
postprocess(c, [In,Out]>>rdf(Out,rdfs:subClassOf,In)).
postprocess(_,_) :- fail.

call_lambda([In,Out]>>G,In,Out) :- !, G.
call_lambda(P,In,Out) :- atomic(P),!, G =.. [P,In,Out], G.

search_to_objs(SearchTerm, PredTerm, Objs, Opts) :-
        setof(Obj, search_to_obj(SearchTerm, PredTerm, Obj, Opts), Objs),
        !.
search_to_objs(SearchTerm, PredTerm, [], _) :-
        debug(info, 'No matches for ~w ~w',[SearchTerm, PredTerm]).
 
search_to_obj(SearchTerm/_, id, Obj, _Opts) :-
        ensure_uri(SearchTerm, Obj),
        rdf_subject(Obj),
        !.
search_to_obj(SearchTerm/_, id, Obj, _Opts) :-
        (   Sep=':' ; Sep='_'),
        concat_atom([Pre,Post],Sep,SearchTerm),
        concat_atom(['http://purl.obolibrary.org/obo/',Pre,'_',Post],Obj),
        rdf_subject(Obj),
        !.
search_to_obj(SearchTerm/FlagStr, id, Obj, _Opts) :-
        !,
        rdf_subject(Obj),
        regex(str(Obj),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, all, Obj, _Opts) :-
        !,
        rdf(Obj,_,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, label, Obj, _Opts) :-
        !,
        rdf(Obj,rdfs:label,Lit),
        regex(str(Lit),SearchTerm,FlagStr).

search_to_obj(SearchTerm/FlagStr, synonym, Obj, _Opts) :-
        label_or_synonym_pred_hook(Pred),
        rdf(Obj,Pred,Lit),
        regex(str(Lit),SearchTerm,FlagStr).


display_quads(Objs, Quads, viz, _, _Opts) :-
        !,
        quads_dict(Quads, Dict),
        write_json_tmp(Dict, OgFile),
        atom_json_term(Style,stylemap{highlightIds: Objs}, []),
        style_file_args(StyleFileArgs),
        sformat(Cmd,'og2dot.js ~w -S \'~w\' -t png ~w',[StyleFileArgs,Style, OgFile]),
        shell(Cmd).
display_quads(_, Quads, json, _, _Opts) :-
        !,
        quads_dict(Quads, Dict),
        atom_json_dict(JsonAtom, Dict, []),
        writeln(JsonAtom).
display_quads(Objs, _, ids, _, _Opts) :-
        !,
        maplist(writeln, Objs).
display_quads(Objs, _, info, _, Opts) :-
        !,
        forall(member(Obj, Objs),
               display_obj(Obj, Opts)).
display_quads(Objs, _, obo, _, Opts) :-
        !,
        forall(member(Obj, Objs),
               display_obo_stanza(Obj, Opts)).

display_obj(Uri, _Opts) :-
        ensure_curie(Uri, Id),
        format('~w !',[Id]),
        forall((rdf(Uri,rdfs:label,Label),ensure_atom(Label,A)),
               format(' ~w',[A])),
        nl.


display_obo_stanza(Uri, _Opts) :-
        format('[Term]~n'),
        ensure_curie(Uri, Id),
        format('id: ~w~n',[Id]),
        forall((rdf(Uri,rdfs:label,Label),ensure_atom(Label,A)),
               format('name: ~w~n',[A])),
        nl.

/*
  lang

qterm( t(SearchTerm, PredTerm, PostTerm, Rels, DispTerm) ) -->
        sterm(SearchTerm),
        pterm(PredTerm),
        postterm(PostTerm),
        rterm(Rels),
        dterm(DispTerm).

rterm(Rels) -->
        seqmap_with_sep(",",rel,Rels).

sterm( _ ) -->
        "'",
        seqmap(nonquote, 
        "'",
        
*/
       