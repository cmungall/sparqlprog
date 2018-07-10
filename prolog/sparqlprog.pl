/* Part of sparqlprog

    Adapted by Chris Mungall 2018
    
	Copyright 2014-2015 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(sparqlprog,[
      sparql_endpoint/2
   ,  sparql_endpoint/3
   ,  current_sparql_endpoint/5
   ,  sparql_endpoint_url/2
   ,  srule/2
   ,  srule/3
   ,  srule/4
   ,  query_goal/3     % Endpoint, Context, Opts
   ,  query_phrase/3   % Endpoint, QueryPhrase, Result
   ,  query_sparql/3 % Endpoint,QueryText,Result
   ,  create_sparql_select/2
   ,  create_sparql_select/3
   ,  create_sparql_select/4
   ,  create_sparql_construct/3
   ,  create_sparql_construct/4
   ,  inject_label_query/5
   ,  service_query_all/4
   ,  (??)/1
   ,  (??)/2
   ,  op(1150,fx,??)
   ,  op(1150,xfy,??)
	]).

/** <module> Query to SPARQL endpoints with a more Prolog-like syntax
 
  Samer Abdallah, Dept. of Computer Science, UCL (2014)
  Based on Yves Raimond's swic package, but completely re-written.

   This module provides a little language for expressing SPARQL queries
   and a database of known SPARQL endpoints. Queries can be executed
   across multiple endpoints in parallel. When using auto-paging,
   multiple queries are made automatically to fetch new bindings as
   they are needed. For example, 
   ==
   EP ?? rdf(A,B,C).
   ==
   will retrieve all triples from all endpoints in parallel, fetching
   100 bindings at a time from each endpoint (assuming the setting
   sparqlprog:limit takes it's default value of 100).
*/

:- use_module(library(sandbox)).
:- use_module(library(settings)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(sparql_dcg).
:- use_module(concurrency).

:- dynamic srule/4.
:- multifile srule/4.

:- dynamic sparql_endpoint/5.
:- multifile sparql_endpoint/5.
:- set_prolog_flag(double_quotes, codes).

:- setting(limit,integer,100,'Default SPARQL SELECT limit').
:- setting(select_options,list,[distinct(true)],'Default select options').

:- meta_predicate query_phrase(+,//,-).

sandbox:safe_meta(sparql_dcg:phrase_to_sparql(Phr,_),[Phr]).
sandbox:safe_primitive(sparql_dcg:select(_,_,_,_,_)).
sandbox:safe_primitive(sparql_dcg:describe(_,_,_,_)).
sandbox:safe_primitive(sparql_dcg:describe(_,_,_)).
sandbox:safe_primitive(sparql_dcg:ask(_,_,_)).

service_query_all(EP,Template,Spec,Results) :-
        setof(Template,??(EP,Spec),Results),
        !.
service_query_all(_,_,_,[]).


%% '??'(+Goal:sparql_goal) is nondet.
%  Equivalent to _ ?? Goal. Will query all endpoints
%  in parallel. Identical bindings may be returned multiple times.
%  See query_goal/3 for details.
??(Spec) :- ??(_,Spec).

%% '??'(EP,+Goal:sparql_goal) is nondet.
%  Equivalent to query_goal(EP,Goal,Opts) where Opts is the value of
%  the setting sparqlprog:select_options. See query_goal/3 for details.
%  IF EP is unbound on entry, it is bound to the endpoint from which
%  the current bindings were obtained.
??(EP,Spec) :-
   debug(sparqlprog,'Rewriting goal: ~q',[Spec]),
   rewrite_goal(Spec,SpecRewrite),
   debug(sparqlprog,'Rewritten goal: ~q',[SpecRewrite]),
   spec_goal_opts(SpecRewrite,Goal,Opts),
   debug(sparqlprog,'Opts: ~q',[Opts0]),
   setting(select_options,Opts0),
   merge_options(Opts,Opts0,Opts1),
   query_goal(EP,Goal,Opts1).

spec_goal_opts(Opts ?? Goal, Goal, Opts) :- !.
spec_goal_opts(Goal,Goal,[]).

:- multifile rewrite_goal_hook/2.

%% rewrite_goal(+InGoal, ?OutGoalDisjunction) is semidet
%
% non-deterministic top-level goal rewrite
% if multiple goals possible, return a disjunction of goals (G1;G2;...;Gn)
% TODO: may be nondet if select variables in In...
rewrite_goal(In,OutDisj) :-
        debug(sparqlprog,'XXX: Rewriting goal: ~q',[In]),
        setof(Out,rewrite_goal(In,Out,1),Outs),
        list_to_disj(Outs,OutDisj).

% deterministic version
xxxrewrite_goal(In,OutDisj) :-
        debug(sparqlprog,'XXX: Rewriting goal: ~q',[In]),
        setof(In-Out,rewrite_goal(In,Out,1),InOuts),
        debug(sparqlprog,'YYY: Rewritten goal: ~q -> ~q',[In,InOuts]),
        unify_keys(In,InOuts),
        plist_to_disj(InOuts,OutDisj),
        !.


plist_to_disj([_-X],X) :- !.
plist_to_disj([_-X|T],(X;T2)) :- plist_to_disj(T,T2).

unify_keys(_,[]).
unify_keys(Term,[TermX-_ | T]) :-
        term_variables(Term, Vars),
        term_variables(TermX, Vars),
        unify_keys(Term,T).




%% rewrite_goal(+InGoal, ?OutGoal, +Depth) is semidet
%
% typically deterministic, but non-deterministic if 
% multiple possible paths
rewrite_goal(In,Out,N) :-
        debug(sparqlprog,'rewriting: ~q',[In]),
        rewrite_goal_hook(In,X), !,
        debug(sparqlprog,'Using hook to transform ~q -> ~q',[In,X]),
        rewrite_goal(X,Out,N).

% ------
% terminals
% ------
rewrite_goal(T, T2,_) :- T=rdf(_,_,_), !, replace_string_unification(T,T2).
rewrite_goal(T, T2,_) :- T=rdf(_,_,_,_), !, replace_string_unification(T,T2).
rewrite_goal(filter(A), filter(A),_) :- !.
rewrite_goal(rdf_has(S,P,O), T2,_) :- !, replace_string_unification(rdf(S,P,O),T2).

% TODO: consider adding semantics
%rewrite_goal(rdf(S,P,O), rdf_has(S,P,O),_) :- !.

%rewrite_goal('??'(Opts,Q), '??'(Opts,Q2), _) :- !, rewrite_goal(Q,Q2).

rewrite_goal(aggregate(A,G,V), aggregate(A,G2,V), D) :- !, rewrite_goal(G,G2,D).
rewrite_goal(aggregate_group(A,GVs,G,V), aggregate_group(A,GVs,G2,V), D) :- !, rewrite_goal(G,G2,D).



% rdfs terminals
rewrite_goal(rdf_where(Q), rdf_where(Q2), D) :-
        !,
        rewrite_goal(Q, Q2, D).
rewrite_goal({Q}, {Q2}, D) :-
        !,
        rewrite_goal(Q, Q2, D).
rewrite_goal(optional(Q), optional(Q2), _) :-
        !,
        rewrite_goal(Q,Q2).
rewrite_goal(rdfs_subclass_of(C,P), rdf(C,oneOrMore(rdfs:subClassOf),P),_) :- !.
rewrite_goal(rdfs_subproperty_of(C,P), rdf(C,oneOrMore(rdfs:subPropertyOf),P),_) :- !.
rewrite_goal(rdfs_individual_of(I,C), (rdf(I,rdf:type,X),rdf(X,zeroOrMore(rdfs:subClassOf),C)),_) :- !.
rewrite_goal(a(I,C), rdf(I,rdf:type,C),_) :- !.
%rewrite_goal(rdf_member(X,L), rdf(L,oneOrMore(rdf:rest)/rdf:last,X),_) :- !.
rewrite_goal(rdf_member(X,L), rdf(L,(zeroOrMore(rdf:rest)/(rdf:first)),X),_) :- !.

% rdf11 rules
rewrite_goal(substring(S,P), contains(S,P), _) :- !.
rewrite_goal(prefix(S,P), str_starts(S,P), _) :- !.

% Match any literal that matches Pattern case insensitively, where the `*' character in Pattern matches zero or more characters
rewrite_goal(like(S,P1), regex(S,P2,i), _) :- !, like_to_regex(P1,P2).

% TODO
%rewrite_goal(word(S,P1), regex(S,P2,i), _) :- !, like_to_regex(P1,P2).


% ------
% non-terminals
% ------
rewrite_goal((A,B),(A2,B2),D) :-
        !,
        rewrite_goal(A,A2,D),
        rewrite_goal(B,B2,D).

rewrite_goal((A;B),(A2;B2),D) :-
        !,
        rewrite_goal(A,A2,D),
        rewrite_goal(B,B2,D).
rewrite_goal(\+A, \+A2, D) :-
        !,
        rewrite_goal(A,A2,D).

% EXPERIMENTING WITH NONDET
rewrite_goal(A,A2,D) :-
        % TODO: see refl/2 test in test_aux
        setof(A-Clause,safe_clause(A,Clause),Clauses),
        !,
        debug(sparqlprog,' ~q CLAUSES==> ~q',[A,Clauses,A]),
        increase_depth(D,D2),
        %list_to_disj(Clauses,X),
        member(A-Clause,Clauses),
        rewrite_goal(Clause,A2,D2).
rewrite_goal(A,A,_).

% for certain predicates, do not use replace_string_unification(T,T3)
% TODO: make this a hook
nofilter(P) :-
        ground(P),
        rdf_global_id(P,Px),
        atomic(Px),
        atom_concat('http://www.bigdata.com/rdf/search#',_,Px).

% rdf11 'like' construct
like_to_regex(Like,Re) :-
        concat_atom(Parts,'*',Like),
        (   Parts=[''|_]
        ->  Init=''
        ;   Init='^'),
        (   reverse(Parts,[''|_])
        ->  Last=''
        ;   Last='$'),
        concat_atom(Parts,'.*',Re1),
        concat_atom([Init,Re1,Last],Re).

        

% We avoid translation rdf(X,rdf:label,"foo") to a direct
% triple in the SPARQL query, since this may fail to
% match (e.g. if xsd:string is the actual type)
% instead we translate to  (rdf(X,rdf:label,VAR),VAR=="foo")
% because this yields a FILTER in the SPARQL giving desired results
replace_string_unification(T,T) :-
        (   T=rdf(_,P,_)
        ;   T=rdf(_,P,_,_)),
        nofilter(P),
        !.
replace_string_unification(T,T3) :-
        debug(sparqlprog,'replacing string unification: ~q',[T]),
        T =.. [P|Args],
        replace_string_unification_args(Args,Args2,T2,T3),
        T2 =.. [P|Args2],
        debug(sparqlprog,'replaced string unification: ~q',[T3]).

replace_string_unification_args([],[],T,T).
replace_string_unification_args([A|Args],[A2|Args2],T,(T2,FreshVar==A)) :-
        string(A),
        !,
        A2 = FreshVar,
        replace_string_unification_args(Args,Args2,T,T2).
replace_string_unification_args([A|Args],[A|Args2],T,T2) :-
        replace_string_unification_args(Args,Args2,T,T2).


        

safe_clause(Goal,Body) :-
        catch(clause(Goal,Body,Ref),_,fail),
        % TODO: come up with a more extensible way to prevent SPARQL builtins being expanded
        \+ ((clause_property(Ref,module(Mod)),
             Mod=emulate_builtins)).



% TODO: attempt at expanding clauses not exported
% not clear this is a good idea...
todo__safe_clause(Goal,LocalBody) :-
        catch(clause(Goal,Body,Ref),_,fail),
        clause_property(Ref,module(M)),
        (   M\=system,
            \+ ((Body=..[rdf|_])),
            catch(clause(M:Body,_),_,fail)
        ->  LocalBody = M:Body
        ;   LocalBody=Body).


        


list_to_disj([X],X) :- !.
list_to_disj([X|T],(X;T2)) :- list_to_disj(T,T2).


increase_depth(D,_) :-
        D > 10,
        !,
        throw(error(max_depth_exceeded(D))).
increase_depth(D,D2) :-
        D2 is D+1.

        
        
%! srule(+Pred, +Args) is det.
%! srule(+Pred, +Args, +Desc) is det.
%
% declare a new sparql rule
srule(P,A) :- srule(P,A,'').
srule(P,A,D) :-
        current_module(M),
        assert(srule(P,A,D,M)).

/*
 * Assert/declare a new sparql end point
 */

%% sparql_endpoint(+EP:ground, +URL:atom, +Options) is det.
%% sparql_endpoint(+EP:ground, +URL:atom) is det.
%
%  Declares EP as a short name for a SPARQL endpoint with the given URL.
%  No options are defined at the moment.
sparql_endpoint(EP,Url) :- sparql_endpoint(EP,Url,[]).
sparql_endpoint(EP,Url,Options) :-
   url_endpoint(Url,Host,Port,Path),
   !,
   retract_declared_endpoint(EP,Url),     
   debug(sparqlprog,'Asserting SPARQL end point ~q: ~q ~q ~q ~q.',[EP,Host,Port,Path,Options]),
   assert(sparql_endpoint(EP,Host,Port,Path,Options)).

retract_declared_endpoint(EP,Url) :-
   sparql_endpoint(EP,Host,Port,Path,_),
   format('% WARNING: Updating already registered SPARQL end point ~q.\n',[Url]),
   retractall(sparql_endpoint(EP,Host,Port,Path,_)),
   !.
retract_declared_endpoint(_,_).

user:term_expansion(:-(sparql_endpoint(EP,Url)), Expanded) :- 
   endpoint_declaration(EP,Url,[],Expanded).
user:term_expansion(:-(sparql_endpoint(EP,Url,Options)), Expanded) :- 
   endpoint_declaration(EP,Url,Options,Expanded).

sparql_endpoint_url(EP,Url) :- sparql_endpoint(EP,Url,_,_,_).


endpoint_declaration(EP,Url,Options, sparqlprog:sparql_endpoint(EP,Host,Port,Path,Options)) :-
	debug(sparqlprog,'Declaring SPARQL end point ~q: ~q ~q ~q ~q.',[EP,Host,Port,Path,Options]),
   url_endpoint(Url,Host,Port,Path).

url_endpoint(Url,Host,Port,Path) :-
	parse_url(Url,Parsed),
	member(host(Host),Parsed),
	member(path(Path),Parsed),
	(member(port(Port),Parsed);Port=80).


%% current_sparql_endpoint(-EP:ground,-Host:atom,-Port:natural,-Path:atom,-Options:list) is nondet.
%
%  Succeeds once for each known endpoint.
current_sparql_endpoint(EP,Host,Port,Path,Options) :-
   sparql_endpoint(EP,Host,Port,Path,Options).


% ----------------------------------------------------
% Goal-based queries 
% These get translated into phrase-based queries.

%% query_goal(+EP,+Goal:sparql_goal,+Opts) is nondet.
%% query_goal(-EP,+Goal:sparql_goal,+Opts) is nondet.
%
%  Runs a SPARQL query against one or more SPARLQ endpoints.
%  Goal is converted into a textual SPARQL query using the DCG
%  defined in sparql_dcg.pl. 
%
%  If EP is ground on entry, the query is run against the specified endpoint.
%  If EP is unbound on entry, the query is run agains all endpoints
%  in parallel, possibly returning multiple results from each.
%
%  (The following applies only to queries that return bindings, not
%  to simple boolean questions, which return only true or false.)
%  Options are as follows:
%     *  limit(L:natural)
%        At-most this many bindings will be returned per SPARQL call.
%     *  offset(O:natural)
%        Begin returning bindings from the Oth result on.
%     *  autopage(Auto:bool)
%        If false, a single SPARQL call is made using any limit and offset
%        options if supplied. If true, the the offset option is ignored
%        and multiple SPARQL queries are made as necessary to supply
%        results, using the limit option to determine the number of results
%        retrieved from the endpoint at a time.
%  Other options are passed to phrase_to_sparql/2.

query_goal(EP,Goal,Opts) :- 
   findall(EP,sparql_endpoint(EP,_,_,_,_),EPs),
   term_variables(Goal,Vars),
   (  Vars = [] % if no variables, do an ASK query, otherwise, SELECT
   -> phrase_to_sparql(ask(Goal),SPARQL),
      parallel_query(simple_query(SPARQL),EPs,EP-true)
   ;  Result =.. [row|Vars],
      setting(limit,DefaultLimit),
      call_dcg((  option_default_select(limit(Limit),DefaultLimit),
                  option_default_select(autopage(Auto),true),
                  (  {Auto=true}
                  -> {Query = autopage_query(Limit,SPARQL)},
                     option_default_select(offset(_),_)
                  ;  {Query = simple_query(SPARQL)},
                     cons(limit(Limit))
                  ) 
               ), Opts, Opts1),
      debug(sparqlprog,'DCG: ~q ~q ~q',[Vars,Goal,Opts1]),
      phrase_to_sparql(select(Vars,Goal,Opts1),SPARQL),
      parallel_query(Query,EPs,EP-Result)
   ).

%% create_sparql_select(+Goal,-SPARQL,+Opts) is det.
%% create_sparql_select(+Goal,-SPARQL) is det.
%
% Generates a sparql SELECT or ASK statement for a 
% prolog goal without executing it.
%
% Goal can be any prolog goal consisting of based 
% rdf/3 or rdf/4 statements, filters, or terms
% that can be rewritten in this way
create_sparql_select(Goal,SPARQL) :-
   create_sparql_select(Goal,SPARQL,[]).

create_sparql_select(Goal,SPARQL,Opts) :-
        create_sparql_select(Goal,Goal,SPARQL,Opts).

create_sparql_select(Select,Goal,SPARQL,Opts) :-
        select(inject_labels(true),Opts,OptsRest),
        !,
        inject_label_query(Select,Goal,Select2,Goal2,Opts),
        create_sparql_select(Select2,Goal2,SPARQL,OptsRest).

                
create_sparql_select(Select,Goal,SPARQL,Opts) :-
        filter_opts(Opts,OptsFiltered),
        rewrite_goal(Goal,Goal2),
        debug(sparqlprog,'Rewritten goal2: ~q',[Goal2]),
        term_variables(Select,Vars),
        debug(sparqlprog,'Vars: ~q',[Vars]),        
        (   Vars = [] % if no variables, do an ASK query, otherwise, SELECT
        ->  phrase_to_sparql(ask(Goal2),SPARQL)
        ;   setting(limit,DefaultLimit),
            call_dcg((  option_default_select(limit(Limit),DefaultLimit),
                        option_default_select(autopage(Auto),true),
                        (   {Auto=true}
                        ->  {Query = autopage_query(Limit,SPARQL)},
                            option_default_select(offset(_),_)
                        ;   {Query = simple_query(SPARQL)},
                            cons(limit(Limit))
                        ) 
                     ), OptsFiltered, Opts1),
            phrase_to_sparql(select(Vars,Goal2,Opts1),SPARQL,Opts)).

filter_opts([],[]).
filter_opts([H|T],[H|T2]) :-
        H=..[P|_],
        P\=inject_labels,
        P\=bindings,
        !,
        filter_opts(T,T2).
filter_opts([_|T],T2) :-
        filter_opts(T,T2).


%% inject_label_query(+Select, +Query, ?Select2, ?Query2, +Opts) is det
%
% Add an optional(rdf(X,rdfs:label,XL)) for every variable X in Select
% TODO: interleave
inject_label_query(Select, Goal, Select2, (Goal,ConjGoal), _Opts) :-
        term_variables(Select,Vars),
        inject_label_for_vars(Vars,ConjGoal,LabelVars),
        conjoin(Select,LabelVars,Select2).

inject_label_for_vars([Var],Goal,[LabelVar]) :-
        !,
        inject_label_for_var(Var,Goal,LabelVar).
inject_label_for_vars([Var|Vars],(Goal1,Goal2),[LabelVar|LabelVars]) :-
        !,
        inject_label_for_var(Var,Goal1,LabelVar),
        inject_label_for_vars(Vars,Goal2,LabelVars).
inject_label_for_var(Var,optional(rdf(Var,rdfs:label,VarLabel)),VarLabel).

conjoin(Term,L,T2) :-
        is_list(Term),
        !,
        append(Term,L,T2).
conjoin(Term,L,[Term|L]) :-
        \+compound(Term),
        !.
conjoin(Term,L,T2) :-
        Term =.. [P|Args],
        append(Args,L,Args2),
        T2 =.. [P|Args2].



       
        

        

%% create_sparql_construct(+Head,+Goal,-SPARQL,+Opts) is det.
%% create_sparql_construct(+Head,+Goal,-SPARQL) is det.
%
% Generates a sparql CONSTRUCT statement for a 
% prolog goal without executing it.
%
% Goal or Head can be any prolog goal consisting of based 
% rdf/3 or rdf/4 statements, filters, or terms
% that can be rewritten in this way
%
% the Head forms the head part of the CONSTRUCT
create_sparql_construct(Head,Goal,SPARQL) :-
   create_sparql_construct(Head,Goal,SPARQL,[]).
create_sparql_construct(Head,Goal,SPARQL,Opts) :-
   rewrite_goal(Goal,Goal2),
   rewrite_goal(Head,Head2),
   debug(sparqlprog,'Rewritten: ~q <- ~q',[Head2,Goal2]),        
   phrase_to_sparql(construct(Head2,Goal2,Opts),SPARQL).


cons(X,T,[X|T]).
option_default_select(Opt,Def,O1,O2) :- select_option(Opt,O1,O2,Def).
simple_query(SPARQL,EP,EP-Result) :- query_sparql(EP,SPARQL,Result).
autopage_query(Limit,SPARQL,EP,EP-Result) :- autopage(EP,SPARQL,Limit,0,Result).

autopage(EP,SPARQL,Limit,Offset,Result) :-
   format(string(Q),'~s LIMIT ~d OFFSET ~d',[SPARQL,Limit,Offset]),
   findall(R,query_sparql(EP,Q,R),Results),
   (  member(Result,Results)
   ;  length(Results,Limit),     % no next page if length(Results) < Limit
      Offset1 is Offset + Limit, % next batch of results
      autopage(EP,SPARQL,Limit,Offset1,Result)
   ).

parallel_query(_,[],_) :- !, fail.
parallel_query(P,[X],Y) :- !, call(P,X,Y).
parallel_query(P,Xs,Y) :-
   maplist(par_goal(P,Y),Xs,Goals),
   concurrent_or(Y,Goals,[on_error(continue)]).

par_goal(P,Y,X,call(P,X,Y)).



%% query_phrase(+EP,+Q:sparqle_phrase(R),R) is nondet.
%% query_phrase(-EP,+Q:sparqle_phrase(R),R) is nondet.
%
% Phrase-based queries using the DCG defined in sparql_dcg.pl.
% The return type depends on the query:
% ==
% select(V:list(var), sparql_goal, options) :: sparql_phrase(row(N)) :- length(V,N).
% describe(resource,sparql_goal)            :: sparql_phrase(rdf).
% describe(resource)                        :: sparql_phrase(rdf).
% ask(sparql_goal)                          :: sparql_phrase(bool).
%
% rdf  ---> rdf(resource,resource,object).
% bool ---> true; false.
% ==
% =|row(N)|= is the type of terms of functor row/N.

query_phrase(EP,Phrase,Result) :- 
        phrase_to_sparql(Phrase,SPARQL),
        query_sparql(EP,SPARQL,Result).


phrase_to_sparql(Phrase,SPARQL) :-
        phrase_to_sparql(Phrase,SPARQL,[]).

phrase_to_sparql(Phrase,SPARQL,Opts) :-
        option(bindings(Bindings),Opts),
        !,
        term_variables(Phrase,Vars),
        copy_term(t(Vars,Phrase,Bindings),t(Vars1,Phrase1,Bindings1)),
        assign_vars_using_bindings(Vars1,Bindings1),
        phrase_vars_to_sparql(Phrase1,Vars1,SPARQL).

phrase_to_sparql(Phrase,SPARQL,_Opts) :-
        term_variables(Phrase,Vars),
        copy_term(t(Vars,Phrase),t(Vars1,Phrase1)),
        phrase_vars_to_sparql(Phrase1,Vars1,SPARQL).

phrase_vars_to_sparql(Phrase1,Vars1,SPARQL) :-
        numbervars(Vars1,0,_),
        (   phrase(Phrase1,Codes)
        ->  true
        ;   throw(unrecognised_query(Phrase1))
        ),
        string_codes(SPARQL,Codes),
        debug(sparqlprog,'SPARQL query: ~s',[SPARQL]).

%! assign_vars_using_bindings(+Vars:list,+Bindings:list) is det
%
% Bindings = [A='A',B='B', ...]
% assign all variables in Bindings to a variable v(Name)
%
% rationale: by default, we assign vars to numbers using numbervars/3
% these are then translated by the DCG to ?v1, ?v2, ... etc
% if the SPARQL is intended to be seen, then it's preferable
% to use the user's own meaningful assigned variable names
assign_vars_using_bindings(_,[]).
assign_vars_using_bindings(Vars1,[VN=v(VN2)|Bindings]) :-
        downcase_first_char(VN,VN2),
        assign_vars_using_bindings(Vars1,Bindings).

downcase_first_char(A,A2) :-
        atom_chars(A,[C|Chars]),
        downcase_atom(C,C2),
        atom_chars(A2,[C2|Chars]).

        
        
% ----------------------------------------------------
% In the end, everything comes through this.

%% query_sparql(?EP,SPARQL,-Result) is nondet.
%
%  Runs textual SPARQL query against an endpoint, exactly as
%  with sparql_query/3. If EP is unbound on entry, all known
%  endpoints will be tried sequentially. 
query_sparql(EP,SPARQL,Result) :-
   sparql_endpoint(EP,Host,Port,Path,EPOpts),
   debug(sparqlprog,'Querying endpoint http://~q:~q~q',[Host,Port,Path]),
   sparql_query(SPARQL,Result,[host(Host),port(Port),path(Path)|EPOpts]).

