:- module(sparqlprog,[
      sparql_endpoint/2
   ,  sparql_endpoint/3
   ,  current_sparql_endpoint/5
   ,  sparql_endpoint_url/2
   ,  srule/2
   ,  srule/3
   ,  srule/4
%   ,  query_goal/3     % Endpoint, Context, Opts
%   ,  query_phrase/3   % Endpoint, QueryPhrase, Result
%   ,  query_sparql/3 % Endpoint,QueryText,Result
   ,  create_sparql_select/2
   ,  create_sparql_select/3
   ,  create_sparql_select/4
   ,  create_sparql_construct/3
   ,  create_sparql_construct/4
   ,  inject_label_query/5
   ,  service_query_all/4
   ,  (??)/1
   ,  (??)/2
   ,  (??)/3
   ,  (??)/4
   ,  op(1150,xfy,join)
   ,  op(1150,fx,??)
   ,  op(1150,xfy,??)
   ]).

:- op(1150,xfy,join).


/** <module> sparqlprog - logic programming with SPARQL

This library provides a prolog interface to SPARQL queries. It allows
logic program queries to be compiled to SPARQL, and then executed on a
remote SPARQL server.

# Quickstart
  

The following can be entered interactively on the prolog console:
  
  ==
  [library(sparqlprog)].
  rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').
  sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').
  dbp ?? rdf(B,rdf:type,dbont:'Band'), rdf(B,dbont:bandMember,M).
  ==

This performs the following steps:  
  
 1. For convenience, the prefix `dbont` is registered using rdf_register_prefix/2
 2. Next we register the short name `dbp` for the [DBPedia endoint](http://dbpedia.org/sparql) using  sparql_endpoint/2.
 3. Then we query for all bands and their members using by queried by calling ??/2 - the query is specified using the standard rdf/3 predicate.

On the console the results should look like:

  ==
  |    dbp ?? rdf(B,rdf:type,dbont:'Band'), rdf(B,dbont:bandMember,M).
  B = 'http://dbpedia.org/resource/Alice_in_Chains',
  M = 'http://dbpedia.org/resource/Sean_Kinney' ;
  B = 'http://dbpedia.org/resource/Alice_in_Chains',
  M = 'http://dbpedia.org/resource/William_DuVall' ;
  B = 'http://dbpedia.org/resource/Alice_in_Chains',
  M = 'http://dbpedia.org/resource/Jerry_Cantrell' ;
  B = 'http://dbpedia.org/resource/Alice_in_Chains',
  M = 'http://dbpedia.org/resource/Mike_Inez' ;
  B = 'http://dbpedia.org/resource/Anthrax_(American_band)',
  M = 'http://dbpedia.org/resource/Scott_Ian' .
  ==

### Using user-defined predicates

You can define your own predicates for use in queries. So long as these stay within the sparqlprog subset, they can be rewritten into a query formed from rdf/3 terms.

For example, you can create a file `dbpedia.pl` with the following content:
  
  ==
  band(X) :- rdf(X,rdf:type,dbont:'Band').
  band_member(S,O) :- rdf(S,dbont:bandMember,O).
  ==

The original query can then be rewritten as:
  
  ==
  dbp ?? band(B), band_member(B,M).
  ==

library(sparqlprog/ontologies/dbpedia) provides basic wrapper predicates for dbpedia.  
  
This becomes more advantageous where we want to re-use predicates that encapsulate some query logic, for example, the following 3-ary predicate connects two bands by a shared member:

  ==
  has_shared_band_member(B1,B2,A) :-
        rdf(A,dbo:associatedBand,B1),
        rdf(A,dbo:associatedBand,B2),
        B1\=B2.
  ==

library(sparqlprog/ontologies/dbpedia/dbpedia_matcher) shows how to
construct a more advanced example for being able to perform semantic
similarity of bands based on shared genres.

sparqlprog is distributed with a number of modules for existing triplestore schemas (with a bias towards life sciences triplestores).

In future some of these will have their own distribution. Some examples:

  * library(sparqlprog/ontologies/faldo), genome locations e.g. faldo:location/5
  * library(sparqlprog/ontologies/ebi), EBI RDF e.g. ebi:homologous_to/2
  * library(sparqlprog/ontologies/biopax3), BioPax level 3 e.g. biopax3:nextStep/2
  * library(sparqlprog/ontologies/disgenet), DisGeNet e.g. disgenet:disease/1, disgenet:gda/3
  * library(sparqlprog/ontologies/chembl), e.g. chembl:has_molecule/2
  * library(sparqlprog/ontologies/nextprot), e.g. nextprot:expression/2
  * library(sparqlprog/ontologies/rhea), e.g. rhea:reaction_chebi_participant/2
  * library(sparqlprog/ontologies/uniprot), e.g. uniprot:protein/1, uniprot:has_disease_annotation/2.

Note that library(sparqlprog/ontologies/wikidata) is deprecated, instead use library(sparqlprog_wikidata), a separate distribution

## using OWL

library(sparqlprog/owl_util) provides predicates for working with OWL ontologies.

For example, owl_edge/4 provides an easy way to extract 'edges' from
an ontology (e.g subClassOf between named classes, or involving
existential restrictions).

library(sparqlprog/owl_search_viz) provides predicates for searching and visualizing OWL ontologies
  
## sparqlprog language

Any program composed of sparqlprog primitive predicates and the following connectors is considered to be a sparqlprog program, and can be translated to SPARQL.

The connectors allowed are:

  * (,)/2 conjunctive queries
  * (;)/2 disjunctive queries
  * (\+)/1 negation
  * (:-)/2 defining predicates

Note that the cut operator `!` is *not* allowed.  
  
The following are sparqlprog primitives:

  * rdf/3
  * rdf/4
  * rdf_has/3
  * rdfs_subclass_of/2
  * rdfs_individual_of/2
  * rdf_path/3
  * optional/1
  * bind/2
  * aggregate_group/4

Additionally, all SPARQL functions are treated as built-in predicates, e.g. regex/3, str_starts/2, lcase/2
  
### Running sparqlprog programs over in-memory database  

SWI-Prolog has its own in-memory database that can be interrogated via rdf/3.  
  
sparqlprog programs can be executed over this in-memory database, as well as remote databases. See sparqlprog/emulate_builtins.pl for examples

### Mixing remote and local execution  

One of the challenges of using SPARQL with a traditional programming
language is the impedance mismatch when combining query logic and
programmatic logic. With sparqlprog, both programs and queries are
specified in the same language.


### Authors

  
* Adapted from Samer Abdallah's sparkle by Chris Mungall 2018
* Samer's code is based on Yves Raimond's swic package, but completely re-written.

...  
  
@author Samer Abdallah
@author Chris Mungall
  
*/


:- use_module(library(sandbox)).
:- use_module(library(settings)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(sparql_dcg).
:- use_module(concurrency).

:- dynamic srule/4.
:- multifile srule/4.

:- dynamic sparql_endpoint/5.
:- multifile sparql_endpoint/5.
:- set_prolog_flag(double_quotes, codes).

:- setting(limit,integer,1000,'Default SPARQL SELECT limit').
:- setting(select_options,list,[distinct(true)],'Default select options').
:- setting(user_agent,atom,'sparqlprog','Value for http user-agent').

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



%! '??'(?EP, +Goal:sparql_goal, +SelectTerm) is nondet.
%! '??'(?EP, +Goal:sparql_goal, +SelectTerm, +Opts:list) is nondet.
%  
%  Query endpoint EP using Goal, selecting variables in SelectTerm
% 
%  EP should be a name declared using sparql_endpoint/2. 
%  IF EP is unbound on entry, it is bound to the endpoint from which
%  the current bindings were obtained.
%  
%  Goal is any prolog query that conforms to the sparqlprog subset.
%  i.e. it consists of sparqlprog predicates such as rdf/3, or defined  
%  predicates that can be compiled down to basic predicates.
%
%  SelectTerm is any prolog term, the variables used in this term will be
%  used to determine the SELECT in the SPARQL query
%
%  Example:
%
%  ==
%  ??(dbp, band_member(B,M), row([B,M]))
%  ==
%  
%  Note in many cases the SELECT variables can be determined from the
%  query in which case ??/2 is more convenient
??(EP,Spec,SelectTerm) :-
        ??(EP,Spec,SelectTerm,[]).
??(EP,Spec,SelectTerm,OptsOrig) :-
        copy_term(OptsOrig,Opts),
        debug(sparqlprog,'Finding subqueries: ~q opts=~q',[Spec,Opts]),
        %expand_opts(OptsIn,Opts),
        expand_subqueries(Spec,Spec2,EP),
        debug(sparqlprog,'Rewriting goal: ~q',[Spec2]),
        rewrite_goal(Spec2,SpecRewrite,Opts),
        debug(sparqlprog,'Rewritten goal: ~q',[SpecRewrite]),
        Goal=SpecRewrite,
        %spec_goal_opts(SpecRewrite,Goal,Opts),
        %debug(sparqlprog,'selecting opts...',[]),
        setting(select_options,Opts0),
        debug(sparqlprog,'Opts: ~q',[Opts0]),
        merge_options(Opts,Opts0,Opts1),
        query_goal(EP,Goal,SelectTerm,Opts1).

%! '??'(?EP, +Goal:sparql_goal) is nondet.
%  
%    Equivalent to ??/3 where the SELECT variables are extracted from
%    variables in Goal.
%
%    Note: if Goal contains an aggregate query then ??/3 should be used.
%
??(EP,Spec) :-
        ??(EP,Spec,Spec).

%! '??'(+Goal:sparql_goal) is nondet.
%
%    equivalent to ??/2, calling all known endpoints in parallel.
%
??(Spec) :- ??(_,Spec).




/*
expand_opts(OptsIn,OptsOut) :-
        select(program(P),OptsIn,T),
        !,
        tmp_file(prog,F),
        open(F,write,WS,[]),
        write(F),
        write(WS,P),
        close(WS),
        open(F,read,S,[]),
        findall(rule(T),read_term(S,T,[]),Rules),
        close(S),
        append(Rules,T,OptsOut).
expand_opts(Opts,Opts).
*/



%spec_goal_opts(Opts ?? Goal, Goal, Opts) :- !.
%spec_goal_opts(Goal,Goal,[]).

subq_term(subq(Q),EP,Q,EP).
subq_term(subq(Q,EP),_,Q,EP).
subq_term(x:Q,EP,Q,EP).

expand_subqueries(V,V,_) :-
        var(V),
        !.
expand_subqueries(In,Right,EP) :-
        nonvar(In),
        In = (Left join Right),
        !,
        ??(EP, Left).
expand_subqueries(In,true,EP) :-
        nonvar(In),
        subq_term(In,EP,Q,EP2),
        !,
        ??(EP2, Q).
expand_subqueries([],[],_) :- !.
expand_subqueries([H|L],[H2|L2],EP) :-
        !,
        expand_subqueries(H,H2,EP),
        expand_subqueries(L,L2,EP).
expand_subqueries(In,Out,EP) :-
        compound(In),
        !,
        In =.. [P|Args],
        expand_subqueries(Args,Args2,EP),
        Out =.. [P|Args2].
expand_subqueries(X,X,_) :- !.


% convert a prolog list [A,B,..] to a disjunctive term A;B;...
plist_to_disj([_-X],X) :- !.
plist_to_disj([_-X|T],(X;T2)) :- plist_to_disj(T,T2).



:- multifile rewrite_goal_hook/2.


%! rewrite_goal(+InGoal, ?OutGoalDisjunction) is semidet
%
% non-deterministic top-level goal rewrite
% if multiple goals possible, return a disjunction of goals (G1;G2;...;Gn)
% TODO: may be nondet if select variables in In...
rewrite_goal(In,OutDisj,Opts) :-
        debug(sparqlprog,'XXX: Rewriting goal: ~q',[In]),
        setof(Out,rewrite_goal(In,Out,1,Opts),Outs),
        list_to_disj(Outs,OutDisj).

/*
% deterministic version
xxxrewrite_goal(In,OutDisj) :-
        debug(sparqlprog,'XXX: Rewriting goal: ~q',[In]),
        setof(In-Out,rewrite_goal(In,Out,1),InOuts),
        debug(sparqlprog,'YYY: Rewritten goal: ~q -> ~q',[In,InOuts]),
        unify_keys(In,InOuts),
        plist_to_disj(InOuts,OutDisj),
        !.

unify_keys(_,[]).
unify_keys(Term,[TermX-_ | T]) :-
        term_variables(Term, Vars),
        term_variables(TermX, Vars),
        unify_keys(Term,T).
  
*/

%! rewrite_goal(+InGoal, ?OutGoal, +Depth) is semidet
%
% typically deterministic, but non-deterministic if 
% multiple possible paths
rewrite_goal(In,Out,N,Opts) :-
        debug(sparqlprog,'rewriting: ~q',[In]),
        rewrite_goal_hook(In,X), !,
        debug(sparqlprog,'Using hook to transform ~q -> ~q',[In,X]),
        rewrite_goal(X,Out,N,Opts).







% ------
% terminals
% ------
rewrite_goal(T, T2,_, _Opts) :- T=rdf(_,_,_), !, replace_string_unification(T,T2).
rewrite_goal(T, T2,_, _Opts) :- T=rdf(_,_,_,_), !, replace_string_unification(T,T2).
rewrite_goal(filter(A), filter(A),_, _Opts) :- !.
rewrite_goal(rdf_has(S,P,O), T2,_, _Opts) :- !, replace_string_unification(rdf(S,P,O),T2).
rewrite_goal(service(S,G), service(S,G2), D, Opts) :- !, rewrite_goal(G,G2,D, Opts).

% TODO: consider adding semantics
%rewrite_goal(rdf(S,P,O), rdf_has(S,P,O),_, _Opts) :- !.

%rewrite_goal('??'(Opts,Q), '??'(Opts,Q2), _, Opts) :- !, rewrite_goal(Q,Q2).

rewrite_goal(aggregate(A,G,V), aggregate(A,G2,V), D, Opts) :- !, rewrite_goal(G,G2,D, Opts).
rewrite_goal(aggregate_group(A,GVs,G,V), aggregate_group(A,GVs,G2,V), D, Opts) :- !, rewrite_goal(G,G2,D, Opts).
rewrite_goal(aggregate_group(A,GVs,G,H,V), aggregate_group(A,GVs,G2,H2,V), D, Opts) :- !, rewrite_goal(G,G2,D, Opts), rewrite_goal(H,H2,D, Opts).

% RDFStar
rewrite_goal(with(G,EdgeProps), rdfstar(S,P,O,EdgeProps), D, Opts) :- !, rewrite_goal(G,rdf(S,P,O), D, Opts).

% direct evaluation
rewrite_goal( (pre(G),G2), G3, D, Opts) :- G, !, rewrite_goal(G2,G3, D, Opts).


% rdfs terminals
rewrite_goal(rdf_where(Q), rdf_where(Q2), D, Opts) :-
        !,
        rewrite_goal(Q, Q2, D, Opts).
rewrite_goal({Q}, {Q2}, D, Opts) :-
        !,
        rewrite_goal(Q, Q2, D, Opts).
rewrite_goal(optional(Q), optional(Q2), _, Opts) :-
        !,
        rewrite_goal(Q,Q2, Opts).
rewrite_goal(exists(Q), exists(Q2), _, Opts) :-
        !,
        rewrite_goal(Q,Q2, Opts).
rewrite_goal(rdfs_subclass_of(C,P), rdf(C,oneOrMore(rdfs:subClassOf),P),_, _Opts) :- !.
rewrite_goal(rdfs_subproperty_of(C,P), rdf(C,oneOrMore(rdfs:subPropertyOf),P),_, _Opts) :- !.
rewrite_goal(rdfs_individual_of(I,C), (rdf(I,rdf:type,X),rdf(X,zeroOrMore(rdfs:subClassOf),C)),_, _Opts) :- !.
rewrite_goal(a(I,C), rdf(I,rdf:type,C),_, _Opts) :- !.
%rewrite_goal(rdf_member(X,L), rdf(L,oneOrMore(rdf:rest)/rdf:last,X),_, _Opts) :- !.
rewrite_goal(rdf_member(X,L), rdf(L,(zeroOrMore(rdf:rest)/(rdf:first)),X),_, _Opts) :- !.

% rdf11 rules
rewrite_goal(substring(S,P), contains(S,P), _, _Opts) :- !.
rewrite_goal(prefix(S,P), str_starts(S,P), _, _Opts) :- !.
rewrite_goal(the(_,_), true, _, _Opts) :- !.

% Match any literal that matches Pattern case insensitively, where the `*' character in Pattern matches zero or more characters
rewrite_goal(like(S,P1), regex(S,P2,i), _, _Opts) :- !, like_to_regex(P1,P2).

% TODO
%rewrite_goal(word(S,P1), regex(S,P2,i), _, Opts) :- !, like_to_regex(P1,P2).


% ------
% non-terminals
% ------
rewrite_goal((A,B),(A2,B2),D, Opts) :-
        !,
        rewrite_goal(A,A2,D, Opts),
        rewrite_goal(B,B2,D, Opts).

rewrite_goal((A;B),(A2;B2),D, Opts) :-
        !,
        rewrite_goal(A,A2,D, Opts),
        rewrite_goal(B,B2,D, Opts).
rewrite_goal(\+A, \+A2, D, Opts) :-
        !,
        rewrite_goal(A,A2,D, Opts).

% EXPERIMENTING WITH NONDET
rewrite_goal(A,A2,D, Opts) :-
        % TODO: see refl/2 test in test_aux
        setof(A-Clause,safe_clause(A,Clause),Clauses),
        !,
        debug(sparqlprog,' ~q CLAUSES==> ~q ~q',[A,Clauses,A2]),
        increase_depth(D,D2),
        %list_to_disj(Clauses,X),
        member(A-Clause,Clauses),
        rewrite_goal(Clause,A2,D2, Opts).
rewrite_goal(A,A2,D, Opts) :-
        % user defined expansion rule
        copy_term(Opts,OptsCopy),
        setof(A-Body,member( rule( (A:-Body) ) ,OptsCopy),Clauses),
        !,
        debug(sparqlprog,' ~q RULE==> ~q ~q',[A,Clauses,A2]),
        increase_depth(D,D2),
        %list_to_disj(Clauses,X),
        member(A-Clause,Clauses),
        rewrite_goal(Clause,A2,D2, Opts).
        %rewrite_goal(X,A2,D2, Opts).
rewrite_goal(A,A2,D, Opts) :-
        % as above, but parse the rule strinmg
        member(rule( Atom ),Opts),
        atom(Atom),
        read_term_from_atom(Atom, (Head :- Body), []),
        Head=A,
        !,
        debug(sparqlprog,' ~q RULE==> ~q',[A,A2]),
        increase_depth(D,D2),
        rewrite_goal(Body,A2,D2, Opts).
rewrite_goal(A,A,_,_).


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
replace_string_unification_args([anystr(A)|Args],[A2|Args2],T,(T2,FreshVar==A)) :-
        string(A),
        !,
        A2 = FreshVar,
        replace_string_unification_args(Args,Args2,T,T2).
replace_string_unification_args([A|Args],[A|Args2],T,T2) :-
        replace_string_unification_args(Args,Args2,T,T2).


no_rewrite(rdf_graph(_)).
no_rewrite(rdf_predicate(_)).
no_rewrite(rdf_is_iri(_)).
no_rewrite(member(_,_)).
no_rewrite(sparqlprog:member(_,_)).

no_rewrite_mod(emulate_builtins).
no_rewrite_mod(rdf11).


:- meta_predicate safe_clause(:,?).

safe_clause(Goal,Body) :-
        % TODO: come up with a more extensible way to prevent SPARQL builtins being expanded
        \+ \+ \+ no_rewrite(Goal),
        catch(clause(Goal,Body,Ref),_,fail),
        \+ \+ \+ ((clause_property(Ref,module(Mod)),
                   no_rewrite_mod(Mod))).



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

%! sparql_endpoint(+EP:ground, +URL:atom, +Options) is det.
%! sparql_endpoint(+EP:ground, +URL:atom) is det.
%
%  Declares EP as a short name for a SPARQL endpoint with the given URL.
%  
%  No options are defined at the moment.
%
%  Example:
%  ==
%  sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').
%  ==
sparql_endpoint(EP,Url) :- sparql_endpoint(EP,Url,[]).
sparql_endpoint(EP,Url,Options) :-
   url_endpoint(Url,Host,Port,Path),
   !,
   retract_declared_endpoint(EP,Url),     
   debug(sparqlprog,'Asserting SPARQL end point ~q: ~q ~q ~q ~q.',[EP,Host,Port,Path,Options]),
   assert(sparql_endpoint(EP,Host,Port,Path,Options)).

retract_declared_endpoint(EP,Url) :-
   sparql_endpoint(EP,Host,Port,Path,_),
   debug(info,'% WARNING: Updating already registered SPARQL end point ~q.\n',[Url]),
   retractall(sparql_endpoint(EP,Host,Port,Path,_)),
   !.
retract_declared_endpoint(_,_).

%user:term_expansion(:-(sparql_endpoint(EP,Url)), Expanded) :- 
%   endpoint_declaration(EP,Url,[],Expanded).
%user:term_expansion(:-(sparql_endpoint(EP,Url,Options)), Expanded) :- 
%   endpoint_declaration(EP,Url,Options,Expanded).

sparql_endpoint_url(EP,Url) :- sparql_endpoint(EP,Url,_,_,_).


endpoint_declaration(EP,Url,Options, sparqlprog:sparql_endpoint(EP,Host,Port,Path,Options)) :-
	debug(sparqlprog,'Declaring SPARQL end point ~q: ~q ~q ~q ~q.',[EP,Host,Port,Path,Options]),
   url_endpoint(Url,Host,Port,Path).

url_endpoint(Url,Host,Port,Path) :-
	parse_url(Url,Parsed),
	member(host(Host),Parsed),
	member(path(Path),Parsed),
	(member(port(Port),Parsed);Port=80).


%! current_sparql_endpoint(-EP:ground,-Host:atom,-Port:natural,-Path:atom,-Options:list) is nondet.
%
%  Succeeds once for each known endpoint.
current_sparql_endpoint(EP,Host,Port,Path,Options) :-
   sparql_endpoint(EP,Host,Port,Path,Options).


% ----------------------------------------------------
% Goal-based queries 
% These get translated into phrase-based queries.

%! query_goal(+EP,+Goal:sparql_goal,+Opts) is nondet.
%! query_goal(-EP,+Goal:sparql_goal,+Opts) is nondet.
%
%  Runs a SPARQL query against one or more SPARQL endpoints.
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
%        options if supplied. If true, then the offset option is ignored
%        and multiple SPARQL queries are made as necessary to supply
%        results, using the limit option to determine the number of results
%        retrieved from the endpoint at a time.
%  Other options are passed to phrase_to_sparql/2.

query_goal(EP,Goal,Opts) :- 
        query_goal(EP,Goal,Goal,Opts).

query_goal(EP,Goal,SelectTerm,Opts) :- 
   findall(EP,sparql_endpoint(EP,_,_,_,_),EPs1),
   sort(EPs1,EPs),
   term_variables(SelectTerm,Vars),
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
      debug(sparqlprog,'Executing parallel query: ~w // ~w // ~w',[SPARQL,Query,EPs]),
      parallel_query(Query,EPs,EP-Result),
      debug(sparqlprog,'RR=~q',[Result])
   ).

%! create_sparql_select(+SelectTerm, +Goal,-SPARQL,+Opts) is det.
%! create_sparql_select(+Goal, -SPARQL,+Opts) is det.
%! create_sparql_select(+Goal, -SPARQL) is det.
%
% Generates a sparql SELECT or ASK statement for a 
% prolog goal without executing it.
%
% Goal can be any sparqlprog program, see ??/3
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
        rewrite_goal(Goal,Goal2,Opts),
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
        P\=label_predicate,
        !,
        filter_opts(T,T2).
filter_opts([_|T],T2) :-
        filter_opts(T,T2).


%! inject_label_query(+Select, +Query, ?Select2, ?Query2, +Opts) is det
%
% Add an optional(rdf(X,rdfs:label,XL)) for every variable X in Select
inject_label_query(Select, Goal, Select2, (Goal,ConjGoal), Opts) :-
        term_variables(Select,Vars),
        option(label_predicate(P),Opts,rdfpred(rdfs:label)),
        inject_label_for_vars(Vars,ConjGoal,LabelVars,P),
        conjoin(Select,LabelVars,Select2).

inject_label_for_vars([Var],Goal,[LabelVar],P) :-
        !,
        inject_label_for_var(Var,Goal,LabelVar,P).
inject_label_for_vars([Var|Vars],(Goal1,Goal2),[LabelVar|LabelVars],P) :-
        !,
        inject_label_for_var(Var,Goal1,LabelVar,P),
        inject_label_for_vars(Vars,Goal2,LabelVars,P).
inject_label_for_var(Var,optional(rdf(Var,P,VarLabel)),VarLabel,rdfpred(P)) :- !.  % e.g. rdfs:label
inject_label_for_var(Var,optional(Goal),VarLabel,P) :-  Goal =.. [P,Var,VarLabel].  % prolog predicate, e.g. enlabel/2


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



       
        

        

%! create_sparql_construct(+Head,+Goal,-SPARQL,+Opts) is det.
%! create_sparql_construct(+Head,+Goal,-SPARQL) is det.
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
   rewrite_goal(Goal,Goal2, Opts),
   rewrite_goal(Head,Head2, Opts),
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

%! parallel_query(+Query, +EPs:list, ?EPResultPairs:list) is nondet.
parallel_query(_,[],_) :- !, format(user_error, 'No endpoints',[]), fail.
parallel_query(P,[X],Y) :- !,
        debug(sparqlprog,'Bypassing parallelism, endpoints= ~q',[X]),
        call(P,X,Y).            % no parallel
parallel_query(P,Xs,Y) :-
   maplist(par_goal(P,Y),Xs,Goals),
   concurrent_or(Y,Goals,[on_error(continue)]).

par_goal(P,Y,X,call(P,X,Y)).



%! query_phrase(+EP,+Q:sparqle_phrase(R),R) is nondet.
%! query_phrase(-EP,+Q:sparqle_phrase(R),R) is nondet.
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

%! query_sparql(?EP,SPARQL,-Result) is nondet.
%
%  Runs textual SPARQL query against an endpoint, exactly as
%  with sparql_query/3. If EP is unbound on entry, all known
%  endpoints will be tried sequentially. 
query_sparql(EP,SPARQL,Result) :-
   sparql_endpoint(EP,Host,Port,Path,EPOpts),
   debug(sparqlprog,'Querying endpoint http://~q:~q~q - ~w',[Host,Port,Path,SPARQL]),
   setting(user_agent,UserAgent),
   sparql_query(SPARQL,Result,[user_agent(UserAgent),host(Host),port(Port),path(Path)|EPOpts]).

