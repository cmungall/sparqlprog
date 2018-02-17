#!/usr/bin/env swipl

:- initialization http_daemon.

:- use_module(library(main)).
:- use_module(library(optparse)).
%:- use_module(library(semweb/rdf_db)).
:- use_module(library(option)).

:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_cache)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
%:- rdf_attach_library('void.ttl').
:- use_module(library(rdf_owl/owl), []).
:- use_module(library(rdf_owl)).
:- use_module(library(doc_http), []).
:- use_module(library(settings)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).

:- set_setting_default(http:public_port,   443).
:- set_setting_default(http:public_scheme, https).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(root, '/', []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- http_handler(root(.), say_hi, []).

:- use_module(library(pengines)).

:- use_module(pengine_sandbox:library(sparqlprog)).
:- use_module(pengine_sandbox:library(sparqlprog/labelutils)).
:- use_module(pengine_sandbox:library(semweb/rdf11)).
:- use_module(library(sandbox)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_sandbox)).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(rdf11:rdf(_,_,_)).
sandbox:safe_primitive(rdf11:rdf_iri(_)).
sandbox:safe_primitive(sparqlprog:'??'(_,_)).
%sandbox:safe_primitive(clause(_,_,_)).

:- use_module(library(http/http_unix_daemon)).


http:location(pldoc, root(documentation), [priority(100)]).


:- rdf_set_cache_options([ global_directory('RDF-Cache'),
                           create_global_directory(true)
                         ]).

:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/labelutils)).
:- use_module(library(sparqlprog/endpoints)).

%:- initialization prolog_ide(thread_monitor).
:- initialization debug(sparqlprog).
:- initialization debug.

tobjprop(X) :- writeln( hhhhii), debug( sparqlprog,'FOO',[]), '??'(go, rdf(X,rdf:type,owl:'ObjectProperty')).
tobjprop(zzz).

sandbox:safe_primitive(user:tobjprop(_)).

say_hi(_Request) :-
    reply_html_page(
	   [title('SparqlProg Pengine')],
	   [h1('SparqlProg Pengine'),
	        p('This is the home page of SparqlProg Pengine'),
                a([href='https://github.com/cmungall/sparqlprog'],'GitHub project')]).
