:- module(io_utils,
          [
           write_result/1,
           write_result/2
           ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/labelutils)).

is_pass_thru(inject_labels).


write_result(Term) :-
        write_result(Term,[]).
write_result(Term,Opts) :-
        option(format(Fmt),Opts),
        Fmt == prolog,
        !,
        format('~q.~n',[Term]).
        
write_result(Term,Opts) :-
        opt_if(dynlabel(true),Opts,Opts2),
        !,
        row_labelify(Term,Term2),
        write_result(Term2,Opts2).
write_result(Term,Opts) :-
        member(format(Fmt),Opts),
        csv_format_separator(Fmt,Sep),
        term_saferow(Term,Term2),
        debug(row,'ROW: ~q',[Term2]),
        csv_write_stream(current_output, [Term2], [separator(Sep)]),
        !.
write_result(Term,_Opts) :-
        write_canonical(Term),
        writeln('.').

atomize(T,A) :-
        sformat(A,'~w',[T]).

% translate a prolog term into an object that is suitable to send to csv_write_stream
%  - translate literals to atoms
%  - flatten lists
%  - translate args in a compound term

term_saferow(T,T3) :-
        term_saferow1(T,T2),
        % flatten to one leve;
        T2 =.. [P|Args],
        maplist(atomize,Args,Args2),
        T3 =.. [P|Args2].


term_saferow1(T,'?') :- var(T),!.
term_saferow1(T^^_,A) :- string(T),!, atom_string(A,T).
term_saferow1(T@_, A) :- string(T),!, atom_string(A,T).
term_saferow1(T@_, A) :- string(T),!, atom_string(A,T).
term_saferow1(literal(type(_,A)), A) :- !.
term_saferow1(literal(lang(_,A)), A) :- !.
term_saferow1(literal(A), A) :- !.
term_saferow1(L,A) :- is_list(L), !, maplist(term_saferow1,L,L2),maplist(atomize,L2,L3),concat_atom(L3,',',A).
term_saferow1(T,T2) :-
        T =.. [P|Args],
        Args = [_|_],
        !,
        maplist(term_saferow1,Args,Args2),
        T2 =.. [P|Args2].
term_saferow1(T,T2) :-
        rdf_global_id(Pre:Id,T),
        !,
        concat_atom([Pre,Id],:,T2).
term_saferow1(T,T).


csv_format_separator(csv,0',).
csv_format_separator(tsv,0'\t).
csv_format_separator(psv,0'|).
