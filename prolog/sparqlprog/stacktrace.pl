:- module(stacktrace,[]).
user:prolog_exception_hook(_,
                           _, _, _) :-
        backtrace(99),
        fail.
