:- module(botLoader, [
    load/2
]).

load(Dir, File) :-
    atomic_list_concat(['../', Dir, '/', File], Path),
    consult(Path).