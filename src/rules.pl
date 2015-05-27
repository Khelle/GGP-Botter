:- module(rules, [
	save/1,
	testRules/0,
	distinct/2
]).

:- use_module(library(unix)).

:- use_module(db).
:- use_module(gdlParser).

%% parses and saves Rules into the db
save(Rules) :-
	split_string(Rules, "\n", "", R),
	gdlParser:gdlPrefixLinesToInfixLines(R, L),
	db:addList(L).

%% loads test rules into db
testRules :-
	gdlParser:testParse(StringRules),
	maplist(strToTerm, StringRules, TermRules),
	db:addList(TermRules).

%% helper predicate - converts string to term
strToTerm(S, T) :-
    atom_codes(A, S),
    term_to_atom(T, A).

%% helper predicate - required to interpret the rules
distinct(X,Y) :- X \= Y.