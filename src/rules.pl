:- module(rules, [
	save/1,
	testRules/0
]).

:- use_module(library(unix)).

:- use_module(db).
:- use_module(gdlParser).
:- use_module(library(debug)).

%% parses and saves Rules into the db
save(Rules) :-
	atom_string(Rules, StringRules),
	split_string(StringRules, "\n", "", R),
	gdlParser:gdlPrefixLinesToInfixLines(R, L),
	maplist(strToTerm, L, TermRules),
	db:addList(TermRules).


%% loads test rules into db
testRules :-
	gdlParser:testParse(StringRules),
	maplist(strToTerm, StringRules, TermRules),
	db:addList(TermRules).

%% helper predicate - converts string to term
strToTerm(S, T) :-
    atom_codes(A, S),
    term_to_atom(T, A).