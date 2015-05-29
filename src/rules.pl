:- module(rules, [
	save/1,
	testRules/0
]).

:- use_module(db).
:- use_module(gdlParser).
:- use_module(library(debug)).

%% parses and saves Rules into the db
save(Rules) :-
	atom_string(Rules, StringRules),
	split_string(StringRules, "\n", "", R),
	gdlParser:gdlPrefixLinesToInfixLines(R, L),
	maplist(strToTerm, L, TermRules),
	db:addList(TermRules),
	logger:log('rules', L, write).

%% loads test rules into db
testRules :-
	gdlParser:testParse(StringRules),
	maplist(strToTerm, StringRules, TermRules),
	db:addList(TermRules).

%% load rules from specified file
loadRulesFile(File) :-
	readFile(File, Rules),
	db:addList(Rules),
	logger:log('rules', Rules, write).

%% helper predicate - converts string to term
strToTerm(S, T) :-
    atom_codes(A, S),
    term_to_atom(T, A).

readLine(In,[Ta|T]) :-
	read_term(In,Ta,[]),
	( Ta == end_of_file -> !; readLine(In,T) ).

%% readFile File and returns list of lines L
readFile(File,L) :-
	open(File,read,In),
	readLine(In,L),
	close(In).