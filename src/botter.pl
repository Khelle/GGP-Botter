:- module(botter, [
	writeLine/2,
	writeFile/2,
	readLine/2,
	readFile/2,
	parseGDL/2,
	findroles/1,
	findpropositions/1,
	findactions/2,
	findinits/1,
	findlegalx/3,
	findlegals/3,
	findnext/4,
	findreward/3,
	findterminalp/2
]).
:- use_module(library(unix)).
:- use_module(db).

%% writeLine to stream Out
writeLine(_,[]).
writeLine(Out,[Ln|L]) :-
	write(Out,Ln),
	nl(Out),
	writeLine(Out,L),
	!.

%% write list of lines L into file File
writeFile(File,L) :-
	open(File,write,Out),
	writeLine(Out,L),
	close(Out).

%% readLine from stream In and return list of lines L
readLine(In,[Ta|T]) :-
	read_term(In,Ta,[]),
	( Ta == end_of_file -> !; readLine(In,T) ).

%% readFile File and returns list of lines L
readFile(File,L) :-
	open(File,read,In),
	readLine(In,L),
	close(In).

parseGDL(Text,L) :-
	writeFile('../data/streamIn', [ Text ]),
	exec(node('../exec.js', '../data/streamIn', '../data/streamOut')),
	readFile('../data/streamout', L).

%% returns a sequence of roles.
findroles(Game) :- true.

%% returns a sequence of propositions.
findpropositions(Game) :- true.

%% returns a sequence of actions for a specified role.
findactions(Role,Game) :- true.

%%  returns a sequence of all propositions that are true in the initial state.
findinits(Game) :- true.

%% returns the first action that is legal for the specified role in the specified state.
findlegalx(Role,State,Game) :- true.

%% returns a sequence of all actions that are legal for the specified role in the specified state.
findlegals(Role,State,Game) :- true.

%% returns a sequence of all propositions that are true in the state that results from the specified roles performing the specified move in the specified state.
findnext(Roles,Move,State,Game) :- true.

%% returns the goal value for the specified role in the specified state.
findreward(Role,State,Game) :- true.

%% returns a boolean indicating whether the specified state is terminal.
findterminalp(State,Game) :- true.
