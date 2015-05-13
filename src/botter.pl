:- module(botter, [
	findRoles/1,
	findPropositions/1,
	findActions/2,
	findInitialState/1,
	findCurrentState/1,
	findFirstLegal/3,
	findAllLegal/3,
	findNext/4,
	findReward/3,
	isTerminal/1,
	writeLine/2,
	writeFile/2,
	readLine/2,
	readFile/2,
	parseGDL/2
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
	exec(node('../gdl/exec.js', '../gdl', '../data/streamIn', '../data/streamOut')),
	readFile('../data/streamout', L).

saveRules(Rules) :-
	parseGDL(Rules, L),
	db:add(L).

%% sets initial state
setInitialState :-
	findInitialState(State),
	updateState(State).

%% computes new state basing on moves performed by players
computeState(Moves) :-
	findCurrentState(CurrentState),
	findRoles(Roles),
	findNext(Roles, Moves, CurrentState, NextState),
	updateState(NextState).

%% saves and backups the new state in db
updateState(State) :-
	db:setState(State),
	db:backupState.

%% returns a sequence of roles.
findRoles(Roles) :- setof(Role, db:role(Role), Roles).

%% returns a sequence of propositions.
findPropositions(Propositions) :- findall(Base, db:base(Base), Propositions).

%% returns a sequence of actions for a specified role.
findActions(Role, Actions) :- setof(Action, db:input(Role, Action), Actions).

%%  returns a sequence of all propositions that are true in the initial state.
findInitialState(State) :- findall(Init, db:init(Init), State).

%% returns a sequence of all propositions that are true in the current state.
findCurrentState(State) :- findall(Init, db:true(Init), State).

%% returns the first action that is legal for the specified role in the specified state.
findFirstLegal(Role, State, Action) :- findAllLegal(Role, State, [Action|_]).

%% returns a sequence of all actions that are legal for the specified role in the specified state.
findAllLegal(Role, State, Actions) :- callWithState(State, setof(Action, db:legal(Role, Action), Actions)).

%% returns a sequence of all propositions that are true in the state that results from the specified roles performing specified moves in the specified state.
findNext(Roles, Moves, State, Next) :- callWithState(State, findNextLoop(Roles, Moves, Next)).
findNextLoop([], [], Next) :- setof(P, db:next(P), Next).
findNextLoop([R|Roles], [M|Moves], Next) :-
	db:setMove(R, M),
	findNextLoop(Roles, Moves, Next),
	db:clearMoves(R).

%% returns the goal value for the specified role in the specified state.
findReward(Role, State, Reward) :- callWithState(State, db:goal(Role, Reward)).

%% returns a boolean indicating whether the specified state is terminal.
isTerminal(State) :- callWithState(State, db:terminal).

callWithState(State, T) :-
	db:setState(State),	% afterwards we MUST revert to original state!
	call(T),		% call succeeded?
	db:revertState, % yes - clean up...
	!; 				% and return true
	db:revertState, % no - clean up...
	fail.			% and return false