:- module(botter, [
	start/0,
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
:- use_module(db).

%% TODO remove this!!!
:- style_check(-singleton).

%% operator declarations
:- op(950,xfy,&).
:- op(500,fy,~).

%% operator definitions
&(X, Y) :-
 	call(X), call(Y).
  
~(X) :-
 	not(call(X)).

%% start program and test db module
start :-
	db:add(kobieta(kasia)),
	db:add(kobieta(justyna)),
	db:add(mezczyzna(kamil)),
	db:add(mezczyzna(rafal)),
	db:add(programuje(kasia)),
	db:add(programuje(kamil)),
	db:add(osoba(X) :- kobieta(X); mezczyzna(X)),
	db:add(przedmiot(X) :- ~osoba(X)),
	db:add(programista(X) :- programuje(X) & mezczyzna(X)),
	db:add(programistka(X) :- programuje(X) & kobieta(X)).

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
