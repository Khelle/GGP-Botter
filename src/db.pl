:- module(db, [
 	add/1,
 	addList/1,
 	remove/1,
    distinct/2,
    backupState/0,
    revertState/0
]).
:- dynamic(record/2).

%% helper functions for buidling knowledge representation
%% add term to knowledge
add(T) :-
	functor(T, N, A),
	dynamic(N/A),
	\+(T),
	!,
	assertz(record(N, A)),
 	assertz(T).

%% add multiple terms to knowledge
addList([]).
addList([H|T]) :-
	add(H),
 	addList(T),
 	!.

%% remove term(s) from knowledge
remove(T) :-
	functor(T, N, A),
	dynamic(N/A),
 	retractall(T).

%% erase database
erase.

%% game rules additions
setState(Propositions) :-
    retractall(db:true(_)),
    setStateLoop(Propositions).
setStateLoop([]).
setStateLoop([P|Propositions]) :-
    assertz(db:true(P)),
    setStateLoop(Propositions).

backupState :-
    retractall(db:bkpState(_)),
    forall(db:true(T), assertz(bkpState(T))).
revertState :-
    retractall(db:true(_)),
    forall(db:bkpState(T), assertz(true(T))).

setMove(Role, Move) :- assertz(does(Role, Move)).
clearMoves(Role) :- retractall(does(Role, _)).

distinct(X,Y) :- X \= Y.
