:- module(db, [
 	add/1,
 	addList/1,
 	remove/1,
 	erase/1
]).
:- dynamic(record/2).
:- dynamic(does/2).
:- dynamic(game_play_clock/1).
:- dynamic(game_start_clock/1).
:- dynamic(game_game_id/1).
:- dynamic(game_role/1).

%% helper functions for buidling knowledge representation
%% add term to knowledge
add(T) :-
	functor(T, N, A),
	(dynamic(N/A)),
	\+(T),
	!,
	assertz(T),
	(record(N, A) -> true; assertz(record(N, A))).

%% add multiple terms to knowledge
addList([]).
addList([H|T]) :-
	add(H),
 	addList(T),
 	!.

%% remove term(s) from knowledge
remove(T) :-
	functor(T, N, A),
	(dynamic(N/A)),
 	retractall(T),
 	functor(Ts, N, A),
 	findall(_, Ts, L),
 	(length(L, 0) -> (retract(record(N, A)) -> !,true); true).

%% erase database
removeFacts([]).
removeFacts([H|T]) :-
	H = [X, Y],
	abolish(X/Y),
	removeFacts(T),
	!.

erase :-
	findall([X,Y], record(X,Y), L),
	removeFacts(L),
	abolish(record/2),
	(dynamic(record/2)).

%% helper predicate - required to interpret the rules
distinct(X,Y) :- 
	X \= Y.