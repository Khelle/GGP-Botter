:- module(db, [
 	add/1,
 	addList/1,
 	remove/1,
    init/0,
    distinct/2
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
init :-
    setInitialState,
    setDoes.

setInitialState :- forall(db:base(T), assertz(db:true(T))).

setDoes :- forall(db:legal(Role, Action), assertz(db:does(Role, Action))).

distinct(X,Y) :- X \= Y.
