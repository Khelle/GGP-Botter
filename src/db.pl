:- module(db, [
 	add/1, remove/1, removeall/1
]).

%% operator definitions
&(X, Y) :-
 	call(X), call(Y).
  
~(X) :-
 	not(call(X)).

%% helper functions for buidling knowledge representation
%% add term to knowledge
add(T) :-
 	assertz(T).
  
%% remove term from knowledge
remove(T) :-
 	retract(T).
  
%% remove all terms with given head=H from knowledge
removeall(H) :-
 	retractall(H).
