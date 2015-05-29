:- module(state, [
    setInitial/0,
    compute/1,
    backupState/0,
    revertState/0,
    setState/1,
    setMove/2,
    clearMoves/1
]).

:- use_module(db).
:- use_module(game).
:- use_module(gdlParser).
:- use_module(logger).

:- use_module(library(debug)).

%% sets initial state
setInitial :-
    game:findInitialState(State),
    logger:log('states', ['Initializing...']),
    saveState(State).

%% computes new state basing on moves performed by players
compute('NIL') :-
    !,
    debug(request, 'Received moves: ~p', ['NIL']),
    logger:log('states', ['Received moves:'|['NIL']]).
compute(Moves) :-
    !,
    logger:log('states', ['Computing...']),
    parseMoves(Moves, MovesList),
    debug(request, 'Received moves: ~p', [MovesList]),
    logger:log('states', ['Received moves:'|MovesList]),
    game:findCurrentState(CurrentState),
    game:findRoles(Roles),
    game:findNext(Roles, MovesList, CurrentState, NextState),
    saveState(NextState),
    debug(request, 'Computed new state...', []).

parseMoves(InfixMoves, MovesList) :-
    atom_string(InfixMoves, StringMoves),
    split_string(StringMoves, "\n", "", M),
    gdlParser:gdlPrefixLinesToInfixLines(M, ML),
    maplist(prepareMove, ML, MovesList).

prepareMove(In, Out) :-
    split_string(In, "", ".", [Dotless]),
    atom_string(Out, Dotless).

saveState(State) :-
    logger:log('states', State),
    setState(State),
    backupState.

%% saves and backups the new state in db
setState(State) :-
    db:remove(true(_)),
    setStateLoop(State).
setStateLoop([]).
setStateLoop([P|Propositions]) :-
    db:add(true(P)),
    setStateLoop(Propositions).

%% backups the current state
backupState :-
    db:remove(state:bkpState(_)),
    forall(db:true(T), db:add(state_bkpState(T))).

%% reverts to the backed-up state
revertState :-
    db:remove(true(_)),
    forall(state_bkpState(T), db:add(true(T))).

%% adds player's moves to the db
setMove(Role, Move) :- db:add(does(Role, Move)).

%% clears player's moves from the db
clearMoves(Role) :- db:remove(does(Role, _)).