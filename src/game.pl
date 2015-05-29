:- module(game, [
    findGameId/1,
    findMyRole/1,
    findStartClock/1,
    findPlayClock/1,
    findRoles/1,
    findPropositions/1,
    findActions/2,
    findInitialState/1,
    findCurrentState/1,
    findFirstLegal/3,
    findAllLegal/3,
    findNext/4,
    findReward/3,
    isTerminal/1
]).

:- use_module(db).
:- use_module(state).

:- dynamic(game_id/1).
:- dynamic(role/1).
:- dynamic(start_clock/1).
:- dynamic(play_clock/1).

findGameId(GameId) :- game_id(GameId).

findMyRole(Role) :- role(Role).

findStartClock(Clock) :- start_clock(Clock).

findPlayClock(Clock) :- play_clock(Clock).

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
    debug(request, 'Roles: ~p~nMoves: ~p', [[R|Roles], [M|Moves]]),
    state:setMove(R, M),
    findNextLoop(Roles, Moves, Next),
    state:clearMoves(R).

%% returns the goal value for the specified role in the specified state.
findReward(Role, State, Reward) :- callWithState(State, db:goal(Role, Reward)).

%% returns a boolean indicating whether the specified state is terminal.
isTerminal(State) :- callWithState(State, db:terminal).

callWithState(State, T) :-
    state:setState(State), % afterwards we MUST revert to original state!
    call(T),        % call succeeded?
    state:revertState, % yes - clean up...
    !;              % and return true
    state:revertState, % no - clean up...
    fail.           % and return false