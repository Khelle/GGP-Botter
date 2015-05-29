:- module(requestHandler, [
    handleStart/5,
    handlePlay/3,
    handleAbort/1,
    handleStop/2
]).

:- use_module(db).
:- use_module(rules).
:- use_module(state).
:- use_module(game).
:- use_module(logger).
:- use_module(library(debug)).

handleStart(GameId, Role, Rules, StartClock, PlayClock) :-
    resetGame,
    setGameInfo(GameId, Role, StartClock, PlayClock),
    rules:save(Rules),
    state:setInitial,
    bot:start.

handlePlay(_, Moves, Played) :-
    state:compute(Moves),
    bot:play(Moved),
    Moved =.. List,
    (length(List, 1) ->
        Played = Moved;
        atomic_list_concat(List, ' ', L1),
        atom_concat('( ', L1, L2), atom_concat(L2, ' )', Played)).

handleAbort(_) :-
    bot:abort.

handleStop(_, Moves) :-
    state:compute(Moves),
    bot:stop.

setGameInfo(GameId, Role, StartClock, PlayClock) :-
    db:add(game_game_id(GameId)),
    db:add(game_role(Role)),
    db:add(game_start_clock(StartClock)),
    db:add(game_play_clock(PlayClock)).

resetGame :-
    logger:clear('states'),
    db:erase.