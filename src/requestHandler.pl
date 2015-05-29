:- module(requestHandler, [
    handleStart/5
]).

:- use_module(db).
:- use_module(rules).
:- use_module(state).
:- use_module(game).
:- use_module(library(debug)).

handleStart(GameId, Role, Rules, StartClock, PlayClock) :-
    debug(request, 'Handle start', []),
    setGameInfo(GameId, Role, StartClock, PlayClock),
    rules:save(Rules),
    state:setInitial,
    debug(request, 'Set initial state', []).

handlePlay(_, Moves, Played) :-
    debug(request, 'Handle play', []),
    state:compute(Moves),
    bot:play(Moved),
    Moved =.. List,
    (length(List, 1) ->
        Played = Moved;
        atomic_list_concat(List, ' ', L1),
        atom_concat('( ', L1, L2), atom_concat(L2, ' )', Played)).

setGameInfo(GameId, Role, StartClock, PlayClock) :-
    db:add(game_game_id(GameId)),
    db:add(game_role(Role)),
    db:add(game_start_clock(StartClock)),
    db:add(game_play_clock(PlayClock)).