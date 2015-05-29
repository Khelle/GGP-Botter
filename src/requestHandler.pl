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
    %% debug(request, 'Rules: ~n~p', [Rules]),
    rules:save(Rules),
    state:setInitial,
    debug(request, 'Set initial state', []).

handlePlay(_, Moves, Played) :-
    debug(request, 'Handle play', []),
    state:compute(Moves),
    %% bot:play(P),
    bot:play(Moved),
    Moved =.. List,
    (length(List, 1) ->
        Played = Moved;
        atomic_list_concat(List, ' ', L1),
        atom_concat('( ', L1, L2), atom_concat(L2, ' )', Played)).

setGameInfo(GameId, Role, StartClock, PlayClock) :-
    db:add(game:game_id(GameId)),
    db:add(game:role(Role)),
    db:add(game:start_clock(StartClock)),
    db:add(game:play_clock(PlayClock)).