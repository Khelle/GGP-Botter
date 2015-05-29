:- module(bot, [
    play/1
]).

:- use_module(game).
:- use_module(db).

play(Played) :-
    game:findMyRole(Role),
    game:findCurrentState(State),
    debug(request, 'Current state:~n~p', [State]),
    (
        game:findRandomLegal(Role, State, Played) ->
        !,
        logger:log('states', ['Found legal move:', Played]);
        Played = 'nil',
        logger:log('states', ['Found no move'])
    ).

start.
abort.
stop.