:- module(bot, [
    play/1,
    start/0,
    abort/0,
    stop/0
]).

:- use_module(game).
:- use_module(db).

play(Played) :-
    game:findMyRole(Role),
    game:findCurrentState(State),
    (
        game:findRandomLegal(Role, State, Played) ->
        !
        %% , logger:log('states', ['Found legal move:', Played])
        ;
        Played = 'nil'
        %% , logger:log('states', ['Found no move'])

    ).

start.
abort.
stop.