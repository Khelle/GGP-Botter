:- module(bot, [
    play/1
]).

:- use_module(game).
:- use_module(db).

play(Played) :-
    game:findMyRole(Role),
    game:findCurrentState(State),
    debug(request, 'Current state:~n~p', [State]),
    game:findFirstLegal(Role, State, Played);
    Played = 'nil'.