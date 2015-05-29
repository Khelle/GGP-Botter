#!swipl -f -q

:- initialization run.

:- use_module(server).

run :-
    current_prolog_flag(argv, [Port, Bot]),
    atom_number(Port, NPort),
    server:start(NPort, Bot).