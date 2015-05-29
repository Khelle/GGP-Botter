:- module('logger', [
    log/2,
    log/3,
    clear/1
]).

log(Channel, Messages) :-
    log(Channel, Messages, append).
log(Channel, Messages, Mode) :-
    getFile(Channel, File),
    writeFile(File, Mode, Messages).

clear(Channel) :-
    log(Channel, ['CLEARED'], write).

getFile(Channel, File) :-
    atomic_list_concat(['../log/', Channel, '.log'], File).

writeLine(_,[]).
writeLine(Out,[Ln|L]) :-
    write(Out,Ln),
    nl(Out),
    writeLine(Out,L),
    !.

%% write list of lines L into file File
writeFile(File,L) :-
    writeFile(File, write, L).
writeFile(File,Mode,L) :-
    open(File,Mode,Out),
    writeLine(Out,L),
    writeLine(Out, ['---------------------------------------------------']),
    close(Out).