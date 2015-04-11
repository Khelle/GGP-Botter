:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(debug)).

%%	Routing:
:- http_handler(/, getRequest, []).


%% Request handlers:
getRequest(_) :-
	format('Content-type: text/plain~n~n'),
    format('Hello World!~n').
	%% write(Request).

%% Server init
start :-
	enableDebug,
	runServer(9147).

runServer(Port) :-
	format('~nStarting player...~n'),
	http_server(http_dispatch, [port(Port)]),
	format('Player ready!~n~n').

%% Debugging hooks
enableDebug :-
	debug(http(request)).
	%% tspy(getRequest/1).