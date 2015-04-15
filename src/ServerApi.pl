:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(debug)).

:- json_object jRequest(param:text).

%% Routing:
:- http_handler(/, getRequest, []).


%% Request handlers:
getRequest(Request) :-
	http_parameters(Request, [
		param(Param, [ optional(true), default('No param') ])
	]),
	prolog_to_json(jRequest(Param), Response),
	reply_json(Response).

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