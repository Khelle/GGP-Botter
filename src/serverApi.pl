:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(threadutil)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(debug)).

:- use_module(requestDataParser).

:- json_object jRequest(param:text).

%% Routing:
:- http_handler(root(.), getRequest, []).

%% Action handlers

% INFO
handleRequest('INFO', Response) :-
	Response = 'AVAILABLE'.

% Unknown data
handleRequest(Data, Response) :-
	%% trace,
	atom(Data),
	Response = 'READY'.

%% Request handlers:
getRequest(Request) :-
	http_read_data(Request, Data, []),
	stripParenthesis(Data, ExtractedData),
	handleRequest(ExtractedData, Response),
    format('Content-type: text/acl~n~n', []),
    format(Response).

%% Data string operations
stripParenthesis(Data, Stripped) :-
	sub_string(Data, 2, _, 2, Substring),
	atom_string(Stripped, Substring).

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
	debug(http(request)),
	tspy(handleRequest/2).