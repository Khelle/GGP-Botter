:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).

:- use_module(requestDataParser).

%% Routing:
:- http_handler(root(.), getRequest, []).

%% Server init - entry point to application
start :- runServer(9147).

%% Action handlers (currently doing nothing)
handleRequest('INFO', _, Response) :- Response = 'available'.
handleRequest('START', _, Response) :- Response = 'ready'.
handleRequest('PLAY', _, Response) :- Response = 'nil'.
handleRequest('PLAY', _, Response) :- Response = 'ready'.
handleRequest('ABORT', _, Response) :- Response = 'aborted'.

% Do we want to handle this case?
handleRequest('PREVIEW', _, Response) :- Response = 'done'.

% Unknown data
handleRequest(_, _, Response) :- Response = 'nil'.

%% Request handlers:
getRequest(Request) :-
	http_read_data(Request, RequestData, []),
	requestDataParser:parseRequest(RequestData, Action, Data),
	handleRequest(Action, Data, Response),
	formatResponse(Response).
    
formatResponse(Response) :-
	format('Content-type: text/acl~n~n'),
    format(Response).

runServer(Port) :-
	format('~nStarting player...~n'),
	http_server(http_dispatch, [port(Port)]),
	format('Player ready!~n~n').