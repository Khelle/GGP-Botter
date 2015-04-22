:- module(parametersReader, [parameters, parseParameters/0]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object parameters(port:integer).

:- dynamic(parameters).

parseParameters :-
	open('../config/parameters.json', read, File),
	json_read(File, Json),
	close(File),
	json_to_prolog(Json, Parameters),
	assert(Parameters).