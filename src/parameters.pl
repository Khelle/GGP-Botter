:- module(parameters, [
    parse/0
]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object params(bots_dir:atom).

:- dynamic(params/1).

parse :-
	open('../config/parameters.json', read, File),
	json_read(File, Json),
	close(File),
	json_to_prolog(Json, Params),
	assert(Params).