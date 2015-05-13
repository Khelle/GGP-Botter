:- module(requestData, [parseRequest/2]).

%% Parses request data into atoms
% Data 		- a list of parameter strings:
% 				[Action, GameId, Role, Rules, StartClock, PlayClock, Move]
% 					Action 	- a textual atom: ['INFO', 'START', 'ABORT', 'PLAY', 'STOP']
parseRequest(RequestData, Data) :-
	stripParenthesis(RequestData, 2, StrippedData),
	getAction(StrippedData, Action),
	getDataList(Action, StrippedData, Data).

%% Remove parenthesis from request string (eg. "( INFO )" --> "INFO")
stripParenthesis(Data, Chars, Stripped) :-
	sub_string(Data, Chars, _, Chars, Substring),
	atom_string(Stripped, Substring).

%% Retrieve Action from Data string
getAction(Data, Action) :- atomic_list_concat([Action|_], ' ', Data).

%% Retrieve parameters list from Data string
getDataList('INFO', _, ['INFO']).
getDataList('ABORT', Data, [Action, GameId]) :- atomic_list_concat([Action, GameId], ' ', Data).
getDataList('PLAY', Data, [Action, GameId, Move]) :-
	atomic_list_concat([Action, GameId | Tail], ' ', Data),
	atomic_list_concat(Tail, ' ', Move).
getDataList('STOP', Data, [Action, GameId, Move]) :-
	atomic_list_concat([Action, GameId | Tail], ' ', Data),
	atomic_list_concat(Tail, ' ', Move).
getDataList('START', Data, [Action, GameId, Role, Rules, StartClock, PlayClock]) :-
	atomic_list_concat([Action, GameId, Role | DataTail], ' ', Data),
	getRulesAndClocks(DataTail, RulesList, StartClock, PlayClock),
	atomic_list_concat(RulesList, ' ', RulesString),
	stripParenthesis(RulesString, 1, Rules).

%% Returns list of rules parts and game clocks
getRulesAndClocks([StartClock, PlayClock], [], StartClock, PlayClock).
getRulesAndClocks([DataHead | DataTail], [DataHead|RulesList], StartClock, PlayClock) :-
	getRulesAndClocks(DataTail, RulesList, StartClock, PlayClock).