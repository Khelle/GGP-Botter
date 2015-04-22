:- module(requestDataParser, [parseRequest/3]).

parseRequest(RequestData, Action, Data) :-
	stripParenthesis(RequestData, StrippedData),
	getAction(StrippedData, Action),
	getDataList(Action, StrippedData, Data).

stripParenthesis(Data, Stripped) :-
	sub_string(Data, 2, _, 2, Substring),
	atom_string(Stripped, Substring).

getAction(Data, Action) :- atomic_list_concat([Action|_], ' ', Data).

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
	atomic_list_concat(RulesList, ' ', Rules).

getRulesAndClocks([StartClock, PlayClock], [], StartClock, PlayClock).
getRulesAndClocks([DataHead | DataTail], [DataHead|RulesList], StartClock, PlayClock) :-
	getRulesAndClocks(DataTail, RulesList, StartClock, PlayClock).