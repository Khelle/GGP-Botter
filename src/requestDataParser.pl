request('START Test.maze.1429717965125 robot (( role robot ) ( base ( cell a ) ) ( base ( cell b ) ) ( base ( cell c ) ) ( base ( cell d ) ) ( base ( gold a ) ) ( base ( gold b ) ) ( base ( gold c ) ) ( base ( gold d ) ) ( base ( gold i ) ) ( base ( step 1 ) ) ( <= ( base ( step ?x ) ) ( succ ?y ?x ) ) ( input robot move ) ( input robot grab ) ( input robot drop ) ( init ( cell a ) ) ( init ( gold c ) ) ( init ( step 1 ) ) ( <= ( next ( cell ?y ) ) ( does robot move ) ( true ( cell ?x ) ) ( adjacent ?x ?y ) ) ( <= ( next ( cell ?x ) ) ( does robot grab ) ( true ( cell ?x ) ) ) ( <= ( next ( cell ?x ) ) ( does robot drop ) ( true ( cell ?x ) ) ) ( <= ( next ( gold ?x ) ) ( does robot move ) ( true ( gold ?x ) ) ) ( <= ( next ( gold i ) ) ( does robot grab ) ( true ( cell ?x ) ) ( true ( gold ?x ) ) ) ( <= ( next ( gold i ) ) ( does robot grab ) ( true ( gold i ) ) ) ( <= ( next ( gold ?y ) ) ( does robot grab ) ( true ( cell ?x ) ) ( true ( gold ?y ) ) ( distinct ?x ?y ) ) ( <= ( next ( gold ?x ) ) ( does robot drop ) ( true ( cell ?x ) ) ( true ( gold i ) ) ) ( <= ( next ( gold ?x ) ) ( does robot drop ) ( true ( gold ?x ) ) ( distinct ?x i ) ) ( <= ( next ( step ?y ) ) ( true ( step ?x ) ) ( succ ?x ?y ) ) ( adjacent a b ) ( adjacent b c ) ( adjacent c d ) ( adjacent d a ) ( succ 1 2 ) ( succ 2 3 ) ( succ 3 4 ) ( succ 4 5 ) ( succ 5 6 ) ( succ 6 7 ) ( succ 7 8 ) ( succ 8 9 ) ( succ 9 10 ) ( <= ( legal robot move ) ( succ 1 2 ) ) ( <= ( legal robot grab ) ( true ( cell ?x ) ) ( true ( gold ?x ) ) ) ( <= ( legal robot drop ) ( true ( gold i ) ) ) ( <= ( goal robot 100 ) ( true ( gold a ) ) ) ( <= ( goal robot 0 ) ( true ( gold ?x ) ) ( distinct ?x a ) ) ( <= terminal ( true ( step 10 ) ) ) ( <= terminal ( true ( gold a ) ) ) ) 10 ').

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