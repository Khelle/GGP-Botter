:- module(gdlParser, [
	gdlPrefixToInfix/2,
	gdlPrefixLinesToInfixLines/2,
	testParse/1
	]).

%% gdl_prefix_to_infix(+Prefix, -Infix)
gdlPrefixToInfix(Prefix, Infix) :-
	string(Prefix), !,
	string_chars(Prefix, PrefixChars),
	gdlPrefixToInfix(PrefixChars, Infix).
gdlPrefixToInfix(Prefix, Infix) :-
	tokenize(Prefix, PrefixTokens),
	phrase(parse(Infix), PrefixTokens).

%% same as above but with lists of lines
gdlPrefixLinesToInfixLines(PrefixLines, InfixLines) :-
	is_list(PrefixLines),
	atomic_list_concat(PrefixLines, "\n", PrefixTerm),
	atom_string(PrefixTerm, Prefix),
	gdlPrefixToInfix(Prefix, Infix),
	split_string(Infix, "\n", "", InfixLines).

%% Tokenizer
tokenize([], []) :-  !.
tokenize(['('|Chars], ['('|Tokens]) :- !, tokenize(Chars, Tokens).
tokenize([')'|Chars], [')'|Tokens]) :- !, tokenize(Chars, Tokens).
tokenize([Space|Chars], Tokens) :- char_type(Space, space), !,
	tokenize(Chars, Tokens).
tokenize(['<','='|Chars], [':-'|Tokens]) :- !, tokenize(Chars, Tokens).
tokenize(['?'|Chars], [Variable|Tokens]) :- !,
	variableToken(Chars, VariableCharsList, Rest),
	atomic_list_concat(VariableCharsList, '', Variable),
	tokenize(Rest, Tokens).
tokenize([Char|Chars], [Atom|Tokens]) :- char_type(Char, alnum), !,
	atomToken([Char|Chars], AtomCharsList, Rest),
	atomic_list_concat(AtomCharsList, '', Atom),
	tokenize(Rest, Tokens).
variableToken([Char|Chars], [UpperCaseChar|LowerCaseChars], Rest) :-
	char_type(Char, alnum), !,
	upcase_atom(Char, UpperCaseChar),
	atomToken(Chars, LowerCaseChars, Rest).
atomToken([Char|Chars], [Char|AtomRest], Rest) :-
	char_type(Char, alnum), !,
	atomToken(Chars, AtomRest, Rest).
atomToken(Chars, [], Chars).

%% Parser
parse(GDL) --> ['('], gdlClauses(Clauses), [')'], { atomic_list_concat(Clauses, '\n', GDLraw), removeQuotes(GDLraw, GDL) }.
gdlClauses([C|Cs]) --> gdlClause(C), gdlClauses(Cs).
gdlClauses([]) --> [].
gdlClause(C) --> gdlFact(C).
gdlClause(C) --> gdlRule(C).
gdlFact(F) --> sExpression(X), {atomic_concat(X,'.',F)}.
gdlRule(R) --> ['('], [':-'], gdlRuleHead(H), gdlRuleBody(B), [')'], { atomic_list_concat([H, ':-', B], ' ', Br), atomic_concat(Br, '.', R)}.
gdlRuleHead(H) --> sExpression(H).
gdlRuleBody(B) --> sSequence(X), { atomic_list_concat(X, ', ', B) }.

sExpression(X) --> sAtom(X).
sExpression(X) --> sList(N), {A =.. N, term_string(A, X)}.
sList(N) --> ['('], sSequence(N), [')'].
sSequence([]) --> [].
sSequence([H|T]) --> sExpression(H), sSequence(T).
sAtom(N) --> [N], {atom(N),N \= '(', N \= ')', N \= ':-' }.

%% Utility to clean useless quotation signs from output
removeQuotes([], []) :- !.
removeQuotes([Quote|T], NT) :- char_type(Quote, quote), !, removeQuotes(T, NT).
removeQuotes([H|T], [H|NT]) :- !, removeQuotes(T, NT).
removeQuotes(String, NewString) :-
	string_chars(String, Chars),
	removeQuotes(Chars, NewChars),
	string_chars(NewString, NewChars).

%% Test
testParse(InfixLines) :- split_string("
	(( role robot )
	( base ( cell a )
	) ( base ( cell b ) )
	( base ( cell c ) ) ( base ( cell
	d ) ) ( base ( gold a ) )
	( base ( gold b ) ) ( base
	( gold c ) ) ( base ( gold d )
	) ( base ( gold i ) ) ( base ( step 1 )
	) ( <= ( base ( step ?x ) ) ( succ ?y ?x ) )
	( input robot move ) ( input robot grab )
	( input robot drop ) ( init ( cell a ) )
	( init ( gold c ) ) ( init ( step 1 ) )
	( <= ( next ( cell ?y ) ) ( does robot move )
	( true ( cell ?x ) ) ( adjacent ?x ?y ) )
	( <= ( next ( cell ?x ) ) ( does robot grab )
	( true ( cell ?x ) ) ) ( <= ( next ( cell ?x )
	) ( does robot drop ) ( true ( cell ?x ) ) )
	( <= ( next ( gold ?x ) ) ( does robot move
	) ( true ( gold ?x ) ) ) ( <= ( next
	( gold i ) ) ( does robot grab )
	( true ( cell ?x ) ) ( true
	( gold ?x ) ) ) ( <= (
	next ( gold i ) ) (
	does robot grab ) ( true ( gold i
	) ) ) ( <= ( next ( gold ?y )
	) ( does robot grab )
	( true ( cell ?x ) )
	( true ( gold ?y ) )
	( distinct ?x ?y ) ) ( <= ( next ( gold ?x ) )
	( does robot drop ) ( true ( cell ?x ) ) ( true
	( gold i ) ) ) ( <= ( next
	( gold ?x ) ) ( does robot drop )
	( true ( gold ?x ) ) ( distinct ?x i ) ) ( <= ( next
	( step ?y ) ) ( true ( step ?x ) ) ( succ ?x ?y ) )
	( adjacent a b ) ( adjacent b c ) ( adjacent c d )
	( adjacent d a ) ( succ 1 2 ) ( succ 2 3 ) ( succ 3 4 )
	( succ 4 5 ) ( succ 5 6 ) ( succ 6 7 ) ( succ 7 8 )
	( succ 8 9 ) ( succ 9 10 ) ( <= ( legal robot move ) ( succ 1 2 ) )
	( <= ( legal robot grab ) ( true ( cell ?x ) ) ( true ( gold ?x ) ) )
	( <= ( legal robot drop ) ( true ( gold i ) ) ) ( <= ( goal robot 100 )
	( true ( gold a ) ) ) ( <= ( goal robot 0 ) ( true ( gold ?x ) )
	( distinct ?x a ) ) ( <= terminal ( true
	( step 10 ) ) ) ( <= terminal ( true ( gold a ) ) ) )", "\n", "", TestLines),
		gdlPrefixLinesToInfixLines(TestLines, InfixLines).
