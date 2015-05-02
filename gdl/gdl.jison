/**************************************************************************************************************************************************************
 * Project: GDL lexer and grammar
 * Authors: 
 * 	Kamil Jamróz
 *
 * ------------------------------------------------------------------------------------------------------------------------------------------------------------
 * Definitions
 *************************************************************************************************************************************************************/

%lex

%options flex case-sensitive

/**************************************************************************************************************************************************************
 * Included code
 *************************************************************************************************************************************************************/

%{

/* load all external modules */
ClassYYCodeGenerator  = require('./lib/compiler.ClassYYCodeGenerator.js').getInstance;
ClassYYTreeNode       = require('./lib/compiler.ClassYYTreeNode.js').getInstance;
ClassYYTree           = require('./lib/compiler.ClassYYTree.js').getInstance;
ClassYYDriver         = require('./lib/compiler.ClassYYDriver.js').getInstance;

/* Initialize objects */
YYCode    = new ClassYYCodeGenerator();
YYTree    = new ClassYYTree(new ClassYYTreeNode());
YYDriver  = new ClassYYDriver(YYTree);

%}

/**************************************************************************************************************************************************************
 * Rules
 *************************************************************************************************************************************************************/

%%

"//"(.*)                    /* skip comments */
\s+                         /* skip whitespaces */
"<="						{ return '<='; }
"("                         { return '('; }
")"                         { return ')'; }
\?[a-z][a-zA-Z0-9\.]*		{ return 'VARIABLE'; }
[a-z][a-zA-Z0-9\.]*			{ return 'CONSTANT'; }
[0-9]+(\.[0-9]+)?\b         { return 'NUMBER'; }
[A-Z][a-zA-Z0-9\.]*			{ return 'IDENTIFIER'; }
\'([^\']*?)\'               { return 'STRING'; }
<<EOF>>                     { return 'EOF'; }
.                           { YYDriver.LogError('Invalid character'); }

/**************************************************************************************************************************************************************
 * Subroutines
 *************************************************************************************************************************************************************/

/lex

/**************************************************************************************************************************************************************
 * Operators associations and precedence
 *************************************************************************************************************************************************************/

%start						Program


/**************************************************************************************************************************************************************
 * Language grammar
 *************************************************************************************************************************************************************/

%%

Atom
	: CONSTANT																		{ $$ = $1; }
	| VARIABLE																		{ $$ = $1.substring(1).replace(/\w\S*/g, function(txt){ return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase(); }); }
	| NUMBER																		{ $$ = $1; }
	| STRING																		{ $$ = $1; }
	;

FactParam
	: '(' Fact ')'																	{ $$ = $2; }
	| Atom 																			{ $$ = new ClassYYTreeNode([ 'ATOM', null, $1 ]); }
	;

FactParams
	: FactParam 																	{ $$ = $1; }
	| FactParams FactParam 															{ $$ = [ $2 ].concat($1); }
	;	

Fact
	: CONSTANT FactParams															{ $$ = new ClassYYTreeNode([ 'FACT', $1 ]).AddChildrenSet($2).SetJoin(','); }
	;

Facts 
	: '(' Fact ')' 																	{ $$ = $2; }
	| Facts '(' Fact ')' 															{ $$ = [ $3 ].concat($1); }
	;

Rule
	: '<=' '(' Fact ')' Facts														{ $$ = new ClassYYTreeNode([ 'RULE', null ]).AddHandles($3).AddChildrenSet($5).SetJoin(','); }
	| '<=' CONSTANT Facts 															{ $$ = new ClassYYTreeNode([ 'RULE', $2 ]).AddChildrenSet($3).SetJoin(','); }
	;

ClauseBody
	: Fact																			{ $$ = $1.StopFlag(); }
	| Rule 																			{ $$ = $1.StopFlag(); }
	;

Clause
	: '(' ClauseBody ')'															{ $$ = $2; }
	;

Clauses 
	: Clause 																		{ $$ = $1; }
	| Clauses Clause 																{ $$ = [ $2 ].concat($1); }
	;

ProgramBody 
	: IDENTIFIER IDENTIFIER CONSTANT '(' ')' NUMBER NUMBER							{ $$ = null; }
	| IDENTIFIER IDENTIFIER CONSTANT '(' Clauses ')' NUMBER NUMBER			  		{ $$ = $5; }
	;

Program
    : EOF                                                                         	{ YYTree.root.AddChildrenSet(null); return YYDriver.Log(); }
    | ProgramBody EOF                                                             	{ YYTree.root.AddChildrenSet($1);   return YYDriver.Log(); }
    ;
