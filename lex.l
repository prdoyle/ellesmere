%{

#include "lex.h"
#include <string.h>

static int   _lineNumber = 1;
static char *_lastString=NULL;
static int   _lastInt=0;

static void setLastString( const char *newString )
	{
	if (_lastString)
		free( _lastString );
	_lastString = strdup( newString );
	}

int         currentLineNumber() { return _lineNumber; }
const char *lastString()        { return _lastString; }
int         lastInt()           { return _lastInt; }
const char *lastWord()          { return _lastString; }

%}

%option   warn nodefault nounput noinput
%option   noyywrap
%option   nodebug

DIGIT       [0-9]
ALPHA       [A-Za-z$]
ALNUM       [0-9A-Za-z_]
OPER        [~!@#$%^&*-+=<>|:,.?/]
ALONE       [(){}]|\[|\]|[;]
WS          [ \t\r]

%%

{WS}+                 { ; }
\n                    { ++_lineNumber; }

{DIGIT}+              { _lastInt = atoi(yytext); return INT; }

{ALPHA}{ALNUM}*       { setLastString(yytext); return WORD; }
{OPER}+_{ALNUM}*      { setLastString(yytext); return WORD; }
{ALONE}               { setLastString(yytext); return WORD; }
{OPER}+               { setLastString(yytext); return WORD; }

\"[^"]*(\\\"[^"]*)*\" { setLastString(yytext); return STRING; }

.                     { setLastString(yytext); return ERROR; }

%%

/*
// Can't use this because it will always be the longest match.  Stupid lex.
// .*                   { yylval.str = yytext;           return ERROR; }
*/
