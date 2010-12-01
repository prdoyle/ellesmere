%{

#include "lex.h"
#include <string.h>

static int   _lineNumber = 1;
static char *_lastString=NULL;
static int   _lastInt=0;

static void setLastWord( const char *newString )
	{
	if (_lastString)
		free( _lastString );
	_lastString = strdup( newString );
	}

static void setLastString( const char *newString )
	{
	if (_lastString)
		free( _lastString );
	_lastString = strdup( newString+1 );
	_lastString[ strlen(_lastString) - 1 ] = 0;
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
OPER        [~!@$%^&*-+=<>|:,.?/]
ALONE       [(){}]|\[|\]|[;]
WS          [ \t\r]

%%

{WS}+                 { ; }
#.*$                  { ; /* comment */ }
\n                    { ++_lineNumber; }

-?{DIGIT}+            { _lastInt = atoi(yytext); return INT; }

{ALPHA}{ALNUM}*       { setLastWord(yytext); return WORD; }
{OPER}+_{ALNUM}*      { setLastWord(yytext); return WORD; }
{ALONE}               { setLastWord(yytext); return WORD; }
{OPER}+               { setLastWord(yytext); return WORD; }

\"[^"]*(\\\"[^"]*)*\" { setLastString(yytext); return STRING; }

.                     { setLastWord(yytext); return ERROR; }

%%

/*
// Can't use this because it will always be the longest match.  Stupid lex.
// .*                   { yylval.str = yytext;           return ERROR; }
*/
