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

%{
/* These just trim down the size of the lexer */
%}
%option noyy_pop_state
%option noyy_top_state
%option noyy_scan_bytes
%option noyy_scan_string
%option noyyset_extra
%option noyyget_leng
%option noyyget_text
%option noyyset_lineno
%option noyyget_in
%option noyyset_in
%option noyyset_out
%option noyyget_lval
%option noyyset_lval
%option noyyset_lloc
%option noyyget_debug
%option noyyset_debug

DIGIT       [0-9]
START       [A-Za-z]
MIDDLE      [0-9A-Za-z_]
END         [!?]?
OPER        [~!@$%^&*-+=<>|?/:]
ALONE       [(){},.;]|\[|\]
WS          [ \t\r]

%%

{WS}+                 { ; }
#.*$                  { ; /* comment */ }
\n                    { ++_lineNumber; }

{START}{MIDDLE}*{END}   { setLastWord(yytext); return WORD; }
{OPER}+                 { setLastWord(yytext); return WORD; }
{OPER}+_{MIDDLE}*{END}  { setLastWord(yytext); return WORD; }
{OPER}+^{MIDDLE}*{END}  { setLastWord(yytext); return WORD; }
{ALONE}                 { setLastWord(yytext); return WORD; }
-                       { setLastWord(yytext); return WORD; }

\"[^"]*(\\\"[^"]*)*\" { setLastString(yytext); return STRING; }

{DIGIT}+              { _lastInt = atoi(yytext); return INT; }

.                     { setLastWord(yytext); return ERROR; }

%%

/*
// Can't use this because it will always be the longest match.  Stupid lex.
// .*                   { yylval.str = yytext;           return ERROR; }
*/
