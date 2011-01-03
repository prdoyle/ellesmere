
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "parser.h"
#include "tokens.h"
#include "memory.h"
#include <stdarg.h>

static TokenStream tokenStream;
static Stack       stack;
//static Context     currentScope;
static ObjectHeap  heap;
static Parser      ps;
FILE *diagnostics;

#ifdef NDEBUG
#define trace(...)
#else
static int trace( FILE *file, const char *format, ... )
	{
	if( !file )
		return 0;

	int result;
	va_list args;
	va_start( args, format );
	result = fl_vwrite( file, format, args );
	va_end( args );
	return result;
	}
#endif

static void push( Object ob )
	{
	sk_push( stack, ob );
	ps_push( ps, ob );
	}

static Object pop()
	{
	ps_popN( ps, 1 );
	return sk_pop( stack );
	}

static int popInt()
	{
	Object popped = pop();
	assert( ob_isInt( popped, heap ) );
	return ob_toInt( popped, heap );
	}

static Symbol popToken()
	{
	Object popped = pop();
	assert( ob_isToken( popped, heap ) );
	return ob_toSymbol( popped, heap );
	}

#if 0
static Action eatUntilObject( Object target )
	{
	trace( diagnostics, "  eatUntilObject( " );
	ob_sendTo( target, diagnostics, heap );
	trace( diagnostics, " )\n");

	Object ob = ts_next( tokenStream );
	while( ob )
		{
		if( ob == target )
			break;
		else
			ob = ts_next( tokenStream );
		}
	return NULL;
	}
#endif

typedef struct gl_struct *GrammarLine;
	 
typedef void (*NativeAction)( Parser ps, Production handle, GrammarLine gl );

struct gl_struct
	{
	NativeAction action;
	char *tokens[10];
	int parm1;
	};
	 
static void nopAction( Parser ps, Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	ps_popN( ps, pn_length( handle, gr ) );
	ps_push( ps, oh_symbolToken( heap, pn_lhs( handle, gr ) ) );
	}

static void passThrough( Parser ps, Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	int depth = gl->parm1;
	Object result = sk_item( stack, depth );
	ps_popN( ps, pn_length( handle, gr ) );
	push( result );
	}

static void addAction( Parser ps, Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left + right, heap ) );
	}

static void subAction( Parser ps, Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left - right, heap ) );
	}

static void mulAction( Parser ps, Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left * right, heap ) );
	}

static void divAction( Parser ps, Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left / right, heap ) );
	}

static void printAction( Parser ps, Production handle, GrammarLine gl )
	{
	ob_sendTo( sk_top( stack ), stdout, heap );
	nopAction( ps, handle, gl );
	}

static struct gl_struct initialGrammar[] =
	{
	{ nopAction,     { "PROGRAM",      "STATEMENTS", ":END_OF_INPUT" } },

	{ nopAction,     { "STATEMENTS",   "STATEMENT" } },
	{ nopAction,     { "STATEMENTS",   "STATEMENTS", "STATEMENT" } },

	{ printAction,   { "STATEMENT",    ":INT" } },

	{ passThrough,   { ":INT",         "(", ":INT", ")" }, 2 },
	{ addAction,     { ":INT",         ":INT", "+", ":INT" } },
	{ subAction,     { ":INT",         ":INT", "-", ":INT" } },

	// TODO: Use a nested grammar for BEDMAS
	{ mulAction,     { ":INT",         ":INT", "*", ":INT" } },
	{ divAction,     { ":INT",         ":INT", "/", ":INT" } },
	};

static Grammar populateGrammar( SymbolTable st )
	{
	Grammar gr = gr_new( sy_byName( initialGrammar[0].tokens[0], st ), asizeof( initialGrammar ), ml_indefinite() );
	int i,j;
	for( i=0; i < asizeof( initialGrammar ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( initialGrammar[i].tokens[0], st ), asizeof( initialGrammar[i].tokens ) );
		for( j=1; j < asizeof( initialGrammar[i].tokens ) && initialGrammar[i].tokens[j]; j++ )
			pn_append( pn, sy_byName( initialGrammar[i].tokens[j], st ), gr );
		pn_stopAppending( pn, gr );
		}
	gr_stopAdding( gr );
	return gr;
	}

int main( int argc, char **argv )
	{
	diagnostics = fdopen( 3, "wt" );
	SymbolTable st = theSymbolTable();
	heap = theObjectHeap();
	Grammar gr = populateGrammar( st );
	ps = ps_new( gr, st, ml_indefinite(), diagnostics );
	stack = sk_new( ml_indefinite() );
	tokenStream = theLexTokenStream( heap, st );
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	Object ob = ts_next( tokenStream );
	while( ob != endOfInput )
		{
		Object nextOb = ts_next( tokenStream );
		if( !nextOb )
			nextOb = endOfInput;
		if( diagnostics )
			{
			trace( diagnostics, "# Token from %p is ", tokenStream );
			ob_sendTo( ob, diagnostics, heap );
			trace( diagnostics, "; next is ");
			ob_sendTo( nextOb, diagnostics, heap );
			trace( diagnostics, "\n");
			}
		push( ob );
		Production handle = ps_handle( ps, nextOb );
		while( handle )
			{
			trace( diagnostics, "  Found handle production %d: ", pn_index( handle, gr ) );
			pn_sendTo( handle, diagnostics, gr, st );
			trace( diagnostics, "\n" );
			assert( pn_index( handle, gr ) < asizeof( initialGrammar ) );
			GrammarLine line = initialGrammar + pn_index( handle, gr );
			NativeAction action = line->action;
			action( ps, handle, line );
			handle = ps_handle( ps, nextOb );
			}
		if( diagnostics )
			{
			trace( diagnostics, "  ");
			sk_sendTo( stack, diagnostics, heap );
			trace( diagnostics, "\n  ");
#if 0
			cx_sendTo( currentScope, diagnostics );
			trace( diagnostics, "\n");
#endif
			}
		ob = nextOb;
		}
#ifndef NDEBUG
	File memreport = fdopen( 4, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

