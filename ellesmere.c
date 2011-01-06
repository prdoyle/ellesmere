
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "parser.h"
#include "tokens.h"
#include "memory.h"
#include <stdarg.h>

static SymbolTable st;
static TokenStream tokenStream;
static Stack       stack;
//static Context     currentScope;
static ObjectHeap  heap;
static Parser      ps;
FILE *diagnostics;
FILE *parserGenTrace;

typedef struct tba_struct *TokenBlockArray;
#define AR_PREFIX  tba
#define AR_TYPE    TokenBlockArray
#define AR_ELEMENT TokenBlock
#define AR_BYVALUE
#include "array_template.h"

static TokenBlockArray productionBodies;

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

static void dumpStack()
	{
	fl_write( diagnostics, "    -- Stack: " );
	sk_sendTo( stack, diagnostics, heap );
	fl_write( diagnostics, "\n" );
	}

static void dumpParserState()
	{
	fl_write( diagnostics, "    -- Parser state: " );
	ps_sendStateTo( ps, diagnostics, heap, st );
	fl_write( diagnostics, "\n" );
	}

static void push( Object ob )
	{
	sk_push( stack, ob );
	dumpStack();
	ps_push( ps, ob );
	dumpParserState();
	}

static Object pop()
	{
	Object result = sk_pop( stack );
	dumpStack();
	ps_popN( ps, 1 );
	dumpParserState();
	return result;
	}

static void popN( int n )
	{
	sk_popN( stack, n );
	dumpStack();
	ps_popN( ps, n );
	dumpParserState();
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
	 
typedef void (*NativeAction)( Production handle, GrammarLine gl );

struct gl_struct
	{
	char *tokens[10];
	NativeAction action;
	int parm1;
	};
	 
static void nopAction( Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	popN( pn_length( handle, gr ) );
	push( oh_symbolToken( heap, pn_lhs( handle, gr ) ) );
	}

static void passThrough( Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	int depth = gl->parm1;
	Object result = sk_item( stack, depth );
	popN( pn_length( handle, gr ) );
	push( result );
	}

static void addAction( Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left + right, heap ) );
	}

static void subAction( Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left - right, heap ) );
	}

static void mulAction( Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left * right, heap ) );
	}

static void divAction( Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	push( ob_fromInt( left / right, heap ) );
	}

static void printAction( Production handle, GrammarLine gl )
	{
	ob_sendTo( sk_top( stack ), stdout, heap );
	printf("\n");
	nopAction( handle, gl );
	}

#if 0
static Symbol _wrappeeField = NULL;

static Symbol wrappeeField()
	{
	if( !_wrappeeField )
		_wrappeeField = sy_byName( ":", st );
	return _wrappeeField;
	}

static Object wrap( Object wrappee, Production handle )
	{
	Symbol tag = pn_lhs( handle, ps_grammar(ps) );
	Object result = ob_create( tag, heap );
	ob_setField( result, wrappeeField(), wrappee, heap );
	return result;
	}

static Object unwrap( Object wrapper )
	{
	return ob_getField( wrapper, wrappeeField(), heap );
	}

static void unwrapAction( Production handle, GrammarLine gl ) 
	{
	Object wrapped = sk_item( stack, gl->parm1 );
	popN( pn_length( handle, ps_grammar(ps) ) );
	push( unwrap( wrapped ) );
	}

#endif

static void recordTokenBlockAction( Production handle, GrammarLine gl );

static void stopRecordingTokenBlockAction( Production handle, GrammarLine gl )
	{
	assert(0); // Never actually gets called.  It's a kind of null terminator.
	}

static void defAction( Production handle, GrammarLine gl )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	int arity = popInt();
	Symbol token = popToken();
	popToken(); // "def" keyword

	// Define the nested grammar with this extra production
	Grammar gr = gr_nested( ps_grammar(ps), 1, ml_indefinite() );
	Symbol intToken = sy_byIndex( SYM_INT, st );
	Production pn = pn_new( gr, intToken, arity );
	pn_append( pn, token, gr );
	int i;
	for( i=0; i < arity; i++ )
		pn_append( pn, intToken, gr );
	pn_stopAppending( pn, gr );
	gr_stopAdding( gr );
	ps = ps_new( gr, st, ml_indefinite(), parserGenTrace );
	fl_write( diagnostics, "    NEW PARSER\n" );

	// Prime the parser state with the current stack contents
	for( i = sk_depth(stack) - 1; i >= 0; i-- )
		ps_push( ps, sk_item( stack, i ) );
	dumpParserState();

	// Store the TokenBlock body from the definition
	int pnIndex = pn_index( pn, gr );
	tba_setCount( productionBodies, pnIndex+1 );
	tba_set( productionBodies, pnIndex, block );
	}

static struct gl_struct grammar1[] =
	{
	{ { ":PROGRAM",       ":VOIDS", ":END_OF_INPUT"                 }, nopAction },
	{ { ":VOIDS",         ":VOID"                                   }, nopAction },
	{ { ":VOIDS",         ":VOIDS", ":VOID"                         }, nopAction },

	{ { ":TOKEN_BLOCK",   ":TB_START", ":VOIDS", "}"                }, stopRecordingTokenBlockAction },
	{ { ":TB_START",      "{",                                      }, recordTokenBlockAction },

	{ { ":VOID",          ":INT"                                    }, printAction },
	{ { ":VOID",          "def", ":TOKEN", ":INT", ":TOKEN_BLOCK"   }, defAction },
	{ { ":VOID",          "print", ":INT"                           }, printAction },

	{{NULL}},
	};

static struct gl_struct arithmetic1[] =
	{
	{ { ":INT",         ":INT", "+", ":INT" }, addAction },
	{ { ":INT",         ":INT", "-", ":INT" }, subAction },

	{{NULL}},
	};

static struct gl_struct arithmetic2[] =
	{
	{ { ":INT",         ":INT", "*", ":INT" }, mulAction },
	{ { ":INT",         ":INT", "/", ":INT" }, divAction },
	{ { ":INT",         "(", ":INT", ")" },    passThrough, 2 },
	{{NULL}},
	};

static GrammarLine initialGrammarNest[] = { grammar1, arithmetic1, arithmetic2 };

static Grammar populateGrammar( SymbolTable st )
	{
	Grammar gr = NULL;
	int i,j,k;
	for( i=0; i < asizeof( initialGrammarNest ); i++ )
		{
		GrammarLine *curArray = initialGrammarNest + i;
		if( gr )
			gr = gr_nested( gr, 20, ml_indefinite() );
		else
			gr = gr_new( sy_byName( (*curArray)[0].tokens[0], st ), 20, ml_indefinite() );
		for( j=0; (*curArray)[j].action; j++ )
			{
			Production pn = pn_new( gr, sy_byName( (*curArray)[j].tokens[0], st ), asizeof( (*curArray)[j].tokens ) );
			for( k=1; k < asizeof( (*curArray)[j].tokens ) && (*curArray)[j].tokens[k]; k++ )
				pn_append( pn, sy_byName( (*curArray)[j].tokens[k], st ), gr );
			pn_stopAppending( pn, gr );
			}
		}
	gr_stopAdding( gr );
	return gr;
	}

static GrammarLine lookupGrammarLine( Production pn, Grammar gr )
	{
	int depth = pn_nestDepth( pn, gr );
	Grammar definingGrammar = gr_outerNth( gr, depth );
	GrammarLine array = initialGrammarNest[ gr_nestDepth( gr ) - depth ];
	int index = pn_index( pn, gr );
	if( gr_outer( definingGrammar ) )
		index -= gr_numProductions( gr_outer( definingGrammar ) );
	return array + index;
	}

static void recordTokenBlockAction( Production handle, GrammarLine gl )
	{
	trace( diagnostics, "  Begin recording token block\n" );
	TokenBlock tb = tb_new( ml_undecided() );
	nopAction( handle, gl );
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	Grammar gr = ps_grammar( ps );
	ts_advance( tokenStream );
	while( ts_current( tokenStream ) )
		{
		Object ob     = ts_current( tokenStream );
		Object nextOb = ts_next( tokenStream );
		if( !nextOb )
			nextOb = endOfInput;
		trace( diagnostics, "# token from %p is ", tokenStream );
		ob_sendTo( ob, diagnostics, heap );
		trace( diagnostics, "; next is ");
		ob_sendTo( nextOb, diagnostics, heap );
		trace( diagnostics, "\n");
		push( ob );
		handle = ps_handle( ps, nextOb );
		while( handle )
			{
			trace( diagnostics, "    Recording handle production %d: ", pn_index( handle, gr ) );
			pn_sendTo( handle, diagnostics, gr, st );
			trace( diagnostics, "\n" );
			if( !tba_get( productionBodies, pn_index( handle, gr ) ) )
				{
				NativeAction action = lookupGrammarLine( handle, gr )->action;
				if( action == stopRecordingTokenBlockAction )
					goto done; // end marker
				}
			nopAction( handle, NULL );
			handle = ps_handle( ps, nextOb );
			}
		tb_append( tb, ob ); // If we get to here, we didn't hit stopRecordingTokenBlockAction
		trace( diagnostics, "# Recorded token " );
		ob_sendTo( ob, diagnostics, heap );
		trace( diagnostics, "\n");
		ts_advance( tokenStream );
		}
	done:
	tb_stopAppending( tb );
	popN( pn_length( handle, gr ) );
	push( ob_fromTokenBlock( tb, heap ) );
	trace( diagnostics, "    Stack after recording: " );
	sk_sendTo( stack, diagnostics, heap );
	trace( diagnostics, "\n" );
	}

int main( int argc, char **argv )
	{
	diagnostics = fdopen( 3, "wt" );
	parserGenTrace = fdopen( 4, "wt" );
	st = theSymbolTable();
	heap = theObjectHeap();
	Grammar gr = populateGrammar( st );
	productionBodies = tba_new( 20 + gr_numProductions( gr ), ml_indefinite() );
	tba_setCount( productionBodies, gr_numProductions( gr ) );
	ps = ps_new( gr, st, ml_indefinite(), parserGenTrace );
	trace( diagnostics, "Parser:\n" );
	ps_sendTo( ps, diagnostics, heap, st );
	trace( diagnostics, "\n" );
	stack = sk_new( ml_indefinite() );
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	tokenStream = theLexTokenStream( heap, st );
	while( ts_current( tokenStream ) )
		{
		Object ob     = ts_current( tokenStream );
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
		TokenBlock bodyToCall = NULL;
		while( handle )
			{
			gr = ps_grammar( ps ); // Grammar can change as the program proceeds
			trace( diagnostics, "  Found handle production %d in grammar %p: ", pn_index( handle, gr ), gr );
			pn_sendTo( handle, diagnostics, gr, st );
			trace( diagnostics, "\n" );
			bodyToCall = tba_get( productionBodies, pn_index( handle, gr ) );
			if( bodyToCall )
				{
				break;
				}
			else
				{
				GrammarLine line = lookupGrammarLine( handle, gr );
				line->action( handle, line );
				handle = ps_handle( ps, nextOb );
				}
			}
		ts_advance( tokenStream );
		if( bodyToCall )
			{
			assert( handle );
			popN( pn_length( handle, gr ) );
			tokenStream = ts_fromBlock( bodyToCall, heap, tokenStream );
			trace( diagnostics, "    Calling body for production %d token stream %p\n", pn_index( handle, gr ), tokenStream );
			}
		while( !ts_current( tokenStream ) && ts_caller( tokenStream ) )
			{
			tokenStream = ts_close( tokenStream ); // return
			trace( diagnostics, "  Returned to TokenStream %p\n", tokenStream );
			}
		}
#ifndef NDEBUG
	File memreport = fdopen( 5, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

