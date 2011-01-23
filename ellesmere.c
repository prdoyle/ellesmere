
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "parser.h"
#include "tokens.h"
#include "memory.h"
#include <stdarg.h>
#include <string.h>

static SymbolTable st;
static TokenStream tokenStream;
static Stack       stack;
static Context     curContext;
static ObjectHeap  heap;
static Parser      ps;
FILE *diagnostics;
FILE *conflictLog;
FILE *parserGenTrace;

typedef struct cf_struct *CallFrame;
struct cf_struct
	{
	Parser ps;
	Stack  stack;
	};

typedef struct cs_struct *CallStack;
#define AR_PREFIX  cs
#define AR_TYPE    CallStack
#define AR_ELEMENT struct cf_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define cs_new( size, ml ) cs_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static CallStack callStack;
static int callStackDepth = 0;

static void cf_push()
	{
	assert( cs_count( callStack ) >= 1 );
	CallFrame cf = cs_element( callStack, callStackDepth );
	cf->ps = ps;
	cf->stack = stack;
	callStackDepth += 1;
	if( callStackDepth >= cs_count(callStack) )
		{
		trace( diagnostics, "Growing call stack to depth=%d\n", callStackDepth );
		cf = cs_nextElement( callStack );
		cf->stack = sk_new( ml_indefinite() );
		}
	else
		{
		cf = cs_element( callStack, callStackDepth );
		sk_popN( cf->stack, sk_depth( cf->stack ) );
		}
	ps = cf->ps = ps_new( ps_automaton(ps), ml_indefinite(), diagnostics );
	stack = cf->stack;
	}

static void cf_pop()
	{
	assert( callStackDepth >= 1 );
	CallFrame cf = cs_element( callStack, --callStackDepth );
	ps_close( ps );
	ps    = cf->ps;
	stack = cf->stack;
	}

struct fn_struct
	{
	Production production;
	TokenBlock body;
	};

typedef struct fna_struct *FunctionArray;
#define AR_PREFIX  fna
#define AR_TYPE    FunctionArray
#define AR_ELEMENT Function
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define fna_new( size, ml ) fna_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static FunctionArray productionBodies;

static void dumpStack()
	{
	if( !diagnostics )
		return;

	trace( diagnostics, "    -- Stack: " );
	sk_sendTo( stack, diagnostics, heap );
	int i;
	for( i = callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( callStack, i );
		trace( diagnostics, "\n%*s", 14, "" );
		sk_sendTo( cf->stack, diagnostics, heap );
		}
	trace( diagnostics, "\n" );
	}

static void dumpParserState()
	{
	if( !diagnostics )
		return;

	trace( diagnostics, "    -- Parser state: " );
	ps_sendTo( ps, diagnostics, heap, st );
	int i;
	for( i = callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( callStack, i );
		trace( diagnostics, "\n%*s", 21, "" );
		ps_sendTo( cf->ps, diagnostics, heap, st );
		}
	trace( diagnostics, "\n" );
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
	ps_popN( ps, 1 );
	return result;
	}

static void popN( int n )
	{
	sk_popN( stack, n );
	ps_popN( ps, n );
	}

static int popInt()
	{
	Object popped = pop();
	assert( ob_isInt( popped, heap ) );
	return ob_toInt( popped, heap );
	}

static void pushToken( int symbolIndex )
	{
	push( oh_symbolToken( heap, sy_byIndex( symbolIndex, st ) ) );
	}

static Symbol popToken()
	{
	Object popped = pop();
	assert( ob_isToken( popped, heap ) );
	return ob_toSymbol( popped, heap );
	}

static void closeTokenStreamsAsNecessary()
	{
	while( !ts_current( tokenStream ) && ts_caller( tokenStream ) )
		{
		tokenStream = ts_close( tokenStream );
		cx_restore( curContext );
		trace( diagnostics, "  Returned to TokenStream %p\n", tokenStream );
		}
	}

typedef struct gl_struct *GrammarLine;
	 
typedef void (*NativeAction)( Production handle, GrammarLine gl );

struct gl_struct
	{
	char *tokens[10];
	struct
		{
		NativeAction action;
		int parm1;
		} response;
	ConflictResolutions cr;
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
	int depth = gl->response.parm1;
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
	int left;
	if( gl->response.parm1 == 2 )
		left = popInt();
	else
		left = 0;
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
	Object wrapped = sk_item( stack, gl->response.parm1 );
	popN( pn_length( handle, ps_grammar(ps) ) );
	push( unwrap( wrapped ) );
	}

#endif

static void recordTokenBlockAction( Production handle, GrammarLine gl );

static void stopRecordingTokenBlockAction( Production handle, GrammarLine gl )
	{
	assert(0); // Never actually gets called.  It's a kind of null terminator.
	}

static void parseTreeAction( Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	Symbol lhs = pn_lhs( handle, gr );
	Object result = ob_create( lhs, heap );
	int i;
	for( i = pn_length(handle, gr) - 1; i >= 0; i-- )
		{
		Symbol field = pn_name( handle, i, gr );
		if( !field )
			field = pn_token( handle, i, gr );
		Object value = pop();
		ob_setField( result, field, value, heap );
		}
	push( result );
	}

static void addProductionAction( Production handle, GrammarLine gl )
	{
	parseTreeAction( handle, gl );
	Object production = sk_top( stack );
	Symbol sym_result = sy_byName( "result", st );
	Symbol sym_parms  = sy_byName( "parms",  st );
	Symbol sym_next   = sy_byName( "next",   st );
	Symbol sym_tag    = sy_byName( "tag",    st );
	Symbol sym_name   = sy_byName( "name",   st );

	// Define the nested grammar with this extra production
	Grammar gr = gr_nested( ps_grammar(ps), 1, ml_indefinite() );
	Production pn = pn_new( gr, ob_toSymbol( ob_getField( production, sym_result, heap ), heap ), 3 );
	Object parm;
	for(
		parm = ob_getField( production, sym_parms, heap);
		ob_hasField( parm, sym_tag, heap );
		parm = ob_getField( parm, sym_next, heap ) )
		{
		Symbol tag  = ob_toSymbol( ob_getField( parm, sym_tag,  heap ), heap );
		if( ob_hasField( parm, sym_name, heap ) )
			pn_appendWithName( pn, ob_toSymbol( ob_getField( parm, sym_name, heap ), heap ), tag, gr );
		else
			pn_append( pn, tag, gr );
		}
	pn_stopAppending( pn, gr );
	gr_stopAdding( gr );
	ps_close( ps );
	ps = ps_new( au_new( gr, st, ml_indefinite(), conflictLog, parserGenTrace ), ml_indefinite(), diagnostics );
	trace( diagnostics, "    NEW PARSER\n" );

	// Prime the parser state with the current stack contents
	int i;
	for( i = sk_depth(stack) - 1; i >= 0; i-- )
		ps_push( ps, sk_item( stack, i ) );
	dumpParserState();

	// Build a context with a symbol for each named parameter
	cx_save( curContext );
	for( i = 0; i < pn_length( pn, gr ); i++ )
		{
		Symbol name  = pn_name( pn, i, gr );
		Symbol tag   = pn_token( pn, i, gr );
		if( name )
			sy_setValue( name, oh_symbolToken( heap, tag ), curContext );
		}

	// Stuff the production index into the :PRODUCTION object so caller can get it
	ob_setField( production,
		sy_byName( "index", st ),
		ob_fromInt( pn_index( pn, gr ), heap ), heap );
	}

static void defAction( Production handle, GrammarLine gl )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	popToken(); // "as" keyword
	Object production = pop();
	popToken(); // "def" keyword
	push( oh_symbolToken( heap, pn_lhs( handle, ps_grammar(ps) ) ) );

	// Pop the context built for the def's production
	cx_restore( curContext );

	// Store the body from the definition
	int pnIndex = ob_toInt( ob_getField( production, sy_byName( "index", st ), heap ), heap );
	Function fn = (Function)ml_alloc( ml_indefinite(), sizeof(*fn) );
	fn->production = gr_production( ps_grammar(ps), pnIndex );
	fn->body = block;
	fna_setCount( productionBodies, pnIndex+1 );
	fna_set( productionBodies, pnIndex, fn );
	}

static void returnAction( Production handle, GrammarLine gl )
	{
	Object result = pop();
	popToken();
	tokenStream = ts_close( tokenStream );
	push( oh_symbolToken( heap, pn_lhs( handle, ps_grammar(ps) ) ) );
	cx_restore( curContext );
	cf_pop();
	trace( diagnostics, "  Returned to TokenStream %p\n", tokenStream );
	push( result );
	}

static void nonzeroAction( Production handle, GrammarLine gl )
	{
	int value = popInt();
	if( value )
		pushToken( SYM_TRUE );
	else
		pushToken( SYM_FALSE );
	}

static void leAction( Production handle, GrammarLine gl )
	{
	int right = popInt();
	popToken();
	int left = popInt();
	if( left <= right )
		pushToken( SYM_TRUE );
	else
		pushToken( SYM_FALSE );
	}

static struct gl_struct grammar1[] =
	{
	{ { ":PROGRAM",         ":VOIDS", ":END_OF_INPUT"                 }, { nopAction } },
	{ { ":VOIDS",           ":VOID",                                  }, { nopAction } },
	{ { ":VOIDS",           ":VOIDS", ":VOID"                         }, { nopAction } },

	{ { ":STATEMENT_BLOCK", "{", ":VOIDS", "}"                        }, { nopAction } },
	{ { ":STATEMENT_BLOCK", "{",           "}"                        }, { nopAction } },

	{ { ":VOID",            "print", ":INT"                           }, { printAction } },

	{ { ":VOID",            "return", ":INT"                          }, { returnAction } },
	{ { ":VOID",            "return", ":VOID"                         }, { returnAction } },

	{ { ":PARAMETER_LIST"                                             }, { parseTreeAction } },
	{ { ":PARAMETER_LIST",  ":TOKEN@tag",      ":PARAMETER_LIST@next" }, { parseTreeAction } },
	{ { ":PARAMETER_LIST",  ":TOKEN@tag", "@", ":TOKEN@name", ":PARAMETER_LIST@next"  }, { parseTreeAction } },
	{ { ":PRODUCTION",      ":TOKEN@result", ":PARAMETER_LIST@parms"  }, { addProductionAction } },
 	{ { ":TOKEN_BLOCK",     ":TB_START", ":VOIDS", "}"                }, { stopRecordingTokenBlockAction } },
 	{ { ":TOKEN_BLOCK",     ":TB_START",           "}"                }, { stopRecordingTokenBlockAction } },
 	{ { ":TB_START",        "{",                                      }, { recordTokenBlockAction } },
	{ { ":VOID",            "def", ":PRODUCTION", "as", ":TOKEN_BLOCK" }, { defAction } },

	{ { ":FALSE",           ":INT"                                    }, { nonzeroAction } },
	{ { ":FALSE",           ":INT", "<=", ":INT"                      }, { leAction } },

	{{NULL}},
	};

static struct gl_struct booleans1[] =
	{
	{ { ":TRUE",           ":INT"                                    }, { nonzeroAction } },
	{ { ":TRUE",           ":INT", "<=", ":INT"                      }, { leAction } },

	{{NULL}},
	};

static struct gl_struct arithmetic1[] =
	{
	{ { ":INT",         ":INT", "+", ":INT" }, { addAction    }, CR_SHIFT_BEATS_REDUCE },
	{ { ":INT",         ":INT", "-", ":INT" }, { subAction, 2 }, CR_SHIFT_BEATS_REDUCE },
	{ { ":INT",                 "-", ":INT" }, { subAction, 1 }, CR_SHIFT_BEATS_REDUCE },

	{{NULL}},
	};

static struct gl_struct arithmetic2[] =
	{
	{ { ":INT",         ":INT", "*", ":INT" }, { mulAction }, CR_SHIFT_BEATS_REDUCE },
	{ { ":INT",         ":INT", "/", ":INT" }, { divAction }, CR_SHIFT_BEATS_REDUCE },
	{ { ":INT",         "(", ":INT", ")" },    { passThrough, 1 } },
	{{NULL}},
	};

static GrammarLine initialGrammarNest[] = { grammar1, booleans1, arithmetic1, arithmetic2 };

static Grammar populateGrammar( SymbolTable st )
	{
	Grammar gr = NULL;
	int i,j,k;
	for( i=0; i < asizeof( initialGrammarNest ); i++ )
		{
		GrammarLine *curArray = initialGrammarNest + i;
		if( gr )
			gr = gr_nested( gr, 5, ml_indefinite() );
		else
			gr = gr_new( sy_byName( (*curArray)[0].tokens[0], st ), 20, ml_indefinite() );
		for( j=0; (*curArray)[j].response.action; j++ )
			{
			Production pn = pn_new( gr, sy_byName( (*curArray)[j].tokens[0], st ), asizeof( (*curArray)[j].tokens ) );
			pn_setConflictResolution( pn, (*curArray)[j].cr, gr );
			for( k=1; k < asizeof( (*curArray)[j].tokens ) && (*curArray)[j].tokens[k]; k++ )
				{
				char *token = (*curArray)[j].tokens[k];
				char *at = strchr( token, '@' );
				if( at != NULL && at != token )
					{
					char *tag = (char*)ml_alloc( ml_indefinite(), at - token + 1 );
					memcpy( tag, token, at-token );
					tag[ at-token ] = 0;
					pn_appendWithName( pn, sy_byName( at+1, st ), sy_byName( tag, st ), gr );
					}
				else
					{
					pn_append( pn, sy_byName( token, st ), gr );
					}
				}
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
	int stopDepth = ps_depth( ps );
	TokenBlock tb = tb_new( ml_undecided() );
	Grammar gr = ps_grammar( ps );
	nopAction( handle, gl );
	handle = ps_handle( ps, ts_current( tokenStream ) );
	if( handle )
		{
		// Assume it's an empty token block
		nopAction( handle, NULL );
		push( ts_current( tokenStream ) );
		ts_advance( tokenStream );
		// FIXME: There could be multiple reduces required to hit the stopRecordingTokenBlockAction.
		// Should really just reduce this like other handles and let it happen naturally
		}
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	while( ts_current( tokenStream ) )
		{
		Object ob     = cx_filter( curContext, ts_current( tokenStream ), heap );
		Object nextOb = cx_filter( curContext, ts_next( tokenStream )   , heap );
		if( !nextOb )
			nextOb = endOfInput;
		if( diagnostics )
			{
			trace( diagnostics, "token from %p is ", tokenStream );
			ob_sendTo( ob, diagnostics, heap );
			trace( diagnostics, "\n  next is ");
			ob_sendTo( nextOb, diagnostics, heap );
			trace( diagnostics, "\n");
			}
		push( ob );
		ts_advance( tokenStream );
		handle = ps_handle( ps, nextOb );
		while( handle )
			{
			if( diagnostics )
				{
				trace( diagnostics, "    Recording handle production %d: ", pn_index( handle, gr ) );
				pn_sendTo( handle, diagnostics, gr, st );
				trace( diagnostics, "\n" );
				}
			if(   pn_index( handle, gr ) < fna_count( productionBodies ) // recursive calls won't yet have a body defined
				&& !fna_get( productionBodies, pn_index( handle, gr ) ) )
				{
				NativeAction action = lookupGrammarLine( handle, gr )->response.action;
				int depthWithoutHandle = ps_depth( ps ) - pn_length( handle, gr );
				if( action == stopRecordingTokenBlockAction && depthWithoutHandle < stopDepth )
					{
					trace( diagnostics, "    # Found the stopRecording handle\n" );
					goto done; // end marker
					}
				}
			nopAction( handle, NULL );
			// tokenStream may have changed!
			Object nextOb = cx_filter( curContext, ts_current( tokenStream ), heap );
			if( !nextOb )
				nextOb = endOfInput;
			handle = ps_handle( ps, nextOb );
			}
		tb_append( tb, ob ); // If we get to here, we didn't hit stopRecordingTokenBlockAction
		trace( diagnostics, "    Recorded token " );
		ob_sendTo( ob, diagnostics, heap );
		trace( diagnostics, "\n");
		}
	done:
	tb_stopAppending( tb );
	popN( pn_length( handle, gr ) );
	push( ob_fromTokenBlock( tb, heap ) );
	if( diagnostics )
		{
		trace( diagnostics, "    Stack after recording: " );
		sk_sendTo( stack, diagnostics, heap );
		trace( diagnostics, "\n" );
		}
	}

int main( int argc, char **argv )
	{
	conflictLog = stderr;
	diagnostics = fdopen( 3, "wt" );    trace( diagnostics,    "# Ellesmere diagnostics\n" );
	parserGenTrace = fdopen( 5, "wt" ); trace( parserGenTrace, "# Ellesmere parserGenTrace\n" );
	st = theSymbolTable();
	heap = theObjectHeap();
	curContext = cx_new( st );
	callStack = cs_new( 30, ml_indefinite() );
	cs_setCount( callStack, 1 );
	Grammar initialGrammar = populateGrammar( st );
	productionBodies = fna_new( 20 + gr_numProductions( initialGrammar ), ml_indefinite() );
	fna_setCount( productionBodies, gr_numProductions( initialGrammar ) );
	ps = ps_new( au_new( initialGrammar, st, ml_indefinite(), conflictLog, parserGenTrace ), ml_indefinite(), diagnostics );
	stack = sk_new( ml_indefinite() );
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	tokenStream = theLexTokenStream( heap, st );
	while( ts_current( tokenStream ) )
		{
		Object ob     = cx_filter( curContext, ts_current( tokenStream ), heap );
		Object nextOb = cx_filter( curContext, ts_next( tokenStream )   , heap );
		if( !nextOb )
			nextOb = endOfInput;
		if( diagnostics )
			{
			trace( diagnostics, "token from %p is ", tokenStream );
			ob_sendTo( ob, diagnostics, heap );
			trace( diagnostics, "\n  next is ");
			ob_sendTo( nextOb, diagnostics, heap );
			trace( diagnostics, "\n");
			}
		push( ob );
		Production handle = ps_handle( ps, nextOb );
		ts_advance( tokenStream );
		while( handle )
			{
			Grammar gr = ps_grammar( ps ); // Grammar can change as the program proceeds
			if( diagnostics )
				{
				trace( diagnostics, "    # Found handle production %d in grammar %p: ", pn_index( handle, gr ), gr );
				pn_sendTo( handle, diagnostics, gr, st );
				trace( diagnostics, "\n" );
				}
			Function functionToCall = fna_get( productionBodies, pn_index( handle, gr ) );
			if( functionToCall )
				{
				assert( handle );
				closeTokenStreamsAsNecessary(); // tail call optimization?
				cx_save( curContext );
				int i;
				for( i = pn_length( handle, gr ) - 1; i >= 0; i-- )
					sy_setValue( pn_token( handle, i, gr ), pop(), curContext );
				tokenStream = ts_fromBlock( functionToCall->body, heap, tokenStream );
				cf_push();
				trace( diagnostics, "    Calling body for production %d token stream %p\n", pn_index( handle, gr ), tokenStream );
				handle = NULL;
				}
			else
				{
				GrammarLine line = lookupGrammarLine( handle, gr );
				line->response.action( handle, line );
				// tokenStream may have changed!
				Object nextOb = cx_filter( curContext, ts_current( tokenStream ), heap );
				if( !nextOb )
					nextOb = endOfInput;
				if( diagnostics )
					{
					trace( diagnostics, "  next is now ");
					ob_sendTo( nextOb, diagnostics, heap );
					trace( diagnostics, "\n");
					}
				handle = ps_handle( ps, nextOb );
				}
			}
		closeTokenStreamsAsNecessary();
		}
#ifndef NDEBUG
	File memreport = fdopen( 6, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

