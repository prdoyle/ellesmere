
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
static InheritanceRelation ir;
FILE *programTrace;            // Follow user program step-by-step
FILE *interpreterDiagnostics;  // High-level messages describing unusual events in the interpreter
FILE *interpreterTrace;        // Follow interpreter step-by-step
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

#undef CALL_STACK

static void cf_push()
	{
#ifdef CALL_STACK
	assert( cs_count( callStack ) >= 1 );
	CallFrame cf = cs_element( callStack, callStackDepth );
	cf->ps = ps;
	cf->stack = stack;
	callStackDepth += 1;
	if( callStackDepth >= cs_count(callStack) )
		{
		trace( interpreterTrace, "Growing call stack to depth=%d\n", callStackDepth );
		cf = cs_nextElement( callStack );
		cf->stack = sk_new( ml_indefinite() );
		}
	else
		{
		cf = cs_element( callStack, callStackDepth );
		sk_popN( cf->stack, sk_depth( cf->stack ) );
		}
	ps = cf->ps = ps_new( ps_automaton(ps), ml_indefinite(), parserGenTrace );
	stack = cf->stack;
#endif
	}

static void cf_pop()
	{
#ifdef CALL_STACK
	assert( callStackDepth >= 1 );
	CallFrame cf = cs_element( callStack, --callStackDepth );
	ps_close( ps );
	ps    = cf->ps;
	stack = cf->stack;
#endif
	}

typedef struct gl_struct *GrammarLine;
typedef void (*NativeAction)( Production handle, GrammarLine gl );

struct fn_struct
	{
	Production production;
	enum {
		FN_NATIVE,
		FN_TOKEN_BLOCK,
	} kind;
	union {
		TokenBlock   tb;
		NativeAction na;
	} body;
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

static FunctionArray productionBodiesDuringExecution;

static void dumpStack0( File fl ) __attribute__((noinline));
static void dumpStack0( File fl )
	{
	trace( fl, "    -- Stack: " );
	sk_sendNTo( stack, 5+ps_reduceContextLength( ps, heap, st ), fl, heap );
	int i;
	for( i = callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( callStack, i );
		trace( fl, "\n%*s", 14, "" );
		sk_sendNTo( cf->stack, 5+ps_reduceContextLength( cf->ps, heap, st ), fl, heap );
		}
	trace( fl, "\n" );
	}

static inline void dumpStack( File fl )
	{
	if( fl )
		dumpStack0( fl );
	}

static void dumpParserState0( File fl ) __attribute__((noinline));
static void dumpParserState0( File fl )
	{
	trace( fl, "    -- Parser state: " );
	ps_sendTo( ps, fl, heap, st );
	int i;
	for( i = callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( callStack, i );
		trace( fl, "\n%*s", 21, "" );
		ps_sendTo( cf->ps, fl, heap, st );
		}
	trace( fl, "\n" );
	}

static inline void dumpParserState( File fl )
	{
	if( fl )
		dumpParserState0( fl );
	}

static void dumpTokenStream0( File fl ) __attribute__((noinline));
static void dumpTokenStream0( File fl )
	{
	trace( fl, "    -- Token stream: " );
	ts_sendTo( tokenStream, fl );
	trace( fl, "\n" );
	}

static inline void dumpTokenStream( File fl )
	{
	if( fl )
		dumpTokenStream0( fl );
	}

static void dumpStuff( File fl )
	{
	dumpStack( fl );
	dumpParserState( fl );
	dumpTokenStream( fl );
	}

static void push( Object ob )
	{
	trace( interpreterTrace, "push(" );
	ob_sendTo( ob, interpreterTrace, heap );
	trace( interpreterTrace, ")\n" );
	sk_push( stack, ob );
	ps_push( ps, ob );
	dumpStuff( interpreterTrace );
	}

static Object pop()
	{
	Object result = sk_pop( stack );
	ps_popN( ps, 1 );
	dumpStuff( interpreterTrace );
	return result;
	}

static void popN( int n )
	{
	sk_popN( stack, n );
	ps_popN( ps, n );
	dumpStuff( interpreterTrace );
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
	while( !ts_current( tokenStream ) && ts_curBlock( tokenStream ) )
		{
		ts_pop( tokenStream );
		cx_restore( curContext );
		trace( programTrace, "  Returned to TokenBlock %p\n", ts_curBlock( tokenStream ) );
		}
	}

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
	popToken();
	int right = popInt();
	int left = popInt();
	push( ob_fromInt( left + right, heap ) );
	}

static void subAction( Production handle, GrammarLine gl )
	{
	popToken();
	int right = popInt();
	int left;
	if( gl->response.parm1 == 2 )
		left = popInt();
	else
		left = 0;
	push( ob_fromInt( left - right, heap ) );
	}

static void mulAction( Production handle, GrammarLine gl )
	{
	popToken();
	int right = popInt();
	int left = popInt();
	push( ob_fromInt( left * right, heap ) );
	}

static void divAction( Production handle, GrammarLine gl )
	{
	popToken();
	int right = popInt();
	int left = popInt();
	push( ob_fromInt( left / right, heap ) );
	}

static void printAction( Production handle, GrammarLine gl )
	{
	int depth = gl->response.parm1;
	ob_sendTo( sk_item( stack, depth ), stdout, heap );
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
	Production pn = pn_new( gr, ob_getTokenField( production, sym_result, heap ), 3 );
	pn_setConflictResolution( pn, gl->response.parm1, gr );
	Object parm;
	for(
		parm = ob_getField( production, sym_parms, heap);
		ob_getField( parm, sym_tag, heap );
		parm = ob_getField( parm, sym_next, heap ) )
		{
		Symbol tag  = ob_getTokenField( parm, sym_tag,  heap );
		if( ob_getField( parm, sym_name, heap ) )
			pn_appendWithName( pn, ob_getTokenField( parm, sym_name, heap ), tag, gr );
		else
			pn_append( pn, tag, gr );
		}
	pn_stopAppending( pn, gr );
	gr_stopAdding( gr );
	ps_close( ps );
	Automaton au = au_new( gr, st, ir, ml_indefinite(), conflictLog, parserGenTrace );
	ps = ps_new( au, ml_indefinite(), parserGenTrace );
	trace( interpreterDiagnostics, "    NEW PARSER\n" );

	// Prime the parser state with the current stack contents
	int i;
	for( i = sk_depth(stack) - 1; i >= 0; i-- )
		ps_push( ps, sk_item( stack, i ) );
	dumpParserState( interpreterTrace );

	// Build a context with a symbol for each named parameter
	cx_save( curContext );
	for( i = 0; i < pn_length( pn, gr ); i++ )
		{
		Symbol name  = pn_name( pn, i, gr );
		Symbol tag   = pn_token( pn, i, gr );
		if( name )
			{
			trace( interpreterTrace, "    -- bound %s to token %s\n", sy_name( name, st ), sy_name( tag, st ) );
			sy_setValue( name, oh_symbolToken( heap, tag ), curContext );
			}
		}

	// Stuff the production index into the PRODUCTION object so caller can get it
	ob_setIntField(
		production,
		sy_byName( "index", st ),
		pn_index( pn, gr ),
		heap );
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
	int pnIndex = ob_getIfIntField( production, sy_byName( "index", st ), -123, heap );
	Function fn = (Function)ml_alloc( ml_indefinite(), sizeof(*fn) );
	fn->production = gr_production( ps_grammar(ps), pnIndex );
	fn->kind       = FN_TOKEN_BLOCK;
	fn->body.tb    = block;
	fna_setCount( productionBodiesDuringExecution, pnIndex+1 );
	fna_set( productionBodiesDuringExecution, pnIndex, fn );
	}

static void returnAction( Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	int depth = gl->response.parm1;
	Object result = sk_item( stack, depth );
	popN( pn_length( handle, gr ) );
	ts_pop( tokenStream );
	push( oh_symbolToken( heap, pn_lhs( handle, ps_grammar(ps) ) ) );
	cx_restore( curContext );
	cf_pop();
	trace( programTrace, "  Returned to TokenBlock %p\n", ts_curBlock( tokenStream ) );
	push( result );
	}

static void nonzeroAction( Production handle, GrammarLine gl )
	{
	popToken();
	int value = popInt();
	if( value )
		pushToken( SYM_TRUE );
	else
		pushToken( SYM_FALSE );
	}

static void leAction( Production handle, GrammarLine gl )
	{
	popToken();
	int right = popInt();
	int left = popInt();
	if( left <= right )
		pushToken( SYM_TRUE );
	else
		pushToken( SYM_FALSE );
	}

static void setAction( Production handle, GrammarLine gl )
	{
	popToken();
	Symbol name = popToken();
	Object rhs = pop();
	sy_setValue( name, rhs, curContext );
	push( oh_symbolToken( heap, pn_lhs( handle, ps_grammar(ps) ) ) );
	}

static struct gl_struct grammar1[] =
	{
	{ { "PROGRAM",         "VOIDS", "END_OF_INPUT"                  }, { nopAction } },
	{ { "VOIDS",           "VOID",                                  }, { nopAction } },
	{ { "VOIDS",           "VOIDS", "VOID"                          }, { nopAction } },

	{ { "STATEMENT_BLOCK", "{", "VOIDS", "}"                        }, { nopAction } },
	{ { "STATEMENT_BLOCK", "{",          "}"                        }, { nopAction } },

	{ { "INT",      "{", "VOIDS", "INT",   "}"                      }, { passThrough, 1 } },
	{ { "INT",      "{",          "INT",   "}"                      }, { passThrough, 1 } },
	{ { "TRUE",     "{", "VOIDS", "TRUE",  "}"                      }, { passThrough, 1 } },
	{ { "TRUE",     "{",          "TRUE",  "}"                      }, { passThrough, 1 } },
	{ { "FALSE",    "{", "VOIDS", "FALSE", "}"                      }, { passThrough, 1 } },
	{ { "FALSE",    "{",          "FALSE", "}"                      }, { passThrough, 1 } },

	{ { "VOID",     "{", "VOIDS", "}"                               }, { nopAction } },
	{ { "VOID",     "{",          "}"                               }, { nopAction } },

	{ { "VOID",     "INT", "print!"                                 }, { printAction, 1 } },

	{ { "VOID",     "INT",  "return!",                              }, { returnAction, 1 } },
	{ { "VOID",     "VOID", "return!",                              }, { returnAction, 1 } },

	{ { "VOID",     "INT@value", "TOKEN@name", "set!"               }, { setAction } },

	{ { "PARAMETER_LIST"                                            }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",      "PARAMETER_LIST@next"  }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",  "@", "TOKEN@name", "PARAMETER_LIST@next"  }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@name", ":", "TOKEN@tag",  "PARAMETER_LIST@next"  }, { parseTreeAction } },
	{ { "PRODUCTION",      "TOKEN@result", "PARAMETER_LIST@parms"   }, { addProductionAction } },
	{ { "PRODUCTION",      "l2r", "TOKEN@result", "PARAMETER_LIST@parms"   }, { addProductionAction, CR_REDUCE_BEATS_SHIFT } },
 	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "}"                 }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START",          "}"                 }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "INT", "}"          }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "INT", "}"                   }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "TRUE", "}"         }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "TRUE", "}"                  }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "FALSE", "}"        }, { stopRecordingTokenBlockAction } },
 	{ { "TOKEN_BLOCK",     "TB_START", "FALSE", "}"                 }, { stopRecordingTokenBlockAction } },
 	{ { "TB_START",        "{",                                     }, { recordTokenBlockAction } },
	{ { "VOID",            "def", "PRODUCTION", "as", "TOKEN_BLOCK" }, { defAction } },

	{ { "INT",             "INT", "INT", "add!"                     }, { addAction } },
	{ { "INT",             "INT", "INT", "sub!"                     }, { subAction, 2 } },
	{ { "INT",             "INT", "INT", "mul!"                     }, { mulAction } },
	{ { "INT",             "INT", "INT", "div!"                     }, { divAction } },

	{ { "FALSE",           "INT", "nz!"                             }, { nonzeroAction } },
	{ { "FALSE",           "INT", "INT", "le!"                      }, { leAction } },

	{{NULL}},
	};

static struct gl_struct booleans1[] =
	{
	{ { "TRUE",            "INT", "nz!"                             }, { nonzeroAction } },
	{ { "TRUE",            "INT", "INT", "le!"                      }, { leAction } },

	{{NULL}},
	};

static GrammarLine initialGrammarNest[] = { grammar1, booleans1 };

static struct gl_struct inheritance[] =
	{
	{{ "BOOLEAN",    "FALSE", "TRUE" }},

	{{NULL}},
	};

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

static InheritanceRelation initialIR( ObjectHeap heap, SymbolTable st, MemoryLifetime ml )
	{
	int superIndex, subIndex;
	InheritanceRelation result = ir_new( heap, st, ml );
	for( superIndex = 0; inheritance[ superIndex ].tokens[0]; superIndex++ )
		{
		GrammarLine gl = inheritance + superIndex;
		Symbol super = sy_byName( gl->tokens[0], st );
		for( subIndex = 1; gl->tokens[ subIndex ]; subIndex++ )
			{
			Symbol sub = sy_byName( gl->tokens[ subIndex ], st );
			ir_add( result, super, sub );
			}
		}
	return result;
	}

static GrammarLine lookupGrammarLine( Production pn, Grammar gr )
	{
	int depth = pn_nestDepth( pn, gr );
	Grammar definingGrammar = gr_outerNth( gr, depth );
	if( gr_nestDepth( definingGrammar ) >= asizeof( initialGrammarNest ) )
		return NULL;

	GrammarLine array = initialGrammarNest[ gr_nestDepth( definingGrammar ) ];
	int index = pn_index( pn, gr );
	if( gr_outer( definingGrammar ) )
		index -= gr_numProductions( gr_outer( definingGrammar ) );
	return array + index;
	}

static void mainParsingLoop( TokenBlock recording, FunctionArray productionBodies )
	{
	Object endOfInput = oh_symbolToken( heap, sy_byIndex( SYM_END_OF_INPUT, st ) );
	int startingDepth = ps_depth( ps );

	if( interpreterTrace )
		{
		trace( interpreterTrace, "Starting mainParsingLoop( " );
		if( recording )
			tb_sendTo( recording, interpreterTrace, heap );
		else
			trace( interpreterTrace, "NULL" );
		trace( interpreterTrace, ", %p ) startingDepth=%d\n", productionBodies, startingDepth );
		}

	while( 1 )
		{
		Production handle;
		for (
			handle = ps_handle( ps, cx_filter( curContext, ts_current( tokenStream ), endOfInput, heap ) );
			handle;
			handle = ps_handle( ps, cx_filter( curContext, ts_current( tokenStream ), endOfInput, heap ) )
			){
			Grammar gr = ps_grammar( ps ); // Grammar can change as the program proceeds
			if( programTrace )
				{
				dumpStack( programTrace );
				dumpParserState( interpreterTrace );
				trace( programTrace, "    # Handle %d: ", pn_index( handle, gr ) );
				pn_sendTo( handle, programTrace, gr, st );
				int i;
				char *sep = "  with  ";
				for( i=0; i < pn_length( handle, gr ); i++ )
					{
					Symbol nameSymbol = pn_name( handle, i, gr );
					if( nameSymbol )
						{
						Object value = sk_item( stack, pn_length( handle, gr ) - i - 1 );
						trace( programTrace, "%s%s=", sep, sy_name( nameSymbol, st ) );
						ob_sendTo( value, programTrace, heap );
						sep = " ";
						}
					}
				trace( programTrace, "\n" );
				}
			Function functionToCall = NULL;
			if(   productionBodies
				&& pn_index( handle, gr ) < fna_count( productionBodies ) // recursive calls won't yet have a body defined
				){
				functionToCall = fna_get( productionBodies, pn_index( handle, gr ) );
				}
			if( functionToCall )
				{
				assert( handle );
				closeTokenStreamsAsNecessary(); // tail call optimization?
				cx_save( curContext );
				int i;
				for( i = pn_length( handle, gr ) - 1; i >= 0; i-- )
					{
					Symbol nameSymbol = pn_name( handle, i, gr );
					Object value = pop();
					if( nameSymbol )
						sy_setValue( nameSymbol, value, curContext );
					}
				ts_push( tokenStream, functionToCall->body.tb );
				cf_push();
				trace( programTrace, "    Calling body %p for production %d\n", tokenStream, pn_index( handle, gr ) );
				}
			else
				{
				GrammarLine line = lookupGrammarLine( handle, gr );
				if( recording )
					{
					int depthWithoutHandle = ps_depth( ps ) - pn_length( handle, gr );
					nopAction( handle, NULL );
					if( line && line->response.action == stopRecordingTokenBlockAction && depthWithoutHandle < startingDepth )
						{
						closeTokenStreamsAsNecessary();
						goto done;
						}
					}
				else
					{
					assert( line );
					trace( interpreterTrace, "   Calling native action %p\n", line );
					line->response.action( handle, line );
					trace( interpreterTrace, "   Done native action %p\n", line );
					}
				}
			}
		Object raw = ts_current( tokenStream );
		if( !raw )
			{
			trace( interpreterTrace, "   Raw token is NULL\n" );
			closeTokenStreamsAsNecessary();
			goto done;
			}
		if( recording )
			{
			if( interpreterTrace )
				{
				trace( interpreterTrace, "   Appending " );
				ob_sendTo( raw, interpreterTrace, heap );
				trace( interpreterTrace, " to ");
				tb_sendTo( recording, interpreterTrace, heap );
				trace( interpreterTrace, "\n");
				}
			tb_append( recording, raw );
			}
		Object toPush = cx_filter( curContext, raw, endOfInput, heap );
		if( interpreterTrace )
			{
			trace( interpreterTrace, "Pushing token from %p: ", ts_curBlock( tokenStream ) );
			ob_sendTo( toPush, interpreterTrace, heap );
			trace( interpreterTrace, "\n");
			}
		push( toPush );
		trace( interpreterTrace, "Advancing %p\n", ts_curBlock( tokenStream ) );
		ts_advance( tokenStream );
		closeTokenStreamsAsNecessary();
		}

	done:
	if( interpreterTrace )
		{
		trace( interpreterTrace, "Exiting mainParsingLoop" );
		if( tokenStream && ts_current( tokenStream ) )
			{
			trace( interpreterTrace, "; current token on %p is ", ts_curBlock( tokenStream ) );
			ob_sendTo( ts_current( tokenStream ), interpreterTrace, heap );
			}
		trace( interpreterTrace, "\n" );
		}
	}

static void recordTokenBlockAction( Production handle, GrammarLine gl )
	{
	Grammar gr = ps_grammar( ps );
	TokenBlock tb = ts_skipBlock( tokenStream );
	if( !tb )
		{
		trace( programTrace, "  Begin recording token block\n" );
		tb = ts_beginBlock( tokenStream );
		tb_append( tb, oh_symbolToken( heap, sy_byName( "{", st ) ) );
		nopAction( handle, gl );

		mainParsingLoop( tb, NULL );

		// Wonder why I don't need this: tb_append( tb, oh_symbolToken( heap, sy_byName( "}", st ) ) );
		tb_stopAppending( tb );
		}
	popN( pn_length( handle, gr ) );
	push( ob_fromTokenBlock( tb, heap ) );
	if( interpreterTrace )
		{
		trace( interpreterTrace, "    Recorded token block: " );
		tb_sendTo( tb, interpreterTrace, heap );
		trace( interpreterTrace, "\n    Now current: " );
		ob_sendTo( ts_current( tokenStream ), interpreterTrace, heap );
		trace( interpreterTrace, "\n" );
		}
	}

static File openTrace( int fd, char *name )
	{
	File result = fdopen( fd, "wt" );
	if( result )
		{
		setbuf( result, 0 );
		trace( result, "# %s\n", name );
		}
	return result;
	}

int main( int argc, char **argv )
	{
	conflictLog = stderr;
	programTrace           = openTrace( 3, "Ellesmere program trace" );
	interpreterDiagnostics = openTrace( 4, "Ellesmere interpreter diagnostics" );
	interpreterTrace       = openTrace( 5, "Ellesmere interpreter trace" );
	parserGenTrace = openTrace( 6, "Ellesmere parserGenTrace" );
	st = theSymbolTable();
	heap = theObjectHeap();
	curContext = cx_new( st );
	callStack = cs_new( 30, ml_indefinite() );
	cs_setCount( callStack, 1 );
	ir = initialIR( heap, st, ml_indefinite() );
	Grammar initialGrammar = populateGrammar( st );
	productionBodiesDuringExecution = fna_new( 20 + gr_numProductions( initialGrammar ), ml_indefinite() );
	fna_setCount( productionBodiesDuringExecution, gr_numProductions( initialGrammar ) );
	Automaton au = au_new( initialGrammar, st, ir, ml_indefinite(), conflictLog, parserGenTrace );
	ps = ps_new( au, ml_indefinite(), parserGenTrace );
	stack = sk_new( ml_indefinite() );
	tokenStream = theLexTokenStream( heap, st );

	mainParsingLoop( NULL, productionBodiesDuringExecution );

#ifndef NDEBUG
	File memreport = fdopen( 6, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

