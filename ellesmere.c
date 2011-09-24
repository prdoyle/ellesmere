
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "parser.h"
#include "tokens.h"
#include "memory.h"
#include "bitvector.h"
#include "records.h"
#include <stdarg.h>
#include <string.h>

typedef struct cf_struct *CallFrame;
struct cf_struct
	{
	Parser ps;
	Stack  stack;
	};

#ifdef NDEBUG
	typedef struct cs_struct *CallStack;
#else
	typedef Array CallStack;
#endif
#define AR_PREFIX  cs
#define AR_TYPE    CallStack
#define AR_ELEMENT struct cf_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define cs_new( size, ml ) cs_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

#undef CALL_STACK

typedef struct th_struct *Thread;

struct th_struct
	{
	SymbolTable          st;
	TokenStream          tokenStream;
	ObjectHeap           heap;
	Parser               ps;
	Object               productionMap;
	Object               executionBindings;
	Object               recordingBindings;
	Object               concretifications;
	CallStack            callStack;
	int                  callStackDepth;

	FILE                *programTrace;            // Follow user program step-by-step
	FILE                *interpreterDiagnostics;  // High-level messages describing unusual events in the interpreter
	FILE                *interpreterTrace;        // Follow interpreter step-by-step
	FILE                *conflictLog;
	FILE                *parserGenTrace;
	} theThread = {0};

static void addProductionsToMap( Grammar gr, int startIndex, Thread th )
	{
	int i;
	for( i = startIndex; i < gr_numProductions( gr ); i++ )
		{
		Production pn = gr_production( gr, i );
		if( pn_symbol( pn, gr ) )
			{
			trace( th->interpreterTrace, "Add to productionMap: %s -> %d\n", sy_name( pn_symbol( pn, gr ), th->st ), i );
			ob_setIntField( th->productionMap, pn_symbol( pn, gr ), i, th->heap );
			}
		}
	}

static void addNewestProductionsToMap( Grammar gr, Thread th )
	{
	addProductionsToMap( gr, gr_numOuterProductions( gr ), th );
	}

static void cf_push()
	{
#ifdef CALL_STACK
	assert( cs_count( th->callStack ) >= 1 );
	CallFrame cf = cs_element( th->callStack, th->callStackDepth );
	cf->ps = ps;
	cf->stack = stack;
	th->callStackDepth += 1;
	if( th->callStackDepth >= cs_count(th->callStack) )
		{
		trace( interpreterTrace, "Growing call stack to depth=%d\n", th->callStackDepth );
		cf = cs_nextElement( th->callStack );
		cf->stack = sk_new( ml_indefinite() );
		}
	else
		{
		cf = cs_element( th->callStack, th->callStackDepth );
		sk_popN( cf->stack, sk_depth( cf->stack ) );
		}
	ps = cf->ps = ps_new( ps_automaton(ps), ml_indefinite(), parserGenTrace );
	stack = cf->stack;
#endif
	}

#if 0
static void cf_pop()
	{
#ifdef CALL_STACK
	assert( th->callStackDepth >= 1 );
	CallFrame cf = cs_element( th->callStack, --th->callStackDepth );
	ps_close( ps );
	ps    = cf->ps;
	stack = cf->stack;
#endif
	}
#endif

typedef void (*NativeAction)( Production handle, GrammarLine gl, Thread th );

struct fn_struct
	{
	enum {
		FN_NATIVE,
		FN_TOKEN_BLOCK,
	} kind;
	union {
		GrammarLine gl;
		TokenBlock  tb;
	} body;
	};

static void dumpStack0( File fl, Thread th ) __attribute__((noinline));
static void dumpStack0( File fl, Thread th )
	{
	trace( fl, "    -- Stack: " );
	sk_sendNTo( ps_operandStack( th->ps ), 5+ps_reduceContextLength( th->ps, th->heap, th->st ), fl, th->heap );
	int i;
	for( i = th->callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( th->callStack, i );
		trace( fl, "\n%*s", 14, "" );
		sk_sendNTo( cf->stack, 5+ps_reduceContextLength( cf->ps, th->heap, th->st ), fl, th->heap );
		}
	trace( fl, "\n" );
	}

static inline void dumpStack( File fl, Thread th )
	{
	if( fl )
		dumpStack0( fl, th );
	}

static void dumpParserState0( File fl, Thread th ) __attribute__((noinline));
static void dumpParserState0( File fl, Thread th )
	{
	trace( fl, "    -- Parser state: " );
	ps_sendTo( th->ps, fl, th->heap, th->st );
	int i;
	for( i = th->callStackDepth-1; i >= 0; i-- )
		{
		CallFrame cf = cs_element( th->callStack, i );
		trace( fl, "\n%*s", 21, "" );
		ps_sendTo( cf->ps, fl, th->heap, th->st );
		}
	trace( fl, "\n" );
	}

static inline void dumpParserState( File fl, Thread th )
	{
	if( fl )
		dumpParserState0( fl, th );
	}

static void dumpTokenStream0( File fl, Thread th ) __attribute__((noinline));
static void dumpTokenStream0( File fl, Thread th )
	{
	trace( fl, "    -- Token stream: " );
	ts_sendTo( th->tokenStream, fl );
	trace( fl, "\n" );
	}

static inline void dumpTokenStream( File fl, Thread th )
	{
	if( fl )
		dumpTokenStream0( fl, th );
	}

static void dumpStuff( File fl, Thread th )
	{
	dumpStack( fl, th );
	dumpParserState( fl, th );
	dumpTokenStream( fl, th );
	}

static void push( Object ob, Thread th )
	{
	trace( th->interpreterTrace, "push(" );
	ob_sendTo( ob, th->interpreterTrace, th->heap );
	trace( th->interpreterTrace, ")\n" );
	ps_push( th->ps, ob );
	dumpStuff( th->interpreterTrace, th );
	}

static Object pop( Thread th )
	{
	Object result = ps_pop( th->ps );
	dumpStuff( th->interpreterTrace, th );
	return result;
	}

static void popN( int n, Thread th )
	{
	ps_popN( th->ps, n );
	dumpStuff( th->interpreterTrace, th );
	}

static int popInt( Thread th )
	{
	Object popped = pop( th );
	assert( ob_isInt( popped, th->heap ) );
	return ob_toInt( popped, th->heap );
	}

static void pushToken( int symbolIndex, Thread th )
	{
	push( oh_symbolToken( th->heap, sy_byIndex( symbolIndex, th->st ) ), th );
	}

static Symbol popToken( Thread th )
	{
	Object popped = pop( th );
	assert( ob_isToken( popped, th->heap ) );
	return ob_toSymbol( popped, th->heap );
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
	 
static void nopAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	popN( pn_length( handle, gr ), th );
	push( oh_symbolToken( th->heap, pn_lhs( handle, gr ) ), th );
	}

static void passThrough( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	int depth = gl->response.parm1;
	Object result = sk_item( ps_operandStack( th->ps ), depth );
	popN( pn_length( handle, gr ), th );
	push( result, th );
	}

static void addAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left + right, th->heap ), th );
	}

static void subAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left;
	if( gl->response.parm1 == 2 )
		left = popInt( th );
	else
		left = 0;
	push( ob_fromInt( left - right, th->heap ), th );
	}

static void mulAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left * right, th->heap ), th );
	}

static void divAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left / right, th->heap ), th );
	}

static void printAction( Production handle, GrammarLine gl, Thread th )
	{
	int depth = gl->response.parm1;
	ob_sendTo( sk_item( ps_operandStack( th->ps ), depth ), stdout, th->heap );
	printf("\n");
	nopAction( handle, gl, th );
	}

static void recordTokenBlockAction( Production handle, GrammarLine gl, Thread th );

static void stopRecordingTokenBlockAction( Production handle, GrammarLine gl, Thread th )
	{
	assert(0); // Never actually gets called.  It's a kind of null terminator.
	}

static void parseTreeAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	Symbol lhs = pn_lhs( handle, gr );
	Object result = ob_create( lhs, th->heap );
	int i;
	for( i = pn_length(handle, gr) - 1; i >= 0; i-- )
		{
		Symbol field = pn_name( handle, i, gr );
		if( !field )
			field = pn_token( handle, i, gr );
		Object value = pop( th );
		ob_setField( result, field, value, th->heap );
		}
	push( result, th );
	}

static void addProductionAction( Production handle, GrammarLine gl, Thread th )
	{
	parseTreeAction( handle, gl, th );
	Object production = sk_top( ps_operandStack( th->ps ) );
	Symbol sym_result = sy_byName( "result", th->st );
	Symbol sym_parms  = sy_byName( "parms",  th->st );
	Symbol sym_next   = sy_byName( "next",   th->st );
	Symbol sym_tag    = sy_byName( "tag",    th->st );
	Symbol sym_name   = sy_byName( "name",   th->st );
	Symbol sym_abstract = sy_byName( "ABSTRACT_PRODUCTION", th->st );

	// Define the nested grammar with this extra production
	Grammar gr = gr_nested( ps_grammar(th->ps), 1, ml_indefinite() );
	Production pn = pn_new( gr, ob_getTokenField( production, sym_result, th->heap ), 3 );
	pn_setConflictResolution( pn, gl->response.parm1, gr );
	Symbol pnSymbol = pn_autoSymbol( pn, th->st, gr );
	ob_setTokenField( production, sy_byName( "productionSymbol", th->st ), pnSymbol, th->heap );

	Object parm;
	for(
		parm = ob_getField( production, sym_parms, th->heap);
		ob_getField( parm, sym_tag, th->heap );
		parm = ob_getField( parm, sym_next, th->heap ) )
		{
		Symbol tag  = ob_getTokenField( parm, sym_tag,  th->heap );
		if( ob_getField( parm, sym_name, th->heap ) )
			pn_appendWithName( pn, ob_getTokenField( parm, sym_name, th->heap ), tag, gr );
		else
			pn_append( pn, tag, gr );
		}
	pn_stopAppending( pn, gr );
	gr_stopAdding( gr );
	gr = gr_augmentedShallow( gr, st_inheritanceRelation( th->st ), sym_abstract, ml_indefinite(), th->parserGenTrace );
	addNewestProductionsToMap( gr, th );
	Automaton au = au_new( gr, th->st, th->heap, ml_indefinite(), th->conflictLog, th->parserGenTrace );
	Parser oldParser = th->ps;
	th->ps = ps_new( au, ml_indefinite(), th->parserGenTrace );
	trace( th->interpreterDiagnostics, "    NEW PARSER\n" );

	if (th->interpreterDiagnostics)
		{
		trace( th->interpreterDiagnostics, "      Initializing from:\n" );
		sk_sendTo( ps_operandStack( oldParser ), th->interpreterDiagnostics, th->heap );
		trace( th->interpreterDiagnostics, "\n" );
		}

	// Prime the parser state with the current stack contents
	int i;
	for( i = sk_depth( ps_operandStack( oldParser ) ) - 1; i >= 0; i-- )
		ps_push( th->ps, sk_item( ps_operandStack( oldParser ), i ) );
	ps_close( oldParser );
	dumpParserState( th->interpreterTrace, th );

	// Add bindings with a symbol for each named parameter
	Object bindings = ob_create( sy_byIndex( SYM_BINDINGS, th->st ), th->heap );
	ob_setFieldX( bindings, SYM_DELEGATE, ts_getBindings( th->tokenStream ), th->heap );
	ts_setBindings( th->tokenStream, bindings );
	for( i = 0; i < pn_length( pn, gr ); i++ )
		{
		Symbol name = pn_name( pn, i, gr );
		if( name )
			{
			Symbol tag = pn_token( pn, i, gr );
			trace( th->interpreterTrace, "    -- bound %s to token %s\n", sy_name( name, th->st ), sy_name( tag, th->st ) );
			ob_setField( bindings, name, oh_symbolToken( th->heap, tag ), th->heap );
			}
		}

	if( th->interpreterTrace )
		{
		trace( th->interpreterTrace, "  New production " );
		pn_sendTo( pn, th->interpreterTrace, gr, th->st );
		trace( th->interpreterTrace, "\n" );
		}
	}

static void defAction( Production handle, GrammarLine gl, Thread th )
	{
	TokenBlock block = ob_toTokenBlock( pop( th ), th->heap );
	popToken( th ); // "as" keyword
	Object production = pop( th );
	popToken( th ); // "def" keyword
	push( oh_symbolToken( th->heap, pn_lhs( handle, ps_grammar(th->ps) ) ), th );

	// Remove argument bindings
	ts_setBindings( th->tokenStream, ob_getFieldX( ts_getBindings( th->tokenStream ), SYM_DELEGATE, th->heap ) );

	// Store the body from the definition
	Symbol pnSymbol = ob_getTokenField( production, sy_byName( "productionSymbol", th->st ), th->heap );
	trace( th->programTrace, "  Defining production %s\n", sy_name( pnSymbol, th->st ) );
	Function fn = (Function)ml_alloc( ml_indefinite(), sizeof(*fn) );
	fn->kind       = FN_TOKEN_BLOCK;
	fn->body.tb    = block;
	ob_setFunctionField( th->executionBindings, pnSymbol, fn, th->heap );
	}

#if 0
static void returnAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	int depth = gl->response.parm1;
	Object result = sk_item( ps_operandStack( th->ps ), depth );
	popN( pn_length( handle, gr ), th );
	ts_pop( th->tokenStream );
	push( oh_symbolToken( th->heap, pn_lhs( handle, ps_grammar(th->ps) ) ), th );
	cf_pop( th );
	trace( th->programTrace, "  Returned to TokenBlock %p\n", ts_curBlock( th->tokenStream ) );
	push( result, th );
	}
#endif

static void nonzeroAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int value = popInt( th );
	if( value )
		pushToken( SYM_TRUE, th );
	else
		pushToken( SYM_FALSE, th );
	}

static void leAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	if( left <= right )
		pushToken( SYM_TRUE, th );
	else
		pushToken( SYM_FALSE, th );
	}

#if 0
static void setAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol name = popToken( th );
	Object rhs = pop( th );
	ob_setField( ts_getBindings( th->tokenStream ), name, rhs, th->heap );
	push( oh_symbolToken( th->heap, pn_lhs( handle, ps_grammar(th->ps) ) ), th );
	}
#endif

static Object recordified( Object ob, Thread th )
	{
	MemoryLifetime ml = ml_begin( 1000, ml_indefinite() );
	BitVector fieldIDs = bv_new( st_count(th->st), ml );
	ob_getFieldSymbols( ob, fieldIDs, th->heap );

	Record rd = rd_new( fieldIDs, ml_indefinite() );

	char buf[40];
	sprintf( buf, "BINDINGS_%d", st_count( th->st ) );
	Symbol tag = sy_byName( buf, th->st );
	sy_setInstanceShape( tag, rd, th->st );

	Object result = ob_create( tag, th->heap );
	int fieldID;
	for( fieldID = bv_firstBit( fieldIDs ); fieldID != bv_END; fieldID = bv_nextBit( fieldIDs, fieldID ) )
		{
		Symbol field = sy_byIndex( fieldID, th->st );
		ob_setField( result, field, ob_getField( ob, field, th->heap ), th->heap );
		}

	ml_end( ml );
	return result;
	}

static void optimizeAction( Production handle, GrammarLine gl, Thread th )
	{
	th->executionBindings = recordified( th->executionBindings, th );
	if( 0 )
		{
		Symbol tag = ob_tag( th->executionBindings, th->heap );
		printf( "optimized executionBindings tag is %s, shape %p\n", sy_name( tag, th->st ), sy_instanceShape( tag, th->st ) );
		rd_sendTo( sy_instanceShape( tag, th->st ), stdout, th->st );
		}
	th->recordingBindings = recordified( th->recordingBindings, th );
	nopAction( handle, gl, th );
	}

static void createAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol tag = popToken( th );
	push( ob_create( tag, th->heap ), th );
	}

static void getFieldAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol field = popToken( th );
	Object receiver = pop( th );
	push( ob_getField( receiver, field, th->heap ), th );
	}

static void setFieldAction( Production handle, GrammarLine gl, Thread th )
	{
	Stack operandStack = ps_operandStack( th->ps );
	Object value = sk_item( operandStack, 1 );
	Symbol field = ob_toSymbol( sk_item( operandStack, 2 ), th->heap );
	Object receiver = sk_item( operandStack, 3 );
	ob_setField( receiver, field, value, th->heap );
	nopAction( handle, gl, th );
	}

static struct gl_struct grammar1[] =
	{
	{ { "PROGRAM",   "VOIDS", "END_OF_INPUT"                                        }, { nopAction } },
	{ { "VOIDS",     "VOID",                                                        }, { nopAction } },
	{ { "VOIDS",     "VOIDS", "VOID"                                                }, { nopAction } },

	{ { "INT",      "{", "VOIDS", "INT",   "}"                                      }, { passThrough, 1 } },
	{ { "INT",      "{",          "INT",   "}"                                      }, { passThrough, 1 } },

	{ { "VOID",     "{", "VOIDS", "}"                                               }, { nopAction } },
	{ { "VOID",     "{",          "}"                                               }, { nopAction } },

	{ { "VOID",     "OBJECT", "print!"                                              }, { printAction, 1 } },

	//{ { "VOID",     "OBJECT",  "return!",                                           }, { returnAction, 1 } },
	//{ { "VOID",     "VOID", "return!",                                              }, { returnAction, 1 } },

	//{ { "VOID",     "OBJECT@value", "TOKEN@name", "set!"                            }, { setAction } },

	{ { "PARAMETER_LIST"                                                            }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",      "PARAMETER_LIST@next"                  }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",  "@", "TOKEN@name", "PARAMETER_LIST@next"   }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@name", ":", "TOKEN@tag",  "PARAMETER_LIST@next"   }, { parseTreeAction } },
	{ { "PRODUCTION",      "TOKEN@result", "PARAMETER_LIST@parms"                   }, { addProductionAction } },
	{ { "PRODUCTION",      "l2r", "TOKEN@result", "PARAMETER_LIST@parms"            }, { addProductionAction, CR_REDUCE_BEATS_SHIFT } },
	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "}"                                 }, { stopRecordingTokenBlockAction } },
	{ { "TOKEN_BLOCK",     "TB_START",          "}"                                 }, { stopRecordingTokenBlockAction } },
	{ { "TOKEN_BLOCK",     "TB_START", "VOIDS", "OBJECT", "}"                       }, { stopRecordingTokenBlockAction } },
	{ { "TOKEN_BLOCK",     "TB_START", "OBJECT", "}"                                }, { stopRecordingTokenBlockAction } },
	{ { "TB_START",        "{",                                                     }, { recordTokenBlockAction } },
	{ { "VOID",            "def", "PRODUCTION", "as", "TOKEN_BLOCK"                 }, { defAction } },

	{ { "VOID",     "optimize!"                                                     }, { optimizeAction, 1 } },

	{ { "CREATABLE", "TOKEN@tag", "create!"                                         }, { createAction } },
	{ { "OBJECT",    "WITH_FIELDS@receiver", "TOKEN@field", "getfield!"             }, { getFieldAction } },
	{ { "VOID",      "MUTABLE@receiver", "TOKEN@field", "OBJECT@value", "setfield!" }, { setFieldAction } },

	{ { "INT",   "INT", "INT", "add!"                                               }, { addAction } },
	{ { "INT",   "INT", "INT", "sub!"                                               }, { subAction, 2 } },
	{ { "INT",   "INT", "INT", "mul!"                                               }, { mulAction } },
	{ { "INT",   "INT", "INT", "div!"                                               }, { divAction } },

	{ { "BOOLEAN",   "INT", "nz!"                                                   }, { nonzeroAction } },
	{ { "BOOLEAN",   "INT", "INT", "le!"                                            }, { leAction } },

	{{NULL}},
	};

static GrammarLine initialGrammarNest[] = { grammar1 };

static struct gl_struct inheritance[] =
	{
	{{ "OBJECT",      "IMMUTABLE", "WITH_FIELDS" }},          // OBJECT includes any sym that can be an object tag
	{{ "IMMUTABLE",   "INT", "BOOLEAN", "STRING" }},
	{{ "WITH_FIELDS", "CREATABLE", "MUTABLE" }},
	{{ "BOOLEAN",     "FALSE", "TRUE" }},

	{{NULL}},
	};

static struct
	{
	char *abstract;
	char *concrete;
	} initialConcretifications[] =
	{
	{ "BOOLEAN", "FALSE" },

	{NULL},
	};

static Grammar populateGrammar( SymbolTable st, Thread th )
	{
	Grammar gr = NULL;
	int i,j,k;
	for( i=0; i < asizeof( initialGrammarNest ); i++ )
		{
		GrammarLine *curArray = initialGrammarNest + i;
		if( gr )
			gr = gr_nested( gr, 5, ml_indefinite() );
		else
			gr = gr_new( sy_byName( (*curArray)[0].tokens[0], th->st ), 20, ml_indefinite() );
		for( j=0; (*curArray)[j].response.action; j++ )
			{
			GrammarLine line = (*curArray) + j;
			Production pn = pn_new( gr, sy_byName( line->tokens[0], th->st ), asizeof( line->tokens ) );
			pn_setConflictResolution( pn, line->cr, gr );
			Symbol pnSymbol = pn_autoSymbol( pn, th->st, gr );
			for( k=1; k < asizeof( line->tokens ) && line->tokens[k]; k++ )
				{
				char *token = line->tokens[k];
				char *at = strchr( token, '@' );
				if( at != NULL && at != token )
					{
					char *tag = (char*)ml_alloc( ml_indefinite(), at - token + 1 );
					memcpy( tag, token, at-token );
					tag[ at-token ] = 0;
					pn_appendWithName( pn, sy_byName( at+1, th->st ), sy_byName( tag, th->st ), gr );
					}
				else
					{
					pn_append( pn, sy_byName( token, th->st ), gr );
					}
				}
			pn_stopAppending( pn, gr );
			Function fn = (Function)ml_alloc( ml_indefinite(), sizeof(*fn) );
			fn->kind       = FN_NATIVE;
			fn->body.gl    = line;
			ob_setFunctionField( th->executionBindings, pnSymbol, fn, th->heap );
			if( line->response.action == stopRecordingTokenBlockAction )
				ob_setFunctionField( th->recordingBindings, pnSymbol, fn, th->heap );
			}
		}
	gr_stopAdding( gr );
	Symbol sym_abstract = sy_byName( "ABSTRACT_PRODUCTION", th->st );
	gr = gr_augmented( gr, st_inheritanceRelation( th->st ), sym_abstract, ml_indefinite(), th->parserGenTrace );
	addProductionsToMap( gr, 0, th );

	for( i=0; initialConcretifications[i].abstract; i++)
		{
		Symbol abstract = sy_byName( initialConcretifications[i].abstract, th->st );
		Symbol concrete = sy_byName( initialConcretifications[i].concrete, th->st );
		ob_setTokenField( th->concretifications, abstract, concrete, th->heap );
		}

	return gr;
	}

static void initializeInheritanceRelation( ObjectHeap heap, SymbolTable st, MemoryLifetime ml, Thread th )
	{
	int superIndex, subIndex;
	InheritanceRelation ir = st_inheritanceRelation( th->st );
	for( superIndex = 0; inheritance[ superIndex ].tokens[0]; superIndex++ )
		{
		GrammarLine gl = inheritance + superIndex;
		Symbol super = sy_byName( gl->tokens[0], th->st );
		for( subIndex = 1; gl->tokens[ subIndex ]; subIndex++ )
			{
			Symbol sub = sy_byName( gl->tokens[ subIndex ], th->st );
			ir_add( ir, super, sub );
			}
		}

	if( th->interpreterTrace )
		{
		trace( th->interpreterTrace, "Initial inheritance relation:\n" );
		ir_sendTo( ir, th->interpreterTrace );
		}
	}

static void mainParsingLoop( TokenBlock recording, Object bindings, Thread th )
	{
	assert( bindings );

	int startingDepth = sk_depth( ps_operandStack( th->ps ) );

	if( th->interpreterTrace )
		{
		trace( th->interpreterTrace, "Starting mainParsingLoop( " );
		if( recording )
			tb_sendTo( recording, th->interpreterTrace, th->heap );
		else
			trace( th->interpreterTrace, "NULL" );
		trace( th->interpreterTrace, ", %p ) startingDepth=%d\n", bindings, startingDepth );
		}

	Object raw = NULL;
	while( 1 )
		{
		Symbol handleSymbol;
		for (
			handleSymbol = ps_handle( th->ps, ts_current( th->tokenStream ) );
			handleSymbol;
			handleSymbol = ps_handle( th->ps, ts_current( th->tokenStream ) )
			){
			Grammar gr = ps_grammar( th->ps ); // Grammar can change as the program proceeds
			Production handleProduction = gr_production( gr, ob_getIntField( th->productionMap, handleSymbol, th->heap ) );
			if( th->programTrace )
				{
				dumpStack( th->programTrace, th );
				dumpParserState( th->interpreterTrace, th );
				trace( th->programTrace, "    # Handle %s: ", sy_name( handleSymbol, th->st ) );
				pn_sendTo( handleProduction, th->programTrace, gr, th->st );
				int i;
				char *sep = "  with  ";
				for( i=0; i < pn_length( handleProduction, gr ); i++ )
					{
					Symbol nameSymbol = pn_name( handleProduction, i, gr );
					if( nameSymbol )
						{
						Object value = sk_item( ps_operandStack( th->ps ), pn_length( handleProduction, gr ) - i - 1 );
						trace( th->programTrace, "%s%s=", sep, sy_name( nameSymbol, th->st ) );
						ob_sendTo( value, th->programTrace, th->heap );
						sep = " ";
						}
					}
				trace( th->programTrace, "\n" );
				}
			Function functionToCall = NULL;

			Object ob = ob_getField( bindings, handleSymbol, th->heap );
			if( ob )
				functionToCall = ob_toFunction( ob, th->heap );
			else // abstract!
				check( recording );

			if( recording )
				{
				int depthWithoutHandle = sk_depth( ps_operandStack( th->ps ) ) - pn_length( handleProduction, gr );
				popN( pn_length( handleProduction, gr ), th );
				Object lhs = oh_symbolToken( th->heap, pn_lhs( handleProduction, gr ) );
				if( th->interpreterTrace )
					{
					trace( th->interpreterTrace, "Checking %s for concretification in: ", sy_name( ob_toSymbol( lhs, th->heap ), th->st ) );
					ob_sendDeepTo( th->concretifications, th->interpreterTrace, th->heap );
					trace( th->interpreterTrace, "\n" );
					}
				Object concretified = ob_getFieldIfPresent( th->concretifications, ob_toSymbol( lhs, th->heap ), lhs, th->heap );
				if( concretified != lhs )
					{
					trace( th->interpreterTrace, "  %s concretified into %s\n", sy_name( ob_toSymbol( lhs, th->heap ), th->st ), sy_name( ob_toSymbol( concretified, th->heap ), th->st ) );
					lhs = concretified;
					}
				push( lhs, th );
				Object ob = ob_getField( bindings, handleSymbol, th->heap );
				if( ob )
					{
					assert( functionToCall->body.gl->response.action == stopRecordingTokenBlockAction );
					if( depthWithoutHandle < startingDepth )
						{
						trace( th->interpreterTrace, "   Done recording\n" );
						goto done;
						}
					else
						trace( th->interpreterTrace, "     Too deep to stop recording yet\n" );
					}
				}
			else if( functionToCall )
				{
				switch( functionToCall->kind )
					{
					case FN_TOKEN_BLOCK:
						{
						assert( handleProduction );
						int i;
						Object argBindings = ob_createX( SYM_BINDINGS, th->heap ); // TODO: Recycle?
						for( i = pn_length( handleProduction, gr ) - 1; i >= 0; i-- )
							{
							Symbol nameSymbol = pn_name( handleProduction, i, gr );
							Object value = pop( th );
							if( nameSymbol )
								ob_setField( argBindings, nameSymbol, value, th->heap );
							}
						ts_push( th->tokenStream, functionToCall->body.tb, argBindings );
						cf_push( th );
						trace( th->programTrace, "    Calling body %p for production %d\n", th->tokenStream, pn_index( handleProduction, gr ) );
						}
						break;
					case FN_NATIVE:
						{
						GrammarLine line = functionToCall->body.gl;
						assert( line );
						trace( th->interpreterTrace, "   Calling native action %p\n", line );
						line->response.action( handleProduction, line, th );
						trace( th->interpreterTrace, "   Done native action %p\n", line );
						}
						break;
					}
				}
			}
		if( recording && raw )
			{
			// This raw token didn't cause the recording to terminate.  Append it.
			tb_append( recording, raw );
			if( th->interpreterTrace )
				{
				trace( th->interpreterTrace, "   Appended " );
				ob_sendTo( raw, th->interpreterTrace, th->heap );
				trace( th->interpreterTrace, " to ");
				tb_sendTo( recording, th->interpreterTrace, th->heap );
				trace( th->interpreterTrace, "\n");
				}
			}
		raw = ts_currentRaw( th->tokenStream );
		if( !raw )
			{
			trace( th->interpreterTrace, "   Raw token is NULL\n" );
			goto done;
			}
		Object toPush = ts_current( th->tokenStream );
		if( th->interpreterTrace )
			{
			trace( th->interpreterTrace, "Pushing token from %p: ", ts_curBlock( th->tokenStream ) );
			ob_sendTo( toPush, th->interpreterTrace, th->heap );
			trace( th->interpreterTrace, "\n" );
			}
		push( toPush, th );
		ts_advance( th->tokenStream );
		if( th->interpreterTrace )
			{
			trace( th->interpreterTrace, "Advanced %p: ", ts_curBlock( th->tokenStream ) );
			ts_sendTo( th->tokenStream, th->interpreterTrace );
			trace( th->interpreterTrace, "\n" );
			}
		}

	done:
	if( th->interpreterTrace )
		{
		trace( th->interpreterTrace, "Exiting mainParsingLoop" );
		if( th->tokenStream && ts_current( th->tokenStream ) )
			{
			trace( th->interpreterTrace, "; current token on %p is ", ts_curBlock( th->tokenStream ) );
			ob_sendTo( ts_current( th->tokenStream ), th->interpreterTrace, th->heap );
			}
		trace( th->interpreterTrace, "\n" );
		}
	}

static void recordTokenBlockAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	TokenBlock tb = ts_skipBlock( th->tokenStream );
	if( !tb )
		{
		tb = ts_beginBlock( th->tokenStream );
		if( th->interpreterTrace )
			{
			trace( th->programTrace, "  Begin recording token block\n" );
			tb_sendTo( tb, th->interpreterTrace, th->heap );
			trace( th->interpreterTrace, "\n" );
			}
		nopAction( handle, gl, th );

		mainParsingLoop( tb, th->recordingBindings, th );

		tb_stopAppending( tb );
		}
	popN( pn_length( handle, gr ), th );
	push( ob_fromTokenBlock( tb, th->heap ), th );
	if( th->interpreterTrace )
		{
		trace( th->interpreterTrace, "    Recorded token block: " );
		tb_sendTo( tb, th->interpreterTrace, th->heap );
		trace( th->interpreterTrace, "\n    Now current: " );
		ob_sendTo( ts_current( th->tokenStream ), th->interpreterTrace, th->heap );
		trace( th->interpreterTrace, "\n" );
		}
	}

static File openTrace( int fd, char *name )
	{
	File result = fdopen( fd, "wt" );
	if( result )
		{
		setbuf( result, 0 );
		setvbuf( result, malloc(BUFSIZ), _IOLBF, 1000 );
		if( name )
			trace( result, "# %s\n", name );
		}
	return result;
	}

int main( int argc, char **argv )
	{
	Thread th = &theThread;
	th->conflictLog = stderr;
	th->programTrace           = openTrace( 3, "3: Ellesmere program trace" );
	th->interpreterDiagnostics = openTrace( 4, "4: Ellesmere interpreter diagnostics" );
	th->interpreterTrace       = openTrace( 5, "5: Ellesmere interpreter trace" );
	th->parserGenTrace = openTrace( 6, "6: Ellesmere parserGenTrace" );
	th->st = theSymbolTable();
	th->heap = theObjectHeap();
	th->callStack = cs_new( 30, ml_indefinite() );
	cs_setCount( th->callStack, 1 );
	initializeInheritanceRelation( th->heap, th->st, ml_indefinite(), th );
	th->executionBindings = ob_create( sy_byIndex( SYM_BINDINGS, th->st ), th->heap );
	th->recordingBindings = ob_create( sy_byIndex( SYM_BINDINGS, th->st ), th->heap );
	th->concretifications = ob_create( sy_byIndex( SYM_BINDINGS, th->st ), th->heap );
	th->productionMap     = ob_create( sy_byName( "PRODUCTION_MAP", th->st ), th->heap );
	Grammar initialGrammar = populateGrammar( th->st, th );
	Automaton au = au_new( initialGrammar, th->st, th->heap, ml_indefinite(), th->conflictLog, th->parserGenTrace );
	th->ps = ps_new( au, ml_indefinite(), th->parserGenTrace );
	th->tokenStream = theLexTokenStream( th->heap, th->st );

	mainParsingLoop( NULL, th->executionBindings, th );

#ifndef NDEBUG
	File memreport = fdopen( 7, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

