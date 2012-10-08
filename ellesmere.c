
#include "stack.h"
#include "parser.h"
#include "tokens.h"
#include "memory.h"
#include "bitvector.h"
#include "records.h"
#include "options.h"
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dlfcn.h>
#include <sys/time.h>
#include <limits.h>

typedef struct cx_struct *Context;
struct cx_struct
	{
	TokenStream stream;
	Object      bindings;
	};

#ifdef NDEBUG
	typedef struct cxs_struct *ContextStack;
#else
	typedef Array ContextStack;
#endif
#define AR_PREFIX  cxs
#define AR_TYPE    ContextStack
#define AR_ELEMENT struct cx_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define cxs_new( size, ml ) cxs_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

typedef struct th_struct *Thread;

struct th_struct
	{
	SymbolTable          st;
	ObjectHeap           heap;
	Parser               ps;
	ContextStack         cxs;
	Object               productionMap;
	Object               executionBindings;
	Object               recordingBindings;
	Object               concretifications;
	OptionSet            os;

	FILE                *parserGenDiagnostics;
	long                 timeBasis;
	} theThread = {0};

static Context currentContext( Thread th )
	{
	return cxs_last( th->cxs, 0 );
	}

static Object activeBindings( Thread th )
	{
	Object result = currentContext( th )->bindings;
	if( os_trace( th->os, on_BINDINGS, "/= Bindings =\\\n" ) )
		{
		ob_sendDeepTo( result, os_logFile( th->os, on_BINDINGS ), th->heap );
		os_trace( th->os, on_BINDINGS, "\\= Bindings /\n" );
		}
	return result;
	}

static Object boundTo( Symbol sy, Object defaultResult, Thread th )
	{
	Object result = ob_getFieldRecursivelyIfPresent(
		activeBindings( th ),
		sy,
		sy_byIndex( SYM_DELEGATE, oh_symbolTable(th->heap) ),
		defaultResult,
		th->heap );
	if( os_tracing( th->os, on_BINDINGS ) )
		{
		File fl = os_traceFile( th->os, on_BINDINGS );
		ob_sendTo( activeBindings(th), fl, th->heap );
		fl_write( fl, "[ %s ] = ", sy_name( sy, th->st ) );
		if( result )
			ob_sendTo( result, fl, th->heap );
		else
			fl_write( fl, "null" );
		fl_write( fl, "\n" );
		}
	return result;
	}

static TokenStream currentStream( Thread th )
	{
	return currentContext( th )->stream;
	}

static Object currentToken( Thread th ) // Token here is a misnomer -- it can be any object
	{
	TokenStream  ts = currentStream( th );
	ObjectHeap heap = ts_heap( ts );
	Object      raw = ts_current( ts );
	if( raw == NULL )
		{
		assert( ts == theLexTokenStream( th->heap ) ); // Digressions are supposed to be actively canceled when they finish
		return oh_symbolToken( th->heap, sy_byIndex( SYM_END_OF_INPUT, th->st ) );
		}
	else if( ob_isToken( raw, th->heap ) )
		return boundTo( ob_toSymbol( raw, heap ), raw, th );
	else
		return raw;
	}

static Object newBindings( Object delegate, Thread th )
	{
	ObjectHeap heap = th->heap;
	Object result = ob_createX( SYM_BINDINGS, heap ); // TODO: recycle?
	if( delegate )
		ob_setFieldX( result, SYM_DELEGATE, delegate, heap );
	if( os_log( th->os, on_BINDINGS, "New binding " ) )
		{
		File log = os_logFile( th->os, on_BINDINGS );
		ob_sendTo( result, log, th->heap );
		if( delegate )
			{
			os_log( th->os, on_BINDINGS, " -> " );
			ob_sendTo( delegate, log, th->heap );
			}
		os_log( th->os, on_BINDINGS, "\n" );
		}
	return result;
	}

static void cancelDigression( Thread th )
	{
	if( cxs_count( th->cxs ) >= 2 ) // Can't "cancel a digression" on theLexTokenStream
		cxs_incCountBy( th->cxs, -1 );
	}

static void digress( Thread th, TokenStream stream, Object bindings )
	{
	if( ts_current( stream ) )
		{
		Context cx   = cxs_nextElement( th->cxs );
		cx->stream   = stream;
		cx->bindings = bindings;
		}
	}

static void advanceToken( Thread th )
	{
	TokenStream ts = currentStream(th);
	ts_advance( ts );
	if( ts_current(ts) == NULL )
		cancelDigression( th );
	}

static int sendTokenPreviewTo( Thread th, File fl )
	{
	int charsSent = 0;
	int i;
	int lengthLimit = 8;
	char *sep = 0;
	for( i = cxs_count( th->cxs )-1; i >= 0; i--, lengthLimit >>= 1 )
		{
		if( sep )
			charsSent += fl_write( fl, "%s", sep );
		sep = "  ||  ";
		Context cx = cxs_element( th->cxs, i );
		if( lengthLimit < 2 )
			lengthLimit = 2;
		ts_sendNTo( cx->stream, lengthLimit, fl );
		}
	return charsSent;
	}

static void addProductionsToMap( Grammar gr, int startIndex, Thread th )
	{
	int i;
	for( i = startIndex; i < gr_numProductions( gr ); i++ )
		{
		Production pn = gr_production( gr, i );
		if( pn_symbol( pn, gr ) )
			{
			os_trace( th->os, on_INTERPRETER, "Add to productionMap: %s -> %d\n", sy_name( pn_symbol( pn, gr ), th->st ), i );
			ob_setIntField( th->productionMap, pn_symbol( pn, gr ), i, th->heap );
			}
		}
	}

static void addNewestProductionsToMap( Grammar gr, Thread th )
	{
	addProductionsToMap( gr, gr_numOuterProductions( gr ), th );
	}

typedef void (*NativeAction)( Production handle, GrammarLine gl, Thread th );

typedef enum
	{
	FN_UNKNOWN,
	FN_NATIVE,
	FN_TOKEN_BLOCK,
	} FunctionKind;

struct fn_struct
	{
	FunctionKind kind;
	union {
		GrammarLine gl;
		TokenBlock  tb;
	} body;
	};

static void dumpStack0( File fl, Thread th ) __attribute__((noinline));
static void dumpStack0( File fl, Thread th )
	{
	TRACE( fl, "    -- Stack: " );
	sk_sendNTo( ps_operandStack( th->ps ), 5+ps_reduceContextLength( th->ps, th->heap, th->st ), fl, th->heap );
	TRACE( fl, "\n" );
	}

static inline void dumpStack( File fl, Thread th )
	{
	if( fl )
		dumpStack0( fl, th );
	}

static void dumpParserState0( File fl, Thread th ) __attribute__((noinline));
static void dumpParserState0( File fl, Thread th )
	{
	TRACE( fl, "    -- Parser state: " );
	ps_sendTo( th->ps, fl, th->heap, th->st );
	TRACE( fl, "\n" );
	}

static inline void dumpParserState( File fl, Thread th )
	{
	if( fl )
		dumpParserState0( fl, th );
	}

static void dumpTokenStream0( File fl, Thread th ) __attribute__((noinline));
static void dumpTokenStream0( File fl, Thread th )
	{
	TRACE( fl, "    -- Token stream: " );
	ts_sendTo( currentStream(th), fl );
	TRACE( fl, "\n" );
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
	File fl = os_traceFile( th->os, on_INTERPRETER );
	if( fl )
		{
		os_trace( th->os, on_INTERPRETER, "push(" );
		ob_sendTo( ob, fl, th->heap );
		os_trace( th->os, on_INTERPRETER, ")\n" );
		}
	ps_push( th->ps, ob );
	dumpStuff( fl, th );
	}

static Object pop( Thread th )
	{
	Object result = ps_pop( th->ps );
	dumpStuff( os_traceFile( th->os, on_INTERPRETER ), th );
	return result;
	}

static void popN( int n, Thread th )
	{
	ps_popN( th->ps, n );
	dumpStuff( os_traceFile( th->os, on_INTERPRETER ), th );
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

typedef enum
	{
	PE_UNSAFE,
	PE_SAFE,
	PE_ABSTRACT, // Can even tolerate placeholders
	} PartialEvaluationSafety;

struct gl_struct
	{
	char *tokens[10];
	struct
		{
		NativeAction action;
		int parm1;
		int parm2;
		} response;
	PartialEvaluationSafety peSafe;
	ConflictResolutions     cr;
	};
	 
NATIVE_ACTION void nopAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	popN( pn_length( handle, gr ), th );
	push( oh_symbolToken( th->heap, pn_lhs( handle, gr ) ), th );
	}

NATIVE_ACTION void passThrough( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	int depth = gl->response.parm1;
	Object result = sk_item( ps_operandStack( th->ps ), depth );
	popN( pn_length( handle, gr ), th );
	push( result, th );
	}

NATIVE_ACTION void passThroughField( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	SymbolIndex field = gl->response.parm1;
	int depth         = gl->response.parm2;
	Object container = sk_item( ps_operandStack( th->ps ), depth );
	popN( pn_length( handle, gr ), th );
	push( ob_getFieldX( container, field, th->heap ), th );
	}

NATIVE_ACTION void addAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left + right, th->heap ), th );
	}

NATIVE_ACTION void subAction( Production handle, GrammarLine gl, Thread th )
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

NATIVE_ACTION void mulAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left * right, th->heap ), th );
	}

NATIVE_ACTION void divAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int right = popInt( th );
	int left = popInt( th );
	push( ob_fromInt( left / right, th->heap ), th );
	}

NATIVE_ACTION void evalAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	TokenBlock tb = ob_toTokenBlock( pop(th), th->heap );
	digress( th, ts_new( tb, th->heap ), activeBindings(th) );
	}

NATIVE_ACTION void printAction( Production handle, GrammarLine gl, Thread th )
	{
	int depth = gl->response.parm1;
	ob_sendTo( sk_item( ps_operandStack( th->ps ), depth ), stdout, th->heap );
	nopAction( handle, gl, th );
	}

NATIVE_ACTION void printlnAction( Production handle, GrammarLine gl, Thread th )
	{
	int depth = gl->response.parm1;
	ob_sendTo( sk_item( ps_operandStack( th->ps ), depth ), stdout, th->heap );
	printf("\n");
	nopAction( handle, gl, th );
	}

NATIVE_ACTION void recordTokenBlockAction( Production handle, GrammarLine gl, Thread th );

NATIVE_ACTION void stopRecordingTokenBlockAction( Production handle, GrammarLine gl, Thread th )
	{
	// If we're calling this, it's because the parser is to "stop recording" a
	// nested token block during another recording.  (The code to actually end
	// a recording is inside mainParsingLoop itself.)  In that case, return a
	// placeholder for the resulting token block.
	Grammar gr = ps_grammar( th->ps );
	popN( pn_length( handle, gr ), th );
	push( oh_symbolPlaceholder( th->heap, pn_lhs( handle, gr ) ), th );
	}

NATIVE_ACTION void parseTreeAction( Production handle, GrammarLine gl, Thread th )
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

static void addTagPlaceholdersToBindings( Object prmBindings, Production pn, Grammar gr, Thread th )
	{
	int i;
	for( i = 0; i < pn_length( pn, gr ); i++ )
		{
		Symbol name = pn_name( pn, i, gr );
		if( name )
			{
			Symbol tag = pn_token( pn, i, gr );
			Object value = oh_valuePlaceholder( th->heap, tag, oh_symbolToken( th->heap, name ) );
			if( os_trace( th->os, on_INTERPRETER, "    -- bound %s@%s to ", sy_name( tag, th->st ), sy_name( name, th->st ) ) )
				{
				ob_sendTo( value, os_traceFile( th->os, on_INTERPRETER ), th->heap );
				os_trace( th->os, on_INTERPRETER, "\n" );
				}
			ob_setField( prmBindings, name, value, th->heap );
			}
		}
	}

NATIVE_ACTION void addProductionAction( Production handle, GrammarLine gl, Thread th )
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
	if ( os_enabled( th->os, on_GRAMMAR_AUGMENTATION ) )
		gr = gr_augmentedShallow( gr, oh_inheritanceRelation( th->heap ), sym_abstract, ml_indefinite(), os_logFile( th->os, on_GRAMMAR_AUGMENTATION ) );
	addNewestProductionsToMap( gr, th );
	Automaton au = au_new( gr, th->st, th->heap, ml_indefinite(), th->os, os_logFile( th->os, on_PARSER_CONFLICT ), os_traceFile( th->os, on_PARSER_GEN ) );
	Parser oldParser = th->ps;
	th->ps = ps_new( au, ml_indefinite(), os_logFile( th->os, on_INTERPRETER ) );

	if( os_log( th->os, on_INTERPRETER, "    NEW PARSER\n" ) )
		{
		os_log( th->os, on_INTERPRETER, "      Initializing from:\n" );
		sk_sendTo( ps_operandStack( oldParser ), os_logFile( th->os, on_INTERPRETER ), th->heap );
		os_log( th->os, on_INTERPRETER, "\n" );
		}

	// Prime the parser state with the current stack contents
	int i;
	for( i = sk_depth( ps_operandStack( oldParser ) ) - 1; i >= 0; i-- )
		ps_push( th->ps, sk_item( ps_operandStack( oldParser ), i ) );
	ps_close( oldParser );
	File interpreterTrace = os_traceFile( th->os, on_INTERPRETER );
	dumpParserState( interpreterTrace, th );

	// Add bindings with a symbol for each named parameter during recording
	Object bindings = newBindings( activeBindings( th ), th );
	currentContext(th)->bindings = bindings;
	addTagPlaceholdersToBindings( bindings, pn, gr, th );

	if( interpreterTrace )
		{
		os_trace( th->os, on_INTERPRETER, "  New production " );
		pn_sendTo( pn, interpreterTrace, gr, th->st );
		os_trace( th->os, on_INTERPRETER, "\n" );
		}
	}

NATIVE_ACTION void defAction( Production handle, GrammarLine gl, Thread th )
	{
	TokenBlock block = ob_toTokenBlock( pop( th ), th->heap );
	popToken( th ); // "as" keyword
	Object production = pop( th );
	popToken( th ); // "def" keyword
	push( oh_symbolToken( th->heap, pn_lhs( handle, ps_grammar(th->ps) ) ), th );

	// Remove argument bindings
	currentContext(th)->bindings = ob_getFieldX( activeBindings(th), SYM_DELEGATE, th->heap );

	// Store the body from the definition
	Symbol pnSymbol = ob_getTokenField( production, sy_byName( "productionSymbol", th->st ), th->heap );
	Function fn = (Function)ml_alloc( ml_indefinite(), sizeof(*fn) );
	fn->kind       = FN_TOKEN_BLOCK;
	fn->body.tb    = block;
	ob_setFunctionField( th->executionBindings, pnSymbol, fn, th->heap );
	if( os_log( th->os, on_EXECUTION, "   Defined production %s: ", sy_name( pnSymbol, th->st ) ) )
		{
		tb_sendTo( block, os_logFile( th->os, on_EXECUTION ), th->heap );
		os_log( th->os, on_EXECUTION, "\n" );
		}
	}

#if 0
NATIVE_ACTION void returnAction( Production handle, GrammarLine gl, Thread th )
	{
	Grammar gr = ps_grammar( th->ps );
	int depth = gl->response.parm1;
	Object result = sk_item( ps_operandStack( th->ps ), depth );
	popN( pn_length( handle, gr ), th );
	cancelDigression( th );
	push( oh_symbolToken( th->heap, pn_lhs( handle, ps_grammar(th->ps) ) ), th );
	cf_pop( th );
	os_trace( th->os, on_EXECUTION, "Returned to TokenBlock %p\n", PH( ts_tokenBlock( currentStream(th) ) ) );
	push( result, th );
	}
#endif

NATIVE_ACTION void nonzeroAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int value = popInt( th );
	if( value )
		pushToken( SYM_TRUE, th );
	else
		pushToken( SYM_FALSE, th );
	}

NATIVE_ACTION void leAction( Production handle, GrammarLine gl, Thread th )
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
NATIVE_ACTION void setAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol name = popToken( th );
	Object rhs = pop( th );
	ob_setField( activeBindings(th), name, rhs, th->heap );
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
	sy_setInstanceShape( tag, rd, th->heap );

	Object result = ob_create( tag, th->heap );
	int fieldID;
	for( fieldID = bv_firstBit( fieldIDs ); fieldID != bv_END; fieldID = bv_nextBit( fieldIDs, fieldID ) )
		ob_setFieldX( result, fieldID, ob_getFieldX( ob, fieldID, th->heap ), th->heap );

	ml_end( ml );
	return result;
	}

NATIVE_ACTION void optimizeAction( Production handle, GrammarLine gl, Thread th )
	{
	th->executionBindings = recordified( th->executionBindings, th );
	th->recordingBindings = recordified( th->recordingBindings, th );
	nopAction( handle, gl, th );
	}

static int64_t currentTimeMillis()
	{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	int64_t sec  = tv.tv_sec;
	int64_t usec = tv.tv_usec;
	return ( sec*1000 ) + ( usec/1000 );
	}

NATIVE_ACTION void timeAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	int delta = (int)( currentTimeMillis() - th->timeBasis );
	push( ob_fromInt( delta, th->heap ), th );
	}

NATIVE_ACTION void createAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol tag = popToken( th );
	push( ob_create( tag, th->heap ), th );
	}

NATIVE_ACTION void getFieldAction( Production handle, GrammarLine gl, Thread th )
	{
	popToken( th );
	Symbol field = popToken( th );
	Object receiver = pop( th );
	push( ob_getField( receiver, field, th->heap ), th );
	}

NATIVE_ACTION void setFieldAction( Production handle, GrammarLine gl, Thread th )
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
	{ { "PROGRAM",   "VOIDS", "END_OF_INPUT"                                        }, { nopAction }, PE_SAFE },
	{ { "VOIDS",     "VOID",                                                        }, { nopAction }, PE_SAFE },
	{ { "VOIDS",     "VOIDS", "VOID"                                                }, { nopAction }, PE_SAFE },

	{ { "INT",      "{", "VOIDS", "INT",   "}"                                      }, { passThrough, 1 } },
	{ { "INT",      "{",          "INT",   "}"                                      }, { passThrough, 1 } },

	{ { "VOID",     "{", "VOIDS", "}"                                               }, { nopAction }, PE_SAFE },
	{ { "VOID",     "{",          "}"                                               }, { nopAction }, PE_SAFE },

	{ { "VOID",     "OBJECT", "print!"                                              }, { printAction, 1 } },
	{ { "VOID",     "OBJECT", "println!"                                            }, { printlnAction, 1 } },

	//{ { "VOID",     "OBJECT",  "return!",                                           }, { returnAction, 1 } },
	//{ { "VOID",     "VOID", "return!",                                              }, { returnAction, 1 } },

	//{ { "VOID",     "OBJECT@value", "TOKEN@name", "set!"                            }, { setAction } },

	{ { "PARAMETER_LIST"                                                            }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",      "PARAMETER_LIST@next"                  }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@tag",  "@", "TOKEN@name", "PARAMETER_LIST@next"   }, { parseTreeAction } },
	{ { "PARAMETER_LIST",  "TOKEN@name", ":", "TOKEN@tag",  "PARAMETER_LIST@next"   }, { parseTreeAction } },
	{ { "PRODUCTION",      "TOKEN@result", "PARAMETER_LIST@parms"                   }, { addProductionAction } },
	{ { "PRODUCTION",      "l2r", "TOKEN@result", "PARAMETER_LIST@parms"            }, { addProductionAction, CR_REDUCE_BEATS_SHIFT } },
	{ { "TB_START",        "{",                                                     }, { recordTokenBlockAction } },
	{ { "TB_BODY",                                                                  }, { nopAction } },
	{ { "TB_BODY",         "VOIDS",                                                 }, { nopAction } },
	{ { "TB_BODY",         "OBJECT",                                                }, { nopAction } },
	{ { "TB_BODY",         "VOIDS", "OBJECT",                                       }, { nopAction } },
	{ { "TB_RECORDING",    "TB_START", "TB_BODY",                                   }, { stopRecordingTokenBlockAction }, PE_SAFE },
	{ { "TOKEN_BLOCK",     "TB_RECORDING", "}"                                      }, { passThroughField, SYM_VALUE, 1 }, PE_SAFE },
	{ { "VOID",            "def", "PRODUCTION", "as", "TOKEN_BLOCK"                 }, { defAction } },

	{ { "VOID",            "TOKEN_BLOCK", "eval!"                                   }, { evalAction } },
	{ { "INT",             "TOKEN_BLOCK", "eval!"                                   }, { evalAction } },

	{ { "VOID",     "optimize"                                                      }, { optimizeAction } },
	{ { "INT",      "time"                                                          }, { timeAction } },

	{ { "CREATABLE", "TOKEN@tag", "create!"                                         }, { createAction } },
	{ { "OBJECT",    "WITH_FIELDS@receiver", "TOKEN@field", "getfield!"             }, { getFieldAction } },
	{ { "VOID",      "MUTABLE@receiver", "TOKEN@field", "OBJECT@value", "setfield!" }, { setFieldAction } },

	{ { "INT",   "INT", "INT", "add!"                                               }, { addAction },    PE_SAFE },
	{ { "INT",   "INT", "INT", "sub!"                                               }, { subAction, 2 }, PE_SAFE },
	{ { "INT",   "INT", "INT", "mul!"                                               }, { mulAction },    PE_SAFE },
	{ { "INT",   "INT", "INT", "div!"                                               }, { divAction },    PE_SAFE },

	{ { "BOOLEAN",   "INT", "nz!"                                                   }, { nonzeroAction }, PE_SAFE },
	{ { "BOOLEAN",   "INT", "INT", "le!"                                            }, { leAction },      PE_SAFE },

	{{NULL}},
	};

static GrammarLine initialGrammarNest[] = { grammar1 };

static struct gl_struct inheritance[] =
	{
	{{ "OBJECT",      "IMMUTABLE", "WITH_FIELDS" }},          // OBJECT includes any sym that can be an object tag
	{{ "IMMUTABLE",   "INT", "BOOLEAN", "STRING" }},
	{{ "BOOLEAN",     "FALSE", "TRUE" }},
	{{ "WITH_FIELDS", "CREATABLE", "MUTABLE" }},
	{{ "MUTABLE",     "BINDINGS" }},

	// "FABRICABLE" means "ordinary objects that can be created and modified"
	{{ "CREATABLE",   "FABRICABLE" }},
	{{ "MUTABLE",     "FABRICABLE" }},


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
				if( token[0] == '#' )
					{
					pn_append( pn, sy_byIndex( SYM_TOKEN, th->st ), gr );
					token++;
					}
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
			fn->body.gl = line;
			fn->kind    = FN_NATIVE;
			ob_setFunctionField( th->executionBindings, pnSymbol, fn, th->heap );
			if( line->peSafe )
				ob_setFunctionField( th->recordingBindings, pnSymbol, fn, th->heap );
			}
		}
	gr_stopAdding( gr );
	Symbol sym_abstract = sy_byName( "ABSTRACT_PRODUCTION", th->st );
	if( os_enabled( th->os, on_GRAMMAR_AUGMENTATION ) )
		gr = gr_augmented( gr, oh_inheritanceRelation( th->heap ), sym_abstract, ml_indefinite(), os_logFile( th->os, on_GRAMMAR_AUGMENTATION ) );
	addProductionsToMap( gr, 0, th );

	if( os_enabled( th->os, on_CONCRETIFICATION ) )
		{
		for( i=0; initialConcretifications[i].abstract; i++)
			{
			Symbol abstract = sy_byName( initialConcretifications[i].abstract, th->st );
			Symbol concrete = sy_byName( initialConcretifications[i].concrete, th->st );
			ob_setTokenField( th->concretifications, abstract, concrete, th->heap );
			}
		}

	return gr;
	}

static void initializeInheritanceRelation( ObjectHeap heap, SymbolTable st, MemoryLifetime ml, Thread th )
	{
	if ( os_disabled( th->os, on_INHERITANCE ) )
		return;

	int superIndex, subIndex;
	InheritanceRelation ir = oh_inheritanceRelation( th->heap );
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

	if(   os_logging( th->os, on_INHERITANCE )
		|| os_tracing( th->os, on_INTERPRETER ) )
		{
		File fl = os_getLogFile( th->os );
		fl_write( fl, "Initial inheritance relation:\n" );
		ir_sendTo( ir, fl );
		}
	}

static void record( TokenBlock recording, int lengthToRecord, Stack utilityStack, Thread th )
	{
	Stack operandStack = ps_operandStack( th->ps );
	assert( sk_depth( utilityStack ) == 0 );
	if( lengthToRecord > 0 )
		{
		sk_mirrorN( utilityStack, lengthToRecord, operandStack );
		while( sk_depth( utilityStack ) >= 1)
			{
			Object toRecord = sk_pop( utilityStack );
			if( ob_isPlaceholder( toRecord, th->heap ) )
				{
				assert( ob_tagX( toRecord, th->heap ) != SYM_RECORDED_PLACEHOLDER ); // Ensure lengthToRecord is accurate
				// Actual value to record is attached to the placeholder
				toRecord = ob_getFieldX( toRecord, SYM_VALUE, th->heap );
				}
			tb_append( recording, toRecord );
			}
		File interpreterLog = os_logFile( th->os, on_INTERPRETER );
		if( interpreterLog )
			{
			os_log( th->os, on_INTERPRETER, "   Recorded %d objects to ", lengthToRecord );
			tb_sendTo( recording, interpreterLog, th->heap );
			os_log( th->os, on_INTERPRETER, "\n");
			}
		}
	}

static Production mainParsingLoop( TokenBlock recording, Object bindings, Thread th )
	{
	assert( bindings );

	Production result = NULL;

	int startingDepth = sk_depth( ps_operandStack( th->ps ) );
	int recordedDepth = startingDepth;

	File interpreterTrace = os_traceFile( th->os, on_INTERPRETER );
	if( interpreterTrace )
		{
		os_trace( th->os, on_INTERPRETER, "Starting mainParsingLoop( " );
		if( recording )
			tb_sendTo( recording, interpreterTrace, th->heap );
		else
			os_trace( th->os, on_INTERPRETER, "(not recording)" );
		os_trace( th->os, on_INTERPRETER, " ) startingDepth=%d\n", startingDepth );
		}

	MemoryLifetime parseTime = ml_begin( 100, ml_indefinite() );
	Stack sk = sk_new( parseTime );

	int itemsFromPrevHandle = startingDepth;
	while( 1 )
		{
		Symbol handleSymbol;
		for (
			handleSymbol = ps_handle( th->ps, currentToken(th) );
			handleSymbol;
			handleSymbol = ps_handle( th->ps, currentToken(th) )
			){
			Grammar gr = ps_grammar( th->ps ); // Grammar can change as the program proceeds
			Production handleProduction = gr_production( gr, ob_getIntField( th->productionMap, handleSymbol, th->heap ) );
			Stack operandStack = ps_operandStack( th->ps );
			int handleLength = pn_length( handleProduction, gr );
			int depthWithoutHandle = sk_depth( operandStack ) - handleLength;

			Object boundObject = ob_getField( bindings, handleSymbol, th->heap );
			Function functionToCall = boundObject? ob_toFunction( boundObject, th->heap ) : NULL;
			File logThisHandle = os_logFile( th->os, on_EXECUTION );
			if( functionToCall )
				{
				if( functionToCall->kind == FN_NATIVE && os_traceFile( th->os, on_EXECUTION ) == NULL )
					{
					// Some native actions are pretty boring.  If we're logging (not tracing), skip the boring ones.
					int i;
					static const NativeAction silentActions[] = { nopAction, passThrough, parseTreeAction, recordTokenBlockAction, stopRecordingTokenBlockAction };
					NativeAction action = functionToCall->body.gl->response.action;
					for( i=0; logThisHandle && i < sizeof( silentActions )/sizeof( silentActions[0] ); i++ )
						if( action == silentActions[i] )
							logThisHandle = NULL;
					}
				}
			if( os_logging( th->os, on_EXECUTION ) )
				{
				if( logThisHandle )
					{
					os_log( th->os, on_EXECUTION, "#  State:" );

					int stackDepth             = sk_depth( ps_operandStack( th->ps ) );
					int newValues              = stackDepth - itemsFromPrevHandle;
					int firstNewValueIndex     = newValues - 1;
					int handleValues           = pn_length( handleProduction, gr );
					int firstHandleValueIndex  = handleValues - 1;
					int interestingValues      = max( newValues, handleValues );
					int totalValues            = max( interestingValues + 2, 10 );

					int prefixSpaces = 0;
					if( totalValues >= stackDepth )
						totalValues = stackDepth;
					else
						prefixSpaces += os_log( th->os, on_EXECUTION, "..." );

					if( 0 && os_log( th->os, on_EXECUTION, "\n# Stack parms:" ) )
						{
						os_log( th->os, on_EXECUTION, " stackDepth=%d", stackDepth );
						os_log( th->os, on_EXECUTION, " depthWithoutHandle=%d", depthWithoutHandle );
						os_log( th->os, on_EXECUTION, " newValues=%d", newValues );
						os_log( th->os, on_EXECUTION, " firstNewValueIndex=%d", firstNewValueIndex );
						os_log( th->os, on_EXECUTION, " handleValues=%d", handleValues );
						os_log( th->os, on_EXECUTION, " firstHandleValueIndex=%d", firstHandleValueIndex );
						os_log( th->os, on_EXECUTION, " interestingValues=%d", interestingValues );
						os_log( th->os, on_EXECUTION, " totalValues=%d", totalValues );
						os_log( th->os, on_EXECUTION, "\n#  State:" );
						}

					int newValuesColumn = INT_MAX;
					int handleColumn    = INT_MAX;
					int currentColumn   = 1; // The %*s thing won't work with zeros
					int i;
					for( i = totalValues-1; i >= 0; i-- )
						{
						currentColumn += os_log( th->os, on_EXECUTION, " " );
						if( i == firstHandleValueIndex )
							handleColumn = currentColumn;
						if( i == firstNewValueIndex )
							newValuesColumn = currentColumn;
						currentColumn += ob_sendTo( sk_item( ps_operandStack( th->ps ), i ), logThisHandle, th->heap );
						}
					newValuesColumn = min( newValuesColumn, currentColumn );
					handleColumn    = min( handleColumn,    currentColumn );
					if( os_log( th->os, on_EXECUTION, "  ||  " ) )
						sendTokenPreviewTo( th, os_logFile( th->os, on_EXECUTION ) );
					os_log( th->os, on_EXECUTION, "\n" );
					if( handleColumn < newValuesColumn )
						os_log( th->os, on_EXECUTION, "#%*s        %*s%*s\n", prefixSpaces, "", handleColumn    , "H", newValuesColumn-handleColumn, "^" );
					else
						os_log( th->os, on_EXECUTION, "#%*s        %*s%*s\n", prefixSpaces, "", newValuesColumn , "^", handleColumn-newValuesColumn, "H" );
					itemsFromPrevHandle = depthWithoutHandle;
					}
				else
					{
					// Every item should appear newly printed at least once
					if( itemsFromPrevHandle > depthWithoutHandle )
						itemsFromPrevHandle = depthWithoutHandle;
					}
				static const char depthStr[] = ""; //"----+----+----+----+----+----+----+----+----+----+----?";
				if( logThisHandle && os_log( th->os, on_EXECUTION, "%-17s: %.*s %s <-", sy_name( handleSymbol, th->st ), cxs_count( th->cxs ), depthStr, sy_name( pn_lhs( handleProduction, gr ), th->st ) ) )
					{
					char *sep = " ";
					int i;
					for( i=0; i < pn_length( handleProduction, gr ); i++ )
						{
						Symbol tokenSymbol = pn_token( handleProduction, i, gr );
						os_log( th->os, on_EXECUTION, "%s%s", sep, sy_name( tokenSymbol, th->st ) );
						Symbol nameSymbol = pn_name( handleProduction, i, gr );
						if( nameSymbol )
							{
							os_log( th->os, on_EXECUTION, "@%s=", sy_name( nameSymbol, th->st ) );
							Object value = sk_item( ps_operandStack( th->ps ), pn_length( handleProduction, gr ) - i - 1 );
							ob_sendTo( value, logThisHandle, th->heap );
							}
						sep = " ";
						}
					os_log( th->os, on_EXECUTION, "\n" );
					}
				}

			if( depthWithoutHandle < startingDepth )
				{
				os_trace( th->os, on_EXECUTION, "   Exiting before reduce past starting depth %d\n", startingDepth );
				int lengthToRecord = sk_depth( operandStack ) - recordedDepth;
				record( recording, lengthToRecord, sk, th );
				result = handleProduction;
				goto done;
				}

			FunctionKind kind = functionToCall? functionToCall->kind : FN_UNKNOWN;
			bool atLeastOneArgumentIsPlaceholder = false;
				{
				int i;
				for( i = pn_length( handleProduction, gr ) - 1; i >= 0 && !atLeastOneArgumentIsPlaceholder; i-- )
					{
					if( ob_isPlaceholder( sk_item( ps_operandStack( th->ps ), i ), th->heap) )
						{
						atLeastOneArgumentIsPlaceholder = true;
						if( logThisHandle )
							os_trace( th->os, on_EXECUTION, "   Argument at depth %d is a placeholder\n", i );
						}
					}
				}
			if( atLeastOneArgumentIsPlaceholder )
				{
				os_trace( th->os, on_INTERPRETER, "At least one argument is a placeholder\n" );
				switch( kind )
					{
					case FN_TOKEN_BLOCK:
						if( 1 ) // TODO: Expand token blocks with placeholders.  Need to evaluate arguments before executing the block.
							{
							kind = FN_UNKNOWN;
							if( logThisHandle )
								os_log( th->os, on_EXECUTION, "   An argument is a placeholder; dont execute\n" );
							}
						break;
					case FN_NATIVE:
						{
						GrammarLine line = functionToCall->body.gl;
						if( line->peSafe == PE_ABSTRACT && optional( "Execute native despite placeholders" ) )
							{
							// go for it
							}
						else
							{
							os_trace( th->os, on_INTERPRETER, "  Switching to FN_UNKNOWN\n" );
							kind = FN_UNKNOWN;
							}
						}
						break;
					case FN_UNKNOWN:
						break;
					}
				}

			switch( kind )
				{
				case FN_TOKEN_BLOCK:
					{
					assert( handleProduction );
					Object argBindings = newBindings( NULL, th );
					int i;
					for( i = pn_length( handleProduction, gr ) - 1; i >= 0; i-- )
						{
						Symbol nameSymbol = pn_name( handleProduction, i, gr );
						Object value = pop( th );
						if( nameSymbol )
							ob_setField( argBindings, nameSymbol, value, th->heap );
						}
					digress( th, ts_new( functionToCall->body.tb, th->heap ), argBindings );
					if( logThisHandle && os_trace( th->os, on_EXECUTION, "   Digressing into " ) )
						{
						tb_sendTo( functionToCall->body.tb, logThisHandle, th->heap );
						os_trace( th->os, on_EXECUTION, "\n" );
						}
					}
					break;
				case FN_NATIVE:
					{
					GrammarLine line = functionToCall->body.gl;
					assert( line );
					if( logThisHandle && os_trace( th->os, on_EXECUTION, "   Calling native " ) )
						{
						Dl_info nativeInfo;
						if( dladdr( line->response.action, &nativeInfo ) && nativeInfo.dli_saddr == line->response.action )
							os_trace( th->os, on_EXECUTION, "%s\n", nativeInfo.dli_sname );
						else
							os_trace( th->os, on_EXECUTION, "%p\n", PH( line->response.action ) );
						}
					line->response.action( handleProduction, line, th );
					}
					break;
				case FN_UNKNOWN:
					{
					os_trace( th->os, on_EXECUTION, "No action for %s\n", sy_name( handleSymbol, th->st ) );
					check( recording );
					// With an unknown action, we give up on trying to do a good job
					// with the tokens we've seen so far.  Append everything to the
					// token block, push a recordedPlaceholder, and proceed hoping
					// that we can do something useful with upcoming tokens.
					Stack operandStack = ps_operandStack( th->ps );
					int lengthToRecord = sk_depth( operandStack ) - recordedDepth;
					record( recording, lengthToRecord, sk, th );
					int handleLength = pn_length( handleProduction, gr );
					popN( handleLength, th );
					Symbol lhs = pn_lhs( handleProduction, gr );
					if( os_enabled( th->os, on_CONCRETIFICATION ) )
						{
						if( interpreterTrace )
							{
							os_trace( th->os, on_INTERPRETER, "Checking %s for concretification in: ", sy_name( lhs, th->st ) );
							ob_sendDeepTo( th->concretifications, interpreterTrace, th->heap );
							os_trace( th->os, on_INTERPRETER, "\n" );
							}
						Object concretified = ob_getFieldIfPresent( th->concretifications, lhs, NULL, th->heap );
						if( concretified )
							{
							os_trace( th->os, on_INTERPRETER, "  %s concretified into %s\n", sy_name( lhs, th->st ), sy_name( ob_toSymbol( concretified, th->heap ), th->st ) );
							lhs = ob_toSymbol( concretified, th->heap );
							}
						}
					push( oh_recordedPlaceholder( th->heap, lhs ), th );
					recordedDepth = sk_depth( operandStack );
					}
					break;
				}
			}
		Object raw = ts_current( currentStream(th) );
		if( !raw )
			{
			os_trace( th->os, on_INTERPRETER, "   Raw token is NULL\n" );
			goto done;
			}
		Object toPush = currentToken( th ); // TODO: Change this to make partial eval less aggressive
		if( interpreterTrace )
			{
			os_trace( th->os, on_INTERPRETER, "Pushing token from %p: ", PH( ts_tokenBlock( currentStream(th) ) ) );
			ob_sendTo( toPush, interpreterTrace, th->heap );
			os_trace( th->os, on_INTERPRETER, "\n" );
			}
		push( toPush, th );
		advanceToken( th );
		if( interpreterTrace )
			{
			os_trace( th->os, on_INTERPRETER, "Advanced %p: ", PH( ts_tokenBlock( currentStream(th) ) ) );
			ts_sendTo( currentStream(th), interpreterTrace );
			os_trace( th->os, on_INTERPRETER, "\n" );
			}
		}

	done:
	if( interpreterTrace )
		{
		os_trace( th->os, on_INTERPRETER, "Exiting mainParsingLoop" );
		if( ts_current( currentStream(th) ) )
			{
			os_trace( th->os, on_INTERPRETER, "; current token on %p is ", PH( ts_tokenBlock( currentStream(th) ) ) );
			ob_sendTo( ts_current( currentStream(th) ), interpreterTrace, th->heap );
			}
		os_trace( th->os, on_INTERPRETER, "\n" );
		}

	ml_end( parseTime );

	return result;
	}

NATIVE_ACTION void recordTokenBlockAction( Production handle, GrammarLine gl, Thread th )
	{
	TokenBlock tb = NULL; // TODO: some sort of token block lookup I guess?  Or maybe this is handled elsewhere?
	File interpreterTrace = os_traceFile( th->os, on_INTERPRETER );
	if( tb )
		{
		popN( pn_length( handle, ps_grammar( th->ps ) ), th );
		}
	else
		{
		tb = tb_new( ml_indefinite() );
		if( interpreterTrace )
			{
			os_trace( th->os, on_INTERPRETER, "  Begin recording token block\n" );
			tb_sendTo( tb, interpreterTrace, th->heap );
			os_trace( th->os, on_INTERPRETER, "\n" );
			}
		nopAction( handle, gl, th );

		Production finishingHandle = mainParsingLoop( tb, th->recordingBindings, th );

		if( interpreterTrace )
			{
			os_trace( th->os, on_INTERPRETER, "  Recording stopped at: " );
			pn_sendTo( handle, interpreterTrace, ps_grammar( th->ps ), th->st );
			os_trace( th->os, on_INTERPRETER, "\n" );
			}

		tb_stopAppending( tb );

		popN( pn_length( finishingHandle, ps_grammar( th->ps ) ), th );
		}
	Object result = ob_create( sy_byName( "TB_RECORDING", th->st ), th->heap );
	ob_setFieldX( result, SYM_VALUE, ob_fromTokenBlock( tb, th->heap ), th->heap );
	push( result, th );
	if( os_log( th->os, on_EXECUTION, "    Recorded token block: " ) ) // TODO: This is more like on_INTERPRETER but that's a mess right now
		{
		tb_sendTo( tb, os_logFile( th->os, on_EXECUTION ), th->heap );
		os_log( th->os, on_EXECUTION, "\n" );
		}
	if( interpreterTrace )
		{
		os_trace( th->os, on_INTERPRETER, "    Now current: " );
		ob_sendTo( ts_current( currentStream(th) ), interpreterTrace, th->heap );
		os_trace( th->os, on_INTERPRETER, "\n" );
		}
	}

#if 0
static File openTrace( int fd, char *name )
	{
	File result = fdopen( fd, "wt" );
	if( result )
		{
		setbuf( result, 0 );
		setvbuf( result, malloc(BUFSIZ), _IOLBF, 1000 );
		if( name )
			TRACE( result, "# %s\n", name );
		}
	return result;
	}
#endif

extern FILE *yyin;

static OptionSet processOptions( int argc, char **argv, MemoryLifetime ml )
	{
	OptionSet result = os_global();
	int i;
	for( i=1; i < argc; i++ )
		{
		char *arg = argv[i];
		if( arg[0] == '-' )
			{
			if( arg[1] == 'L' )
				{
				switch( arg[2] )
					{
					case '=':
						arg += 3;
						break;
					case 0:
						arg = argv[++i];
						break;
					default:
						arg += 2;
						break;
					}
				FILE *logFile = fopen( arg, "w" );
				if( logFile )
					{
					os_setLogFile( result, logFile );
					}
				else
					{
					fprintf( stderr, "Error opening log file '%s': %s\n", arg, strerror(errno) );
					exit(1);
					}
				}
			else
				{
				MemoryLifetime deltaTime = ml_begin( 1000, ml );
				OptionDelta delta = od_parse( arg+1, arg + strlen( arg ), deltaTime );
				od_applyTo( delta, result, ml );
				ml_end( deltaTime );
				}
			}
		else
			{
			FILE *inputFile = fopen( arg, "r" );
			if( inputFile )
				{
				yyin = inputFile;
				}
			else
				{
				fprintf( stderr, "Fatal error opening input file '%s': %s\n", arg, strerror(errno) );
				exit(1);
				}
			}
		}
	return result;
	}

int main( int argc, char **argv )
	{
	Thread th = &theThread;
	th->os = processOptions( argc, argv, ml_indefinite() );
	th->parserGenDiagnostics = NULL;
	th->heap = theObjectHeap();
	th->st = theSymbolTable( th->heap );
	initializeInheritanceRelation( th->heap, th->st, ml_indefinite(), th );
	th->executionBindings = newBindings( NULL, th );
	th->recordingBindings = newBindings( NULL, th );
	th->concretifications = ob_create( sy_byName( "CONCRETIFICATIONS", th->st ), th->heap );
	th->productionMap     = ob_create( sy_byName( "PRODUCTION_MAP",    th->st ), th->heap );
	th->cxs = cxs_new( 10, ml_indefinite() );
	digress( th, theLexTokenStream( th->heap ), newBindings( NULL, th ) );
	Grammar initialGrammar = populateGrammar( th->st, th );
	Automaton au = au_new( initialGrammar, th->st, th->heap, ml_indefinite(), th->os, os_logFile( th->os, on_PARSER_CONFLICT ), os_traceFile( th->os, on_PARSER_GEN ) );
	th->ps = ps_new( au, ml_indefinite(), os_logFile( th->os, on_INTERPRETER ) );
	th->timeBasis = currentTimeMillis();

	mainParsingLoop( NULL, th->executionBindings, th );

#ifndef NDEBUG
	File memreport = fdopen( 7, "wt" );
	ml_sendReportTo( memreport );
#endif
	return 0;
	}

//MERGE:70

