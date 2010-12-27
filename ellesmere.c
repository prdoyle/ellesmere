
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"
#include "tokens.h"
#include "memory.h"
#include <stdarg.h>

static TokenStream tokenStream;
static Stack       stack;
static Context     currentScope;
static ObjectHeap  heap;
static Dispatcher  di;
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

static Action push( Object ob )
	{
	sk_push( stack, ob );
	return di_action( di, ob, currentScope );
	}

static Object pop()
	{
	return sk_pop( stack );
	}

static Symbol popToken()
	{
	Object popped = pop();
	assert( ob_isToken( popped, heap ) );
	return ob_toSymbol( popped, heap );
	}

static Action valueof( Action an, Context cx )
	{
	Symbol symbol = ob_toSymbol( pop(), heap );
	return push( sy_value( symbol, cx ) );
	}

static Action popAction( Action an, Context cx )
	{
	Symbol symbol = ob_toSymbol( pop(), heap );
	popToken();
	sy_setValue( symbol, pop(), cx );
	sy_setImmediateAction( symbol, an_fromFunctionAndSymbol( valueof, symbol ), cx );
	trace( diagnostics, "  sy_setValue( %p, %p, %p )\n", symbol, sy_value( symbol, cx ), cx );
	return NULL;
	}

static Action dupAction( Action an, Context cx )
	{
	popToken();
	return push( sk_top(stack) );
	}

static Action deep( Action an, Context cx )
	{
	int depth = ob_toInt( pop(), heap );
	popToken();
	push( sk_item( stack, depth ) );
	return NULL;
	}

static Action print( Action an, Context cx )
	{
	popToken();
	ob_sendTo( pop(), stdout, heap );
	printf("\n");
	return NULL;
	}

static Action add( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	popToken();
	return push( ob_fromInt( left + right, heap ) );
	}

static Action sub( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	popToken();
	return push( ob_fromInt( left - right, heap ) );
	}

static Action set( Action an, Context cx )
	{
	Object value  = pop();
	Object ob     = pop();
	Symbol field  = ob_toSymbol( pop(), heap );
	popToken();
	ob_setField( ob, field, value, heap );
	return NULL;
	}

static Action get( Action an, Context cx )
	{
	Object ob     = pop();
	Symbol field  = ob_toSymbol( pop(), heap );
	popToken();
	return push( ob_getField( ob, field, heap ) );
	}

static Action new( Action an, Context cx )
	{
	Symbol tag = ob_toSymbol( pop(), heap );
	popToken();
	return push( ob_create( tag, heap ) );
	}

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

static Action brancheq( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left == right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchne( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left != right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchlt( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left < right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchle( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left <= right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchgt( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left > right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchge( Action an, Context cx )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	Object target = pop();
	popToken();
	if( left >= right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action hop( Action an, Context cx )
	{
	popToken();
	ts_next( tokenStream );
	return NULL;
	}

static Action blockto( Action an, Context cx )
	{
	Symbol terminator = ob_toSymbol( pop(), heap );
	popToken();
	TokenBlock tb = ts_recordUntil( tokenStream, terminator );
	return push( ob_fromTokenBlock( tb, heap ) );
	}

static Action call( Action an, Context cx )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	popToken();
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	return NULL;
	}

static Action gotoAction( Action an, Context cx )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	popToken();
	tokenStream = ts_fromBlock( block, heap, ts_caller( tokenStream ) );
	return NULL;
	}

static Action returnAction( Action an, Context cx )
	{
	popToken();
	check( ts_caller( tokenStream ) != NULL );
	tokenStream = ts_close( tokenStream );
	cx_restore( currentScope );
	return NULL;
	}

static Action reduce( Action an, Context cx )
	{
	TokenBlock block = ob_toTokenBlock( sy_value( an_symbol(an), cx ), heap );
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	cx_save( currentScope );
	return NULL;
	}

static Action def( Action an, Context cx )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	int        arity = ob_toInt( pop(), heap );
	Symbol    symbol = ob_toSymbol( pop(), heap );
	popToken();
	sy_setImmediateAction( symbol, an_fromFunctionAndSymbol( reduce, symbol ), cx );
	sy_setArity( symbol, arity, cx );
	sy_setValue( symbol, ob_fromTokenBlock( block, heap ), cx );
	return NULL;
	}

static struct ist_struct
	{
	char *name;
	ActionFunction function;
	int arity;
	bool isSymbolic;
	} initialSymbolTable [] =
	{
	{ "add",                   add                ,2   },
	{ "blockto",               blockto            ,1,1 },
	{ "brancheq",              brancheq           ,3   },
	{ "branchge",              branchge           ,3   },
	{ "branchgt",              branchgt           ,3   },
	{ "branchle",              branchle           ,3   },
	{ "branchlt",              branchlt           ,3   },
	{ "branchne",              branchne           ,3   },
	{ "call",                  call               ,1   },
	{ "deep",                  deep               ,1   },
	{ "def",                   def                ,3,1 },
	{ "dup",                   dupAction               },
	{ "get",                   get                ,2,1 },
	{ "goto",                  gotoAction         ,1   },
	{ "hop",                   hop                     },
	{ "new",                   new                ,1,1 },
	{ "pop",                   popAction          ,1,1 },
	{ "print",                 print                   },
	{ "return",                returnAction            },
	{ "set",                   set                ,3,1 },
	{ "sub",                   sub                ,2   },
	};

static Context populateScope( Context cx )
	{
	int i;
	for( i=0; i < sizeof( initialSymbolTable ) / sizeof( initialSymbolTable[0] ); i++ )
		{
		struct ist_struct *entry = initialSymbolTable + i;
		Symbol sy = sy_byName( entry->name, cx_symbolTable(cx) );
		sy_setImmediateAction ( sy, an_fromFunction( entry->function ), cx );
		sy_setArity           ( sy, entry->arity, cx );
		sy_setIsSymbolic      ( sy, entry->isSymbolic, cx );
		}
	return cx;
	}

int main(int argc, char **argv)
	{
	diagnostics = fdopen( 3, "wt" );
	SymbolTable st = theSymbolTable();
	currentScope = populateScope( cx_new( st ) );
	heap = theObjectHeap();
	di = di_new( heap, st, NULL, diagnostics );
	stack = sk_new( ml_indefinite() );
	tokenStream = theLexTokenStream( heap, st );
	Object ob = ts_next( tokenStream );
	while( ob )
		{
		Action an = NULL;
		if( diagnostics )
			{
			trace( diagnostics, "# Token from %p is ", tokenStream );
			ob_sendTo( ob, diagnostics, heap );
			trace( diagnostics, "\n");
			}
		an = push( ob );
		while( an )
			an = an_perform( an, currentScope );
		if( diagnostics )
			{
			trace( diagnostics, "  ");
			sk_sendTo( stack, diagnostics, heap );
			trace( diagnostics, "\n  ");
			di_sendTo( di, diagnostics );
			trace( diagnostics, "\n  ");
			cx_sendTo( currentScope, diagnostics );
			trace( diagnostics, "\n");
			}
		ob = ts_next( tokenStream );
		}
#ifndef NDEBUG
	File memreport = fdopen( 4, "wt" );
	ml_sendReportTo( memreport );
#endif
	}

//MERGE:70

