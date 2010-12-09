
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"
#include "tokens.h"
#include <stdarg.h>

static TokenStream tokenStream;
static Stack       stack;
static Scope       currentScope;
static ObjectHeap  heap;
static Dispatcher  di;
static Object      globals;
FILE *diagnostics;

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

static Action valueof( Action an, Scope sc )
	{
	Symbol symbol = ob_toSymbol( pop(), heap );
	return push( sy_value( symbol, sc ) );
	}

static Action popAction( Action an, Scope sc )
	{
	Symbol symbol = ob_toSymbol( pop(), heap );
	popToken();
	sy_setValue( symbol, pop(), sc );
	sy_setImmediateAction( symbol, an_fromFunction( valueof ), sc );
	trace( diagnostics, "  sy_setValue( %p, %p, %p )\n", symbol, sy_value( symbol, sc ), sc );
	return NULL;
	}

static Action dupAction( Action an, Scope sc )
	{
	popToken();
	return push( sk_top(stack) );
	}

static Action deep( Action an, Scope sc )
	{
	int depth = ob_toInt( pop(), heap );
	popToken();
	push( sk_item( stack, depth ) );
	return NULL;
	}

static Action print( Action an, Scope sc )
	{
	popToken();
	ob_sendTo( pop(), stdout, heap );
	printf("\n");
	return NULL;
	}

static Action add( Action an, Scope sc )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	popToken();
	return push( ob_fromInt( left + right, heap ) );
	}

static Action sub( Action an, Scope sc )
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	popToken();
	return push( ob_fromInt( left - right, heap ) );
	}

static Action set( Action an, Scope sc )
	{
	Object value  = pop();
	Object ob     = pop();
	Symbol field  = ob_toSymbol( pop(), heap );
	popToken();
	ob_setField( ob, field, value, heap );
	return NULL;
	}

static Action get( Action an, Scope sc )
	{
	Object ob     = pop();
	Symbol field  = ob_toSymbol( pop(), heap );
	popToken();
	return push( ob_getField( ob, field, heap ) );
	}

static Action new( Action an, Scope sc )
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

static Action brancheq( Action an, Scope sc )
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

static Action branchne( Action an, Scope sc )
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

static Action branchlt( Action an, Scope sc )
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

static Action branchle( Action an, Scope sc )
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

static Action branchgt( Action an, Scope sc )
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

static Action branchge( Action an, Scope sc )
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

static Action hop( Action an, Scope sc )
	{
	popToken();
	ts_next( tokenStream );
	return NULL;
	}

static Action blockto( Action an, Scope sc )
	{
	Symbol terminator = ob_toSymbol( pop(), heap );
	popToken();
	TokenBlock tb = ts_recordUntil( tokenStream, terminator );
	return push( ob_fromTokenBlock( tb, heap ) );
	}

static Action call( Action an, Scope sc )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	popToken();
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	return NULL;
	}

static Action gotoAction( Action an, Scope sc )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	popToken();
	tokenStream = ts_fromBlock( block, heap, ts_caller( tokenStream ) );
	return NULL;
	}

static Action returnAction( Action an, Scope sc )
	{
	popToken();
	check( ts_caller( tokenStream ) != NULL );
	tokenStream = ts_caller( tokenStream );
	currentScope = sc_outer( currentScope );
	return NULL;
	}

static Action reduce( Action an, Scope sc )
	{
	TokenBlock block = ob_toTokenBlock( sy_value( an_symbol(an), sc ), heap );
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	currentScope = sc_new( currentScope );
	return NULL;
	}

static Action def( Action an, Scope sc )
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	int        arity = ob_toInt( pop(), heap );
	Symbol    symbol = ob_toSymbol( pop(), heap );
	popToken();
	sy_setImmediateAction( symbol, an_fromFunctionAndSymbol( reduce, symbol ), sc );
	sy_setArity( symbol, arity, sc );
	sy_setValue( symbol, ob_fromTokenBlock( block, heap ), sc );
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

static Scope populateScope( Scope sc )
	{
	int i;
	for( i=0; i < sizeof( initialSymbolTable ) / sizeof( initialSymbolTable[0] ); i++ )
		{
		struct ist_struct *entry = initialSymbolTable + i;
		Symbol sy = sy_byName( entry->name, sc_symbolTable(sc) );
		sy_setImmediateAction ( sy, an_fromFunction( entry->function ), sc );
		sy_setArity           ( sy, entry->arity, sc );
		sy_setIsSymbolic      ( sy, entry->isSymbolic, sc );
		}
	return sc;
	}

static token_t nextToken(){ return (token_t)yylex(); }

int main(int argc, char **argv)
	{
	diagnostics = fdopen( 3, "wt" );
	SymbolTable st = theSymbolTable();
	currentScope = sc_new( populateScope( st_outermostScope( st ) ) );
	heap = theObjectHeap();
	di = di_new( heap, st, NULL, diagnostics );
	globals = ob_create( sy_byName( "$GLOBALS", st ), heap );
	stack = sk_new();
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
			sc_sendTo( currentScope, diagnostics );
			trace( diagnostics, "\n");
			}
		ob = ts_next( tokenStream );
		}
	}

//MERGE:30


