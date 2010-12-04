
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"
#include "tokens.h"
#include <stdarg.h>

static TokenStream tokenStream;
static Stack       stack;
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
	return di_action( di, ob_tag( ob, heap ) );
	}

static Object pop()
	{
	di_discard( di, 1 );
	return sk_pop( stack );
	}

static Action popAction()
	{
	pop();
	return NULL;
	}

static Action pop2()
	{
	Object keeper = pop();
	pop();
	return push( keeper );
	}

static Action dupAction()
	{
	return push( sk_top(stack) );
	}

static Action deep()
	{
	int depth = ob_toInt( pop(), heap );
	push( sk_item( stack, depth ) );
	return NULL;
	}

static Action print()
	{
	ob_sendTo( pop(), stdout, heap );
	printf("\n");
	return NULL;
	}

static Action add()
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	return push( ob_fromInt( left + right, heap ) );
	}

static Action sub()
	{
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	return push( ob_fromInt( left - right, heap ) );
	}

static Action global()
	{
	return push( globals );
	}

static Action set()
	{
	Symbol field  = ob_toSymbol( pop(), heap );
	Object ob     = pop();
	Object value  = pop();
	ob_setField( ob, field, value, heap );
	return NULL;
	}

static Action get()
	{
	Symbol field  = ob_toSymbol( pop(), heap );
	Object ob     = pop();
	return push( ob_getField( ob, field, heap ) );
	}

static Action new()
	{
	Symbol tag = ob_toSymbol( pop(), heap );
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

static Action brancheq()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left == right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchne()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left != right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchlt()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left < right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchle()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left <= right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchgt()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left > right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action branchge()
	{
	Object target = pop();
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	if( left >= right )
		return eatUntilObject( target );
	else
		return NULL;
	}

static Action hop()
	{
	ts_next( tokenStream );
	return NULL;
	}

static Action block()
	{
	Symbol terminator = ob_toSymbol( pop(), heap );
	TokenBlock tb = ts_recordUntil( tokenStream, terminator );
	return push( ob_fromTokenBlock( tb, heap ) );
	}

static Action call()
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	return NULL;
	}

static Action gotoAction()
	{
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	tokenStream = ts_fromBlock( block, heap, ts_caller( tokenStream ) );
	return NULL;
	}

static Action returnAction()
	{
	check( ts_caller( tokenStream ) != NULL );
	tokenStream = ts_caller( tokenStream );
	return NULL;
	}

static struct ist_struct
	{
	char *name;
	ActionFunction function;
	} initialSymbolTable [] =
	{
	{ "add",                   add                     },
	{ "block",                 block                   },
	{ "brancheq",              brancheq                },
	{ "branchge",              branchge                },
	{ "branchgt",              branchgt                },
	{ "branchle",              branchle                },
	{ "branchlt",              branchlt                },
	{ "branchne",              branchne                },
	{ "call",                  call                    },
	{ "deep",                  deep                    },
	{ "dup",                   dupAction               },
	{ "get",                   get                     },
	{ "global",                global                  },
	{ "goto",                  gotoAction              },
	{ "hop",                   hop                     },
	{ "new",                   new                     },
	{ "pop",                   popAction               },
	{ "pop2",                  pop2                    },
	{ "print",                 print                   },
	{ "return",                returnAction            },
	{ "set",                   set                     },
	{ "sub",                   sub                     },
	};

static SymbolTable populateSymbolTable( SymbolTable st )
	{
	int i;
	for( i=0; i < sizeof( initialSymbolTable ) / sizeof( initialSymbolTable[0] ); i++ )
		{
		struct ist_struct *entry = initialSymbolTable + i;
		sy_setImmediateAction( sy_byName( entry->name, st ), an_fromFunction( entry->function ), st );
		}
	return st;
	}

static token_t nextToken(){ return (token_t)yylex(); }

int main(int argc, char **argv)
	{
	diagnostics = fdopen( 3, "wt" );
	SymbolTable st = populateSymbolTable( theSymbolTable() );
	di = di_new( st, NULL );
	heap = theObjectHeap();
	globals = ob_create( sy_byName( "$GLOBALS", st ), heap );
	stack = sk_new();
	tokenStream = theLexTokenStream( heap, st );
	Object ob = ts_next( tokenStream );
	while( ob )
		{
		if( diagnostics )
			{
			trace( diagnostics, "# Token from %p is ", tokenStream );
			ob_sendTo( ob, diagnostics, heap );
			trace( diagnostics, "\n");
			}
		switch( sy_index( ob_tag(ob, heap), st ) )
			{
			case SYM_TOKEN:
				{
				Symbol sy = ob_toSymbol( ob, heap );
				Action an = di_action( di, sy );
				if( an )
					{
					while( an )
						an = an_perform( an );
					}
				else if( ob_hasField( globals, sy, heap ) )
					{
					di_discard( di, 1 );
					push( ob_getField( globals, sy, heap ) );
					}
				else
					sk_push( stack, oh_symbolToken( heap, sy ) );
				break;
				}
			default:
				push( ob );
				break;
			}
		if( diagnostics )
			{
			trace( diagnostics, "  ");
			sk_sendTo( stack, diagnostics, heap );
			trace( diagnostics, "\n  ");
			di_sendTo( di, diagnostics );
			trace( diagnostics, "\n");
			}
		ob = ts_next( tokenStream );
		}
	}

//MERGE:30


