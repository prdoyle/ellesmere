
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"
#include "tokens.h"

static TokenStream tokenStream;
static Stack       stack;
static ObjectHeap  heap;
static Dispatcher  di;
static Object      globals;

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

static Action popAction( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	pop();
	return NULL;
	}

static Action dupAction( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	push( sk_top(stack) );
	return NULL;
	}

static Action deep( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	int distance = ob_toInt( pop(), heap );
	push( sk_item( stack, distance ) );
	return NULL;
	}

static Action print( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	ob_sendTo( pop(), stdout, heap );
	printf("\n");
	return NULL;
	}

static Action add( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	int right = ob_toInt( pop(), heap );
	int left  = ob_toInt( pop(), heap );
	return push( ob_fromInt( left + right, heap ) );
	}

static Action global( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	return push( globals );
	}

static Action set( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol field  = ob_toSymbol( pop(), heap );
	Object ob     = pop();
	Object value  = pop();
	ob_setField( ob, field, value, heap );
	return NULL;
	}

static Action get( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol field  = ob_toSymbol( pop(), heap );
	Object ob     = pop();
	return push( ob_getField( ob, field, heap ) );
	}

static Action new( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol tag = ob_toSymbol( pop(), heap );
	return push( ob_create( tag, heap ) );
	}

static Action ifzero( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol target = ob_toSymbol( pop(), heap );
	int    flag   = ob_toInt( pop(), heap );
	if( flag )
		return NULL;
	Object ob = ts_next( tokenStream );
	while( ob )
		{
		if(   ob_tag(ob, heap) == sy_byIndex( SYM_TOKEN, theSymbolTable() )
			&& ob_toSymbol(ob, heap) == target )
			break;
		else
			ob = ts_next( tokenStream );
		}
	return NULL;
	}

static Action hop( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	ts_next( tokenStream );
	return NULL;
	}

static Action block( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol terminator = ob_toSymbol( pop(), heap );
	TokenBlock tb = ts_recordUntil( tokenStream, terminator );
	return push( ob_fromTokenBlock( tb, heap ) );
	}

static Action call( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	tokenStream = ts_fromBlock( block, heap, tokenStream );
	return NULL;
	}

static Action gotoAction( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	TokenBlock block = ob_toTokenBlock( pop(), heap );
	tokenStream = ts_fromBlock( block, heap, ts_caller( tokenStream ) );
	return NULL;
	}

static Action returnAction( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
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
	{ "call",                  call                    },
	{ "deep",                  deep                    },
	{ "dup",                   dupAction               },
	{ "get",                   get                     },
	{ "global",                global                  },
	{ "goto",                  gotoAction              },
	{ "hop",                   hop                     },
	{ "ifzero",                ifzero                  },
	{ "new",                   new                     },
	{ "pop",                   popAction               },
	{ "print",                 print                   },
	{ "return",                returnAction            },
	{ "set",                   set                     },
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

#define trace printf

int main(int argc, char **argv)
	{
	FILE *diagnostics = fdopen( 3, "wt" );
	if( !diagnostics )
		diagnostics = fopen( "/dev/null", "wt" );
	SymbolTable st = populateSymbolTable( theSymbolTable() );
	di = di_new( st, NULL );
	heap = theObjectHeap();
	globals = ob_create( sy_byName( "$GLOBALS", st ), heap );
	stack = sk_new();
	tokenStream = theLexTokenStream( heap, st );
	Object ob = ts_next( tokenStream );
	while( ob )
		{
		fprintf( diagnostics, "== Token from %p is ", tokenStream );
		ob_sendTo( ob, diagnostics, heap );
		fprintf( diagnostics, " ==\n");
		switch( sy_index( ob_tag(ob, heap), st ) )
			{
			case SYM_TOKEN:
				{
				Symbol sy = ob_toSymbol( ob, heap );
				Action an = di_action( di, sy );
				if( an )
					{
					while( an )
						an = an_perform( an, di_actor(di) );
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
		fprintf( diagnostics, "  ");
		sk_sendTo( stack, diagnostics, heap );
		fprintf( diagnostics, "\n  ");
		di_sendTo( di, diagnostics );
		fprintf( diagnostics, "\n");
		ob = ts_next( tokenStream );
		}
	}

//MERGE:30


