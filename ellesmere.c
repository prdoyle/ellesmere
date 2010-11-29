
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"

static token_t nextToken(){ return (token_t)yylex(); }

static Stack      stack;
static ObjectHeap heap;
static Dispatcher di;
static Object     globals;

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
	Symbol field  = ob_tag( pop(), heap );
	Object ob     = pop();
	Object value  = pop();
	ob_setField( ob, field, value, heap );
	return NULL;
	}

static Action get( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	Symbol field  = ob_tag( pop(), heap );
	Object ob     = pop();
	return push( ob_getField( ob, field, heap ) );
	}

static SymbolTable populateSymbolTable( SymbolTable st )
	{
	sy_setImmediateAction( sy_byName( "pop", st ), an_fromFunction( popAction ), st );
	sy_setImmediateAction( sy_byName( "dup", st ), an_fromFunction( dupAction ), st );
	sy_setImmediateAction( sy_byName( "deep", st ), an_fromFunction( deep ), st );
	sy_setImmediateAction( sy_byName( "print", st ), an_fromFunction( print ), st );
	sy_setImmediateAction( sy_byName( "add", st ),   an_fromFunction( add ),   st );
	sy_setImmediateAction( sy_byName( "global", st ), an_fromFunction( global ), st );
	sy_setImmediateAction( sy_byName( "set", st ), an_fromFunction( set ), st );
	sy_setImmediateAction( sy_byName( "get", st ), an_fromFunction( get ), st );
	return st;
	}

#define trace printf

int main(int argc, char **argv)
	{
	SymbolTable st = populateSymbolTable( theSymbolTable() );
	di = di_new( st, NULL );
	heap = theObjectHeap();
	globals = ob_create( sy_byName( "$GLOBALS", st ), heap );
	stack = sk_new();
	token_t token = nextToken();
	while( token )
		{
		switch( token )
			{
			case ERROR:
			case NUM_TOKENS:
				printf( "Error: <<%s>>\n", lastString() );
				break;
			case NO_TOKEN:
				printf( "No token!\n" );
				break;
			case INT:
				{
				trace("Int: %d\n", lastInt());
				push( ob_fromInt( lastInt(), heap ) );
				break;
				}
			case STRING:
				{
				printf( "String: %s\n", lastString() );
				push( ob_fromString( lastString(), heap ) );
				break;
				}
			case WORD:
				{
				Symbol sy = sy_byName(lastWord(), st);
				printf( "Word #%d %s\n", sy_index(sy, st), sy_name(sy, st) );
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
					sk_push( stack, ob_create( sy, heap ) );
				break;
				}
			}
		sk_sendTo( stack, stdout, heap );
		printf("\n");
		/*
		di_sendTo( di, stdout );
		printf("\n");
		*/
		token = nextToken();
		}
	}

//MERGE:30


