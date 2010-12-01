
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
	token_t token = nextToken();
	while( token )
		{
		if( token == WORD && sy_byName(lastWord(), theSymbolTable()) == target )
			break;
		else
			token = nextToken();
		}
	return NULL;
	}

static Action hop( Actor ar )
	{
	assert( di == di_fromActor( ar ) );
	nextToken();
	return NULL;
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
	sy_setImmediateAction( sy_byName( "new", st ), an_fromFunction( new ), st );
	sy_setImmediateAction( sy_byName( "ifzero", st ), an_fromFunction( ifzero ), st );
	sy_setImmediateAction( sy_byName( "hop", st ), an_fromFunction( hop ), st );
	return st;
	}

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
	token_t token = nextToken();
	while( token )
		{
		switch( token )
			{
			case ERROR:
			case NUM_TOKENS:
				fprintf( diagnostics, "Error: <<%s>>\n", lastString() );
				break;
			case NO_TOKEN:
				fprintf( diagnostics, "No token!\n" );
				break;
			case INT:
				{
				fprintf( diagnostics, "Int: %d\n", lastInt());
				push( ob_fromInt( lastInt(), heap ) );
				break;
				}
			case STRING:
				{
				fprintf( diagnostics, "String: %s\n", lastString() );
				push( ob_fromString( lastString(), heap ) );
				break;
				}
			case WORD:
				{
				Symbol sy = sy_byName(lastWord(), st);
				fprintf( diagnostics, "Word #%d %s\n", sy_index(sy, st), sy_name(sy, st) );
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
			}
		sk_sendTo( stack, diagnostics, heap );
		fprintf( diagnostics,"\n");
		di_sendTo( di, diagnostics );
		fprintf( diagnostics,"\n");
		token = nextToken();
		}
	}

//MERGE:30


