
#include "lex.h"
#include "lex.l.h"
#include "stack.h"
#include "dispatcher.h"

static token_t nextToken(){ return (token_t)yylex(); }

static Stack      stack;
static ObjectHeap heap;

static Action add( Actor ar )
	{
	Dispatcher di = di_fromActor( ar );
	//di_sendTo( di, stdout );
	//printf("\n");
	di_discard( di, 2 );
	//di_sendTo( di, stdout );
	//printf("\n");
	int right = ob_toInt( sk_pop(stack), heap );
	int left  = ob_toInt( sk_pop(stack), heap );
	sk_push( stack, ob_fromInt( left + right, heap ) );
	return di_action( di, ob_tag( sk_top(stack), heap ) );
	}

static Action print( Actor ar )
	{
	Dispatcher di = di_fromActor( ar );
	//di_sendTo( di, stdout );
	//printf("\n");
	di_discard( di, 1 );
	//di_sendTo( di, stdout );
	//printf("\n");
	ob_sendTo( sk_pop(stack), stdout, heap );
	printf("\n");
	return NULL;
	}

static SymbolTable populateSymbolTable( SymbolTable st )
	{
	sy_setImmediateAction( sy_byName( "+", st ),     an_fromFunction( add ),   st );
	sy_setImmediateAction( sy_byName( "print", st ), an_fromFunction( print ), st );
	return st;
	}

#define trace printf

int main(int argc, char **argv)
	{
	SymbolTable st = populateSymbolTable( theSymbolTable() );
	Dispatcher di = di_new( st, NULL );
	heap = theObjectHeap();
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
				sk_push( stack, ob_fromInt( lastInt(), heap ) );
				di_action( di, ob_tag( sk_top(stack), heap ) );
				break;
				}
			case STRING:
				{
				printf( "String: %s\n", lastString() );
				sk_push( stack, ob_fromString( lastString(), heap ) );
				di_action( di, ob_tag( sk_top(stack), heap ) );
				break;
				}
			case WORD:
				{
				Symbol sy = sy_byName(lastWord(), st);
				printf( "Word #%d %s\n", sy_index(sy, st), sy_name(sy, st) );
				Action an = di_action( di, sy );
				while( an )
					an = an_perform( an, di_actor(di) );
				break;
				}
			}
		/*
		sk_sendTo( stack, stdout, heap );
		printf("\n");
		di_sendTo( di, stdout );
		printf("\n");
		*/
		token = nextToken();
		}
	}

//MERGE:30


