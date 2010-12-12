
#include "tokens.h"
#include "memory.h"
#include "lex.h"
#include "lex.l.h"

typedef enum
	{
	LEX,
	BLOCK,
	} StreamKind;

struct tb_struct
	{
	int count;
	Object tokens[1];
	};

static int tb_size( int count )
	{
	TokenBlock tb;
	return
		  sizeof(*tb)
		- sizeof(tb->tokens)
		+ sizeof(tb->tokens[0]) * count;
	}

static TokenBlock tb_alloc( int count )
	{
	TokenBlock result = (TokenBlock)mem_alloc( tb_size(count) );
	result->count = count;
	return result;
	}

static TokenBlock tb_realloc( TokenBlock tb, int count )
	{
	TokenBlock result = (TokenBlock)mem_realloc( tb, tb_size(count) );
	result->count = count;
	return result;
	}

struct ts_struct
	{
	StreamKind  kind;
	ObjectHeap  heap;
	TokenStream caller;
	TokenStream free; // A TokenStream that could be used for caller
	union
		{
		struct
			{
			SymbolTable st;
			} lex;
		struct
			{
			TokenBlock tb;
			int index;
			} block;
		} data;
	};

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st )
	{
	TokenStream result = (TokenStream)mem_alloc(sizeof(*result));
	result->kind   = LEX;
	result->heap   = heap;
	result->caller = NULL;
	result->data.lex.st = st;
	result->free   = NULL;
	return result;
	}

FUNC TokenStream ts_fromBlock( TokenBlock block, ObjectHeap heap, TokenStream caller )
	{
	TokenStream result;
	//setbuf( stdout, 0 );
	if( caller && caller->free )
		{
		result = caller->free;
		caller->free = NULL;
		//printf("Reusing TokenStream %p\n", result);
		}
	else
		{
		result = (TokenStream)mem_alloc(sizeof(*result));
		//printf("New TokenStream %p\n", result);
		}
	result->kind   = BLOCK;
	result->heap   = heap;
	result->caller = caller;
	result->data.block.tb    = block;
	result->data.block.index = 0;
	result->free   = NULL;
	return result;
	}

FUNC Object ts_next( TokenStream ts )
	{
	switch( ts->kind )
		{
		case LEX:
			switch( yylex() )
				{
				case NUM_TOKENS:
				case ERROR:
					check(!"Error token!");
					// fall through
				case NO_TOKEN:
					break;
				case INT:
					return ob_fromInt( lastInt(), ts->heap );
				case STRING:
					return ob_fromString( lastString(), ts->heap );
				case WORD:
					return oh_symbolToken( ts->heap, sy_byName( lastWord(), ts->data.lex.st ) );
				}
			break;
		case BLOCK:
			if( ts->data.block.index < ts->data.block.tb->count )
				return ts->data.block.tb->tokens[ ts->data.block.index++ ];
			break;
		}
	return NULL;
	}

FUNC TokenStream ts_caller( TokenStream ts )
	{
	return ts->caller;
	}

FUNC ObjectHeap ts_heap( TokenStream ts )
	{
	return ts->heap;
	}

FUNC TokenStream ts_close( TokenStream ts )
	{
	TokenStream result = ts->caller;
	if( result->free == NULL )
		result->free = ts;
	else
		free( ts );
	return result;
	}

FUNC TokenBlock ts_recordUntil( TokenStream ts, Symbol terminator )
	{
	int capacity = 100;
	int count = 0;
	TokenBlock tb = tb_alloc( capacity );
	Object terminatorToken = oh_symbolToken( ts->heap, terminator );
	Object curToken;
	for( curToken = ts_next(ts); curToken && curToken != terminatorToken; curToken = ts_next(ts) )
		{
		if( count == capacity )
			{
			capacity *= 2;
			tb = tb_realloc( tb, capacity );
			}
		tb->tokens[ count++ ] = curToken;
		}
	return tb_realloc( tb, count ); // shrink to the proper size
	}

//MERGE:30

