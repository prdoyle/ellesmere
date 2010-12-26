
#include "tokens.h"
#include "memory.h"
#include "lex.h"
#include "lex.l.h"

typedef enum
	{
	LEX,
	BLOCK,
	} StreamKind;

struct ts_struct
	{
	StreamKind  kind;
	ObjectHeap  heap;
	TokenStream caller;
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

typedef struct tss_struct *TokenStreamStack;
#define AR_PREFIX  tss
#define AR_TYPE    TokenStreamStack
#define AR_ELEMENT TokenStream
#define AR_BYVALUE
#include "array_template.h"

struct tb_struct
	{
	int count;
	TokenStreamStack streams;
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

static TokenBlock tb_alloc( int count, MemoryLifetime ml )
	{
	TokenBlock result = (TokenBlock)ml_alloc( ml, tb_size(count) );
	result->count = count;
	result->streams = tss_new( 2, ml ); // more than 2x recursion probably means deep recursion
	return result;
	}

static TokenBlock tb_realloc( TokenBlock tb, int oldCount, int count, MemoryLifetime ml )
	{
	TokenBlock result = (TokenBlock)ml_realloc( ml, tb, tb_size(oldCount), tb_size(count) );
	result->count = count;
	return result;
	}

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st )
	{
	static TokenStream result = NULL;
	if( !result )
		{
		result = (TokenStream)ml_alloc( ml_singleton(), sizeof(*result) );
		result->kind   = LEX;
		result->heap   = heap;
		result->caller = NULL;
		result->data.lex.st = st;
		}
	return result;
	}

FUNC TokenStream ts_fromBlock( TokenBlock block, ObjectHeap heap, TokenStream caller )
	{
	TokenStream result;
	if( tss_count( block->streams ) >= 1 )
		{
		result = tss_getLast( block->streams, 0 );
		tss_incCountBy( block->streams, -1 );
		}
	else
		result = (TokenStream)ml_alloc( ml_undecided(), sizeof(*result) );
	result->kind   = BLOCK;
	result->heap   = heap;
	result->caller = caller;
	result->data.block.tb    = block;
	result->data.block.index = 0;
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
	if( ts->kind == BLOCK )
		tss_append( ts->data.block.tb->streams, ts );
	return ts->caller;
	}

FUNC TokenBlock ts_recordUntil( TokenStream ts, Symbol terminator )
	{
	int capacity = 100;
	int count = 0;
	MemoryLifetime ml = ml_undecided();
	TokenBlock tb = tb_alloc( capacity, ml );
	Object terminatorToken = oh_symbolToken( ts->heap, terminator );
	Object curToken;
	for( curToken = ts_next(ts); curToken && curToken != terminatorToken; curToken = ts_next(ts) )
		{
		if( count == capacity )
			{
			tb = tb_realloc( tb, capacity, capacity*2, ml );
			capacity *= 2;
			}
		tb->tokens[ count++ ] = curToken;
		}
	return tb_realloc( tb, capacity, count, ml ); // shrink to the proper size
	}

//MERGE:30

