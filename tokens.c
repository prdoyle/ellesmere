
#include "tokens.h"
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
	TokenBlock result = (TokenBlock)malloc( tb_size(count) );
	result->count = count;
	return result;
	}

static TokenBlock tb_realloc( TokenBlock tb, int count )
	{
	TokenBlock result = (TokenBlock)realloc( tb, tb_size(count) );
	result->count = count;
	return result;
	}

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

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st )
	{
	TokenStream result = (TokenStream)malloc(sizeof(*result));
	result->kind   = LEX;
	result->heap   = heap;
	result->caller = NULL;
	result->data.lex.st = st;
	return result;
	}

FUNC TokenStream ts_fromBlock( TokenBlock block, ObjectHeap heap, TokenStream caller )
	{
	TokenStream result = (TokenStream)malloc(sizeof(*result));
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
					check(false);
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

FUNC TokenStream ts_caller ( TokenStream ts )
	{
	return ts->caller;
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

//MERGE:12

