
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
			Object current;
			Object next;
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

typedef struct oba_struct *ObjectArray;
#define AR_PREFIX  oba
#define AR_TYPE    ObjectArray
#define AR_ELEMENT Object
#define AR_BYVALUE
#include "array_template.h"

struct tb_struct
	{
	ObjectArray      tokens;
	TokenStreamStack streams;
	};

static Object getLexToken( TokenStream ts )
	{
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
	return NULL;
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
		result->data.lex.st      = st;
		result->data.lex.current = getLexToken( result );
		result->data.lex.next    = getLexToken( result );
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

static Object blockToken( TokenStream ts, int index )
	{
	int stopIndex = oba_count( ts->data.block.tb->tokens );
	if( index < stopIndex )
		return oba_get( ts->data.block.tb->tokens, index );
	else
		return NULL;
	}

FUNC Object ts_current( TokenStream ts )
	{
	switch( ts->kind )
		{
		case LEX:
			return ts->data.lex.current;
		case BLOCK:
			return blockToken( ts, ts->data.block.index );
		}
	assert(0);
	return NULL;
	}

FUNC Object ts_next( TokenStream ts )
	{
	switch( ts->kind )
		{
		case LEX:
			return ts->data.lex.next;
		case BLOCK:
			return blockToken( ts, ts->data.block.index + 1);
		}
	assert(0);
	return NULL;
	}

FUNC void ts_advance( TokenStream ts )
	{
	switch( ts->kind )
		{
		case LEX:
			ts->data.lex.current = ts->data.lex.next;
			ts->data.lex.next    = getLexToken( ts );
			break;
		case BLOCK:
			if( ts->data.block.index < oba_count( ts->data.block.tb->tokens ) )
				ts->data.block.index += 1;
			break;
		}
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

enum { DEFAULT_TOKEN_BLOCK_LENGTH=29 };

FUNC TokenBlock tb_new( MemoryLifetime ml )
	{
	TokenBlock result = (TokenBlock)ml_alloc( ml, sizeof(*result) );
	result->tokens  = oba_new( DEFAULT_TOKEN_BLOCK_LENGTH, ml );
	result->streams = tss_new( 2, ml ); // more than 2x recursion probably means deep recursion
	return result;
	}

FUNC void tb_append( TokenBlock tb, Object token )
	{
	oba_append( tb->tokens, token );
	}

FUNC void tb_stopAppending( TokenBlock tb )
	{
	oba_shrinkWrap( tb->tokens );
	}

//MERGE:30

