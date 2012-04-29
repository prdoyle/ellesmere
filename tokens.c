
#include "tokens.h"
#include "memory.h"
#include "lex.h"
#include "lex.l.h"
#include "options.h"

typedef enum
	{
	LEX,
	BLOCK,
	} StreamKind;

struct ts_struct
	{
	StreamKind kind;
	ObjectHeap heap;
	union
		{
		struct
			{
			Object current;
			} lex;
		struct
			{
			TokenBlock tb;
			int        index;
			} block;
		};
	};

#ifdef NDEBUG
	typedef struct tss_struct *TokenStreamStack;
#else
	typedef Array TokenStreamStack;
#endif
#define AR_PREFIX  tss
#define AR_TYPE    TokenStreamStack
#define AR_ELEMENT TokenStream
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define tss_new( size, ml ) tss_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

#ifdef NDEBUG
	typedef struct tba_struct *TokenBlockArray;
#else
	typedef Array TokenBlockArray;
#endif
#define AR_PREFIX  tba
#define AR_TYPE    TokenBlockArray
#define AR_ELEMENT TokenBlock
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define tba_new( size, ml ) tba_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct tb_struct
	{
	SymbolIndex      tag;        // By having this here, we can tell a TokenBlock from an Object and therefore don't need to wrap the former
	ObjectArray      tokens;
	int              startIndex; // index of this TB within its containing TB
	TokenBlockArray  subBlocks;
	};

static Object getLexToken( TokenStream ts )
	{
	int token = yylex();
	switch( token )
		{
		case ERROR:
			check(!"Error token!");
			// fall through
		case NUM_TOKENS:
			// fall through
		case NO_TOKEN:
			break;
		case INT:
			return ob_fromInt( lastInt(), ts->heap );
		case STRING:
			return ob_fromString( lastString(), ts->heap );
		case WORD:
			return oh_symbolToken( ts->heap, sy_byName( lastWord(), oh_symbolTable( ts->heap ) ) );
		}
	return NULL;
	}

FUNC int ts_sendNTo( TokenStream ts, int tokenLimit, File fl )
	{
	ObjectHeap heap = ts->heap;
	int charsSent = 0;
	switch( ts->kind )
		{
		case BLOCK:
			{
			int startIndex = ts->block.index;
			TokenBlock tb  = ts->block.tb;
			int stopIndex  = startIndex + tokenLimit;
			char *ending = NULL;
			if( stopIndex >= tb_length( tb ) )
				stopIndex = tb_length( tb );
			else
				ending = "  ...";

			int charsSent = 0;
			int i;
			for( i=startIndex; i < stopIndex; i++ )
				{
				charsSent += fl_write( fl, " " );
				charsSent += ob_sendTo( oba_get( tb->tokens, i ), fl, heap );
				}
			if( ending )
				charsSent += fl_write( fl, "%s", ending );
			}
			break;
		case LEX:
			if( tokenLimit >= 1 && ts->lex.current )
				{
				charsSent += ob_sendTo( ts->lex.current, fl, ts->heap );
				charsSent += fl_write( fl, "  ..." ); // Never can tell when you're at the end of theLexTokenStream
				}
			break;
		}
	return charsSent;
	}

FUNC TokenStream theLexTokenStream( ObjectHeap heap )
	{
	static TokenStream result = NULL;
	if( !result )
		{
		result = (TokenStream)ml_alloc( ml_singleton(), sizeof(*result) );
		result->kind        = LEX;
		result->heap        = heap;
		result->lex.current = getLexToken( result );
		}
	return result;
	}

FUNC TokenStream ts_new( TokenBlock tb, ObjectHeap heap )
	{
	TokenStream result = (TokenStream)ml_alloc( ml_singleton(), sizeof(*result) );
	result->kind        = BLOCK;
	result->heap        = heap;
	result->block.tb    = tb;
	result->block.index = 0;
	return result;
	}

FUNC TokenBlock ts_tokenBlock( TokenStream ts )
	{
	return ts->kind == BLOCK? ts->block.tb : NULL;
	}

FUNC Object ts_current( TokenStream ts )
	{
	switch( ts->kind )
		{
		case BLOCK:
			{
			int     index = ts->block.index;
			TokenBlock tb = ts->block.tb;
			int stopIndex = tb_length( tb );
			if( index < stopIndex )
				return oba_get( tb->tokens, index );
			else
				return NULL;
			}
		case LEX:
			return ts->lex.current;
		}
	assert(0);
	return NULL;
	}

FUNC void ts_advance( TokenStream ts )
	{
	switch( ts->kind )
		{
		case BLOCK:
			assert( ts_current( ts ) ); // don't advance past end
			ts->block.index++;
			break;
		case LEX:
			ts->lex.current = getLexToken( ts );
			break;
		}
	}

FUNC ObjectHeap ts_heap( TokenStream ts )
	{
	return ts->heap;
	}

enum { DEFAULT_TOKEN_BLOCK_LENGTH=29 };

FUNC TokenBlock tb_new( MemoryLifetime ml )
	{
	TokenBlock result = (TokenBlock)ml_alloc( ml, sizeof(*result) );
	result->tag        = SYM_TOKEN_BLOCK;
	result->tokens     = oba_new( DEFAULT_TOKEN_BLOCK_LENGTH, ml );
	result->startIndex = 0;
	result->subBlocks  = (TokenBlockArray)0xdead0300;
	return result;
	}

FUNC int tb_length( TokenBlock tb )
	{
	return oba_count( tb->tokens );
	}

FUNC void tb_append( TokenBlock tb, Object token )
	{
	oba_append( tb->tokens, token );
	}

FUNC void tb_appendBlock( TokenBlock tb, TokenBlock suffix )
	{
	int i;
	for( i=0; i < tb_length( suffix ); i++ )
		tb_append( tb, oba_get( suffix->tokens, i ) );
	}

FUNC void tb_stopAppending( TokenBlock tb )
	{
	oba_shrinkWrap( tb->tokens );
	}

FUNC int ts_sendTo( TokenStream ts, File fl )
	{
	int tokenCount = min( 10, ts_tokenBlock(ts)? tb_length( ts_tokenBlock(ts) ) : 1 );
	return fl_write( fl, "TOKEN_STREAM_%p: ", PH( ts ) ) + ts_sendNTo( ts, tokenCount, fl );
	}

FUNC int tb_sendNTo( TokenBlock tb, int startIndex, int tokenLimit, File fl, ObjectHeap heap )
	{
	int i;
	int charsSent = 0;
	int length = tb_length( tb );
	int stopIndex = min( startIndex + tokenLimit, length );
	for( i=startIndex; i < stopIndex; i++ )
		{
		charsSent += fl_write( fl, " " );
		charsSent += ob_sendTo( oba_get( tb->tokens, i ), fl, heap );
		}
	if( stopIndex < length )
		charsSent += fl_write( fl, "  ..." );
	return charsSent;
	}

FUNC int tb_sendTo( TokenBlock tb, File fl, ObjectHeap heap )
	{
	int charsSent = fl_write( fl, "TOKEN_BLOCK_%p length %d:", PH( tb ), tb_length( tb ) );
	charsSent += tb_sendNTo( tb, 0, tb_length( tb ), fl, heap );
	return charsSent;
	}

//MERGE:30

