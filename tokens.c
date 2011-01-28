
#include "tokens.h"
#include "memory.h"
#include "lex.h"
#include "lex.l.h"

typedef enum
	{
	LEX,
	BLOCK,
	} StreamKind;

typedef struct di_struct *Digression;
struct di_struct
	{
	TokenBlock tb;
	int index;
	};

typedef struct dis_struct *DigressionStack;
#define AR_PREFIX  dis
#define AR_TYPE    DigressionStack
#define AR_ELEMENT struct di_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define dis_new( size, ml ) dis_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

typedef struct oba_struct *ObjectArray;
#define AR_PREFIX  oba
#define AR_TYPE    ObjectArray
#define AR_ELEMENT Object
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define oba_new( size, ml ) oba_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct ts_struct
	{
	ObjectHeap      heap;
	SymbolTable     st;
	DigressionStack digressions;
	struct
		{
		Object current;
		Object next;
		} lex;
	};

typedef struct tss_struct *TokenStreamStack;
#define AR_PREFIX  tss
#define AR_TYPE    TokenStreamStack
#define AR_ELEMENT TokenStream
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define tss_new( size, ml ) tss_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct tb_struct
	{
	ObjectArray      tokens;
	TokenStreamStack streams;
	};

static Object getLexToken( TokenStream ts )
	{
	int token = yylex();
	switch( token )
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
			return oh_symbolToken( ts->heap, sy_byName( lastWord(), ts->st ) );
		}
	return NULL;
	}

static Object di_token( Digression di, int offset )
	{
	int index = di->index + offset;
	int stopIndex = oba_count( di->tb->tokens );
	if( index < stopIndex )
		return oba_get( di->tb->tokens, index );
	else
		return NULL;
	}

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st )
	{
	static TokenStream result = NULL;
	if( !result )
		{
		result = (TokenStream)ml_alloc( ml_singleton(), sizeof(*result) );
		result->heap = heap;
		result->st   = st;
		result->digressions = dis_new( 20, ml_singleton() );
		result->lex.current = getLexToken( result );
		result->lex.next    = getLexToken( result );
		}
	return result;
	}

static Digression ts_digression( TokenStream ts )
	{
	if( dis_count( ts->digressions ) >= 1 )
		return dis_last( ts->digressions, 0 );
	else
		return NULL;
	}

FUNC Object ts_current( TokenStream ts )
	{
	Digression di = ts_digression( ts );
	if( di )
		return di_token( di, 0 );
	else
		return ts->lex.current;
	}

FUNC Object ts_next( TokenStream ts )
	{
	Digression di = ts_digression( ts );
	if( di )
		return di_token( di, 1 );
	else
		return ts->lex.next;
	}

FUNC void ts_advance( TokenStream ts )
	{
	Digression di = ts_digression( ts );
	if( di )
		{
		if( di->index < oba_count( di->tb->tokens ) )
			di->index += 1;
		}
	else
		{
		ts->lex.current = ts->lex.next;
		ts->lex.next    = getLexToken( ts );
		}
	}

FUNC void ts_push( TokenStream ts, TokenBlock tb )
	{
	Digression di = dis_nextElement( ts->digressions );
	di->tb = tb;
	di->index = 0;
	}

FUNC ObjectHeap ts_heap( TokenStream ts )
	{
	return ts->heap;
	}

FUNC TokenBlock ts_curBlock( TokenStream ts )
	{
	Digression di = ts_digression( ts );
	if( di )
		return di->tb;
	else
		return NULL;
	}

FUNC TokenBlock ts_pop( TokenStream ts )
	{
	Digression di = ts_digression( ts );
	assert( di );
	TokenBlock result = di->tb;
	tss_append( di->tb->streams, ts );
	dis_incCountBy( ts->digressions, -1 );
	return result;
	}

enum { DEFAULT_TOKEN_BLOCK_LENGTH=29 };

FUNC TokenBlock ts_beginBlock( TokenStream ts )
	{
	MemoryLifetime ml = ml_singleton(); // theLexTokenStream is a singleton
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

FUNC int ts_sendTo( TokenStream ts, File fl )
	{
	int charsSent = fl_write( fl, "TokenStream_%p: ", ts );
	int i;
	for( i = dis_count( ts->digressions )-1; i >= 0; i-- )
		{
		Digression di = dis_element( ts->digressions, i );
		charsSent += ob_sendTo( di_token( di, 0 ), fl, ts->heap );
		charsSent += fl_write( fl, " (" );
		charsSent += ob_sendTo( di_token( di, 1 ), fl, ts->heap );
		charsSent += fl_write( fl, ") " );
		}
	charsSent += ob_sendTo( ts->lex.current, fl, ts->heap );
	charsSent += fl_write( fl, " (" );
	charsSent += ob_sendTo( ts->lex.next,    fl, ts->heap );
	charsSent += fl_write( fl, ") " );
	return charsSent;
	}

FUNC int tb_sendTo( TokenBlock tb, File fl, ObjectHeap heap )
	{
	int i;
	int charsSent = fl_write( fl, "TokenBlock_%p length %d:", tb, oba_count( tb->tokens ) );
	for( i=0; i < oba_count( tb->tokens ); i++ )
		{
		charsSent += fl_write( fl, " " );
		charsSent += ob_sendTo( oba_get( tb->tokens, i ), fl, heap );
		}
	return charsSent;
	}

//MERGE:30

