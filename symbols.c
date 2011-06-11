
#include "symbols.h"
#include "memory.h"
#include "symbols_impl.h"
#include "objects.h"
#include <string.h>
#include <stdint.h>

#define AR_PREFIX  sta
#define AR_TYPE    SymbolTable
#define AR_ELEMENT struct sy_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define sta_new( size, ml ) sta_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static struct sy_struct predefinedSymbols[] =
	{
	{ "NULL_SYMBOL" },
	{ "INT" },
	{ "STRING" },
	{ "TOKEN" },
	{ "FUNCTION" },
	{ "TOKEN_BLOCK" },
	{ "TOKEN_STREAM" },
	{ "GRAMMAR" },
	{ "END_OF_INPUT" },
	{ "STATE_NODE" },
	{ "ITEM_SET_NUM" },
	{ "REDUCE_CONTEXT_LENGTH" },
	{ "BOOLEAN" },
	{ "FALSE" },
	{ "TRUE" },
	{ "SYMBOL" },
	};

FUNC SymbolTable theSymbolTable()
	{
	static SymbolTable result = NULL;
	if( !result )
		{
		result = sta_new( 100 + NUM_PREDEFINED_SYMBOLS, ml_singleton() );
		sta_setCount( result, NUM_PREDEFINED_SYMBOLS );
		memcpy( sta_element( result, 0 ), predefinedSymbols, sizeof(predefinedSymbols) );
		}
	return result;
	}

FUNC SymbolIndex st_count( SymbolTable st )
	{
	return sta_count( st );
	}

FUNC Symbol sy_byIndex( SymbolIndex index, SymbolTable st )
	{
	assert( index < st_count(st) );
	return sta_element( st, index );
	}

FUNC Symbol sy_byName( const char *name, SymbolTable st )
	{
	Symbol sy;
	SymbolIndex i;
	for( i=0; i < st_count(st); i++ )
		if(!strcmp( name, sy_name( sy_byIndex(i,st), st ) ))
			return sy_byIndex(i,st);
	sy = sta_nextElement( st );
	memset( sy, 0, sizeof(*sy) );
	sy->name = strdup( name );
	return sy;
	}

FUNC SymbolIndex sy_index( Symbol sy, SymbolTable st )
	{
	assert( sta_element( st, 0 ) <= sy && sy <= sta_element( st, sta_count(st)-1 ) );
	return sy - sta_element( st, 0 );
	}

FUNC const char *sy_name( Symbol sy, SymbolTable st )
	{
	return sy->name;
	}

FUNC Record sy_instanceShape( Symbol sy, SymbolTable st )
	{
	return sy->instanceShape;
	}

FUNC void sy_setInstanceShape( Symbol sy, Record rd, SymbolTable st )
	{
	sy->instanceShape = rd;
	}

typedef struct ss_struct
	{
	Symbol sy;
	struct sy_scopedDefs scopedDefs;
	} *SymbolSnapshot;

static void ss_init( SymbolSnapshot ss, Symbol sy )
	{
	ss->sy = sy;
	ss->scopedDefs = sy->scopedDefs;
	}

typedef struct cp_struct *Checkpoint;
#define AR_PREFIX  cp
#define AR_TYPE    Checkpoint
#define AR_ELEMENT struct ss_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define cp_new( size, ml ) cp_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static SymbolSnapshot ss_bySymbol( Symbol sy, Checkpoint cp )
	{
	int i;
	for( i=0; i < cp_count( cp ); i++ )
		{
		SymbolSnapshot ss = cp_element( cp, i );
		if( ss->sy == sy )
			return ss;
		}
	return NULL;
	}

static void cp_reviveAndClear( Checkpoint cp )
	{
	int i;
	for( i=0; i < cp_count( cp ); i++ )
		{
		SymbolSnapshot ss = cp_element( cp, i );
		ss->sy->scopedDefs = ss->scopedDefs;
		ss->sy = (Symbol)0xdead1; // poison
		}
	cp_setCount( cp, 0 );
	}

typedef struct us_struct *UndoStack;
#define AR_PREFIX  us
#define AR_TYPE    UndoStack
#define AR_ELEMENT Checkpoint
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define usnew( size, ml ) usnewAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct cx_struct
	{
	SymbolTable    st;
	MemoryLifetime ml;
	UndoStack      us;
	UndoStack      freeList;
	};

FUNC Context cx_new( SymbolTable st )
	{
	MemoryLifetime ml = ml_undecided();
	Context result = (Context)ml_alloc( ml, sizeof(*result) );
	result->st = st;
	result->ml = ml;
	result->us = us_new( 11, ml );
	result->freeList = us_new( 11, ml );
	return result;
	}

FUNC void cx_save( Context cx )
	{
	if( us_count( cx->freeList ) >= 1 )
		{
		us_append( cx->us, us_getLast( cx->freeList, 0 ) );
		us_incCountBy( cx->freeList, -1 );
		}
	else
		us_append( cx->us, cp_new( 13, cx->ml ) );
	}

FUNC void cx_restore( Context cx )
	{
	check( us_count( cx->us ) >= 1 );
	cp_reviveAndClear( us_getLast( cx->us, 0 ) );
	us_append( cx->freeList, us_getLast( cx->us, 0 ) );
	us_incCountBy( cx->us, -1 );
	}

FUNC SymbolTable cx_symbolTable( Context cx )
	{
	return cx->st;
	}

FUNC Object cx_filter( Context cx, Object ob, Object defaultIfNull, ObjectHeap heap )
	{
	if( ob && ob_isToken( ob, heap ) )
		{
		Symbol sy = ob_toSymbol( ob, heap );
		Object value = sy_value( sy, cx );
		if( value )
			return value;
		}
	return ob ? ob : defaultIfNull;
	}

FUNC int sy_sendTo( Symbol sy, File fl, SymbolTable st )
	{
	if( !fl )
		return 0;
	return fl_write( fl, "%s", sy_name( sy, st ) );
	}

FUNC int cx_sendTo( Context cx, File fl )
	{
	int charsSent = 0;
	if( !fl )
		return 0;
	charsSent += fl_write( fl, "Scope_%p{", cx );
	if( us_count( cx->us ) >= 1 )
		{
		char *sep = "";
		int i;
		Checkpoint cp = us_getLast( cx->us, 0 );
		for( i=0; i < cp_count(cp); i++ )
			{
			charsSent += fl_write( fl, "%s%s", sep, sy_name( cp_element( cp, i )->sy, cx->st ) );
			sep = ", ";
			}
		}
	charsSent += fl_write( fl, "}", cx );
	return charsSent;
	}

FUNC Object sy_value( Symbol sy, Context cx )
	{
	return sy->scopedDefs.value;
	}

static void sy_save( Symbol sy, Context cx )
	{
	if( us_count( cx->us ) >= 1 )
		{
		Checkpoint cp = us_getLast( cx->us, 0 );
		if( !ss_bySymbol( sy, cp ) )
			ss_init( cp_nextElement( cp ), sy );
		}
	}

FUNC void sy_setValue( Symbol sy, Object value, Context cx )
	{
	sy_save( sy, cx );
	sy->scopedDefs.value = value;
	}

//MERGE:20

