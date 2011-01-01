
#include "symbols.h"
#include "memory.h"
#include "symbols_impl.h"
#include <string.h>
#include <stdint.h>

#define AR_PREFIX  sta
#define AR_TYPE    SymbolTable
#define AR_ELEMENT struct sy_struct
#undef AR_BYVALUE
#include "array_template.h"

struct an_struct
	{
	ActionFunction function;
	Symbol         sy;
	};

static struct sy_struct predefinedSymbols[] =
	{
	{ ":NULL_SYMBOL" },
	{ ":INT" },
	{ ":STRING" },
	{ ":TOKEN" },
	{ ":TOKEN_BLOCK" },
	{ ":TOKEN_STREAM" },
	{ ":END_OF_INPUT" },
	{ ":STATE_NODE" },
	{ ":ITEM_SET_NUM" },
	};

FUNC SymbolTable theSymbolTable()
	{
	static SymbolTable result = NULL;
	if( !result )
		{
		result = sta_new( 200 + NUM_PREDEFINED_SYMBOLS, ml_singleton() );
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

FUNC Action sy_immediateAction( Symbol sy, Context cx )
	{
	return sy->scopedDefs.immediateAction;
	}

FUNC int sy_arity( Symbol sy, Context cx )
	{
	return sy->scopedDefs.arity;
	}

FUNC bool sy_isSymbolic( Symbol sy, Context cx )
	{
	return sy->scopedDefs.isSymbolic;
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

FUNC void sy_setImmediateAction ( Symbol sy, Action an, Context cx )
	{
	sy_save( sy, cx );
	sy->scopedDefs.immediateAction = an;
	}

FUNC void sy_setArity( Symbol sy, int arity, Context cx )
	{
	sy_save( sy, cx );
	sy->scopedDefs.arity = arity;
	}

FUNC void sy_setIsSymbolic( Symbol sy, bool isSymbolic, Context cx )
	{
	sy_save( sy, cx );
	sy->scopedDefs.isSymbolic = isSymbolic;
	}

FUNC void sy_setValue( Symbol sy, Object value, Context cx )
	{
	sy_save( sy, cx );
	sy->scopedDefs.value = value;
	}

FUNC Action an_perform( Action an, Context cx )
	{
	assert( an && cx );
	return an->function( an, cx );
	}

FUNC Action an_fromFunctionAndSymbol( ActionFunction af, Symbol sy )
	{
	Action result = sy->recentAction;
	if( !result || result->function != af )
		sy->recentAction = result = (Action)ml_alloc( ml_undecided(), sizeof(*result) );
	result->function = af;
	result->sy       = sy;
	return result;
	}

FUNC Action an_fromFunction( ActionFunction af )
	{
	Action result = (Action)ml_alloc( ml_undecided(), sizeof(*result) );
	result->function = af;
	result->sy = NULL;
	return result;
	}

FUNC Symbol an_symbol( Action an )
	{
	return an->sy;
	}

//MERGE:20

