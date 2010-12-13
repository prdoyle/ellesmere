
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
	{ "$NO_SYMBOL" },
	{ "$INT" },
	{ "$STRING" },
	{ "$TOKEN" },
	{ "$TOKEN_BLOCK" },
	{ "$TOKEN_STREAM" },
	};

FUNC SymbolTable theSymbolTable()
	{
	static SymbolTable result = NULL;
	if( !result )
		{
		result = sta_new( 200 + NUM_PREDEFINED_SYMBOLS );
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
	sta_incCount( st );
	sy = sta_element( st, st_count(st) - 1 );
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

struct sdl_struct
	{
	struct sdl_struct *next;
	Symbol sy;
	struct sy_scopedDefs scopedDefs;
	};

static SymbolDefList sdl_new( Symbol sy, SymbolDefList next )
	{
	SymbolDefList result = sy->freeSDL;
	if( result )
		sy->freeSDL = NULL;
	else
		result = (SymbolDefList)mem_alloc( sizeof(*result) );
	result->sy = sy;
	result->scopedDefs = sy->scopedDefs;
	result->next = next;
	return result;
	}

static SymbolDefList sdl_bySymbol( SymbolDefList sdl, Symbol sy )
	{
	if( !sdl || sdl->sy == sy )
		return sdl;
	else
		return sdl_bySymbol( sdl->next, sy );
	}

static SymbolDefList sdl_applyAndFree( SymbolDefList sdl )
	{
	SymbolDefList next = sdl->next;
	sdl->sy->scopedDefs = sdl->scopedDefs;
	if( !sdl->sy->freeSDL )
		sdl->sy->freeSDL = sdl;
	sdl->sy = (Symbol)0xdead; // poison
	return next;
	}

typedef struct ub_struct
	{
	int length;
	int capacity;
	SymbolDefList *defLists;
	} *UndoBuffer;

#define INITIAL_UNDO_CAPCITY 17

static void ub_init( UndoBuffer ub )
	{
	ub->length = 0;
	ub->capacity = INITIAL_UNDO_CAPCITY;
	ub->defLists = (SymbolDefList*)mem_alloc( ub->capacity * sizeof( ub->defLists[0] ) );
	}

static SymbolDefList *ub_curListPtr( UndoBuffer ub )
	{
	assert( ub->length >= 1 );
	return ub->defLists + ub->length-1;
	}

struct cx_struct
	{
	SymbolTable      st;
	struct ub_struct ub;
	};

FUNC Context cx_new( SymbolTable st )
	{
	Context result = (Context)mem_alloc( sizeof(*result) );
	result->st = st;
	ub_init( &result->ub );
	return result;
	}

FUNC void cx_save( Context cx )
	{
	UndoBuffer ub = &cx->ub;
	if( ub->length == ub->capacity )
		{
		ub->capacity *= 2;
		ub->defLists = (SymbolDefList*)mem_realloc( ub->defLists, ub->capacity * sizeof( ub->defLists[0] ) );
		}
	ub->length++;
	*ub_curListPtr( ub ) = NULL;
	}

FUNC void cx_restore( Context cx )
	{
	check( cx->ub.length >= 1 );
	UndoBuffer ub = &cx->ub;
	SymbolDefList cur;
	for( cur = *ub_curListPtr( ub ); cur; cur = sdl_applyAndFree( cur ) )
		{}
	--ub->length;
	}

FUNC SymbolTable cx_symbolTable( Context cx )
	{
	return cx->st;
	}

static void sdl_sendTo( SymbolDefList sdl, File fl, SymbolTable st, char *sep )
	{
	if( sdl && fl )
		{
		fl_write( fl, "%s%s", sep, sy_name( sdl->sy, st ) );
		sdl_sendTo( sdl->next, fl, st, ", " );
		}
	}

FUNC void cx_sendTo( Context cx, File fl )
	{
	if( !fl )
		return;
	fl_write( fl, "Scope_%p{", cx );
	if( cx->ub.length >= 1 )
		sdl_sendTo( *ub_curListPtr( &cx->ub ), fl, cx_symbolTable( cx ), "" );
	fl_write( fl, "}", cx );
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
	SymbolDefList *listPtr;
	if( cx->ub.length == 0 )
		return;
	listPtr = ub_curListPtr( &cx->ub );
	if( !sdl_bySymbol( *listPtr, sy ) )
		*listPtr = sdl_new( sy, *listPtr );
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
		sy->recentAction = result = (Action)mem_alloc( sizeof(*result) );
	result->function = af;
	result->sy       = sy;
	return result;
	}

FUNC Action an_fromFunction( ActionFunction af )
	{
	Action result = (Action)mem_alloc( sizeof(*result) );
	result->function = af;
	result->sy = NULL;
	return result;
	}

FUNC Symbol an_symbol( Action an )
	{
	return an->sy;
	}

//MERGE:20

