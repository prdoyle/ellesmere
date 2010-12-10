
#include "symbols.h"
#include "memory.h"
#include "symbols_impl.h"
#include <string.h>
#include <stdint.h>

struct st_struct
	{
	SymbolIndex count;
	struct sy_struct symbols[MAX_SYMBOLS];
	};

struct an_struct
	{
	ActionFunction function;
	Symbol         sy;
	};

FUNC SymbolTable theSymbolTable()
	{
	static struct st_struct _theSymbolTable = { NUM_PREDEFINED_SYMBOLS,
		{
		{ "$NO_SYMBOL" },
		{ "$INT" },
		{ "$STRING" },
		{ "$TOKEN" },
		{ "$TOKEN_BLOCK" },
		{ "$TOKEN_STREAM" },
		}
		};
	assert(  _theSymbolTable.symbols[ _theSymbolTable.count-1 ].name );
	assert( !_theSymbolTable.symbols[ _theSymbolTable.count   ].name );
	return &_theSymbolTable;
	}

FUNC SymbolIndex st_count( SymbolTable st )
	{
	return theSymbolTable()->count;
	}

FUNC Symbol sy_byIndex( SymbolIndex index, SymbolTable st )
	{
	assert( index < st->count );
	return st->symbols + index;
	}

FUNC Symbol sy_byName( const char *name, SymbolTable st )
	{
	Symbol sy;
	SymbolIndex i;
	for( i=0; i < st_count(st); i++ )
		if( !strcmp( name, sy_name( st->symbols+i, st ) ) )
			return st->symbols+i;
	assert( st->count < sizeof(st->symbols) / sizeof(st->symbols[0]) );
	sy = st->symbols + st->count++;
	memset( sy, 0, sizeof(*sy) );
	sy->name = strdup( name );
	return sy;
	}

FUNC SymbolIndex sy_index( Symbol sy, SymbolTable st )
	{
	assert( st->symbols <= sy && sy < ( st->symbols + st->count ) );
	return sy - st->symbols;
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

