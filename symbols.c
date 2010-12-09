
#include "symbols.h"
#include "objects.h"
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

typedef struct sdl_struct
	{
	struct sdl_struct *next;
	Symbol      sy;
	Action      immediateAction;
	int         arity;
	bool        isSymbolic;
	Object      value;
	} *SymbolDefList;

static SymbolDefList sdl_new( Symbol sy, Action immediateAction, int arity, bool isSymbolic, Object value, SymbolDefList next )
	{
	SymbolDefList result = (SymbolDefList)mem_alloc( sizeof(*result) );
	result->sy = sy;
	result->immediateAction = immediateAction;
	result->arity = arity;
	result->isSymbolic = isSymbolic;
	result->value = value;
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

struct cx_struct
	{
	SymbolDefList defs;
	union
		{
		Context  outer;
		intptr_t st;   // Low-tagged
		} data;
	};

FUNC Context st_outermostScope( SymbolTable st )
	{
	// TODO: Shouldn't this always return the same Context given the same st?
	Context result = (Context)mem_alloc( sizeof(*result) );
	result->defs = NULL;
	result->data.st = ((intptr_t)st) | 1;
	return result;
	}

FUNC Context cx_new( Context outer )
	{
	Context result = (Context)mem_alloc( sizeof(*result) );
	result->defs = NULL;
	result->data.outer = outer;
	return result;
	}

FUNC SymbolTable cx_symbolTable( Context cx )
	{
	if( cx->data.st & 1 )
		return (SymbolTable)( cx->data.st & ~1 );
	else
		return cx_symbolTable( cx->data.outer );
	}

FUNC Context cx_outer( Context cx )
	{
	if( cx->data.st & 1 )
		return NULL;
	else
		return cx->data.outer;
	}

static void sdl_sendTo( SymbolDefList sdl, File fl, SymbolTable st )
	{
	if( sdl && fl )
		{
		fl_write( fl, "%s ", sy_name( sdl->sy, st ) );
		sdl_sendTo( sdl->next, fl, st );
		}
	}

FUNC void cx_sendTo( Context cx, File fl )
	{
	if( !fl )
		return;
	fl_write( fl, "Scope_%p{ ", cx );
	sdl_sendTo( cx->defs, fl, cx_symbolTable( cx ) );
	fl_write( fl, "}", cx );
	}

FUNC Action sy_immediateAction( Symbol sy, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		return sdl->immediateAction;
	else if( cx_outer(cx) )
		return sy_immediateAction( sy, cx_outer( cx ) );
	else
		return NULL;
	}

FUNC void sy_setImmediateAction ( Symbol sy, Action an, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		sdl->immediateAction = an;
	else
		cx->defs = sdl_new( sy, an, 0, false, NULL, cx->defs );
	}

FUNC int sy_arity( Symbol sy, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		return sdl->arity;
	else if( cx_outer(cx) )
		return sy_arity( sy, cx_outer( cx ) );
	else
		return 0;
	}

FUNC void sy_setArity( Symbol sy, int arity, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		sdl->arity = arity;
	else
		cx->defs = sdl_new( sy, NULL, arity, false, NULL, cx->defs );
	}

FUNC bool sy_isSymbolic( Symbol sy, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		return sdl->isSymbolic;
	else if( cx_outer(cx) )
		return sy_isSymbolic( sy, cx_outer( cx ) );
	else
		return false;
	}

FUNC void sy_setIsSymbolic( Symbol sy, bool isSymbolic, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		sdl->isSymbolic = isSymbolic;
	else
		cx->defs = sdl_new( sy, NULL, 0, isSymbolic, NULL, cx->defs );
	}

FUNC Object sy_value( Symbol sy, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		return sdl->value;
	else if( cx_outer(cx) )
		return sy_value( sy, cx_outer( cx ) );
	else
		return NULL;
	}

FUNC void sy_setValue( Symbol sy, Object value, Context cx )
	{
	SymbolDefList sdl = sdl_bySymbol( cx->defs, sy );
	if( sdl )
		sdl->value = value;
	else
		cx->defs = sdl_new( sy, NULL, 0, false, value, cx->defs );
	assert( sy_value( sy, cx ) == value );
	}

FUNC Action an_perform( Action an, Context cx )
	{
	assert( an && cx );
	return an->function( an, cx );
	}

FUNC Action an_fromFunctionAndSymbol( ActionFunction af, Symbol sy )
	{
	Action result = (Action)mem_alloc( sizeof(*result) );
	result->function = af;
	result->sy       = sy;
	return result;
	}

FUNC Action an_fromFunction( ActionFunction af )
	{
	return an_fromFunctionAndSymbol( af, NULL );
	}

FUNC Symbol an_symbol( Action an )
	{
	return an->sy;
	}

//MERGE:10

