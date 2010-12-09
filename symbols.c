
#include "symbols.h"
#include "objects.h"
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
	SymbolDefList result = (SymbolDefList)malloc( sizeof(*result) );
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

struct sc_struct
	{
	SymbolDefList defs;
	union
		{
		Scope    outer;
		intptr_t st;   // Low-tagged
		} data;
	};

FUNC Scope st_outermostScope( SymbolTable st )
	{
	// TODO: Shouldn't this always return the same Scope given the same st?
	Scope result = (Scope)malloc( sizeof(*result) );
	result->defs = NULL;
	result->data.st = ((intptr_t)st) | 1;
	return result;
	}

FUNC Scope sc_new( Scope outer )
	{
	Scope result = (Scope)malloc( sizeof(*result) );
	result->defs = NULL;
	result->data.outer = outer;
	return result;
	}

FUNC SymbolTable sc_symbolTable( Scope sc )
	{
	if( sc->data.st & 1 )
		return (SymbolTable)( sc->data.st & ~1 );
	else
		return sc_symbolTable( sc->data.outer );
	}

FUNC Scope sc_outer( Scope sc )
	{
	if( sc->data.st & 1 )
		return NULL;
	else
		return sc->data.outer;
	}

static void sdl_sendTo( SymbolDefList sdl, File fl, SymbolTable st )
	{
	if( sdl && fl )
		{
		fl_write( fl, "%s ", sy_name( sdl->sy, st ) );
		sdl_sendTo( sdl->next, fl, st );
		}
	}

FUNC void sc_sendTo( Scope sc, File fl )
	{
	if( !fl )
		return;
	fl_write( fl, "Scope_%p{ ", sc );
	sdl_sendTo( sc->defs, fl, sc_symbolTable( sc ) );
	fl_write( fl, "}", sc );
	}

FUNC Action sy_immediateAction( Symbol sy, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		return sdl->immediateAction;
	else if( sc_outer(sc) )
		return sy_immediateAction( sy, sc_outer( sc ) );
	else
		return NULL;
	}

FUNC void sy_setImmediateAction ( Symbol sy, Action an, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		sdl->immediateAction = an;
	else
		sc->defs = sdl_new( sy, an, 0, false, NULL, sc->defs );
	}

FUNC int sy_arity( Symbol sy, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		return sdl->arity;
	else if( sc_outer(sc) )
		return sy_arity( sy, sc_outer( sc ) );
	else
		return 0;
	}

FUNC void sy_setArity( Symbol sy, int arity, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		sdl->arity = arity;
	else
		sc->defs = sdl_new( sy, NULL, arity, false, NULL, sc->defs );
	}

FUNC bool sy_isSymbolic( Symbol sy, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		return sdl->isSymbolic;
	else if( sc_outer(sc) )
		return sy_isSymbolic( sy, sc_outer( sc ) );
	else
		return false;
	}

FUNC void sy_setIsSymbolic( Symbol sy, bool isSymbolic, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		sdl->isSymbolic = isSymbolic;
	else
		sc->defs = sdl_new( sy, NULL, 0, isSymbolic, NULL, sc->defs );
	}

FUNC Object sy_value( Symbol sy, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		return sdl->value;
	else if( sc_outer(sc) )
		return sy_value( sy, sc_outer( sc ) );
	else
		return NULL;
	}

FUNC void sy_setValue( Symbol sy, Object value, Scope sc )
	{
	SymbolDefList sdl = sdl_bySymbol( sc->defs, sy );
	if( sdl )
		sdl->value = value;
	else
		sc->defs = sdl_new( sy, NULL, 0, false, value, sc->defs );
	assert( sy_value( sy, sc ) == value );
	}

FUNC Action an_perform( Action an, Scope sc )
	{
	assert( an && sc );
	return an->function( an, sc );
	}

FUNC Action an_fromFunctionAndSymbol( ActionFunction af, Symbol sy )
	{
	Action result = (Action)malloc( sizeof(*result) );
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

