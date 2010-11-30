
#include "symbols.h"
#include "objects.h"
#include "symbols_impl.h"
#include <string.h>

struct st_struct
	{
	SymbolIndex count;
	struct sy_struct symbols[500];
	};

struct an_struct
	{
	ActionFunction function;
	};

FUNC SymbolTable theSymbolTable()
	{
	static struct st_struct _theSymbolTable = { NUM_PREDEFINED_SYMBOLS,
		{
		{ "NO_SYMBOL" },
		{ "INT" },
		{ "STRING" },
		{ "TOKEN" },
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

FUNC Action sy_immediateAction( Symbol sy, SymbolTable st )
	{
	return sy->immediateAction;
	}

FUNC void sy_setImmediateAction ( Symbol sy, Action an, SymbolTable st )
	{
	sy->immediateAction = an;
	}

FUNC Action an_fromFunction( ActionFunction af )
	{
	Action result = (Action)malloc( sizeof(*result) );
	result->function = af;
	return result;
	}

FUNC Action an_perform( Action an, Actor ar )
	{
	assert( an && ar );
	return an->function( ar );
	}

//MERGE:10

