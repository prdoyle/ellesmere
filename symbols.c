
#include "symbols.h"
#include <string.h>

struct sy_struct
	{
	const char *name;
	};

struct st_struct
	{
	SymbolIndex count;
	struct sy_struct symbols[500];
	};

FUNC SymbolTable theSymbolTable()
	{
	static struct st_struct _theSymbolTable = { NUM_PREDEFINED_SYMBOLS,
		{
		{ "NO_SYMBOL" },
		{ "INT" },
		{ "STRING" },
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

FUNC Symbol st_byIndex( SymbolIndex index, SymbolTable st )
	{
	assert( index < st->count );
	return st->symbols + index;
	}

FUNC Symbol st_byName( const char *name, SymbolTable st )
	{
	Symbol sy;
	SymbolIndex i;
	for ( i=0; i < st_count(st); i++ )
		if ( !strcmp( name, sy_name( st->symbols+i, st ) ) )
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

//MERGE:10

