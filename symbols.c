
#include "symbols.h"
#include "memory.h"
#include "objects.h"
#include <string.h>
#include <stdint.h>

struct sy_struct
	{
	const char *name;
	Object token;
	Record instanceShape;
	};

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
	{ "ANY" },
	{ "BOOLEAN" },
	{ "FALSE" },
	{ "TRUE" },
	{ "SYMBOL" },
	{ "ARRAY" },
	{ "ELEMENT_COUNT" },
	{ "SUPERTAGS" },
	{ "SUBTAGS" },
	{ "BINDINGS" },
	{ "DELEGATE" },
	};

FUNC SymbolTable theSymbolTable()
	{
	static SymbolTable result = NULL;
	if( !result )
		{
		result = sta_new( 1000 + NUM_PREDEFINED_SYMBOLS, ml_singleton() );
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
	// TODO: BAH!  This doesn't work if the array resizes itself
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
	// TODO: Add an inheritance relation with ANY
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

FUNC int sy_sendTo( Symbol sy, File fl, SymbolTable st )
	{
	if( !fl )
		return 0;
	return fl_write( fl, "%s", sy_name( sy, st ) );
	}

#include "symbol_tokens.h"

FUNC Object oh_symbolToken( ObjectHeap heap, Symbol sy )
	{
	if( !sy->token )
		sy->token = ob_createToken( sy, heap );
	return sy->token;
	}

//MERGE:20

