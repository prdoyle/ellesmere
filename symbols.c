
#include "symbols.h"
#include "memory.h"
#include "objects.h"
#include <string.h>
#include <stdint.h>

typedef struct sys_struct
	{
	const char *name;
	Object token;
	Record instanceShape;
	} *SymbolStorage;

#define AR_PREFIX  sta
#define AR_TYPE    SymbolTable
#define AR_ELEMENT struct sys_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define sta_new( size, ml ) sta_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static struct sys_struct predefinedSymbols[] =
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

enum { FIRST_SYMBOL_INDEX=0 }; // We want NULL_SYMBOL to correspond to zero

FUNC SymbolIndex sy_index( Symbol sy, SymbolTable st )
	{
	SymbolIndex result = (SymbolIndex)( FIRST_SYMBOL_INDEX + ((intptr_t)sy) );
	assert( 0 <= result && result < st_count( st ) );
	return result;
	}

FUNC Symbol sy_byIndex( SymbolIndex index, SymbolTable st )
	{
	assert( 0 <= index && index < st_count(st) );
	return (Symbol)(intptr_t)index;
	}

static SymbolStorage sy2sys( Symbol sy, SymbolTable st )
	{
	return sta_element( st, sy_index( sy, st ) );
	}

static Symbol sys2sy( SymbolStorage sys, SymbolTable st )
	{
	return sy_byIndex( sys - sta_element( st, 0 ), st );
	}

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

FUNC Symbol sy_byName( const char *name, SymbolTable st )
	{
	SymbolIndex i;
	for( i=0; i < st_count(st); i++ )
		if(!strcmp( name, sy_name( sy_byIndex(i,st), st ) ))
			return sy_byIndex(i,st);
	SymbolStorage sys = sta_nextElement( st );
	memset( sys, 0, sizeof(*sys) );
	sys->name = strdup( name );
	// TODO: Add an inheritance relation with ANY
	return sys2sy( sys, st );
	}

FUNC const char *sy_name( Symbol sy, SymbolTable st )
	{
	return sy2sys( sy, st )->name;
	}

FUNC Record sy_instanceShape( Symbol sy, SymbolTable st )
	{
	return sy2sys( sy, st )->instanceShape;
	}

FUNC void sy_setInstanceShape( Symbol sy, Record rd, SymbolTable st )
	{
	sy2sys( sy, st )->instanceShape = rd;
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
	SymbolStorage sys = sy2sys( sy, oh_fieldSymbolTable( heap ) );
	if( !sys->token )
		sys->token = oh_createToken( sy, heap );
	return sys->token;
	}

//MERGE:20

