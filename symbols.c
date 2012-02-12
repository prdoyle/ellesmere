
#include "symbols.h"
#include "memory.h"
#include "objects.h"
#include <string.h>
#include <stdint.h>
#include "parser.h" // for InheritanceRelation stuff, which shouldn't really be in that header anyway...

typedef struct sys_struct
	{
	const char *name;
	Object token;
	Record instanceShape;
	} *SymbolStorage;

#ifdef NDEBUG
	typedef struct sysa_struct *SymbolStorageArray; // type-safe phony struct
#else
	typedef Array SymbolStorageArray; // give the debugger some symbol info it can use
#endif
#define AR_PREFIX  sysa
#define AR_TYPE    SymbolStorageArray
#define AR_ELEMENT struct sys_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define sysa_new( size, ml ) sysa_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

static struct sys_struct predefinedSymbols[] =
	{
	{ "NULL_SYMBOL" },
	{ "ANY" },
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
	{ "ARRAY" },
	{ "ELEMENT_COUNT" },
	{ "SUPERTAGS" },
	{ "SUBTAGS" },
	{ "BINDINGS" },
	{ "DELEGATE" },
	};

struct st_struct
	{
	SymbolStorageArray   array;
	InheritanceRelation  ir;
	};

FUNC SymbolIndex sy_index( Symbol sy, SymbolTable st )
	{
	SymbolIndex result = (SymbolIndex)(intptr_t)sy;
	assert( NULL_SYMBOL_INDEX <= result && result < st_count( st ) );
	return result;
	}

FUNC Symbol sy_byIndex( SymbolIndex index, SymbolTable st )
	{
	assert( 0 <= index && index < st_count(st) );
	return (Symbol)(intptr_t)index;
	}

static SymbolStorage sy2sys( Symbol sy, SymbolTable st )
	{
	return sysa_element( st->array, sy_index( sy, st ) );
	}

static Symbol sys2sy( SymbolStorage sys, SymbolTable st )
	{
	return sy_byIndex( sys - sysa_element( st->array, 0 ), st );
	}

FUNC SymbolTable theSymbolTable( ObjectHeap theObjectHeap )
	{
	static SymbolTable result = NULL;
	if( !result )
		{
		// Allocate and initialize from the above array
		//
		result = (SymbolTable)ml_alloc( ml_singleton(), sizeof(*result) );
		result->array = sysa_new( 100 + NUM_PREDEFINED_SYMBOLS, ml_singleton() );
		sysa_setCount( result->array, NUM_PREDEFINED_SYMBOLS );
		memcpy( sysa_element( result->array, 0 ), predefinedSymbols, sizeof(predefinedSymbols) );

		result->ir = ir_new( theObjectHeap, result, ml_singleton() );
		int i;
		Symbol any = sy_byIndex( SYM_ANY, result );
		for( i=SYM_ANY+1; i < st_count(result); i++ )
			ir_add( result->ir, any, sy_byIndex( i, result ) );
		}
	return result;
	}

FUNC SymbolIndex st_count( SymbolTable st )
	{
	return sysa_count( st->array );
	}

FUNC InheritanceRelation st_inheritanceRelation ( SymbolTable st )
	{
	return st->ir;
	}

FUNC Symbol sy_byName( const char *name, SymbolTable st )
	{
	SymbolIndex i;
	for( i=0; i < st_count(st); i++ )
		if(!strcmp( name, sy_name( sy_byIndex(i,st), st ) ))
			return sy_byIndex(i,st);
	SymbolStorage sys = sysa_nextElement( st->array );
	memset( sys, 0, sizeof(*sys) );
	sys->name = strdup( name );
	Symbol result = sys2sy( sys, st );
	if( st_inheritanceRelation( st ) ) // Can be NULL while bootstrapping the InheritanceRelation
		ir_add( st_inheritanceRelation( st ), sy_byIndex( SYM_ANY, st ), result );
	return result;
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

#include "objects_symbols_backdoor.h"

FUNC Object st_getToken( SymbolTable st, Symbol sy )
	{
	return sy2sys( sy, st )->token;
	}

FUNC void st_setToken( SymbolTable st, Symbol sy, Object token )
	{
	sy2sys( sy, st )->token = token;
	}

//MERGE:20

