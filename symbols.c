
#include "symbols.h"
#include "memory.h"
#include <string.h>

typedef struct sys_struct
	{
	const char *name;
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
	{ "ANY" },                         // Everything inherits this
	{ "INT" },                         // Small-magnitude integer with efficient representation
	{ "STRING" },                      // Sequence of characters
	{ "TOKEN" },                       // Singleton object representing a symbol
	{ "FUNCTION" },                    // Action to be performed in response to a reduce action in the parser
	{ "TOKEN_BLOCK" },                 // Sequence of objects (called "tokens" for historical reasons; I should probably change this)
	{ "TOKEN_STREAM" },
	{ "GRAMMAR" },
	{ "END_OF_INPUT" },
	{ "ITEM_SET_NUM" },                // Automaton state node field containing a diagnostic reference number 
	{ "REDUCE_CONTEXT_LENGTH" },       // Automaton state node field used to help in formatting diagnostic messages
	{ "BOOLEAN" },
	{ "FALSE" },
	{ "TRUE" },
	{ "SYMBOL" },                      // InheritanceRelation node field indicating the symbol represented by that node
	{ "ARRAY" },                       // Tag for array objects
	{ "ELEMENT_COUNT" },               // Length field for array objects
	{ "SUPERTAGS" },                   // InheritanceRelation node field indicating the array of InheritanceRelation nodes of the parent symbols
	{ "SUBTAGS" },                     // InheritanceRelation node field indicating the array of InheritanceRelation nodes of the child symbols
	{ "BINDINGS" },                    // Object mapping symbols to their values during interpretation
	{ "DELEGATE" },                    // Standard field symbol pointing at another object to handle requests that this one can't.  Used by BINDINGS.
	{ "VALUE" },                       // General field symbol pointing at another object to be considered this one's "value".  Used by VALUE_PLACEHOLDER.
	{ "TAG" },                         // The "datatype" of a value being represented by a PLACEHOLDER
	{ "PLACEHOLDER" },                 // Object representing an unknown value during partial evaluation
	{ "VALUE_PLACEHOLDER" },           // PLACEHOLDER subtype where the expression is a single object
	{ "TOKEN_BLOCK_PLACEHOLDER" },     // PLACEHOLDER subtype where the expression is captured by a token block
	{ "RECORDED_PLACEHOLDER" },        // PLACEHOLDER subtype where the expression has already been stored in a token block
	};

struct st_struct
	{
	SymbolStorageArray   array;
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

FUNC SymbolTable theSymbolTable()
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
		}
	return result;
	}

FUNC SymbolIndex st_count( SymbolTable st )
	{
	return sysa_count( st->array );
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
	#if 0 // TODO: reinstate automatic inheritance from ANY
	// TODO: Consider checking os_enabled on on_INHERITANCE
	if( st_inheritanceRelation( st ) ) // Can be NULL while bootstrapping the InheritanceRelation
		ir_add( st_inheritanceRelation( st ), sy_byIndex( SYM_ANY, st ), result );
	#endif
	return result;
	}

FUNC const char *sy_name( Symbol sy, SymbolTable st )
	{
	return sy2sys( sy, st )->name;
	}

FUNC int sy_sendTo( Symbol sy, File fl, SymbolTable st )
	{
	if( !fl )
		return 0;
	return fl_write( fl, "%s", sy_name( sy, st ) );
	}

//MERGE:20

