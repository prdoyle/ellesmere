
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "base.h"
#include "file.h"

typedef enum
	{
	NULL_SYMBOL_INDEX=0,

	// Objects with these tags have no fields
	//
	SYM_INT,
	SYM_STRING,
	SYM_TOKEN,
	// Reflected internal data structures
	SYM_FUNCTION,
	SYM_TOKEN_BLOCK,
	SYM_TOKEN_STREAM,
	SYM_GRAMMAR,

	NUM_FIELDLESS_OBJECT_TAGS,

	// Other symbols used internally
	//
	SYM_END_OF_INPUT             = NUM_FIELDLESS_OBJECT_TAGS,
	SYM_STATE_NODE,
	SYM_ITEM_SET_NUM,
	SYM_REDUCE_CONTEXT_LENGTH,
	SYM_ANY,
	SYM_BOOLEAN,
	SYM_FALSE,
	SYM_TRUE,
	SYM_SYMBOL,
	SYM_ARRAY,
	SYM_ELEMENT_COUNT,
	SYM_SUPERTAGS,
	SYM_SUBTAGS,
	SYM_BINDINGS,
	SYM_DELEGATE,

	NUM_PREDEFINED_SYMBOLS
	} SymbolIndex;

FUNC SymbolTable theSymbolTable();
FUNC SymbolIndex st_count   ( SymbolTable st );

FUNC SymbolIndex       sy_index ( Symbol sy, SymbolTable st );
FUNC const const char *sy_name  ( Symbol sy, SymbolTable st );
FUNC Symbol            sy_byIndex ( SymbolIndex index, SymbolTable st ); // 0 <= index < st_count(st)
FUNC Symbol            sy_byName  ( const char *name, SymbolTable st );  // Creates a symbol if none already exists
FUNC Record            sy_instanceShape    ( Symbol sy, SymbolTable st );
FUNC void              sy_setInstanceShape ( Symbol sy, Record rd, SymbolTable st );
FUNC int               sy_sendTo  ( Symbol sy, File fl, SymbolTable st );

#endif

