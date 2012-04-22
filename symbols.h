
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "file.h"

typedef struct st_struct *SymbolTable;
typedef struct sy_struct *Symbol;

typedef enum
	{
	NULL_SYMBOL_INDEX=0,
	SYM_ANY, // Putting this first is useful for bootstrapping the InheritanceRelation

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
	SYM_ITEM_SET_NUM,
	SYM_REDUCE_CONTEXT_LENGTH,
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
	SYM_VALUE,
	SYM_TAG,

	// Placeholders go at the end, unless you want to change ob_isPlaceholder
	//
	SYM_PLACEHOLDER,
	SYM_VALUE_PLACEHOLDER,
	SYM_TOKEN_BLOCK_PLACEHOLDER,
	SYM_RECORDED_PLACEHOLDER,

	NUM_PREDEFINED_SYMBOLS
	} SymbolIndex;

FUNC SymbolTable theSymbolTable();

FUNC SymbolIndex  st_count  ( SymbolTable st );

FUNC SymbolIndex       sy_index ( Symbol sy, SymbolTable st );
FUNC const const char *sy_name  ( Symbol sy, SymbolTable st );
FUNC Symbol            sy_byIndex ( SymbolIndex index, SymbolTable st ); // 0 <= index < st_count(st)
FUNC Symbol            sy_byName  ( const char *name, SymbolTable st );  // Creates a symbol if none already exists
FUNC int               sy_sendTo  ( Symbol sy, File fl, SymbolTable st );

#endif

