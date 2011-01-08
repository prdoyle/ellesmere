
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "base.h"
#include "file.h"

typedef enum
	{
	NULL_SYMBOL_INDEX=0,
	SYM_INT,
	SYM_STRING,
	SYM_TOKEN,
	SYM_FUNCTION,
	SYM_TOKEN_STREAM,
	SYM_END_OF_INPUT,
	NUM_SPECIAL_OBJECT_TAGS,

	SYM_STATE_NODE         = NUM_SPECIAL_OBJECT_TAGS,
	SYM_ITEM_SET_NUM,
	NUM_PREDEFINED_SYMBOLS
	} SymbolIndex;

FUNC SymbolTable theSymbolTable();
FUNC SymbolIndex st_count   ( SymbolTable st );

FUNC SymbolIndex       sy_index ( Symbol sy, SymbolTable st );
FUNC const const char *sy_name  ( Symbol sy, SymbolTable st );
FUNC Symbol            sy_byIndex ( SymbolIndex index, SymbolTable st ); // 0 <= index < st_count(st)
FUNC Symbol            sy_byName  ( const char *name, SymbolTable st ); // Creates a symbol if none already exists
FUNC int               sy_sendTo  ( Symbol sy, File fl, SymbolTable st );

FUNC Context      cx_new( SymbolTable st );
FUNC void         cx_save( Context cx );
FUNC void         cx_restore( Context cx );
FUNC SymbolTable  cx_symbolTable( Context cx );
FUNC Object       cx_filter( Context cx, Object ob, ObjectHeap heap );
FUNC int          cx_sendTo( Context cx, File fl );

// Attributes of symbols
FUNC Object            sy_value              ( Symbol sy, Context cx );
FUNC void              sy_setValue           ( Symbol sy, Object value, Context cx );

#endif

