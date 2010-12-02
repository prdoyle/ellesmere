
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "base.h"

typedef enum
	{
	NO_SYMBOL_INDEX=0,
	SYM_INT,
	SYM_STRING,
	SYM_TOKEN,
	SYM_TOKEN_BLOCK,
	SYM_TOKEN_STREAM,

	NUM_PREDEFINED_SYMBOLS
	} SymbolIndex;

FUNC SymbolTable theSymbolTable();
FUNC SymbolIndex st_count   ( SymbolTable st );

FUNC SymbolIndex       sy_index ( Symbol sy, SymbolTable st );
FUNC const const char *sy_name  ( Symbol sy, SymbolTable st );
FUNC Symbol            sy_byIndex ( SymbolIndex index, SymbolTable st ); // 0 <= index < st_count(st)
FUNC Symbol            sy_byName  ( const char *name, SymbolTable st ); // Creates a symbol if none already exists

FUNC Action            sy_immediateAction    ( Symbol sy, SymbolTable st );
FUNC void              sy_setImmediateAction ( Symbol sy, Action an, SymbolTable st );

typedef Action (*ActionFunction)( Actor ar );
FUNC Action an_fromFunction( ActionFunction af );
FUNC Action an_perform( Action an, Actor ar );

#endif

