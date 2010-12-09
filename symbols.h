
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "base.h"
#include "file.h"

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

FUNC Context      st_outermostScope( SymbolTable st );
FUNC Context      cx_new( Context outer );
FUNC SymbolTable  cx_symbolTable( Context cx );
FUNC Context      cx_outer( Context cx );
FUNC void         cx_sendTo( Context cx, File fl );

// Attributes of symbols
FUNC Action            sy_immediateAction    ( Symbol sy, Context cx );
FUNC void              sy_setImmediateAction ( Symbol sy, Action an, Context cx );
FUNC int               sy_arity              ( Symbol sy, Context cx );
FUNC void              sy_setArity           ( Symbol sy, int arity, Context cx );
FUNC bool              sy_isSymbolic         ( Symbol sy, Context cx );
FUNC void              sy_setIsSymbolic      ( Symbol sy, bool isSymbolic, Context cx );
FUNC Object            sy_value              ( Symbol sy, Context cx );
FUNC void              sy_setValue           ( Symbol sy, Object value, Context cx );

// Action handling
typedef Action (*ActionFunction)( Action an, Context cx );
FUNC Action an_perform( Action an, Context cx );
FUNC Action an_fromFunction( ActionFunction af );
FUNC Action an_fromFunctionAndSymbol( ActionFunction af, Symbol sy );
FUNC Symbol an_symbol( Action an );

#endif

