
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

FUNC Scope st_outermostScope( SymbolTable st );
FUNC Scope sc_new( Scope outer );
FUNC SymbolTable sc_symbolTable( Scope sc );
FUNC Scope       sc_outer( Scope sc );
FUNC void        sc_sendTo( Scope sc, File fl );

// Attributes of symbols
FUNC Action            sy_immediateAction    ( Symbol sy, Scope sc );
FUNC void              sy_setImmediateAction ( Symbol sy, Action an, Scope sc );
FUNC int               sy_arity              ( Symbol sy, Scope sc );
FUNC void              sy_setArity           ( Symbol sy, int arity, Scope sc );
FUNC bool              sy_isSymbolic         ( Symbol sy, Scope sc );
FUNC void              sy_setIsSymbolic      ( Symbol sy, bool isSymbolic, Scope sc );
FUNC Object            sy_value              ( Symbol sy, Scope sc );
FUNC void              sy_setValue           ( Symbol sy, Object value, Scope sc );

// Action handling
typedef Action (*ActionFunction)( Action an, Scope sc );
FUNC Action an_perform( Action an, Scope sc );
FUNC Action an_fromFunction( ActionFunction af );
FUNC Action an_fromFunctionAndSymbol( ActionFunction af, Symbol sy );
FUNC Symbol an_symbol( Action an );

#endif

