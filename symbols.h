
#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "base.h"

typedef struct st_struct *SymbolTable;
typedef struct sy_struct *Symbol;

typedef enum
	{
	NO_SYMBOL_INDEX=0,
	SYM_INT,
	SYM_STRING,

	NUM_PREDEFINED_SYMBOLS
	} SymbolIndex;

FUNC SymbolTable theSymbolTable();
FUNC SymbolIndex st_count   ( SymbolTable st );
FUNC Symbol      st_byIndex ( SymbolIndex index, SymbolTable st );
FUNC Symbol      st_byName  ( const char *name,  SymbolTable st );

static inline Symbol st_int    ( SymbolTable st ){ return st_byIndex( SYM_INT,    st ); }
static inline Symbol st_string ( SymbolTable st ){ return st_byIndex( SYM_STRING, st ); }

FUNC SymbolIndex       sy_index ( Symbol sy, SymbolTable st );
FUNC const const char *sy_name  ( Symbol sy, SymbolTable st );

#endif

