
#ifndef SYMBOL_TOKENS_H
#define SYMBOL_TOKENS_H

// You probably don't want to use these.  They are a cheesy back-door into the
// symbol table to prevent symbols.o from having to be linked against objects.o.
//
// The ultimate solution will probably be either to stop pretending we can have
// more than one heap, or to have the heap itself manage the symbol table.
//
FUNC Object st_getToken( SymbolTable st, Symbol sy );
FUNC void   st_setToken( SymbolTable st, Symbol sy, Object token );

#endif

