
#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "symbols.h"

FUNC Grammar    gr_new( Symbol goal, int numProductionsEstimate );
FUNC void       gr_stopAdding     ( Grammar gr );
FUNC Symbol     gr_goal           ( Grammar gr );
FUNC int        gr_numProductions ( Grammar gr );
FUNC Production gr_production     ( Grammar gr, int index );
FUNC int        gr_sendTo         ( Grammar gr, File fl, SymbolTable st );

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate );
FUNC void       pn_appendWithName ( Production pr, Symbol name, Symbol token, Grammar gr );
FUNC void       pn_stopAppending  ( Production pr, Grammar gr );
FUNC int        pn_index          ( Production pr, Grammar gr );
FUNC Symbol     pn_lhs            ( Production pr, Grammar gr );
FUNC int        pn_length         ( Production pr, Grammar gr );
FUNC Symbol     pn_token          ( Production pr, int index, Grammar gr );
FUNC Symbol     pn_name           ( Production pr, int index, Grammar gr ); // NULL if none
FUNC int        pn_sendTo         ( Production pn, File fl, Grammar gr, SymbolTable st );
FUNC int        pn_sendItemTo     ( Production pn, int dotPosition, File fl, Grammar gr, SymbolTable st );

static inline void pn_append( Production pn, Symbol token, Grammar gr )
	{ pn_appendWithName( pn, NULL, token, gr ); }

#endif

