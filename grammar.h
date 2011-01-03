
#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "symbols.h"

FUNC Grammar    gr_new    ( Symbol  goal,  int numProductionsEstimate, MemoryLifetime ml );
FUNC Grammar    gr_nested ( Grammar outer, int numProductionsEstimate, MemoryLifetime ml );

FUNC void       gr_stopAdding     ( Grammar gr );
FUNC Symbol     gr_goal           ( Grammar gr );
FUNC Grammar    gr_outer          ( Grammar gr );
FUNC Grammar    gr_outerNth       ( Grammar gr, int depth );
FUNC int        gr_numProductions ( Grammar gr );
FUNC int        gr_numItems       ( Grammar gr );
FUNC Production gr_production     ( Grammar gr, int index );
FUNC int        gr_nestDepth      ( Grammar gr );
FUNC int        gr_sendTo         ( Grammar gr, File fl, SymbolTable st );

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate );
FUNC void       pn_appendWithName ( Production pr, Symbol name, Symbol token, Grammar gr );
FUNC void       pn_stopAppending  ( Production pr, Grammar gr );
FUNC int        pn_index          ( Production pr, Grammar gr );
FUNC Symbol     pn_lhs            ( Production pr, Grammar gr );
FUNC int        pn_length         ( Production pr, Grammar gr );
FUNC int        pn_nestDepth      ( Production pr, Grammar gr );
FUNC Symbol     pn_token          ( Production pr, int index, Grammar gr );
FUNC Symbol     pn_name           ( Production pr, int index, Grammar gr ); // NULL if none
FUNC int        pn_sendTo         ( Production pn, File fl, Grammar gr, SymbolTable st );
FUNC int        pn_sendItemTo     ( Production pn, int dotPosition, File fl, Grammar gr, SymbolTable st );

static inline void pn_append( Production pn, Symbol token, Grammar gr )
	{ pn_appendWithName( pn, NULL, token, gr ); }

#endif

