
#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "symbols.h"

FUNC Grammar    gr_new       ( Symbol  goal,  int numProductionsEstimate, MemoryLifetime ml );
FUNC Grammar    gr_nested    ( Grammar outer, int numProductionsEstimate, MemoryLifetime ml );
FUNC Grammar    gr_augmented ( Grammar original, InheritanceRelation ir,  MemoryLifetime ml, File diagnostics );

FUNC void       gr_stopAdding     ( Grammar gr );
FUNC Symbol     gr_goal           ( Grammar gr );
FUNC Grammar    gr_outer          ( Grammar gr );
FUNC Grammar    gr_outerNth       ( Grammar gr, int depth );
FUNC int        gr_numProductions ( Grammar gr );
FUNC int        gr_numOuterProductions ( Grammar gr );
FUNC int        gr_numItems       ( Grammar gr );
FUNC Production gr_production     ( Grammar gr, int index );
FUNC int        gr_nestDepth      ( Grammar gr );
FUNC int        gr_sendTo         ( Grammar gr, File fl, SymbolTable st );

typedef enum
	{
	CR_NONE,

	CR_SHIFT_BEATS_REDUCE,
	CR_REDUCE_BEATS_SHIFT,
	CR_ARBITRARY_REDUCE,   // Don't sweat reduce-reduce conflicts

	CR_COUNT
	} ConflictResolutions;

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate );
FUNC void       pn_appendWithName ( Production pn, Symbol name, Symbol token, Grammar gr );
FUNC void       pn_setConflictResolution ( Production pn, ConflictResolutions cr, Grammar gr );
FUNC void       pn_stopAppending  ( Production pn, Grammar gr );
FUNC int        pn_index          ( Production pn, Grammar gr );
FUNC Symbol     pn_lhs            ( Production pn, Grammar gr );
FUNC int        pn_length         ( Production pn, Grammar gr );
FUNC int        pn_nestDepth      ( Production pn, Grammar gr );
FUNC Symbol     pn_token          ( Production pn, int index, Grammar gr );
FUNC Symbol     pn_name           ( Production pn, int index, Grammar gr ); // NULL if none
FUNC ConflictResolutions pn_conflictResolution ( Production pn, Grammar gr );
FUNC int        pn_sendTo         ( Production pn, File fl, Grammar gr, SymbolTable st );
FUNC int        pn_sendItemTo     ( Production pn, int dotPosition, File fl, Grammar gr, SymbolTable st );

static inline void pn_append( Production pn, Symbol token, Grammar gr )
	{ pn_appendWithName( pn, NULL, token, gr ); }

#endif

