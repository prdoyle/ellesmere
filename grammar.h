
#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "symbols.h"

FUNC Grammar    gr_new( Symbol goal, int numProductionsEstimate );
FUNC void       gr_stopAdding( Grammar gr ); // optional hint
FUNC int        gr_numProductions( Grammar gr ); 
FUNC Production gr_production( Grammar gr, int index ); 

FUNC Production pr_new( Grammar gr, Symbol lhs, int lengthEstimate );
FUNC void       pr_appendWithName ( Production pr, Symbol name, Symbol token, Grammar gr );
FUNC void       pr_stopAppending  ( Production pr, Grammar gr ); // optional hint
FUNC Symbol     pr_lhs            ( Production pr, Grammar gr );
FUNC int        pr_length         ( Production pr, Grammar gr );
FUNC Symbol     pr_token          ( Production pr, int index, Grammar gr );
FUNC Symbol     pr_name           ( Production pr, int index, Grammar gr ); // NULL if none

static inline void pr_append( Production pr, Symbol token, Grammar gr )
	{ pr_appendWithName( pr, NULL, token, gr ); }

#endif

