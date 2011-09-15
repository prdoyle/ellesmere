
#include "grammar.h"
#include "memory.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct pe_struct
	{
	Symbol name;
	Symbol token;
	} *ProductionElement;

typedef struct rhs_struct *RightHandSide;
#define AR_PREFIX  rhs
#define AR_TYPE    RightHandSide
#define AR_ELEMENT struct pe_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define rhs_new( size, ml ) rhs_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

typedef struct pns_struct
	{
	Symbol symbol;
	Symbol lhs;
	RightHandSide rhs;
	ConflictResolutions cr;
	} *ProductionStorage;

typedef struct pra_struct *ProductionArray;
#define AR_PREFIX  pra
#define AR_TYPE    ProductionArray
#define AR_ELEMENT struct pns_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define pra_new( size, ml ) pra_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct gr_struct
	{
	enum { OUTERMOST, NESTED } kind;
	ProductionArray pra;
	MemoryLifetime ml;
	union
		{
		Symbol  goal;
		Grammar outer;
		} data;
	// cached values for performance
	int numItems;
	int numInheritedProductions;
	};

// Production "references" are actually indexes within the ProductionArray,
// because the latter can move if it gets resized.  (And we don't want to
// declare pn_struct to have an int element because that would require memory
// allocation and an extra level of indirection.)

static inline int pn2int( Production pn )
	{
	return ((int)(intptr_t)pn);
	}

// We offset the indexes by 1 so there are no zeros.  That way no Production
// will compare equal to NULL.

enum { OUTERMOST_INDEX_OFFSET=1 };

FUNC int pn_index( Production pn, Grammar gr )
	{
	int result = pn2int(pn) - OUTERMOST_INDEX_OFFSET;
	assert( 0 <= result && result < gr_numProductions(gr) );
	return result;
	}

static int gr_countNumInheritedProductions( Grammar gr )
	{
	switch( gr->kind )
		{
		case OUTERMOST:
			return 0;
		case NESTED:
			return gr_numProductions( gr->data.outer );
		}
	assert(0);
	return 0;
	}

static inline bool gr_productionIsInherited( Grammar gr, int index )
	{
	assert( gr->numInheritedProductions == gr_countNumInheritedProductions( gr ) );
	return index < gr->numInheritedProductions;
	}

static inline Production pns2pn( ProductionStorage pns, Grammar gr )
	{
	assert( gr->numInheritedProductions == gr_countNumInheritedProductions( gr ) );
	return (Production)(
		  OUTERMOST_INDEX_OFFSET
		+ gr->numInheritedProductions
		+ ( pns - pra_element( gr->pra, 0 ) ) );
	}

static inline ProductionStorage pn2pns( Production pn, Grammar gr )
	{
	while( gr_productionIsInherited( gr, pn_index( pn, gr ) ) )
		gr = gr->data.outer;
	assert( gr->numInheritedProductions == gr_countNumInheritedProductions( gr ) );
	return pra_element( gr->pra, pn_index( pn, gr ) - gr->numInheritedProductions );
	}

FUNC void pn_appendWithName( Production pn, Symbol name, Symbol token, Grammar gr )
	{
	ProductionStorage pns = pn2pns(pn,gr);
	ProductionElement pe;
	pe = rhs_nextElement( pns->rhs );
	pe->token = token;
	pe->name  = name;
	gr->numItems++;
	}

FUNC ConflictResolutions pn_conflictResolution( Production pn, Grammar gr )
	{
	return pn2pns(pn,gr)->cr;
	}

FUNC void pn_setSymbol( Production pn, Symbol symbol, Grammar gr )
	{
	pn2pns(pn,gr)->symbol = symbol;
	}

FUNC Symbol pn_autoSymbol( Production pn, SymbolTable st, Grammar gr )
	{
	if( pn_symbol( pn, gr ) )
		fl_write( stderr, "ERROR: Tried to autoSymbol production %s\n", sy_name( pn_symbol( pn, gr ), st ) );
	assert( !pn_symbol( pn, gr ) );
	char *buf = alloca( 40 );
	sprintf(buf, "PN_%p_%d", gr, pn_index( pn, gr ) );
	Symbol result = sy_byName( buf, st );
	pn_setSymbol( pn, result, gr );
	return result;
	}

FUNC Symbol pn_symbol( Production pn, Grammar gr )
	{
	return pn2pns(pn,gr)->symbol;
	}

FUNC void pn_setConflictResolution( Production pn, ConflictResolutions cr, Grammar gr )
	{
	pn2pns(pn,gr)->cr = cr;
	}

FUNC void pn_stopAppending( Production pn, Grammar gr )
	{
	rhs_shrinkWrap( pn2pns(pn,gr)->rhs );
	}

FUNC Symbol pn_lhs( Production pn, Grammar gr )
	{
	return pn2pns(pn,gr)->lhs;
	}

FUNC int pn_length( Production pn, Grammar gr )
	{
	return rhs_count( pn2pns(pn,gr)->rhs );
	}

FUNC int pn_nestDepth( Production pn, Grammar gr )
	{
	int result;
	int pnIndex = pn_index( pn, gr );
	for( result = 0; gr_productionIsInherited( gr, pnIndex ); result++ )
		gr = gr->data.outer;
	return result;
	}

FUNC int gr_nestDepth( Grammar gr )
	{
	int result;
	for( result = 0; gr->kind != OUTERMOST; result++ )
		gr = gr->data.outer;
	return result;
	}

FUNC Symbol pn_token( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn2pns(pn,gr)->rhs, index )->token;
	}

FUNC Symbol pn_name( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn2pns(pn,gr)->rhs, index )->name;
	}

FUNC Grammar gr_new( Symbol goal, int numProductionsEstimate, MemoryLifetime ml )
	{
	Grammar result = (Grammar)ml_alloc( ml, sizeof(*result) );
	result->kind        = OUTERMOST;
	result->data.goal   = goal;
	result->pra         = pra_new( numProductionsEstimate, ml );
	result->ml          = ml;
	result->numItems    = 0;
	result->numInheritedProductions = 0;
	return result;
	}

FUNC Grammar gr_nested( Grammar outer, int numProductionsEstimate, MemoryLifetime ml )
	{
	Grammar result = (Grammar)ml_alloc( ml, sizeof(*result) );
	result->kind        = NESTED;
	result->data.outer  = outer;
	result->pra         = pra_new( numProductionsEstimate, ml );
	result->ml          = ml;
	result->numItems    = gr_numItems( outer );
	result->numInheritedProductions = gr_numProductions( outer );
	return result;
	}

FUNC int gr_numProductions( Grammar gr )
	{
	assert( gr->numInheritedProductions == gr_countNumInheritedProductions( gr ) );
	return pra_count( gr->pra ) + gr->numInheritedProductions;
	}

FUNC int gr_numOuterProductions( Grammar gr )
	{
	assert( gr->numInheritedProductions == gr_countNumInheritedProductions( gr ) );
	return gr->numInheritedProductions;
	}

static int gr_countItems( Grammar gr )
	{
	int result = 0; int i;
	for( i=0; i < gr_numProductions( gr ); i++ )
		result += 1 + pn_length( gr_production(gr, i), gr );
	return result;
	}

FUNC int gr_numItems( Grammar gr )
	{
	assert( gr->numItems == gr_countItems(gr) );
	return gr->numItems;
	}

FUNC void gr_stopAdding( Grammar gr )
	{
	pra_shrinkWrap( gr->pra );
	}

FUNC Symbol gr_goal( Grammar gr )
	{
	switch( gr->kind )
		{
		case OUTERMOST:
			return gr->data.goal;
		case NESTED:
			return gr_goal( gr->data.outer );
		}
	assert(0);
	return NULL;
	}

FUNC Grammar gr_outerNth( Grammar gr, int depth )
	{
	if( depth == 0 )
		return gr;
	else switch( gr->kind )
		{
		case OUTERMOST:
			return NULL;
		case NESTED:
			return gr_outerNth( gr->data.outer, depth-1 );
		}
	assert(0);
	return NULL;
	}

FUNC Grammar gr_outer( Grammar gr )
	{
	return gr_outerNth( gr, 1 );
	}

FUNC Production gr_production( Grammar gr, int index )
	{
	while( gr_productionIsInherited( gr, index ) )
		gr = gr->data.outer;
	return pns2pn( pra_element( gr->pra, index - gr->numInheritedProductions ), gr );
	}

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate )
	{
	ProductionStorage result;
	result = pra_nextElement( gr->pra );
	result->symbol = NULL;
	result->lhs = lhs;
	result->rhs = rhs_new( lengthEstimate, gr->ml );
	result->cr  = CR_NONE;
	gr->numItems++;
	return pns2pn( result, gr );
	}

FUNC Production pn_copy( Grammar oldGrammar, Production oldProduction, Grammar gr, Symbol lhs, int numTokensToCopy )
	{
	ProductionStorage storage = pra_nextElement( gr->pra ); // Do this first because it can cause gr->pra (and hence oldGrammar->pra) to get reallocated
	ProductionStorage old = pn2pns( oldProduction, oldGrammar );
	int length = pn_length( oldProduction, oldGrammar );
	*storage = *old;
	storage->lhs = lhs;
	storage->rhs = rhs_new( length, gr->ml );
	int i;
	for( i=0; i < numTokensToCopy; i++ )
		*rhs_nextElement( storage->rhs ) = *rhs_element( old->rhs, i );
	Production result = pns2pn( storage, gr );
	gr->numItems += 1 + numTokensToCopy;
	return result;
	}

FUNC int pn_sendItemTo( Production pn, int dotPosition, File fl, Grammar gr, SymbolTable st )
	{
	int i;
	int charsSent = fl_write( fl, "%s <-", sy_name( pn_lhs(pn,gr), st ) ); // usually the arrow points the other way but this makes more sense if the lhs is considered a return type
	for( i=0; i < pn_length(pn,gr); i++ )
		{
		charsSent += fl_write( fl, (i == dotPosition)? "^%s" : " %s", sy_name( pn_token( pn, i, gr ), st ) );
		if( pn_name( pn, i, gr ) )
			charsSent += fl_write( fl, "@%s", sy_name( pn_name( pn, i, gr ), st ) );
		}
	if( i == dotPosition )
		charsSent += fl_write( fl, "^" );
	return charsSent;
	}

FUNC int pn_sendTo( Production pn, File fl, Grammar gr, SymbolTable st )
	{
	int charsSent = pn_sendItemTo( pn, -1, fl, gr, st );
	Symbol sy = pn2pns( pn, gr )->symbol;
	if( sy )
		charsSent += fl_write( fl, "  << %s >>", sy_name( sy, st ) );
	return charsSent;
	}

static int sendTo( Grammar gr, File fl, SymbolTable st, int indent )
	{
	int i;
	int charsSent = fl_write( fl, "%*sGrammar %p( %s )\n  %*s{\n", indent, "", gr, sy_name( gr_goal(gr), st ), indent, "" );
	if( gr->kind == NESTED )
		charsSent += sendTo( gr->data.outer, fl, st, indent+2 );
	for( i=gr->numInheritedProductions; i < gr_numProductions(gr); i++ )
		{
		charsSent += fl_write( fl, "  %*s", indent, "" );
		charsSent += pn_sendTo( gr_production( gr, i ), fl, gr, st );
		charsSent += fl_write( fl, "\n" );
		}
	charsSent += fl_write( fl, "  %*s}\n", indent, "" );
	return charsSent;
	}

FUNC int gr_sendTo( Grammar gr, File fl, SymbolTable st )
	{
	return sendTo( gr, fl, st, 0 );
	}

// MERGE:22

