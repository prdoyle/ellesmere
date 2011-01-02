
#include "grammar.h"
#include "memory.h"
#include <stdint.h>

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

typedef struct pns_struct
	{
	Symbol lhs;
	RightHandSide rhs;
	} *ProductionStorage;

typedef struct pra_struct *ProductionArray;
#define AR_PREFIX  pra
#define AR_TYPE    ProductionArray
#define AR_ELEMENT struct pns_struct
#undef AR_BYVALUE
#include "array_template.h"

struct gr_struct
	{
	enum { OUTERMOST, NESTED } kind;
	ProductionArray pra;
	MemoryLifetime ml;
	int numItems;
	union
		{
		Symbol  goal;
		Grammar outer;
		} data;
	};

// Production "references" are actually indexes within the ProductionArray,
// because the latter can move if it gets resized.  (And we don't want to
// declare pn_struct to have an int element because that would require memory
// allocation and an extra level of indirection.)

static int pn2int( Production pn )
	{
	return ((int)(intptr_t)pn);
	}

// We offset the indexes by 1 so there are no zeros.  That way no Production
// will compare equal to NULL.

enum { OUTERMOST_INDEX_OFFSET=1 };

FUNC int pn_index( Production pn, Grammar gr )
	{
	return pn2int(pn) - OUTERMOST_INDEX_OFFSET;
	}

static int gr_numInheritedProductions( Grammar gr )
	{
	switch( gr->kind )
		{
		case OUTERMOST:
			return 0;
		case NESTED:
			return gr_numProductions( gr->data.outer );
		}
	assert(0);
	}

static bool gr_productionIsInherited( Grammar gr, int index )
	{
	switch( gr->kind )
		{
		case OUTERMOST:
			return false;
		case NESTED:
			return index < gr_numInheritedProductions( gr );
		}
	assert(0);
	}

static inline Production pns2pn( ProductionStorage pns, Grammar gr )
	{
	return (Production)(
		  OUTERMOST_INDEX_OFFSET
		+ gr_numInheritedProductions( gr )
		+ ( pns - pra_element( gr->pra, 0 ) ) );
	}

static inline ProductionStorage pn2pns( Production pn, Grammar gr )
	{
	if( gr_productionIsInherited( gr, pn_index( pn, gr ) ) )
		return pn2pns( pn, gr->data.outer );
	else
		return pra_element( gr->pra, pn_index( pn, gr ) - gr_numInheritedProductions( gr ) );
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
	return rhs_element( pn2pns(pn,gr)->rhs, index )->token;
	}

FUNC Grammar gr_new( Symbol goal, int numProductionsEstimate, MemoryLifetime ml )
	{
	Grammar result = (Grammar)ml_alloc( ml, sizeof(*result) );
	result->kind        = OUTERMOST;
	result->data.goal   = goal;
	result->pra         = pra_new( numProductionsEstimate, ml );
	result->ml          = ml;
	result->numItems    = 0;
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
	return result;
	}

FUNC int gr_numProductions( Grammar gr )
	{
	return pra_count( gr->pra ) + gr_numInheritedProductions( gr );
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
	}

FUNC Production gr_production( Grammar gr, int index )
	{
	if( gr_productionIsInherited( gr, index ) )
		return gr_production( gr->data.outer, index );
	else
		return pns2pn( pra_element( gr->pra, index - gr_numInheritedProductions( gr ) ), gr );
	}

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate )
	{
	ProductionStorage result;
	result = pra_nextElement( gr->pra );
	result->lhs = lhs;
	result->rhs = rhs_new( lengthEstimate, gr->ml );
	gr->numItems++;
	return pns2pn( result, gr );
	}

FUNC int pn_sendItemTo( Production pn, int dotPosition, File fl, Grammar gr, SymbolTable st )
	{
	int i;
	int charsSent = fl_write( fl, "%s ->", sy_name( pn_lhs(pn,gr), st ) );
	for( i=0; i < pn_length(pn,gr); i++ )
		{
		charsSent += fl_write( fl, (i == dotPosition)? "^%s" : " %s", sy_name( pn_token( pn, i, gr ), st ) );
		}
	if( i == dotPosition )
		charsSent += fl_write( fl, "^" );
	return charsSent;
	}

FUNC int pn_sendTo( Production pn, File fl, Grammar gr, SymbolTable st )
	{
	return pn_sendItemTo( pn, -1, fl, gr, st );
	}

static int sendTo( Grammar gr, File fl, SymbolTable st, int indent )
	{
	int i;
	int charsSent = fl_write( fl, "%*sGrammar( %s )\n  %*s{\n", indent, "", sy_name( gr_goal(gr), st ), indent, "" );
	if( gr->kind == NESTED )
		charsSent += sendTo( gr->data.outer, fl, st, indent+2 );
	for( i=gr_numInheritedProductions(gr); i < gr_numProductions(gr); i++ )
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

