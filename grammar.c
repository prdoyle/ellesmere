
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
	Symbol goal;
	ProductionArray pra;
	};

// Production "references" are actually indexes within the ProductionArray,
// because the latter can move if it gets resized.  (And we don't want to
// declare pn_struct to have an int element because that would require memory
// allocation and an extra level of indirection.)

FUNC int pn_index( Production pn, Grammar gr )
	{
	return (int)(intptr_t)pn;
	}

static inline Production pns2pn( ProductionStorage pns, Grammar gr )
	{
	return (Production)( pns - pra_element( gr->pra, 0 ) );
	}

static inline ProductionStorage pn2pns( Production pn, Grammar gr )
	{
	return pra_element( gr->pra, pn_index( pn, gr ) );
	}

FUNC void pn_appendWithName( Production pn, Symbol name, Symbol token, Grammar gr )
	{
	ProductionStorage pns = pn2pns(pn,gr);
	ProductionElement pe;
	rhs_incCount( pns->rhs );
	pe = rhs_last( pns->rhs, 0 );
	pe->token = token;
	pe->name  = name;
	}

FUNC void pn_stopAppending( Production pn, Grammar gr )
	{
	RightHandSide rhs = pn2pns(pn,gr)->rhs;
	rhs_setCapacity( rhs, rhs_count( rhs ) );
	}

FUNC Symbol pn_lhs( Production pn, Grammar gr )
	{
	return pn2pns(pn,gr)->lhs;
	}

FUNC int pn_length( Production pn, Grammar gr )
	{
	return rhs_count( pn2pns(pn,gr)->rhs );
	}

FUNC Symbol pn_token( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn2pns(pn,gr)->rhs, index )->token;
	}

FUNC Symbol pn_name( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn2pns(pn,gr)->rhs, index )->token;
	}

FUNC Grammar gr_new( Symbol goal, int numProductionsEstimate )
	{
	Grammar result = (Grammar)mem_alloc( sizeof(*result) );
	result->goal = goal;
	result->pra  = pra_new( numProductionsEstimate );
	return result;
	}

FUNC int gr_numProductions( Grammar gr )
	{
	return pra_count( gr->pra );
	}

FUNC void gr_stopAdding( Grammar gr )
	{
	pra_setCapacity( gr->pra, gr_numProductions(gr) );
	}

FUNC Production gr_production( Grammar gr, int index )
	{
	return pns2pn( pra_element( gr->pra, index ), gr );
	}

FUNC Production pn_new( Grammar gr, Symbol lhs, int lengthEstimate )
	{
	ProductionStorage result;
	pra_incCount( gr->pra );
	result = pra_last( gr->pra, 0 );
	result->lhs = lhs;
	result->rhs = rhs_new( lengthEstimate );
	return pns2pn( result, gr );
	}

// MERGE:22

