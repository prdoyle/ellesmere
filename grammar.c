
#include "grammar.h"
#include "memory.h"

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

struct pn_struct
	{
	Symbol lhs;
	RightHandSide rhs;
	};

FUNC void pr_appendWithName( Production pn, Symbol name, Symbol token, Grammar gr )
	{
	ProductionElement pe;
	rhs_incCount( pn->rhs );
	pe = rhs_last( pn->rhs, 0 );
	pe->token = token;
	pe->name  = name;
	}

FUNC void pr_stopAppending( Production pn, Grammar gr )
	{
	rhs_setCapacity( pn->rhs, rhs_count( pn->rhs ) );
	}

FUNC Symbol pr_lhs( Production pn, Grammar gr )
	{
	return pn->lhs;
	}

FUNC int pr_length( Production pn, Grammar gr )
	{
	return rhs_count( pn->rhs );
	}

FUNC Symbol pr_token( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn->rhs, index )->token;
	}

FUNC Symbol pr_name( Production pn, int index, Grammar gr )
	{
	return rhs_element( pn->rhs, index )->token;
	}

typedef struct pra_struct *ProductionArray;
#define AR_PREFIX  pra
#define AR_TYPE    ProductionArray
#define AR_ELEMENT struct pn_struct
#undef AR_BYVALUE
#include "array_template.h"

struct gr_struct
	{
	Symbol goal;
	ProductionArray pra;
	};

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
	return pra_element( gr->pra, index );
	}

FUNC Production pr_new( Grammar gr, Symbol lhs, int lengthEstimate )
	{
	Production result;
	pra_incCount( gr->pra );
	result = pra_last( gr->pra, 0 );
	result->lhs = lhs;
	result->rhs = rhs_new( lengthEstimate );
	return result;
	}

// MERGE:22

