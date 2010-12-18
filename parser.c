
#include "parser.h"
#include "bitvector.h"
#include "memory.h"

typedef struct it_struct
	{
	Production pn;
	int dot; // "Current" position of the dot is before the token at this index
	} *Item;

typedef struct ita_struct *ItemArray;
#define AR_PREFIX  ita
#define AR_TYPE    ItemArray
#define AR_ELEMENT struct it_struct
#undef AR_BYVALUE
#include "array_template.h"

typedef struct pg_struct
	{
	MemoryBatch mb;
	ItemArray   items;
	BitVector   leftmostItems;
	BitVector   rightmostItems;
	} *ParserGenerator;

static ParserGenerator pg_new( Grammar gr, MemoryBatch mb )
	{
	int i,j;
	ParserGenerator result = (ParserGenerator)mb_alloc( mb, sizeof(*result) );
	result->mb = mb;

	// Populate item table
	result->items = ita_newInMB( 1000, mb );
	result->leftmostItems  = bv_newInMB( gr_numProductions(gr), mb );
	result->rightmostItems = bv_newInMB( gr_numProductions(gr), mb );
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		bv_set( result->leftmostItems, ita_count( result->items ) );
		for( j=0; j <= pn_length( pn, gr ); j++ )
			{
			Item it = ita_nextElement( result->items );
			it->pn  = pn;
			it->dot = j;
			}
		bv_set( result->rightmostItems, ita_count( result->items ) );
		}
	ita_shrinkWrap( result->items );
	 bv_shrinkWrap( result->leftmostItems );
	 bv_shrinkWrap( result->rightmostItems );

	return result;
	}

FUNC Parser ps_new( Grammar gr )
	{
	MemoryBatch     mb = mb_new( 10000 );
	ParserGenerator pg = pg_new( gr, mb );
	// TODO: generate the parser
	mb_free( mb );
	return pg? NULL: NULL;
	}

// MERGE: 55

