
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include <string.h>

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

typedef struct sste_struct
	{
	Symbol sy;
	BitVector leftmostItems;  // items with pn_lhs( pn ) == sy && dot == 0
	BitVector expectingItems; // items with dot immediately before sy
	} *SymbolSideTableEntry;

typedef struct sst_struct *SymbolSideTable;
#define AR_PREFIX  sst
#define AR_TYPE    SymbolSideTable
#define AR_ELEMENT struct sste_struct
#undef AR_BYVALUE
#include "array_template.h"

typedef struct its_struct *ItemSet;
struct its_struct
	{
	ItemSet   prev, next;
	BitVector items;
	Object    stateNode;
	bool      isExpanded;
	};

typedef struct pg_struct
	{
	Grammar          gr;
	MemoryBatch      mb;
	ObjectHeap       heap;
	ItemArray        items;
	BitVector        rightmostItems;
	SymbolTable      st;
	SymbolSideTable  sst;
	int             *sstIndexes;
	ItemSet          itemSetList;
	} *ParserGenerator;

static ItemSet its_new( ItemSet prev, BitVector items, ParserGenerator pg, ItemSet next )
	{
	ItemSet result = (ItemSet)mb_alloc( pg->mb, sizeof(*result) );
	result->items = items;
	result->stateNode = ob_create( sy_byIndex( SYM_STATE_NODE, pg->st ), pg->heap );
	result->next = next;
	result->isExpanded = false;
	return result;
	}

static SymbolSideTableEntry pg_sideTableEntry( ParserGenerator pg, Symbol sy )
	{
	SymbolSideTableEntry result;
	int sstIndex = pg->sstIndexes[ sy_index( sy, pg->st ) ];
	if( sstIndex )
		result = sst_element( pg->sst, sstIndex );
	else
		{
		pg->sstIndexes[ sy_index( sy, pg->st ) ] = sst_count( pg->sst );
		result = sst_nextElement( pg->sst );
		result->sy = sy;
		result->leftmostItems  = NULL;
		result->expectingItems = NULL;
		}
	return result;
	}

static void pg_closeItemSet( ParserGenerator pg, BitVector itemSet )
	{
	int i;
	int oldPopulation = 0;
	int curPopulation = bv_population( itemSet );
	while( curPopulation > oldPopulation )
		{
		oldPopulation = curPopulation;
		for( i = bv_firstBit( itemSet ); i != bv_END; i = bv_nextBit( itemSet, i ) )
			{
			Item it = ita_element( pg->items, i );
			Production pn = it->pn;
			if( pn_length( pn, pg->gr ) == 0 )
				assert(0); // TODO: empty productions?
			if( !bv_isSet( pg->rightmostItems, i ) )
				{
				Symbol nextToken = pn_token( pn, it->dot, pg->gr );
				SymbolSideTableEntry lhs = pg_sideTableEntry( pg, nextToken );
				BitVector itemsToAdd = lhs->leftmostItems;
				if( itemsToAdd )
					bv_or( itemSet, itemsToAdd );
				}
			}
		curPopulation = bv_population( itemSet );
		}
	}

static void pg_computeItemsExpectingToken( ParserGenerator pg, BitVector result, BitVector itemSet, Symbol token )
	{
	BitVector expectingItems = pg_sideTableEntry( pg, token )->expectingItems;
	if( !expectingItems )
		{
		bv_clear( result );
		return;
		}
	bv_copy( result, itemSet );
	bv_and( result, expectingItems );
	if( bv_isEmpty( result ) )
		return;
	}

static ItemSet pg_findItemSet( ParserGenerator pg, BitVector items )
	{
	ItemSet result, stop;
	result = stop = pg->itemSetList;
	if( result )
		{
		if( bv_equals( result->items, items ) )
			return result;
		for( result = result->next; result != stop; result = result->next )
			if( bv_equals( result->items, items ) )
				return result;
		}
	return NULL;
	}

static ItemSet pg_createItemSet( ParserGenerator pg, BitVector items )
	{
	ItemSet cur = pg->itemSetList; ItemSet result;
	if( cur )
		{
		result = its_new( cur->prev, items, pg, cur );
		result->prev->next = result;
		result->next->prev = result;
		}
	else
		{
		result = its_new( NULL, items, pg, NULL );
		result->prev = result->next = result;
		}
	return result;
	}

#if 0
static ItemSet pg_findOrCreateItemSet( ParserGenerator pg, BitVector items )
	{
	ItemSet result = pg_findItemSet( pg, items );
	if( result )
		return result;
	else
		return pg_createItemSet( pg, items );
	}
#endif

static ParserGenerator pg_new( Grammar gr, SymbolTable st, MemoryBatch mb, ObjectHeap heap )
	{
	int i,j; int itemIndex;
	ParserGenerator pg = (ParserGenerator)mb_alloc( mb, sizeof(*pg) );
	pg->gr = gr;
	pg->st = st;
	pg->mb = mb;
	pg->heap = heap;

	// Pass 1: Populate item table
	{
	pg->items = ita_newInMB( 1000, mb );
	pg->rightmostItems = bv_newInMB( gr_numProductions(gr), mb );
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		for( j=0; j <= pn_length( pn, gr ); j++ )
			{
			Item it = ita_nextElement( pg->items );
			it->pn  = pn;
			it->dot = j;
			}
		bv_set( pg->rightmostItems, ita_count( pg->items ) );
		}
	ita_shrinkWrap( pg->items );
	 bv_shrinkWrap( pg->rightmostItems );
	}

	// Pass 2: Populate the symbol side table
	{
	const int sstIndexesSize = st_count(st) * sizeof(pg->sstIndexes[0]);
	pg->sstIndexes = (int*)mb_alloc( mb, sstIndexesSize );
	memset( pg->sstIndexes, 0, sstIndexesSize );
	pg->sst = sst_newInMB( 100, mb );
	sst_incCount( pg->sst ); // sst index zero is used for "null" so skip that one
	itemIndex = 0;
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		Symbol lhs = pn_lhs( pn, gr );
		SymbolSideTableEntry entry = pg_sideTableEntry( pg, lhs );
		Item it = ita_element( pg->items, itemIndex );
		if( !entry->leftmostItems )
			entry->leftmostItems = bv_newInMB( ita_count( pg->items ), mb );
		assert( it->pn == pn && it->dot == 0 );
		assert( pn_lhs( it->pn, gr ) == lhs );
		bv_set( entry->leftmostItems, itemIndex );
		for( j=0; j < pn_length( pn, gr ); j++ )
			{
			it = ita_element( pg->items, itemIndex );
			entry = pg_sideTableEntry( pg, pn_token( it->pn, j, gr ) );
			itemIndex++;
			if( !entry->expectingItems )
				entry->expectingItems = bv_newInMB( ita_count( pg->items ), mb );
			bv_set( entry->expectingItems, itemIndex );
			}
		// now the item where j == pn_length
		itemIndex++;
		}
	}

	// Seed the itemSet list and start generating
	{
	ItemSet curItemSet, startItemSet;
	startItemSet = curItemSet = pg_createItemSet( pg, pg_sideTableEntry( pg, gr_goal(pg->gr) )->leftmostItems );
	while( curItemSet )
		{
		int itemIndex; BitVector nextItems = bv_newInMB( itemIndex, mb );
		BitVector itemsLeft = bv_newInMB( itemIndex, mb );
		bv_copy( itemsLeft, curItemSet->items );
		assert( !curItemSet->isExpanded );
		curItemSet->isExpanded = true;
		for( itemIndex = bv_firstBit( itemsLeft ); itemIndex != bv_END; itemIndex = bv_nextBit( itemsLeft, itemIndex ) )
			{
			Item it = ita_element( pg->items, itemIndex ); ItemSet nextItemSet;
			Symbol expected = pn_token( it->pn, it->dot, pg->gr );
			pg_computeItemsExpectingToken( pg, nextItems, itemsLeft, expected );
			bv_minus( itemsLeft, nextItems );
			bv_shift( nextItems );
			pg_closeItemSet( pg, nextItems );
			nextItemSet = pg_findItemSet( pg, nextItems );
			if( !nextItemSet )
				{
				// Use the nextItems bitvector we created, and allocate a new one for the next guy
				nextItemSet = pg_createItemSet( pg, nextItems );
				nextItems = bv_newInMB( itemIndex, mb );
				}
			ob_setField( curItemSet->stateNode, expected, nextItemSet->stateNode, pg->heap );
			if( nextItemSet->isExpanded )
				{
				ItemSet stop = nextItemSet;
				for( nextItemSet = nextItemSet->next; nextItemSet != stop; nextItemSet = nextItemSet->next )
					if( !nextItemSet->isExpanded )
						break;
				}
			}
		}
	}

	return pg;
	}

FUNC Parser ps_new( Grammar gr, SymbolTable st )
	{
	MemoryBatch     mb = mb_new( 10000 );
	ParserGenerator pg = pg_new( gr, st, mb, theObjectHeap() ); // TODO: allocate objects in mb
	// TODO: generate the parser
	mb_free( mb );
	return pg? NULL: NULL;
	}

// MERGE: 55

