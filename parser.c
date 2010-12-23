
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include <string.h>

#ifdef PARSER_T
	#define trace   fl_write
	#define traceBV bv_sendTo
	#define traceBVX bv_sendFormattedTo
#else
	#define trace(...)
	#define traceBV(...)
	#define traceBVX(...)
#endif

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
	result->prev = prev;
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
	assert( result->sy == sy );
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
		pg->itemSetList = result = its_new( NULL, items, pg, NULL );
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
	ParserGenerator pg = (ParserGenerator)mb_alloc( mb, sizeof(*pg) );
	pg->gr = gr;
	pg->st = st;
	pg->mb = mb;
	pg->heap = heap;
	return pg;
	}

static void pg_populateItemTable( ParserGenerator pg )
	{
	int i,j;
	MemoryBatch mb = pg->mb;
	Grammar gr = pg->gr;
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
		bv_set( pg->rightmostItems, ita_count(pg->items) - 1 );
		}
	ita_shrinkWrap( pg->items );
	 bv_shrinkWrap( pg->rightmostItems );
	}

static void pg_populateSymbolSideTable( ParserGenerator pg )
	{
	int i,j;
	MemoryBatch mb = pg->mb;
	Grammar gr = pg->gr;
	SymbolTable st = pg->st;
	int itemIndex;
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
			if( !entry->expectingItems )
				entry->expectingItems = bv_newInMB( ita_count( pg->items ), mb );
			bv_set( entry->expectingItems, itemIndex );
			itemIndex++;
			}
		// now the item where j == pn_length
		itemIndex++;
		}
	}

static void pg_computeAutomaton( ParserGenerator pg )
	{
	MemoryBatch mb = pg->mb;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = ita_count( pg->items );
	BitVector nextItems = bv_newInMB( itemCount, mb );
	startItemSet = curItemSet = pg_createItemSet( pg, pg_sideTableEntry( pg, gr_goal(pg->gr) )->leftmostItems );
	pg_closeItemSet( pg, curItemSet->items );
	st_count( st ); // just to use the variable and silence a warning
	while( curItemSet )
		{
		int i; ItemSet stopItemSet;
		BitVector itemsLeft = bv_newInMB( itemCount, mb );

		bv_copy( itemsLeft, curItemSet->items );
		trace( stdout, "  Expanding ItemSet %p\n    stateNode: %s_%p\n    items left: ", curItemSet, sy_name( ob_tag( curItemSet->stateNode, pg->heap ), st ), curItemSet->stateNode );
		traceBV( itemsLeft, stdout );
		trace( stdout, "\n" );

		bv_minus( itemsLeft, pg->rightmostItems );
		traceBVX( itemsLeft, stdout, "    minus rightmost: %d", ", %d" );
		trace( stdout, "\n" );

		assert( !curItemSet->isExpanded );
		curItemSet->isExpanded = true;
		for( i = bv_firstBit( itemsLeft ); i != bv_END; i = bv_nextBit( itemsLeft, i ) )
			{
			Item it = ita_element( pg->items, i ); ItemSet nextItemSet;
			Symbol expected = pn_token( it->pn, it->dot, pg->gr );
			trace( stdout, "    Item %d is expecting %s\n", i, sy_name( expected, st ) );

			pg_computeItemsExpectingToken( pg, nextItems, itemsLeft, expected );
			traceBVX( nextItems, stdout, "      similar items: %d", ", %d" );
			trace( stdout, "\n" );

			bv_minus( itemsLeft, nextItems );
			traceBVX( itemsLeft, stdout, "          itemsLeft: %d", ", %d" );
			trace( stdout, "\n" );

			bv_shift( nextItems );
			traceBVX( nextItems, stdout, "            shifted: %d", ", %d" );
			trace( stdout, "\n" );

			pg_closeItemSet( pg, nextItems );
			traceBVX( nextItems, stdout, "             closed: %d", ", %d" );
			trace( stdout, "\n" );

			nextItemSet = pg_findItemSet( pg, nextItems );
			if( nextItemSet )
				{
				trace( stdout, "  Found existing ItemSet %p with items: ", nextItemSet );
				traceBV( nextItems, stdout );
				trace( stdout, "\n" );
				}
			else
				{
				// Use the nextItems bitvector we created, and allocate a new one for the next guy
				nextItemSet = pg_createItemSet( pg, nextItems );
				nextItems = bv_newInMB( itemCount, mb );
				trace( stdout, "    Created new itemSet %p\n", nextItems );
				}
			ob_setField( curItemSet->stateNode, expected, nextItemSet->stateNode, pg->heap );
			}
		stopItemSet = curItemSet;
		for( curItemSet = curItemSet->next; curItemSet != stopItemSet; curItemSet = curItemSet->next )
			if( !curItemSet->isExpanded )
				break;
		if( curItemSet->isExpanded )
			{
			curItemSet = NULL;
			trace( stdout, "All ItemSets are expanded\n" );
			}
		else
			{
			trace( stdout, "    Skipped to %p with items: ", curItemSet );
			traceBV( curItemSet->items, stdout );
			trace( stdout, "\n" );
			}
		}
	}

FUNC Parser ps_new( Grammar gr, SymbolTable st )
	{
	MemoryBatch     mb = mb_new( 10000 );
	ParserGenerator pg = pg_new( gr, st, mb, theObjectHeap() ); // TODO: allocate objects in mb
	// TODO: generate the parser
	pg_populateItemTable( pg );
	pg_populateSymbolSideTable( pg );
	pg_computeAutomaton( pg );
	mb_free( mb );
	return pg? NULL: NULL;
	}

#ifdef PARSER_T

static char *grammar1[][10] =
	{
	{ "S", "E" },
	{ "E", "E", "*", "B" },
	{ "E", "E", "+", "B" },
	{ "E", "B" },
	{ "B", "0" },
	{ "B", "1" },
	};

int main( int argc, char *argv[] )
	{
	int i, j; SymbolTable st; Symbol goal; Grammar gr; ParserGenerator pg;

	st = theSymbolTable();
	goal = sy_byName( grammar1[0][0], st );
	gr = gr_new( goal, asizeof( grammar1 ) );
	for( i=0; i < asizeof( grammar1 ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( grammar1[i][0], st ), 10 );
		for( j=1; grammar1[i][j]; j++ )
			pn_append( pn, sy_byName( grammar1[i][j], st ), gr );
		}
	gr_sendTo( gr, stdout, st );

	pg = pg_new( gr, st, mb_new( 10000 ), theObjectHeap() );

	pg_populateItemTable( pg );
	fl_write( stdout, "Items:\n" );
	for( i=0; i < ita_count( pg->items ); i++ )
		{
		Item it = ita_element( pg->items, i );
		fl_write( stdout, "  %3d: ", i );
		pn_sendItemTo( it->pn, it->dot, stdout, gr, st );
		fl_write( stdout, "\n" );
		}
	fl_write( stdout, "  rightmostItems: " );
	bv_sendTo( pg->rightmostItems, stdout );
	fl_write( stdout, "\n" );

	pg_populateSymbolSideTable( pg );
	fl_write( stdout, "SymbolSideTable:\n" );
	for( i=1; i < sst_count( pg->sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		int symbolIndex = sy_index( sste->sy, st );
		if( pg->sstIndexes[ symbolIndex ] == i )
			{
			fl_write( stdout, "  %3d: %s\n", i, sy_name( sste->sy, st ) );
			if( sste->leftmostItems )
				{
				fl_write( stdout, "     leftmostItems: " );
				bv_sendTo( sste->leftmostItems, stdout );
				fl_write( stdout, "\n" );
				}
			if( sste->expectingItems )
				{
				fl_write( stdout, "    expectingItems: " );
				bv_sendTo( sste->expectingItems, stdout );
				fl_write( stdout, "\n" );
				}
			}
		else
			{
			fl_write( stdout, "INDEX MISMATCH: entry %d is symbol %s. index %d, which has entry %d\n",
				i, sste->sy, symbolIndex, pg->sstIndexes[ symbolIndex ] );
			}
		}

	pg_computeAutomaton( pg );
	ob_sendDeepTo( pg->itemSetList->stateNode, stdout, pg->heap );

	return 0;
	}

#endif


// MERGE: 55

