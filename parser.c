
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include "stack.h"
#include <string.h>
#include <stdlib.h>

#define ITEM_SET_NUMS

typedef BitVector ItemVector;   // BitVectors of item indexes
typedef BitVector SymbolVector; // BitVectors of symbol side-table indexes

#if 1
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
	SymbolVector lookahead;  // May not be the optimal lookahead (ie. for LALR)
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
	ItemVector leftmostItems;  // items with pn_lhs( pn ) == sy && dot == 0
	ItemVector expectingItems; // items with dot immediately before sy
	SymbolVector first;
	SymbolVector follow;
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
	ItemVector items;
	Object     stateNode;
	int       *edgePriorities; // 1 + the nest depth of the production that wants an outgoing edge for each symbol.  Lower = more important
	};

static bool its_isExpanded( ItemSet its )
	{
	return its->edgePriorities != NULL;
	}

typedef struct itst_struct *ItemSetTable;
#define AR_PREFIX  itst
#define AR_TYPE    ItemSetTable
#define AR_ELEMENT struct its_struct
#undef AR_BYVALUE
#include "array_template.h"

typedef struct pg_struct
	{
	Grammar          gr;
	MemoryLifetime   generateTime;
	MemoryLifetime   parseTime;
	ObjectHeap       heap;
	ItemArray        items;
	ItemVector       rightmostItems;
	SymbolTable      st;
	SymbolSideTable  sst;
	int             *sstIndexes;
	SymbolVector     nullableSymbols;
	ItemSetTable     itemSets;
	} *ParserGenerator;

static int pg_symbolSideTableIndex( ParserGenerator pg, Symbol sy )
	{
	return pg->sstIndexes[ sy_index( sy, pg->st ) ];
	}

static SymbolSideTableEntry pg_sideTableEntry( ParserGenerator pg, Symbol sy, File traceFile )
	{
	SymbolSideTableEntry result;
	int sstIndex = pg_symbolSideTableIndex( pg, sy );
	if( sstIndex )
		result = sst_element( pg->sst, sstIndex );
	else
		{
		sstIndex = sst_count( pg->sst );
		fl_write( traceFile, "  -- s%d = %s --\n", sstIndex, sy_name( sy, pg->st ) );
		pg->sstIndexes[ sy_index( sy, pg->st ) ] = sstIndex;
		result = sst_nextElement( pg->sst );
		result->sy = sy;
		result->leftmostItems  = NULL;
		result->expectingItems = NULL;
		}
	assert( result->sy == sy );
	return result;
	}

static int its_index( ItemSet its, ParserGenerator pg )
	{
	return its - itst_element( pg->itemSets, 0 );
	}

static bool pg_itemIsRightmost( ParserGenerator pg, int itemIndex )
	{
	return bv_isSet( pg->rightmostItems, itemIndex );
	}

static void pg_closeItemVector( ParserGenerator pg, ItemVector itemVector, File traceFile )
	{
	int i;
	int oldPopulation = 0;
	int curPopulation = bv_population( itemVector );
	while( curPopulation > oldPopulation )
		{
		oldPopulation = curPopulation;
		for( i = bv_firstBit( itemVector ); i != bv_END; i = bv_nextBit( itemVector, i ) )
			{
			Item it = ita_element( pg->items, i );
			Production pn = it->pn;
			if( !pg_itemIsRightmost( pg, i ) )
				{
				Symbol nextToken = pn_token( pn, it->dot, pg->gr );
				SymbolSideTableEntry lhs = pg_sideTableEntry( pg, nextToken, traceFile );
				ItemVector itemsToAdd = lhs->leftmostItems;
				if( itemsToAdd )
					bv_or( itemVector, itemsToAdd );
				}
			}
		curPopulation = bv_population( itemVector );
		}
	}

static void pg_computeItemsExpectingToken( ParserGenerator pg, ItemVector result, ItemVector itemSet, Symbol token, File traceFile )
	{
	ItemVector expectingItems = pg_sideTableEntry( pg, token, traceFile )->expectingItems;
	if( !expectingItems )
		{
		bv_clear( result );
		return;
		}
	bv_copy( result, itemSet );
	bv_and( result, expectingItems );
	}

static ItemSet pg_findItemSet( ParserGenerator pg, ItemVector items )
	{
	int i;
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		if( bv_equals( its->items, items ) )
			return its;
		}
	return NULL;
	}

static ItemSet pg_createItemSet( ParserGenerator pg, ItemVector items )
	{
	ItemSet result = itst_nextElement( pg->itemSets );
	result->items = items;
	result->stateNode = ob_create( sy_byIndex( SYM_STATE_NODE, pg->st ), pg->heap );
#ifdef ITEM_SET_NUMS
	ob_setField(
		result->stateNode,
		sy_byIndex( SYM_ITEM_SET_NUM, pg->st ),
		ob_fromInt( its_index( result, pg ), pg->heap ),
		pg->heap );
#endif
	result->edgePriorities = NULL;
	return result;
	}

#if 0
static ItemSet pg_findOrCreateItemSet( ParserGenerator pg, ItemVector items )
	{
	ItemSet result = pg_findItemSet( pg, items );
	if( result )
		return result;
	else
		return pg_createItemSet( pg, items );
	}
#endif

static ParserGenerator pg_new( Grammar gr, SymbolTable st, MemoryLifetime generateTime, MemoryLifetime parseTime, ObjectHeap heap )
	{
	ParserGenerator pg = (ParserGenerator)ml_alloc( generateTime, sizeof(*pg) );
	pg->gr = gr;
	pg->st = st;
	pg->generateTime = generateTime;
	pg->parseTime    = parseTime;
	pg->heap = heap;
	return pg;
	}

static void pg_populateItemTable( ParserGenerator pg, File traceFile )
	{
	int i,j;
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	pg->items = ita_new( 1000, ml );
	pg->rightmostItems = bv_new( gr_numProductions(gr), ml );
	fl_write( traceFile, "Populating item table\n" );
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		for( j=0; j <= pn_length( pn, gr ); j++ )
			{
			fl_write( traceFile, "  i%d: ", ita_count( pg->items ) );
			Item it = ita_nextElement( pg->items );
			it->pn  = pn;
			it->dot = j;
			pn_sendItemTo( it->pn, it->dot, traceFile, gr, pg->st );
			fl_write( traceFile, "\n" );
			}
		bv_set( pg->rightmostItems, ita_count(pg->items) - 1 );
		}
	ita_shrinkWrap( pg->items );
	 bv_shrinkWrap( pg->rightmostItems );
	}

static void pg_populateSymbolSideTable( ParserGenerator pg, File traceFile )
	{
	int i,j;
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	SymbolTable st = pg->st;
	int itemIndex;
	pg->sstIndexes = (int*)ml_allocZeros( ml, st_count(st) * sizeof(pg->sstIndexes[0]));
	pg->sst = sst_new( 100, ml );
	pg->nullableSymbols = bv_new( 100, ml );
	sst_incCount( pg->sst ); // sst index zero is used for "null" so skip that one
	itemIndex = 0;
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		Symbol lhs = pn_lhs( pn, gr );
		SymbolSideTableEntry entry = pg_sideTableEntry( pg, lhs, traceFile );
		Item it = ita_element( pg->items, itemIndex );
		if( !entry->leftmostItems )
			entry->leftmostItems = bv_new( ita_count( pg->items ), ml );
		assert( it->pn == pn && it->dot == 0 );
		assert( pn_lhs( it->pn, gr ) == lhs );
		bv_set( entry->leftmostItems, itemIndex );
		for( j=0; j < pn_length( pn, gr ); j++ )
			{
			it = ita_element( pg->items, itemIndex );
			entry = pg_sideTableEntry( pg, pn_token( it->pn, j, gr ), traceFile );
			if( !entry->expectingItems )
				entry->expectingItems = bv_new( ita_count( pg->items ), ml );
			bv_set( entry->expectingItems, itemIndex );
			itemIndex++;
			}
		if( j==0 )
			bv_set( pg->nullableSymbols, pg_symbolSideTableIndex( pg, lhs ) );
		itemIndex++; // Account for the item where j == pn_length
		}
	assert( itemIndex == gr_numItems( pg->gr ) );
	sst_shrinkWrap( pg->sst );
	bv_shrinkWrap( pg->nullableSymbols );

	// Some more initialization now that we know how many side table entries there are
	int numSymbols = sst_count( pg->sst );
	for( i=0; i < numSymbols; i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		sste->first     = bv_new( numSymbols, pg->generateTime );
		sste->follow    = bv_new( numSymbols, pg->generateTime );
		bv_set( sste->first, i );
		}
	}

static Object pg_computeLR0StateNodes( ParserGenerator pg, File traceFile )
	{
	trace( traceFile, "Computing LR0 state nodes\n" );
	MemoryLifetime ml = pg->generateTime;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = gr_numItems( pg->gr );
	ItemVector nextItems = bv_new( itemCount, ml );
	pg->itemSets = itst_new( itemCount * itemCount, ml ); // guesstimate of number of item sets
	startItemSet = curItemSet = pg_createItemSet( pg, pg_sideTableEntry( pg, gr_goal(pg->gr), traceFile )->leftmostItems );
	Object startState = startItemSet->stateNode;
	pg_closeItemVector( pg, curItemSet->items, traceFile );
	st_count( st ); // just to use the variable and silence a warning
	while( curItemSet )
		{
		int i;
		ItemVector itemsLeft = bv_new( itemCount, ml );

		bv_copy( itemsLeft, curItemSet->items );
		trace( traceFile, "  Expanding ItemSet_%d\n    stateNode: %s_%p\n    items left: ",
			its_index( curItemSet, pg ), sy_name( ob_tag( curItemSet->stateNode, pg->heap ), st ), curItemSet->stateNode );
		traceBVX( itemsLeft, traceFile, "i%d", ", i%d" );
		trace( traceFile, "\n" );

		bv_minus( itemsLeft, pg->rightmostItems );
		trace( traceFile, "    minus rightmost: " );
		traceBVX( itemsLeft, traceFile, "i%d", ", i%d" );
		trace( traceFile, "\n" );

		assert( !its_isExpanded( curItemSet ) );
		int size = sst_count(pg->sst) * sizeof(curItemSet->edgePriorities[0]);
		curItemSet->edgePriorities = (int*)ml_allocZeros( pg->generateTime, size );
		assert( its_isExpanded( curItemSet ) );
		for( i = bv_firstBit( itemsLeft ); i != bv_END; i = bv_nextBit( itemsLeft, i ) )
			{
			Item it = ita_element( pg->items, i ); ItemSet nextItemSet;
			Symbol expected = pn_token( it->pn, it->dot, pg->gr );
			trace( traceFile, "    i%d is expecting %s\n", i, sy_name( expected, st ) );

			pg_computeItemsExpectingToken( pg, nextItems, itemsLeft, expected, traceFile );
			trace( traceFile, "      similar items: " );
			traceBVX( nextItems, traceFile, "i%d", ", i%d" );
			trace( traceFile, "\n" );

			Item shallowestItem = ita_element( pg->items, bv_lastBit( nextItems ) );
			int highestPriority = 1 + pn_nestDepth( shallowestItem->pn, pg->gr );
			int ssti = pg_symbolSideTableIndex( pg, expected );
			curItemSet->edgePriorities[ ssti ] = highestPriority;
			trace( traceFile, "      edgePriorities[ %d ] = %d\n", ssti, highestPriority );

			bv_minus( itemsLeft, nextItems );
			trace( traceFile, "          itemsLeft: " );
			traceBVX( itemsLeft, traceFile, "i%d", ", i%d" );
			trace( traceFile, "\n" );

			bv_shift( nextItems );
			trace( traceFile, "            shifted: " );
			traceBVX( nextItems, traceFile, "i%d", ", i%d" );
			trace( traceFile, "\n" );

			pg_closeItemVector( pg, nextItems, traceFile );
			trace( traceFile, "             closed: " );
			traceBVX( nextItems, traceFile, "i%d", ", i%d" );
			trace( traceFile, "\n" );

			nextItemSet = pg_findItemSet( pg, nextItems );
			if( nextItemSet )
				{
				trace( traceFile, "      Found existing ItemSet_%d with items: ", its_index( nextItemSet, pg ) );
				traceBVX( nextItems, traceFile, "i%d", ", i%d" );
				trace( traceFile, "\n" );
				}
			else
				{
				// Use the nextItems bitvector we created, and allocate a new one for the next guy
				nextItemSet = pg_createItemSet( pg, nextItems );
				nextItems = bv_new( itemCount, ml );
				trace( traceFile, "      Created ItemSet_%d\n", its_index( nextItemSet, pg ) );
				}
			ob_setField( curItemSet->stateNode, expected, nextItemSet->stateNode, pg->heap );
			}
		for( i = 1+its_index( curItemSet, pg ); i < itst_count( pg->itemSets ); i++ )
			{
			curItemSet = itst_element( pg->itemSets, i );
			if( its_isExpanded( curItemSet ) )
				trace( traceFile, "    ItemSet_%d already expanded\n", its_index( curItemSet, pg ) );
			else
				break;
			}
		if( its_isExpanded( curItemSet ) )
			{
			curItemSet = NULL;
			trace( traceFile, "All ItemSets are expanded\n" );
			}
		}
	return startState;
	}

static void pg_computeFirstSets( ParserGenerator pg, File traceFile )
	{
	int pnNum;
	bool somethingChanged = true;
	while( somethingChanged )
		{
		somethingChanged = false;
		for( pnNum=0; pnNum < gr_numProductions( pg->gr ); pnNum++ )
			{
			Production pn = gr_production( pg->gr, pnNum );
			int lhsIndex  = pg_symbolSideTableIndex( pg, pn_lhs( pn, pg->gr ) );
			SymbolSideTableEntry lhs = sst_element( pg->sst, lhsIndex );
			int i;
			for( i = 0; i < pn_length( pn, pg->gr ); i++ )
				{
				int tokIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i, pg->gr ) );
				SymbolVector tokFirst = sst_element( pg->sst, tokIndex )->first;
				somethingChanged |= bv_orChanged( lhs->first, tokFirst );
				if( !bv_isSet( pg->nullableSymbols, tokIndex ) )
					break; // This token is not nullable so subsequent tokens don't belong in the first set
				}
			}
		}
	}

static void pg_computeFollowSets( ParserGenerator pg, File traceFile )
	{
	int pnNum;
	bool somethingChanged = true;
	fl_write( traceFile, "Computing follow sets\n" );
	while( somethingChanged )
		{
		somethingChanged = false;
		fl_write( traceFile, "  Looping through productions\n" );
		for( pnNum=0; pnNum < gr_numProductions( pg->gr ); pnNum++ )
			{
			Production pn = gr_production( pg->gr, pnNum );
			fl_write( traceFile, "    #%d: ", pnNum );
			pn_sendTo( pn, traceFile, pg->gr, pg->st );
			fl_write( traceFile, "\n" );
			int lhsIndex  = pg_symbolSideTableIndex( pg, pn_lhs( pn, pg->gr ) );
			SymbolSideTableEntry lhs = sst_element( pg->sst, lhsIndex );
			int i = pn_length( pn, pg->gr ) - 1;
			// Last token has no successor so treat it specially
			if( i >= 0 )
				{
				int lastIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i, pg->gr ) );
				SymbolSideTableEntry last = sst_element( pg->sst, lastIndex );
				somethingChanged |= bv_orChanged( last->follow, lhs->follow );
				if( traceFile )
					{
					fl_write( traceFile, "      Copied follow from lhs " );
					sy_sendTo( lhs->sy, traceFile, pg->st );
					fl_write( traceFile, " to " );
					sy_sendTo( last->sy, traceFile, pg->st );
					fl_write( traceFile, ": " );
					bv_sendFormattedTo( lhs->follow, traceFile, "s%d", ", s%d" );
					fl_write( traceFile, "\n" );
					}
				}
			// Loop through the other tokens right-to-left propagating follow info across nullable tokens
			for( i--; i >= 0; i-- )
				{
				int curIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i, pg->gr ) );
				SymbolSideTableEntry cur = sst_element( pg->sst, curIndex );
				int nextIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i+1, pg->gr ) );
				SymbolSideTableEntry next = sst_element( pg->sst, nextIndex );
				somethingChanged |= bv_orChanged( cur->follow, next->first );
				if( traceFile )
					{
					fl_write( traceFile, "      Copied first from " );
					sy_sendTo( next->sy, traceFile, pg->st );
					fl_write( traceFile, " to follow of " );
					sy_sendTo( cur->sy, traceFile, pg->st );
					fl_write( traceFile, ": " );
					bv_sendFormattedTo( next->first, traceFile, "s%d", ", s%d" );
					fl_write( traceFile, "\n" );
					}
				if( bv_isSet( pg->nullableSymbols, nextIndex ) )
					{
					somethingChanged |= bv_orChanged( cur->follow, next->follow );
					if( traceFile )
						{
						fl_write( traceFile, "      Copied follow from " );
						sy_sendTo( next->sy, traceFile, pg->st );
						fl_write( traceFile, " to " );
						sy_sendTo( cur->sy, traceFile, pg->st );
						fl_write( traceFile, ": " );
						bv_sendFormattedTo( next->follow, traceFile, "s%d", ", s%d" );
						fl_write( traceFile, "\n" );
						}
					}
				}
			}
		}
	}

static void pg_computeSLRLookaheads( ParserGenerator pg, File traceFile )
	{
	int i;
	int numItems = ita_count( pg->items );
	Item *items = (Item*)ml_alloc( pg->generateTime, numItems * sizeof(*items) );
	for( i=0; i < numItems; i++ )
		{
		Item it = ita_element( pg->items, i );
		items[i] = it;
		SymbolSideTableEntry lhs = pg_sideTableEntry( pg, pn_lhs( it->pn, pg->gr ), traceFile );
		it->lookahead = bv_new( sst_count( pg->sst ), pg->generateTime );
		bv_copy( it->lookahead, lhs->follow );
		}
	}

#if 0
static void pg_adjustLookaheadsForNesting( ParserGenerator pg, File traceFile )
	{
	int i;
	int numItems = ita_count( pg->items );
	int numSymbols = sst_count( pg->sst );

	// Account for nesting by masking off conflicting lookaheads at lower depths
	// Items are naturally in descending order of depth so we just need to
	// process them in reverse order.
	//
	SymbolVector overridden = bv_new( numSymbols, pg->generateTime );
	SymbolVector willBeOverridden = bv_new( numSymbols, pg->generateTime );
	int prevDepth = -1;
	for( i=numItems-1; i >= 0; i-- )
		{
		Item it = ita_element( pg->items, i );
		int depth = pn_nestDepth( it->pn, pg->gr );
		if( depth > prevDepth )
			{
			bv_or( overridden, willBeOverridden );
			bv_clear( willBeOverridden );
			prevDepth = depth;
			}
		bv_minus( it->lookahead, overridden );
		bv_or( willBeOverridden, it->lookahead );
		}
	}
#endif

#if 0
static void its_getItemsActivatedBy( ItemSet its, int token, ItemVector result, ParserGenerator pg )
	{
	int i;
	bv_copy ( result, sst_element( pg->sst, token )->expectingItems );
	bv_and  ( result, its->items );
	for( i = bv_firstBit( its->items ); i != bv_END; i = bv_nextBit( its->items, i ) )
		{
		Item it = ita_element( pg->items, i );
		if( pg_itemIsRightmost( pg, i ) )
			if( bv_isSet( it->lookahead, token ) )
				bv_set( result, i );
		}
	}
#endif

static void it_getFollow( Item it, SymbolVector result, ParserGenerator pg, File traceFile )
	{
	// TODO: There's probably an efficient way to pre-compute these
	int i;
	fl_write( traceFile, "          Computing follow for: " );
	pn_sendItemTo( it->pn, it->dot, traceFile, pg->gr, pg->st );
	fl_write( traceFile, "\n" );
	for( i = it->dot; i < pn_length( it->pn, pg->gr ); i++ )
		{
		int curIndex = pg_symbolSideTableIndex( pg, pn_token( it->pn, i, pg->gr ) );
		SymbolVector curFirst = sst_element( pg->sst, curIndex )->first;
		bv_or( result, curFirst );
		fl_write( traceFile, "            Symbol %s at %d adds ", sy_name( sst_element( pg->sst, curIndex )->sy, pg->st ), i );
		bv_sendFormattedTo( curFirst, traceFile, "s%d", ", s%d" );
		fl_write( traceFile, "\n" );
		if( !bv_isSet( pg->nullableSymbols, curIndex ) )
			break;
		}
	if( i == pn_length( it->pn, pg->gr ) )
		{
		bv_or( result, it->lookahead );
		fl_write( traceFile, "            Adding lookahead " );
		bv_sendFormattedTo( it->lookahead, traceFile, "s%d", ", s%d" );
		fl_write( traceFile, "\n" );
		}
	}

static void pg_reportConflict( ParserGenerator pg, ItemSet its, Item winner, Item loser, SymbolVector conflictingSymbols, File conflictLog, char *format, ... )
	{
	// TODO: Report the symbol
	va_list args;
	va_start( args, format );
	fl_write(  conflictLog, "Conflict in ItemSet_%d: ", its_index( its, pg ) );
	fl_vwrite( conflictLog, format, args );
	fl_write(  conflictLog, "\n  Winner: " );
	pn_sendItemTo( winner->pn, winner->dot, conflictLog, pg->gr, pg->st );
	fl_write(  conflictLog, "\n   Loser: " );
	pn_sendItemTo( loser->pn, loser->dot, conflictLog, pg->gr, pg->st );
	fl_write(  conflictLog, "\n Symbols: " );
	bv_sendFormattedTo( conflictingSymbols, conflictLog, "s%d", ", s%d" );
	fl_write(  conflictLog, "\n" );
	va_end( args );
	}

static bool resolveConflict( Item left, ConflictResolutions leftCR, Item right, ConflictResolutions rightCR, ParserGenerator pg )
	{
	Grammar gr = pg->gr;
	return
		   pn_conflictResolution( left->pn,  gr ) == leftCR
		&& pn_conflictResolution( right->pn, gr ) == rightCR;
	}

static bool both( Item left, Item right, ConflictResolutions cr, ParserGenerator pg )
	{
	return resolveConflict( left, cr, right, cr, pg );
	}

static void pg_computeReduceActions( ParserGenerator pg, File conflictLog, File traceFile )
	{
	Grammar gr = pg->gr; ObjectHeap heap = pg->heap;
	int i,j,k;

	fl_write( traceFile, "Computing reduce actions\n" );
	ItemVector   reduceItems        = bv_new( ita_count(pg->items), pg->generateTime );
	SymbolVector reduceSymbols      = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector competitorSymbols  = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector conflictingSymbols = bv_new( sst_count(pg->sst),   pg->generateTime );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		fl_write( traceFile, "  ItemSet_%d:\n", i );
		ItemSet its = itst_element( pg->itemSets, i );
		Object stateNode = its->stateNode;
		bv_copy ( reduceItems, pg->rightmostItems );
		bv_and  ( reduceItems, its->items );
		fl_write( traceFile, "    Items: " );
		bv_sendFormattedTo( its->items, traceFile, "i%d", ", i%d" );
		fl_write( traceFile, "\n" );
		fl_write( traceFile, "    ReduceItems: " );
		bv_sendFormattedTo( reduceItems, traceFile, "i%d", ", i%d" );
		fl_write( traceFile, "\n" );
		for( j = bv_firstBit( reduceItems ); j != bv_END; j = bv_nextBit( reduceItems, j ) )
			{
			Item it = ita_element( pg->items, j );
			Production pn = it->pn;
			fl_write( traceFile, "    Item i%d: ", j );
			pn_sendItemTo( pn, it->dot, traceFile, pg->gr, pg->st );
			fl_write( traceFile, "\n" );
			bv_copy( reduceSymbols, it->lookahead );
			fl_write( traceFile, "      Original ReduceSymbols: " );
			bv_sendFormattedTo( reduceSymbols, traceFile, "s%d", ", s%d" );
			fl_write( traceFile, "\n" );

			// Scan down for the last item of lower priority
			//
			int lastLowerItem;
			for( lastLowerItem = bv_prevBit( its->items, j); lastLowerItem != bv_END; lastLowerItem = bv_prevBit( its->items, lastLowerItem ) )
				{
				Item x = ita_element( pg->items, lastLowerItem );
				if( pn_nestDepth( x->pn, gr ) < pn_nestDepth( pn, gr ) )
					break;
				}

			// Filter out reduceSymbols covered by higher-priority items,
			// and equal-priority items that override this reduce.
			//
			int startItem = ( lastLowerItem == bv_END )? bv_firstBit( its->items ) : bv_nextBit( its->items, lastLowerItem );
			for( k = startItem; k != bv_END; k = bv_nextBit( its->items, k ) )
				{
				Item competitor = ita_element( pg->items, k );
				if( competitor == it )
					continue;
				bv_clear( competitorSymbols );
				if( pg_itemIsRightmost( pg, k ) )
					{
					fl_write( traceFile, "      Checking reduce item i%d: ", k );
					pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
					fl_write( traceFile, "\n        follow: " );
					it_getFollow( competitor, competitorSymbols, pg, traceFile );
					bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
					fl_write( traceFile, "\n" );
					}
				else
					{
					fl_write( traceFile, "      Checking shift item i%d: ", k );
					pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
					fl_write( traceFile, "\n        first: " );
					SymbolSideTableEntry expected = pg_sideTableEntry( pg, pn_token( competitor->pn, competitor->dot, pg->gr ), traceFile );
					bv_or( competitorSymbols, expected->first );
					bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
					fl_write( traceFile, "\n" );
					}
				if( !bv_intersects( reduceSymbols, competitorSymbols ) )
					{
					fl_write( traceFile, "        No conflict\n" );
					continue;
					}
				int winningMargin = pn_nestDepth( competitor->pn, pg->gr ) - pn_nestDepth( it->pn, pg->gr );
				if( winningMargin > 0 )
					{
					fl_write( traceFile, "        Competitor has lower priority\n" );
					continue;
					}
				else if( winningMargin == 0 )
					{
					bv_copy( conflictingSymbols, reduceSymbols );
					bv_and ( conflictingSymbols, competitorSymbols );
					if( pg_itemIsRightmost( pg, k ) )
						{
						fl_write( traceFile, "        CONFLICT\n" );
						pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, "Reduce-reduce" );
						continue;
						}
					else
						{
						if( both( it, competitor, CR_SHIFT_BEATS_REDUCE, pg ) )
							{
							fl_write( traceFile, "        CR_SHIFT_BEATS_REDUCE\n" );
							}
						else if( both( it, competitor, CR_REDUCE_BEATS_SHIFT, pg ) )
							{
							fl_write( traceFile, "        CR_REDUCE_BEATS_SHIFT\n" );
							continue;
							}
						else if( it->pn == competitor->pn )
							{
							fl_write( traceFile, "        Self-left-associativity favours reduce\n" );
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, "Self shift-reduce" );
							continue;
							}
						else
							{
							fl_write( traceFile, "        Favouring reduce over shift until I figure out something better\n" );
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, "Shift-reduce" );
							continue;
							}
						}
					}

				bv_minus( reduceSymbols, competitorSymbols );
				fl_write( traceFile, "        Competitor has higher priority; removing lookaheads: " );
				bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
				fl_write( traceFile, "\n" );
				}
			fl_write( traceFile, "      Filtered ReduceSymbols: " );
			bv_sendFormattedTo( reduceSymbols, traceFile, "s%d", ", s%d" );
			fl_write( traceFile, "\n" );

			// Add reduce edges
			//
			for( k = bv_firstBit( reduceSymbols ); k != bv_END; k = bv_nextBit( reduceSymbols, k ) )
				{
				SymbolSideTableEntry reduceOn = sst_element( pg->sst, k );
				ob_setField( stateNode, reduceOn->sy, ob_fromInt( pn_index( pn, gr ), heap ), heap );
				}
			}
		}

	}

struct ps_struct
	{
	Grammar gr;
	Stack stateStack;
	ObjectHeap stateHeap;
	};

FUNC Parser ps_new( Grammar gr, SymbolTable st, MemoryLifetime ml, File conflictLog, File traceLog )
	{
	MemoryLifetime generateTime = ml_begin( 10000, ml );
	ParserGenerator pg = pg_new( gr, st, generateTime, ml, theObjectHeap() );
	pg_populateItemTable( pg, traceLog );
	pg_populateSymbolSideTable( pg, traceLog );
	Object startState = pg_computeLR0StateNodes( pg, traceLog );
	pg_computeFirstSets( pg, traceLog );
	pg_computeFollowSets( pg, traceLog );
	pg_computeSLRLookaheads( pg, traceLog );
	pg_computeReduceActions( pg, conflictLog, traceLog );
	ml_end( generateTime );

	Parser result = (Parser)ml_alloc( ml, sizeof(*result) );
	result->gr = gr;
	result->stateStack = sk_new( ml );
	result->stateHeap  = theObjectHeap();
	sk_push( result->stateStack, startState );
	return result;
	}

FUNC Grammar ps_grammar( Parser ps )
	{
	return ps->gr;
	}

static Object ps_nextState( Parser ps, Object ob )
	{
	ObjectHeap oh = ps->stateHeap;
	Object curState = sk_top( ps->stateStack );
	Symbol token = ob_tag( ob, oh );
	// Not sure how I'm dealing with tokens as first-class objects yet...
	if( ob_isToken( ob, oh ) )
		{
		Symbol literalToken = ob_toSymbol( ob, oh );
		if( ob_hasField( curState, literalToken, oh ) )
			token = literalToken;
		}
	if( ob_hasField( curState, token, oh ) )
		return ob_getField( curState, token, oh );
	else
		return NULL;
	}

FUNC bool ps_expects( Parser ps, Object ob )
	{
	return ps_nextState( ps, ob ) != NULL;
	}

FUNC void ps_push( Parser ps, Object ob )
	{
	Object nextState = ps_nextState( ps, ob );
	check( nextState );
	sk_push( ps->stateStack, nextState );
	}

FUNC int ps_depth( Parser ps )
	{
	return sk_depth( ps->stateStack );
	}

FUNC Production ps_handle( Parser ps, Object lookahead )
	{
	ObjectHeap oh = ps->stateHeap;
	Object nextState = ps_nextState( ps, lookahead );
	if( ob_isInt( nextState, oh ) )
		return gr_production( ps->gr, ob_toInt( nextState, oh ) );
	else
		return NULL;
	}

FUNC void ps_popN( Parser ps, int count )
	{
	assert( sk_depth( ps->stateStack ) >= count+1 ); // must always leave the startState on the stack
	sk_popN( ps->stateStack, count );
	}

FUNC int ps_sendTo( Parser ps, File fl, ObjectHeap heap, SymbolTable st )
	{
	Object startState = sk_item( ps->stateStack, sk_depth( ps->stateStack )-1 );
	return ob_sendDeepTo( startState, fl, heap );
	}

FUNC int ps_sendStateTo( Parser ps, File fl, ObjectHeap heap, SymbolTable st )
	{
	int charsSent = 0;
#ifdef ITEM_SET_NUMS
	int i;
	char *sep = "";
	Symbol isn = sy_byIndex( SYM_ITEM_SET_NUM, st );
	for( i=0; i < sk_depth( ps->stateStack ); i++ )
		{
		charsSent += fl_write( fl, "%s%d", sep, ob_toInt( ob_getField( sk_item( ps->stateStack, i ), isn, heap ), heap ) );
		sep = " ";
		}
#endif
	return charsSent;
	}

#ifdef PARSER_T

enum
	{
	Shift,
	Reduce,
	Accept,

	FirstConflict,
	ShiftReduceConflict = FirstConflict,
	ReduceReduceConflict,
	} LR0StateKinds;

static char *LR0StateKindNames[] =
	{
	"Shift",
	"Reduce",
	"Accept",
	"Shift/Reduce conflict",
	"Reduce/Reduce conflict",
	};

static int its_LR0StateKind( ItemSet its, ParserGenerator pg )
	{
	int numShifts, numReduces; ItemVector items;

	items = bv_new( ita_count( pg->items ), pg->generateTime );
	bv_copy  ( items, its->items );
	bv_minus ( items, pg->rightmostItems );
	numShifts = bv_population( items );

	items = bv_new( ita_count( pg->items ), pg->generateTime );
	bv_copy  ( items, its->items );
	bv_and   ( items, pg->rightmostItems );
	numReduces = bv_population( items );

	switch( numReduces )
		{
		case 0:
			return Shift;
		case 1:
			if( numShifts == 0 )
				{
				Item it = ita_element( pg->items, bv_firstBit( items ) );
				if( pn_lhs( it->pn, pg->gr ) == gr_goal( pg->gr ) )
					return Accept;
				else
					return Reduce;
				}
			else
				return ShiftReduceConflict;
		default:
			return ReduceReduceConflict;
		}
	}

typedef char *GrammarLine[10];

#if 0
static GrammarLine grammar[] =
	{
	{ "S",  "E", ":END_OF_INPUT" },
	{ "E",  "E", "*", "B" },
	{ "E",  "E", "+", "B" },
	{ "E",  "B" },
	{ "B",  "0" },
	{ "B",  "1" },
	};
#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "S",  "E", ":END_OF_INPUT" },
	{ "E",  "E", "+", "T" },
	{ "E",  "E", "-", "T" },
	{ "E",  "T" },
	{ "T",  "T", "*", "F" },
	{ "T",  "T", "/", "F" },
	{ "T",  "F" },
	{ "F",  "0" },
	{ "F",  "1" },
	{ "F",  "2" },
	{ "F",  "3" },
	};

#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "PROGRAM",      "STATEMENTS", ":END_OF_INPUT" },

	{ "STATEMENTS",   "STATEMENT" },
	{ "STATEMENTS",   "STATEMENTS", "STATEMENT" },

	{ "STATEMENT",    ":INT" },

	{ ":INT",         "INFIX" },
	{ "INFIX",        "(", "INFIX", ")" },
	{ "INFIX",        "INFIX", "+", "TERM" },
	{ "INFIX",        "INFIX", "-", "TERM" },
	{ "INFIX",        "TERM" },
	{ "TERM",         "TERM", "*", "FACTOR" },
	{ "TERM",         "TERM", "/", "FACTOR" },
	{ "TERM",         "FACTOR" },
	//{ "FACTOR",       ":INT" }, // hmm, how do I allow a user to put any int expression in here?
	{ "FACTOR",  "0" },
	{ "FACTOR",  "1" },
	{ "FACTOR",  "2" },
	{ "FACTOR",  "3" },
	};

#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "PROGRAM",      "STATEMENTS", ":END_OF_INPUT" },

	{ "STATEMENTS",   "STATEMENT" },
	{ "STATEMENTS",   "STATEMENTS", "STATEMENT" },

	{ "STATEMENT",    ":INT" },

	{ ":INT",    ":INT", "+", ":INT" },
	{ ":INT",    ":INT", "-", ":INT" },
	{ ":INT",    "(", ":INT", ")" },

	{ ":INT",  "0" },
	{ ":INT",  "1" },
	{ ":INT",  "2" },
	{ ":INT",  "3" },
	};

static GrammarLine grammar2[] =
	{
	{ ":INT",    ":INT", "*", ":INT" },
	{ ":INT",    ":INT", "/", ":INT" },
	};
#endif

#if 1
static GrammarLine grammar[] =
	{
	{ ":PROGRAM", ":VOIDS", ":END_OF_INPUT"              },
	{ ":VOIDS",   ":VOID"                                },
	{ ":VOIDS",   ":VOIDS", ":VOID"                      },

	{ ":TOKEN_BLOCK",  ":TB_START", ":VOIDS", "}"        },
	{ ":TB_START",     "{",                              },

	{ ":VOID",          ":TOKEN_STREAM"                  },
	{ ":TOKEN_STREAM",  ":TOKEN_BLOCK"                   },

	{ ":VOID",    ":INT"                                 },
	{ ":VOID",    "def", ":TOKEN", ":TOKEN_BLOCK"        },
	{ ":VOID",    "print", ":INT"                        },

	{ ":INT",         ":INT", "+", ":INT" },
	{ ":INT",         ":INT", "-", ":INT" },
	};

static GrammarLine grammar2[] =
	{
	{ ":INT",         ":INT", "*", ":INT" },
	{ ":INT",         ":INT", "/", ":INT" },
	{ ":INT",         "(", ":INT", ")" },
	};
#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "S",  "E", ":END_OF_INPUT" },
	{ "E",  "E", "*", "B" },
	{ "E",  "E", "+", "B" },
	{ "E",  "B" },
	{ "B",  "0" },
	{ "B",  "1" },
	};
#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "program",    "statements", ":END_OF_INPUT" },
	{ "statements", "statements", "statement" },
	{ "statements" },

	{ "statement",  "print", ":INT" },
	{ "statement",  "print", ":STRING" },

	{ ":INT",       "add", ":INT", ":INT" },
	{ ":INT",       "sub", ":INT", ":INT" },

	{ ":INT",       "fib", ":INT" },

	{ ":INT",       "numWheels", "Truck" },
	{ ":INT",       "numCylinders", "Truck" },

	{ "Truck",      "FireTruck" },
	{ ":INT",       "numHoses", "FireTruck" },
	//{ ":INT",       "numWheels", "FireTruck" }, // If subclass overrides, we end up with an R/R conflict

	};
#endif

#if 0
static GrammarLine grammar[] = // http://www.scribd.com/doc/7185137/First-and-Follow-Set
	{
	{ "S", "a", "A", "B", "e" },
	{ "A", "A", "b", "c" },
	{ "A", "b" },
	{ "B", "d" },
	};
#endif

#if 0
static GrammarLine grammar[] = // LR0
	{
	{ "S", "E", "#" },
	{ "E", "E", "-", "T" },
	{ "E", "T" },
	{ "T", "n" },
	{ "T", "(", "E", ")" },
	};
#endif

static void dumpItemLookaheads( ParserGenerator pg, File traceFile )
	{
	int i;
	fl_write( traceFile, "Item lookaheads:\n" );
	for( i=0; i < ita_count( pg->items ); i++ )
		{
		Item it = ita_element( pg->items, i );
		fl_write( traceFile, "  %3d: ", i );
		pn_sendItemTo( it->pn, it->dot, traceFile, pg->gr, pg->st );
		fl_write( traceFile, "\n    lookahead: " );
		bv_sendFormattedTo( it->lookahead, traceFile, "s%d", ", s%d" );
		fl_write( traceFile, "\n" );
		}
	}

int main( int argc, char *argv[] )
	{
	int i, j; SymbolTable st; Symbol goal; Grammar gr; ParserGenerator pg;
	File traceFile = fdopen( 3, "wt" );
	File dotFile   = stdout;

	st = theSymbolTable();
	goal = sy_byName( grammar[0][0], st );
	gr = gr_new( goal, asizeof( grammar ), ml_indefinite() );
	for( i=0; i < asizeof( grammar ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( grammar[i][0], st ), 10 );
		for( j=1; grammar[i][j]; j++ )
			pn_append( pn, sy_byName( grammar[i][j], st ), gr );
		pn_stopAppending( pn, gr );
		}
	gr_stopAdding( gr );
	gr = gr_nested( gr, asizeof( grammar2 ), ml_indefinite() );
	for( i=0; i < asizeof( grammar2 ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( grammar2[i][0], st ), 10 );
		for( j=1; grammar2[i][j]; j++ )
			pn_append( pn, sy_byName( grammar2[i][j], st ), gr );
		pn_stopAppending( pn, gr );
		}
	gr_stopAdding( gr );
	gr_sendTo( gr, traceFile, st );

	pg = pg_new( gr, st, ml_begin( 10000, ml_indefinite() ), ml_indefinite(), theObjectHeap() );

	pg_populateItemTable( pg, traceFile );
	fl_write( traceFile, "Items:\n" );
	for( i=0; i < ita_count( pg->items ); i++ )
		{
		Item it = ita_element( pg->items, i );
		fl_write( traceFile, "  %3d: ", i );
		pn_sendItemTo( it->pn, it->dot, traceFile, gr, st );
		fl_write( traceFile, "\n" );
		}
	fl_write( traceFile, "  rightmostItems: " );
	bv_sendFormattedTo( pg->rightmostItems, traceFile, "i%d", ", i%d" );
	fl_write( traceFile, "\n" );

	pg_populateSymbolSideTable( pg, traceFile );
	fl_write( traceFile, "SymbolSideTable:\n" );
	for( i=1; i < sst_count( pg->sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		int symbolIndex = sy_index( sste->sy, st );
		if( pg->sstIndexes[ symbolIndex ] == i )
			{
			fl_write( traceFile, "  %3d: %s\n", i, sy_name( sste->sy, st ) );
			if( sste->leftmostItems )
				{
				fl_write( traceFile, "     leftmostItems: " );
				bv_sendFormattedTo( sste->leftmostItems, traceFile, "i%d", ", i%d" );
				fl_write( traceFile, "\n" );
				}
			if( sste->expectingItems )
				{
				fl_write( traceFile, "    expectingItems: " );
				bv_sendFormattedTo( sste->expectingItems, traceFile, "i%d", ", i%d" );
				fl_write( traceFile, "\n" );
				}
			}
		else
			{
			fl_write( traceFile, "INDEX MISMATCH: entry %d is symbol %s. index %d, which has entry %d\n",
				i, sste->sy, symbolIndex, pg->sstIndexes[ symbolIndex ] );
			}
		}

	pg_computeLR0StateNodes( pg, traceFile );

	pg_computeFirstSets( pg, traceFile );
	pg_computeFollowSets( pg, traceFile );
	fl_write( traceFile, "Symbol sets:\n" );
	for( i=1; i < sst_count( pg->sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		fl_write( traceFile, "  %3d: %s\n", i, sy_name( sste->sy, st ) );
		fl_write( traceFile, "     first: " );
		bv_sendFormattedTo( sste->first, traceFile, "s%d", ", s%d" );
		fl_write( traceFile, "\n" );
		fl_write( traceFile, "     follow: " );
		bv_sendFormattedTo( sste->follow, traceFile, "s%d", ", s%d" );
		fl_write( traceFile, "\n" );
		}

	pg_computeSLRLookaheads( pg, traceFile );
	dumpItemLookaheads( pg, traceFile );
	pg_computeReduceActions( pg, traceFile, traceFile );
	ob_sendDeepTo( itst_element( pg->itemSets, 0 )->stateNode, traceFile, pg->heap );

	fl_write( dotFile, "digraph \"G\" { overlap=false \n" );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		fl_write( dotFile, "n%p [label=\"%d %s\\n", its->stateNode, i, LR0StateKindNames[ its_LR0StateKind( its, pg ) ] );
		for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
			{
			Item it = ita_element( pg->items, j );
			pn_sendItemTo( it->pn, it->dot, dotFile, pg->gr, pg->st );
			fl_write( dotFile, "\\n" );
			}
		fl_write( dotFile, "\"]\n" );
		}
	ob_sendDotEdgesTo( itst_element( pg->itemSets, 0 )->stateNode, dotFile, pg->heap );
	fl_write( dotFile, "}\n" );

#if 0
	fl_write( traceFile, "Parsing...\n" );
	Parser ps = ps_new( gr, st, ml_indefinite(), traceFile );
	ObjectHeap heap = theObjectHeap();
	static char *sentence[] = { "1", "+", "2", "*", "2", "/", "2", "+", "3" };
	int stop = asizeof( sentence );
	for( i=0; i <= stop; i++ )
		{
#ifdef ITEM_SET_NUMS
		fl_write( traceFile, "State is %d\n", ob_toInt( ob_getField(
			sk_top( ps->stateStack ),
			sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ), heap ) );
#endif
		char *cur  = (i >= stop)?   ":END_OF_INPUT" : sentence[i];
		char *next = (i+1 >= stop)? ":END_OF_INPUT" : sentence[i+1];
		fl_write( traceFile, "  Token: %s\n", cur );
		Symbol sy = sy_byName( cur, st );
		ps_push( ps, oh_symbolToken( heap, sy ) );
#ifdef ITEM_SET_NUMS
		fl_write( traceFile, " -- new state is %d\n", ob_toInt( ob_getField(
			sk_top( ps->stateStack ),
			sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ), heap ) );
#endif
		Object lookahead = oh_symbolToken( heap, sy_byName( next, st ) );
		Production handle = ps_handle( ps, lookahead );
		while( handle )
			{
			fl_write( traceFile, "    Reduce: " );
			pn_sendTo( handle, traceFile, gr, st );
			fl_write( traceFile, "\n" );
			ps_popN( ps, pn_length( handle, gr ) );
			ps_push( ps, oh_symbolToken( heap, pn_lhs( handle, gr ) ) );
#ifdef ITEM_SET_NUMS
			fl_write( traceFile, " -- new state is %d\n", ob_toInt( ob_getField(
				sk_top( ps->stateStack ),
				sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ), heap ) );
#endif
			handle = ps_handle( ps, lookahead );
			}
		}
	fl_write( traceFile, "...done\n" );
#endif

	return 0;
	}

#endif


// MERGE: 55

