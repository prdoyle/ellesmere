
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include "stack.h"
#include "records.h"
#include <string.h>
#include <stdlib.h>

typedef BitVector ItemVector;   // BitVectors of item indexes
typedef BitVector SymbolVector; // BitVectors of symbol side-table indexes

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
#ifndef NDEBUG
	#define ita_new( size, ml ) ita_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

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
#ifndef NDEBUG
	#define sst_new( size, ml ) sst_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

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
#ifndef NDEBUG
	#define itst_new( size, ml ) itst_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

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
	Symbol           stateNodeTag;
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
		trace( traceFile, "  -- s%d = %s --\n", sstIndex, sy_name( sy, pg->st ) );
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

#ifdef REDUCE_CONTEXT_LENGTH
static int its_maxDot( ItemSet its, ParserGenerator pg )
	{
	int result = 0;
	int i;
	for( i = bv_firstBit( its->items ); i != bv_END; i = bv_nextBit( its->items, i ) )
		{
		Item it = ita_element( pg->items, i );
		if( it->dot > result )
			result = it->dot;
		}
	return result;
	}
#endif

static ItemSet pg_createItemSet( ParserGenerator pg, ItemVector items )
	{
	ItemSet result = itst_nextElement( pg->itemSets );
	result->items = items;
	result->stateNode = ob_create( pg->stateNodeTag, pg->heap );
#ifdef ITEM_SET_NUMS
	ob_setField(
		result->stateNode,
		sy_byIndex( SYM_ITEM_SET_NUM, pg->st ),
		ob_fromInt( its_index( result, pg ), pg->heap ),
		pg->heap );
#endif
#ifdef REDUCE_CONTEXT_LENGTH
	ob_setField(
		result->stateNode,
		sy_byIndex( SYM_REDUCE_CONTEXT_LENGTH, pg->st ),
		ob_fromInt( its_maxDot( result, pg ), pg->heap ),
		pg->heap );
#endif
	result->edgePriorities = NULL;
	return result;
	}

static ParserGenerator pg_new( Grammar gr, SymbolTable st, Symbol stateNodeTag, MemoryLifetime generateTime, MemoryLifetime parseTime, ObjectHeap heap )
	{
	ParserGenerator pg = (ParserGenerator)ml_alloc( generateTime, sizeof(*pg) );
	pg->gr = gr;
	pg->st = st;
	pg->generateTime = generateTime;
	pg->parseTime    = parseTime;
	pg->heap         = heap;
	pg->stateNodeTag = stateNodeTag;
	return pg;
	}

static void pg_populateItemTable( ParserGenerator pg, File traceFile )
	{
	int i,j;
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	pg->items = ita_new( 150, ml );
	pg->rightmostItems = bv_new( gr_numProductions(gr), ml );
	trace( traceFile, "Populating item table\n" );
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		for( j=0; j <= pn_length( pn, gr ); j++ )
			{
			trace( traceFile, "  i%d: ", ita_count( pg->items ) );
			Item it = ita_nextElement( pg->items );
			it->pn  = pn;
			it->dot = j;
			pn_sendItemTo( it->pn, it->dot, traceFile, gr, pg->st );
			trace( traceFile, "\n" );
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
	sst_incCount( pg->sst ); // sst index zero is used for "null" so skip that one
	pg->nullableSymbols = bv_new( 100, ml );
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
	BitVector stateNodeFields = bv_new( numSymbols, pg->generateTime );
	for( i=1; i < numSymbols; i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		sste->first     = bv_new( numSymbols, pg->generateTime );
		sste->follow    = bv_new( numSymbols, pg->generateTime );
		bv_set( sste->first, i );
		bv_set( stateNodeFields, sy_index( sste->sy, st ) );
		}
	sy_setInstanceShape( pg->stateNodeTag, rd_new( stateNodeFields, pg->parseTime ), st );
	}

static Object pg_computeLR0StateNodes( ParserGenerator pg, File traceFile )
	{
	trace( traceFile, "Computing LR0 state nodes\n" );
	MemoryLifetime ml = pg->generateTime;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = gr_numItems( pg->gr );
	ItemVector nextItems = bv_new( itemCount, ml );
	pg->itemSets = itst_new( itemCount, ml ); // guesstimate of number of item sets.  In practice it seems to be very roughly equal to the number of items
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
	trace( traceFile, "Computing follow sets\n" );
	while( somethingChanged )
		{
		somethingChanged = false;
		trace( traceFile, "  Looping through productions\n" );
		for( pnNum=0; pnNum < gr_numProductions( pg->gr ); pnNum++ )
			{
			Production pn = gr_production( pg->gr, pnNum );
			trace( traceFile, "    #%d: ", pnNum );
			pn_sendTo( pn, traceFile, pg->gr, pg->st );
			trace( traceFile, "\n" );
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
					trace( traceFile, "      Copied follow from lhs " );
					sy_sendTo( lhs->sy, traceFile, pg->st );
					trace( traceFile, " to " );
					sy_sendTo( last->sy, traceFile, pg->st );
					trace( traceFile, ": " );
					bv_sendFormattedTo( lhs->follow, traceFile, "s%d", ", s%d" );
					trace( traceFile, "\n" );
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
					trace( traceFile, "      Copied first from " );
					sy_sendTo( next->sy, traceFile, pg->st );
					trace( traceFile, " to follow of " );
					sy_sendTo( cur->sy, traceFile, pg->st );
					trace( traceFile, ": " );
					bv_sendFormattedTo( next->first, traceFile, "s%d", ", s%d" );
					trace( traceFile, "\n" );
					}
				if( bv_isSet( pg->nullableSymbols, nextIndex ) )
					{
					somethingChanged |= bv_orChanged( cur->follow, next->follow );
					if( traceFile )
						{
						trace( traceFile, "      Copied follow from " );
						sy_sendTo( next->sy, traceFile, pg->st );
						trace( traceFile, " to " );
						sy_sendTo( cur->sy, traceFile, pg->st );
						trace( traceFile, ": " );
						bv_sendFormattedTo( next->follow, traceFile, "s%d", ", s%d" );
						trace( traceFile, "\n" );
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
	trace( traceFile, "          Computing follow for: " );
	pn_sendItemTo( it->pn, it->dot, traceFile, pg->gr, pg->st );
	trace( traceFile, "\n" );
	for( i = it->dot; i < pn_length( it->pn, pg->gr ); i++ )
		{
		int curIndex = pg_symbolSideTableIndex( pg, pn_token( it->pn, i, pg->gr ) );
		SymbolVector curFirst = sst_element( pg->sst, curIndex )->first;
		bv_or( result, curFirst );
		trace( traceFile, "            Symbol %s at %d adds ", sy_name( sst_element( pg->sst, curIndex )->sy, pg->st ), i );
		bv_sendFormattedTo( curFirst, traceFile, "s%d", ", s%d" );
		trace( traceFile, "\n" );
		if( !bv_isSet( pg->nullableSymbols, curIndex ) )
			break;
		}
	if( i == pn_length( it->pn, pg->gr ) )
		{
		bv_or( result, it->lookahead );
		trace( traceFile, "            Adding lookahead " );
		bv_sendFormattedTo( it->lookahead, traceFile, "s%d", ", s%d" );
		trace( traceFile, "\n" );
		}
	}

static void pg_reportConflict( ParserGenerator pg, ItemSet its, Item winner, Item loser, SymbolVector conflictingSymbols, File conflictLog, File traceFile, char *format, ... )
	{
	// TODO: Report the symbol
	va_list args;
	va_start( args, format );
	trace(  conflictLog, "Conflict in ItemSet_%d: ", its_index( its, pg ) );
	fl_vwrite( conflictLog, format, args );
	trace(  conflictLog, "\n  Winner: " );
	pn_sendItemTo( winner->pn, winner->dot, conflictLog, pg->gr, pg->st );
	trace(  conflictLog, "\n   Loser: " );
	pn_sendItemTo( loser->pn, loser->dot, conflictLog, pg->gr, pg->st );
	trace(  conflictLog, "\n Symbols: " );
	bv_sendFormattedTo( conflictingSymbols, conflictLog, "s%d", ", s%d" );
	trace(  conflictLog, "\n" );
	va_end( args );
	trace( traceFile, "        CONFLICT\n" );
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

	trace( traceFile, "Computing reduce actions\n" );
	ItemVector   reduceItems        = bv_new( ita_count(pg->items), pg->generateTime );
	SymbolVector reduceSymbols      = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector competitorSymbols  = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector conflictingSymbols = bv_new( sst_count(pg->sst),   pg->generateTime );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		trace( traceFile, "  ItemSet_%d:\n", i );
		ItemSet its = itst_element( pg->itemSets, i );
		Object stateNode = its->stateNode;
		bv_copy ( reduceItems, pg->rightmostItems );
		bv_and  ( reduceItems, its->items );
		trace( traceFile, "    Items: " );
		bv_sendFormattedTo( its->items, traceFile, "i%d", ", i%d" );
		trace( traceFile, "\n" );
		trace( traceFile, "    ReduceItems: " );
		bv_sendFormattedTo( reduceItems, traceFile, "i%d", ", i%d" );
		trace( traceFile, "\n" );
		for( j = bv_firstBit( reduceItems ); j != bv_END; j = bv_nextBit( reduceItems, j ) )
			{
			Item it = ita_element( pg->items, j );
			Production pn = it->pn;
			trace( traceFile, "    Item i%d: ", j );
			pn_sendItemTo( pn, it->dot, traceFile, pg->gr, pg->st );
			trace( traceFile, "\n" );
			bv_copy( reduceSymbols, it->lookahead );
			trace( traceFile, "      Original ReduceSymbols: " );
			bv_sendFormattedTo( reduceSymbols, traceFile, "s%d", ", s%d" );
			trace( traceFile, "\n" );

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
					trace( traceFile, "      Checking reduce item i%d: ", k );
					pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
					trace( traceFile, "\n        follow: " );
					it_getFollow( competitor, competitorSymbols, pg, NULL );
					bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
					trace( traceFile, "\n" );
					}
				else
					{
					trace( traceFile, "      Checking shift item i%d: ", k );
					pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
					trace( traceFile, "\n        first: " );
					SymbolSideTableEntry expected = pg_sideTableEntry( pg, pn_token( competitor->pn, competitor->dot, pg->gr ), traceFile );
					bv_or( competitorSymbols, expected->first );
					bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
					trace( traceFile, "\n" );
					}
				if( !bv_intersects( reduceSymbols, competitorSymbols ) )
					{
					trace( traceFile, "        No conflict\n" );
					continue;
					}
				int winningMargin = pn_nestDepth( competitor->pn, pg->gr ) - pn_nestDepth( it->pn, pg->gr );
				if( winningMargin > 0 )
					{
					trace( traceFile, "        Competitor has lower priority\n" );
					continue;
					}
				else if( winningMargin == 0 )
					{
					bv_copy( conflictingSymbols, reduceSymbols );
					bv_and ( conflictingSymbols, competitorSymbols );
					if( pg_itemIsRightmost( pg, k ) )
						{
						if( both( it, competitor, CR_ARBITRARY_REDUCE, pg ) )
							{
							trace( traceFile, "        CR_ARBITRARY_REDUCE -- favouring competitor\n" );
							}
						else
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Reduce-reduce" );
							trace( traceFile, "        BAD!\n" );
							}
						continue;
						}
					else
						{
						if( both( it, competitor, CR_SHIFT_BEATS_REDUCE, pg ) )
							{
							trace( traceFile, "        CR_SHIFT_BEATS_REDUCE\n" );
							}
						else if( both( it, competitor, CR_REDUCE_BEATS_SHIFT, pg ) )
							{
							trace( traceFile, "        CR_REDUCE_BEATS_SHIFT\n" );
							continue;
							}
						else if( it->pn == competitor->pn )
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Self shift-reduce" );
							trace( traceFile, "        Self-left-associativity favours reduce\n" );
							continue;
							}
						else
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Shift-reduce" );
							trace( traceFile, "        Favouring reduce over shift until I figure out something better\n" );
							continue;
							}
						}
					}

				bv_minus( reduceSymbols, competitorSymbols );
				trace( traceFile, "        Competitor has higher priority; removing lookaheads: " );
				bv_sendFormattedTo( competitorSymbols, traceFile, "s%d", ", s%d" );
				trace( traceFile, "\n" );
				}
			trace( traceFile, "      Filtered ReduceSymbols: " );
			bv_sendFormattedTo( reduceSymbols, traceFile, "s%d", ", s%d" );
			trace( traceFile, "\n" );

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
	Automaton au;
	Stack stateStack;
	File diagnostics;
	File detailedDiagnostics;
	};

typedef struct psa_struct *ParserArray;
#define AR_PREFIX  psa
#define AR_TYPE    ParserArray
#define AR_ELEMENT Parser
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define psa_new( size, ml ) psa_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct au_struct
	{
	Grammar gr;
	Object startState;
	ObjectHeap stateHeap;
	// Perf tweaks
	ParserArray parsers;
	// Debugging
	ParserGenerator pg;
	};

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

static int pg_sendDotTo( ParserGenerator pg, File dotFile )
	{
	int charsSent = fl_write( dotFile, "digraph \"G\" { overlap=false \n" );
	int i,j;
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		charsSent += fl_write( dotFile, "n%p [label=\"%d %s\\n", its->stateNode, i, LR0StateKindNames[ its_LR0StateKind( its, pg ) ] );
#ifdef REDUCE_CONTEXT_LENGTH
		charsSent += fl_write( dotFile, "(reduce context: %d)\\n", ob_toInt( ob_getField( its->stateNode, sy_byIndex( SYM_REDUCE_CONTEXT_LENGTH, pg->st ), pg->heap ), pg->heap ) );
#endif
		for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
			{
			Item it = ita_element( pg->items, j );
			charsSent += pn_sendItemTo( it->pn, it->dot, dotFile, pg->gr, pg->st );
			charsSent += fl_write( dotFile, "\\n" );
			}
		charsSent += fl_write( dotFile, "\"]\n" );
		}
	Object startNode = itst_element( pg->itemSets, 0 )->stateNode;
	charsSent += ob_sendDotEdgesTo( startNode, dotFile, pg->heap );
	charsSent += fl_write( dotFile, "}\n" );
	return charsSent;
	}

FUNC Automaton au_new( Grammar gr, SymbolTable st, MemoryLifetime ml, File conflictLog, File diagnostics )
	{
	trace( diagnostics, "Generating automaton for {\n" );
	gr_sendTo( gr, diagnostics, st );
#ifdef NDEBUG
	MemoryLifetime generateTime = ml_begin( 100000, ml );
#else
	MemoryLifetime generateTime = ml;
#endif
	trace( diagnostics, "}\n" );

	Automaton result = (Automaton)ml_alloc( ml, sizeof(*result) );
	char stateTagName[50];
	sprintf( stateTagName, "SN%d", st_count( st ) );
	Symbol stateNodeTag = sy_byName( stateTagName, st );

	ParserGenerator pg = pg_new( gr, st, stateNodeTag, generateTime, ml, theObjectHeap() );
	pg_populateItemTable( pg, diagnostics );
	pg_populateSymbolSideTable( pg, diagnostics );
	Object startState = pg_computeLR0StateNodes( pg, diagnostics );
	pg_computeFirstSets( pg, diagnostics );
	pg_computeFollowSets( pg, diagnostics );
	pg_computeSLRLookaheads( pg, diagnostics );
	pg_computeReduceActions( pg, conflictLog, diagnostics );

	result->gr = gr;
	result->stateHeap  = theObjectHeap();
	result->startState = startState;
	result->parsers    = psa_new( 2, ml );
	if (diagnostics)
		{
		trace( diagnostics, "Finished generating automaton %p:\n", result );
		//au_sendTo( result, diagnostics, theObjectHeap(), st );
		pg_sendDotTo( pg, diagnostics );
		trace( diagnostics, "\n" );
		}

	if( generateTime == ml )
		result->pg = pg;
	else
		{
		ml_end( generateTime );
		result->pg = NULL;
		}

	return result;
	}

FUNC Grammar au_grammar( Automaton au )
	{
	return au->gr;
	}

FUNC Parser ps_new( Automaton au, MemoryLifetime ml, File diagnostics )
	{
	Parser result = NULL;
	int i;
	trace( diagnostics, "PARSER Scanning %d existing parsers on %p\n", psa_count( au->parsers ), au );
	for( i = 0; i < psa_count( au->parsers ) && !result; i++ )
		{
		Parser ps = psa_get( au->parsers, i );
		if( sk_depth( ps->stateStack ) == 0 )
			{
			result = ps;
			trace( diagnostics, "PARSER Found existing parser %p on %p @ %d\n", result, au, i );
			}
		else
			{
			trace( diagnostics, "PARSER Existing parser %p on %p @ %d has depth %d\n", ps, au, i, sk_depth( ps->stateStack ) );
			}
		}
	if( !result )
		{
		result = (Parser)ml_alloc( ml, sizeof(*result) );
		result->au = au;
		result->stateStack = sk_new( ml );
		trace( diagnostics, "PARSER Allocated new parser %p on %p @ %d\n", result, au, psa_count( au->parsers ) );
		psa_append( au->parsers, result );
		}
	sk_push( result->stateStack, au->startState );
	result->diagnostics = diagnostics;
	result->detailedDiagnostics = NULL;
	return result;
	}

FUNC void ps_close( Parser ps )
	{
	trace( ps->diagnostics, "PARSER Freed parser %p\n", ps );
	sk_popAll( ps->stateStack );
	}

FUNC Automaton ps_automaton( Parser ps )
	{
	return ps->au;
	}

#ifdef ITEM_SET_NUMS
static int ps_itemSetNum( Parser ps, int depth, Symbol isn )
	{
	ObjectHeap heap = theObjectHeap(); // Cheating a bit
	return ob_toInt( ob_getField( sk_item( ps->stateStack, depth ), isn, heap ), heap );
	}
#endif

static Object ps_nextState( Parser ps, Object ob )
	{
	ObjectHeap oh = ps->au->stateHeap;
	Object curState = sk_top( ps->stateStack );
	Symbol token = ob_tag( ob, oh );
	if( ps->detailedDiagnostics )
		{
		trace( ps->detailedDiagnostics, "NEXT STATE from %d ob: ", ps_itemSetNum( ps, 0, sy_byIndex( SYM_ITEM_SET_NUM, theSymbolTable() ) ) );
		ob_sendTo( ob, ps->detailedDiagnostics, theObjectHeap() );
		trace( ps->detailedDiagnostics, "\n" );
		}
	Object result = NULL;
	if( ob_isToken( ob, oh ) )
		{
		Symbol literalToken = ob_toSymbol( ob, oh );
		result = ob_getField( curState, literalToken, oh );
		}
	if( !result )
		result = ob_getField( curState, token, oh );
	if( ps->detailedDiagnostics )
		{
		trace( ps->detailedDiagnostics, "  result: " );
		ob_sendTo( result, ps->detailedDiagnostics, theObjectHeap() );
		trace( ps->detailedDiagnostics, "\n" );
		}
	return result;
	}

FUNC bool ps_expects( Parser ps, Object ob )
	{
	return ps_nextState( ps, ob ) != NULL;
	}

FUNC void ps_push( Parser ps, Object ob )
	{
	ObjectHeap oh = ps->au->stateHeap;
	Object nextState = ps_nextState( ps, ob );
	if( !nextState )
		{
		ParserGenerator pg = ps_automaton(ps)->pg;
		if( pg )
			{
			// Extended debug info
			int i,j,k;
			int charsSent = 0;
			fl_write( stderr, "State:\n" );
			Symbol isn = sy_byIndex( SYM_ITEM_SET_NUM, pg->st );
			SymbolVector  follow = bv_new( sst_count(pg->sst),  ml_undecided() );
			ItemVector curItems  = bv_new( gr_numItems(pg->gr), ml_undecided() );
			ItemVector nextItems = bv_new( gr_numItems(pg->gr), ml_undecided() );
			bool firstIteration = true; // easiest to initialize during first iteration of loop below
			// Too bad the nextItems logic requires us to print the innermost frame first...
			for( i = 0; i < sk_depth( ps->stateStack ); i++ )
				{
				bv_clear( nextItems );
				int itemSetNum = ps_itemSetNum( ps, i, isn );
				ItemSet its = itst_element( pg->itemSets, itemSetNum );
				for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
					{
					Item it = ita_element( pg->items, j );
					// Figure out what stuff to print
					if( !bv_isSet( curItems, j ) && !firstIteration )
						continue;
					switch( it->dot )
						{
						case 0:
							// Nothing to do
							break;
						case 1:
							{
							// Once we get past this item, we'll be at position 0, so
							// we'll want everything before this guy's lhs.
							SymbolSideTableEntry lhs = pg_sideTableEntry( pg, pn_lhs( it->pn, pg->gr ), NULL );
							bv_or( nextItems, lhs->expectingItems );
							}
							// fall through
						default:
							bv_set( nextItems, j-1 );
							break;
						}

					// Print it
					charsSent += fl_write( stderr, "    " );
					charsSent += pn_sendItemTo( it->pn, it->dot, stderr, pg->gr, pg->st );
					charsSent += fl_write( stderr, "  [ " );
					char *sep = "";
					bv_clear( follow );
					it_getFollow( it, follow, pg, NULL );
					for( k = bv_firstBit( follow ); k != bv_END; k = bv_nextBit( follow, k ) )
						{
						SymbolSideTableEntry sste = sst_element( pg->sst, k );
						charsSent += fl_write( stderr, "%s%s", sep, sy_name( sste->sy, pg->st ) );
						sep = " ";
						}
					charsSent += fl_write( stderr, " ]\n" );
					}
				charsSent += fl_write( stderr, "  ----\n" );
				firstIteration = false;
				bv_copy( curItems, nextItems );
				}
			}
		fl_write( stderr, "Unexpected %s: ", sy_name( ob_tag( ob, oh ), pg->st ) );
		ob_sendTo( ob, stderr, oh );
		fl_write( stderr, "\n" );
		}
	check( nextState );
	sk_push( ps->stateStack, nextState );
	}

FUNC int ps_depth( Parser ps )
	{
	return sk_depth( ps->stateStack );
	}

FUNC Production ps_handle( Parser ps, Object lookahead )
	{
	ObjectHeap oh = ps->au->stateHeap;
	Object nextState = ps_nextState( ps, lookahead );
	if( ob_isInt( nextState, oh ) )
		return gr_production( ps->au->gr, ob_toInt( nextState, oh ) );
	else
		return NULL;
	}

FUNC void ps_popN( Parser ps, int count )
	{
	assert( sk_depth( ps->stateStack ) >= count+1 ); // must always leave the startState on the stack
	sk_popN( ps->stateStack, count );
	}

FUNC int au_sendTo( Automaton au, File fl, ObjectHeap heap, SymbolTable st )
	{
	return ob_sendDeepTo( au->startState, fl, heap );
	}

FUNC int ps_sendTo( Parser ps, File fl, ObjectHeap heap, SymbolTable st )
	{
	if( !fl )
		return 0;

	int charsSent = 0; //fl_write( fl, "%p(%p): ", ps, ps_automaton(ps) );
	//sk_sendTo( ps->stateStack, fl, heap );
#ifdef ITEM_SET_NUMS
	int i;
	char *sep = "";
	Symbol isn = sy_byIndex( SYM_ITEM_SET_NUM, st );
	for( i=0; i < sk_depth( ps->stateStack ); i++ )
		{
		charsSent += fl_write( fl, "%s%d", sep, ps_itemSetNum(ps, i, isn) );
		sep = " ";
		}
#endif
	return charsSent;
	}

#ifdef REDUCE_CONTEXT_LENGTH
FUNC int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st )
	{
	return ob_toInt( ob_getField( sk_top( ps->stateStack ), sy_byIndex( SYM_REDUCE_CONTEXT_LENGTH, st ), heap ), heap );
	}
#endif

#ifdef PARSER_T

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

	pg_sendDotTo( pg, dotFile );

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

