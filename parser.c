
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "walk.h"
#include "records.h"
#include "symbols.h"
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#define INCLUDE_ITEMS_IN_DOT_NODES (1)

#define ITEM_STATE_PREFIX "state"

typedef BitVector ItemVector;   // BitVectors of item indexes
typedef BitVector SymbolVector; // BitVectors of symbol side-table indexes

typedef struct it_struct
	{
	Production pn;
	int dot; // "Current" position of the dot is before the token at this index
	SymbolVector lookahead;  // May not be the optimal lookahead (ie. for LALR)
	int canonical; // lowest-numbered item with the same symbols to the right of the dot, and the same production symbol (ie. reduce action)
	} *Item;

#ifdef NDEBUG
	typedef struct ita_struct *ItemArray;
#else
	typedef Array ItemArray;
#endif
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

#ifdef NDEBUG
	typedef struct sst_struct *SymbolSideTable;
#else
	typedef Array SymbolSideTable;
#endif
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

#ifdef NDEBUG
	typedef struct itst_struct *ItemSetTable;
#else
	typedef Array ItemSetTable;
#endif
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
		TRACE( traceFile, "  -- s%d = '%s' --\n", sstIndex, sy_name( sy, pg->st ) );
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

static void pg_canonicalizeItemVector( ParserGenerator pg, ItemVector itemVector, File traceFile )
	{
	if (USE_CANONICAL_ITEMS)
		{
		int i;
		for( i = bv_firstBit( itemVector ); i != bv_END; i = bv_nextBit( itemVector, i ) )
			{
			Item it = ita_element( pg->items, i );
			bv_unset( itemVector, i );
			bv_set( itemVector, it->canonical );
			}
		}
	}

static void pg_computeItemsExpectingToken( ParserGenerator pg, ItemVector result, ItemVector itemSet, Symbol token, File traceFile )
	{
	ItemVector expectingItems = pg_sideTableEntry( pg, token, traceFile )->expectingItems;
	if( !expectingItems )
		{
		bv_clear( result ); // No items are expecting token
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
	ob_setFieldX(
		result->stateNode,
		SYM_ITEM_SET_NUM,
		ob_fromInt( its_index( result, pg ), pg->heap ),
		pg->heap );
#endif
#ifdef REDUCE_CONTEXT_LENGTH
	ob_setFieldX(
		result->stateNode,
		SYM_REDUCE_CONTEXT_LENGTH,
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
	TRACE( traceFile, "Populating item table\n" );

	// We process the productions starting from the end so that the
	// canonicalization mechanism will work with nested grammars, and will
	// favour inner productions over inherited ones, because the inner
	// productions tend to have the correct conflict resolution settings.
	//
	// A safer approach would be not to canonicalize one production to another
	// with a different conflict resolution setting, but that would needlessly
	// double the number of items, and therefore the size of bitvectors.
	// Perhaps proper canonicalization requires a second pass, to select the
	// production with the most dominant conflict resolution setting.
	//
	for( i = gr_numProductions(gr)-1; i >= 0; i-- )
		{
		Production pn = gr_production( gr, i );
		for( j=0; j <= pn_length( pn, gr ); j++ )
			{
			TRACE( traceFile, "  i%d: ", ita_count( pg->items ) );
			Item it = ita_nextElement( pg->items );
			it->pn  = pn;
			it->dot = j;
			pn_sendItemTo( it->pn, it->dot, traceFile, gr, pg->st );
			it->canonical = ita_elementIndex( pg->items, it );
			if( USE_CANONICAL_ITEMS )
				{
				int candidateIndex,e;
				for ( candidateIndex=0; candidateIndex < it->canonical; candidateIndex++ )
					{
					Item candidate = ita_element( pg->items, candidateIndex );
					if ( // This could be overly conservative
							pn_symbol( pn, gr ) == pn_symbol( candidate->pn, gr )
						&& pn_length( pn, gr ) == pn_length( candidate->pn, gr )
						&& it->dot == candidate->dot
						){
						for ( e=j; e < pn_length( pn, gr ); e++ )
							{
							if (  pn_token( pn, e, gr ) != pn_token( candidate->pn, e, gr )
								|| pn_name ( pn, e, gr ) != pn_name ( candidate->pn, e, gr )
								){
								candidate = NULL;
								break;
								}
							}
						if ( candidate != NULL )
							{
							it->canonical = candidateIndex;
							break;
							}
						}
					}
				assert( it->canonical >= 0 );
				if( it->canonical != ita_elementIndex( pg->items, it ) )
					{
					TRACE( traceFile, " # canonical: i%d\n", it->canonical );
					break;
					}
				else
					{
					TRACE( traceFile, "\n" );
					}
				}
			}
		if( ita_last( pg->items, 0 )->dot == pn_length( pn, gr ) )
			bv_set( pg->rightmostItems, ita_count(pg->items) - 1 );
		}
	ita_shrinkWrap( pg->items );
	 bv_shrinkWrap( pg->rightmostItems );
	}

static int sendSymByNumber( void *symbolTable, int symIndex, File file )
	{
	SymbolTable st = (SymbolTable)symbolTable;
	return fl_write( file, "%s", sy_name( sy_byIndex( symIndex, st ), st ) );
	}

static int sendSymBySSTIndex( void *parserGenerator, int sstIndex, File file )
	{
	ParserGenerator pg = (ParserGenerator)parserGenerator;
	SymbolSideTableEntry sste = sst_element( pg->sst, sstIndex );
	return fl_write( file, "%s", sy_name( sste->sy, pg->st ) );
	}

static int sendItem( void *parserGenerator, int itemIndex, File file )
	{
	ParserGenerator pg = (ParserGenerator)parserGenerator;
	Item it = ita_element( pg->items, itemIndex );
	int charsSent = 0;
	charsSent += fl_write( file, "      i%-3d: ", itemIndex );
	charsSent += pn_sendItemTo( it->pn, it->dot, file, pg->gr, pg->st );
	charsSent += fl_write( file, "\n" );
	return charsSent;
	}

static int pg_sendSymbolSideTableTo( ParserGenerator pg, File diagnostics )
	{
	if( !diagnostics )
		return 0;

	SymbolSideTable sst = pg->sst;
	int i, charsSent=0;
	for( i=1; i < sst_count( sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( sst, i );
		charsSent += fl_write( diagnostics, "  s%d: '%s'\n", i, sy_name( sste->sy, pg->st ) );
		if( sste->leftmostItems )
			{
			charsSent += fl_write( diagnostics, "    leftmostItems:\n" );
			bv_sendFormattedToX( sste->leftmostItems, diagnostics, sendItem, pg, "", "", "" );
			}
		if( sste->expectingItems )
			{
			charsSent += fl_write( diagnostics, "    expectingItems:\n" );
			bv_sendFormattedToX( sste->expectingItems, diagnostics, sendItem, pg, "", "", "" );
			}
		if( sste->first )
			{
			charsSent += fl_write( diagnostics, "    first: " );
			bv_sendFormattedTo( sste->first, diagnostics, sendSymBySSTIndex, pg );
			charsSent += fl_write( diagnostics, "\n" );
			}
		if( sste->follow )
			{
			charsSent += fl_write( diagnostics, "    follow: " );
			bv_sendFormattedTo( sste->follow, diagnostics, sendSymBySSTIndex, pg );
			charsSent += fl_write( diagnostics, "\n" );
			}
		}
	return charsSent;
	}

static void pg_populateSymbolSideTable( ParserGenerator pg, File traceFile )
	{
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	SymbolTable st = pg->st;
	int itemIndex;
	pg->sstIndexes = (int*)ml_allocZeros( ml, st_count(st) * sizeof(pg->sstIndexes[0]));
	pg->sst = sst_new( 100, ml );
	sst_incCount( pg->sst ); // sst index zero is used for "null" so skip that one
	pg->nullableSymbols = bv_new( 100, ml );
	for( itemIndex = 0; itemIndex < ita_count( pg->items ); itemIndex++ )
		{
		Item it = ita_element( pg->items, itemIndex );
		Production pn = it->pn;
		Symbol lhs = pn_lhs( pn, gr );
		SymbolSideTableEntry lhsEntry = pg_sideTableEntry( pg, lhs, traceFile );
		if( !lhsEntry->leftmostItems )
			lhsEntry->leftmostItems = bv_new( ita_count( pg->items ), ml );
		if( it->dot == 0 )
			bv_set( lhsEntry->leftmostItems, itemIndex );
		if( it->dot < pn_length( pn, gr ) )
			{
			SymbolSideTableEntry dotEntry = pg_sideTableEntry( pg, pn_token( it->pn, it->dot, gr ), traceFile );
			if( !dotEntry->expectingItems )
				dotEntry->expectingItems = bv_new( ita_count( pg->items ), ml );
			bv_set( dotEntry->expectingItems, itemIndex );
			}
		if( pn_length( pn, gr ) == 0 )
			bv_set( pg->nullableSymbols, pg_symbolSideTableIndex( pg, lhs ) );
		}
	sst_shrinkWrap( pg->sst );
	bv_shrinkWrap( pg->nullableSymbols );

	// Some more initialization now that we know how many side table entries there are
	int numSymbols = sst_count( pg->sst );
	BitVector stateNodeFields = bv_new( numSymbols, pg->generateTime );
	int i;
	for( i=1; i < numSymbols; i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		sste->first     = bv_new( numSymbols, pg->generateTime );
		sste->follow    = bv_new( numSymbols, pg->generateTime );
		bv_set( sste->first, i );
		bv_set( stateNodeFields, sy_index( sste->sy, st ) );
		}
	sy_setInstanceShape( pg->stateNodeTag, rd_new( stateNodeFields, pg->parseTime ), pg->heap );

	TRACE( traceFile, "Initial SymbolSideTable:\n" );
	pg_sendSymbolSideTableTo( pg, traceFile );
	}

static Object pg_computeLR0StateNodes( ParserGenerator pg, File traceFile )
	{
	TRACE( traceFile, "Computing LR0 state nodes\n" );
	MemoryLifetime ml = pg->generateTime;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = gr_numItems( pg->gr );
	ItemVector nextItems = bv_new( itemCount, ml );
	pg->itemSets = itst_new( itemCount, ml ); // guesstimate of number of item sets.  In practice it seems to be very roughly equal to the number of items
	startItemSet = curItemSet = pg_createItemSet( pg, bv_dup( pg_sideTableEntry( pg, gr_goal(pg->gr), traceFile )->leftmostItems, ml ) );
	Object startState = startItemSet->stateNode;
	pg_closeItemVector( pg, curItemSet->items, traceFile );
	pg_canonicalizeItemVector( pg, curItemSet->items, traceFile );
	st_count( st ); // just to use the variable and silence a warning
	while( curItemSet )
		{
		int i;
		ItemVector itemsLeft = bv_new( itemCount, ml );

		bv_copy( itemsLeft, curItemSet->items );
		if( TRACE( traceFile, "  Expanding " ITEM_STATE_PREFIX "%d\n    stateNode: %s_%p\n         items left: ",
			its_index( curItemSet, pg ), sy_name( ob_tag( curItemSet->stateNode, pg->heap ), st ), PH( curItemSet->stateNode ) ) )
			{
			traceBVX( itemsLeft, traceFile, sendBitNumber, "i%d" );
			TRACE( traceFile, "\n" );
			}

		bv_minus( itemsLeft, pg->rightmostItems );
		if( TRACE( traceFile, "    minus rightmost: " ) )
			{
			traceBVX( itemsLeft, traceFile, sendBitNumber, "i%d" );
			TRACE( traceFile, "\n" );
			}

		assert( !its_isExpanded( curItemSet ) );
		int size = sst_count(pg->sst) * sizeof(curItemSet->edgePriorities[0]);
		curItemSet->edgePriorities = (int*)ml_allocZeros( pg->generateTime, size );
		assert( its_isExpanded( curItemSet ) );
		for( i = bv_firstBit( itemsLeft ); i != bv_END; i = bv_nextBit( itemsLeft, i ) )
			{
			Item it = ita_element( pg->items, i ); ItemSet nextItemSet;
			Symbol expected = pn_token( it->pn, it->dot, pg->gr );
			if( TRACE( traceFile, "    i%d is expecting '%s':  ", i, sy_name( expected, st ) ) )
				{
				pn_sendItemTo( it->pn, it->dot, traceFile, pg->gr, st );
				TRACE( traceFile, "\n" );
				}

			pg_computeItemsExpectingToken( pg, nextItems, itemsLeft, expected, traceFile );
			if( TRACE( traceFile, "      similar items: " ) )
				{
				traceBVX( nextItems, traceFile, sendBitNumber, "i%d" );
				TRACE( traceFile, "\n" );
				}

			Item shallowestItem = ita_element( pg->items, bv_lastBit( nextItems ) );
			int highestPriority = 1 + pn_nestDepth( shallowestItem->pn, pg->gr );
			int ssti = pg_symbolSideTableIndex( pg, expected );
			curItemSet->edgePriorities[ ssti ] = highestPriority;
			TRACE( traceFile, "      edgePriorities['%s'] = %d\n", sy_name( expected, st ), highestPriority );

			bv_minus( itemsLeft, nextItems );
			if( TRACE( traceFile, "          itemsLeft: " ) )
				{
				traceBVX( itemsLeft, traceFile, sendBitNumber, "i%d" );
				TRACE( traceFile, "\n" );
				}

			bv_shift( nextItems );
			if( TRACE( traceFile, "            shifted: " ) )
				{
				traceBVX( nextItems, traceFile, sendBitNumber, "i%d" );
				TRACE( traceFile, "\n" );
				}

			pg_closeItemVector( pg, nextItems, traceFile );
			if( TRACE( traceFile, "             closed: " ) )
				{
				traceBVX( nextItems, traceFile, sendBitNumber, "i%d" );
				TRACE( traceFile, "\n" );
				}

			pg_canonicalizeItemVector( pg, nextItems, traceFile );
			if( TRACE( traceFile, "          canonical: " ) )
				{
				traceBVX( nextItems, traceFile, sendBitNumber, "i%d" );
				TRACE( traceFile, "\n" );
				}

			nextItemSet = pg_findItemSet( pg, nextItems );
			if( nextItemSet )
				{
				if( TRACE( traceFile, "      Found existing " ITEM_STATE_PREFIX "%d with items: ", its_index( nextItemSet, pg ) ) )
					{
					traceBVX( nextItems, traceFile, sendBitNumber, "i%d" );
					TRACE( traceFile, "\n" );
					}
				}
			else
				{
				// Use the nextItems bitvector we created, and allocate a new one for the next guy
				nextItemSet = pg_createItemSet( pg, nextItems );
				nextItems = bv_new( itemCount, ml );
				TRACE( traceFile, "      Created " ITEM_STATE_PREFIX "%d\n", its_index( nextItemSet, pg ) );
				}
			ob_setField( curItemSet->stateNode, expected, nextItemSet->stateNode, pg->heap );
			}
		for( i = 1+its_index( curItemSet, pg ); i < itst_count( pg->itemSets ); i++ )
			{
			curItemSet = itst_element( pg->itemSets, i );
			if( its_isExpanded( curItemSet ) )
				TRACE( traceFile, "    " ITEM_STATE_PREFIX "%d already expanded\n", its_index( curItemSet, pg ) );
			else
				break;
			}
		if( its_isExpanded( curItemSet ) )
			{
			curItemSet = NULL;
			TRACE( traceFile, "All ItemSets are expanded\n" );
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

	TRACE( traceFile, "SymbolSideTable after pg_computeFirstSets:\n" );
	pg_sendSymbolSideTableTo( pg, traceFile );
	}

static void pg_computeFollowSets( ParserGenerator pg, File traceFile )
	{
	int pnNum;
	bool somethingChanged = true;
	TRACE( traceFile, "Computing follow sets\n" );
	while( somethingChanged )
		{
		somethingChanged = false;
		TRACE( traceFile, "  Looping through productions\n" );
		for( pnNum=0; pnNum < gr_numProductions( pg->gr ); pnNum++ )
			{
			Production pn = gr_production( pg->gr, pnNum );
			TRACE( traceFile, "    #%d: ", pnNum );
			pn_sendTo( pn, traceFile, pg->gr, pg->st );
			TRACE( traceFile, "\n" );
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
					TRACE( traceFile, "      Copied follow from lhs " );
					sy_sendTo( lhs->sy, traceFile, pg->st );
					TRACE( traceFile, " to " );
					sy_sendTo( last->sy, traceFile, pg->st );
					TRACE( traceFile, ": " );
					bv_sendFormattedTo( lhs->follow, traceFile, sendSymBySSTIndex, pg );
					TRACE( traceFile, "\n" );
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
					TRACE( traceFile, "      Copied first from " );
					sy_sendTo( next->sy, traceFile, pg->st );
					TRACE( traceFile, " to follow of " );
					sy_sendTo( cur->sy, traceFile, pg->st );
					TRACE( traceFile, ": " );
					bv_sendFormattedTo( next->first, traceFile, sendSymBySSTIndex, pg );
					TRACE( traceFile, "\n" );
					}
				if( bv_isSet( pg->nullableSymbols, nextIndex ) )
					{
					somethingChanged |= bv_orChanged( cur->follow, next->follow );
					if( traceFile )
						{
						TRACE( traceFile, "      Copied follow from " );
						sy_sendTo( next->sy, traceFile, pg->st );
						TRACE( traceFile, " to " );
						sy_sendTo( cur->sy, traceFile, pg->st );
						TRACE( traceFile, ": " );
						bv_sendFormattedTo( next->follow, traceFile, sendSymBySSTIndex, pg );
						TRACE( traceFile, "\n" );
						}
					}
				}
			}
		}

	TRACE( traceFile, "SymbolSideTable after pg_computeFollowSets:\n" );
	pg_sendSymbolSideTableTo( pg, traceFile );
	}

#if 0
static int its_sendTo( ItemSet its, File diagnostics, ParserGenerator pg )
	{
	return bv_sendFormattedTo( its->items, diagnostics, sendItem, pg );
	}
#endif

static void pg_computeSLRLookaheads( ParserGenerator pg, File traceFile )
	{
	int i;
	int numItems = ita_count( pg->items );
	Item *items = (Item*)ml_alloc( pg->generateTime, numItems * sizeof(*items) );
	for( i=0; i < numItems; i++ )
		{
		Item it = ita_element( pg->items, i );
		items[i] = it; // uh... what the heck is this for?
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
	if( TRACE( traceFile, "          Computing follow for: " ) )
		{
		pn_sendItemTo( it->pn, it->dot, traceFile, pg->gr, pg->st );
		TRACE( traceFile, "\n" );
		}
	for( i = it->dot; i < pn_length( it->pn, pg->gr ); i++ )
		{
		int curIndex = pg_symbolSideTableIndex( pg, pn_token( it->pn, i, pg->gr ) );
		SymbolVector curFirst = sst_element( pg->sst, curIndex )->first;
		bv_or( result, curFirst );
		if( TRACE( traceFile, "            Symbol '%s' at %d adds ", sy_name( sst_element( pg->sst, curIndex )->sy, pg->st ), i ) )
			{
			bv_sendFormattedTo( curFirst, traceFile, sendSymBySSTIndex, pg );
			TRACE( traceFile, "\n" );
			}
		if( !bv_isSet( pg->nullableSymbols, curIndex ) )
			break;
		}
	if( i == pn_length( it->pn, pg->gr ) )
		{
		bv_or( result, it->lookahead );
		if( TRACE( traceFile, "            Adding lookahead " ) )
			{
			bv_sendFormattedTo( it->lookahead, traceFile, sendSymBySSTIndex, pg );
			TRACE( traceFile, "\n" );
			}
		}
	}

static void pg_reportConflict( ParserGenerator pg, ItemSet its, Item winner, Item loser, SymbolVector conflictingSymbols, File conflictLog, File traceFile, char *format, ... )
	{
	// TODO: Report the symbol
	if( conflictLog )
		{
		va_list args;
		va_start( args, format );
		TRACE(  conflictLog, "Conflict in " ITEM_STATE_PREFIX "%d: ", its_index( its, pg ) );
		fl_vwrite( conflictLog, format, args );
		TRACE(  conflictLog, "\n  Winner: " );
		pn_sendItemTo( winner->pn, winner->dot, conflictLog, pg->gr, pg->st );
		TRACE(  conflictLog, "\n   Loser: " );
		pn_sendItemTo( loser->pn, loser->dot, conflictLog, pg->gr, pg->st );
		TRACE(  conflictLog, "\n Symbols: " );
		bv_sendFormattedTo( conflictingSymbols, conflictLog, sendSymBySSTIndex, pg );
		TRACE(  conflictLog, "\n" );
		va_end( args );
		}
	TRACE( traceFile, "        CONFLICT\n" );
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

	TRACE( traceFile, "Computing reduce actions\n" );
	ItemVector   reduceItems        = bv_new( ita_count(pg->items), pg->generateTime );
	SymbolVector reduceSymbols      = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector competitorSymbols  = bv_new( sst_count(pg->sst),   pg->generateTime );
	SymbolVector conflictingSymbols = bv_new( sst_count(pg->sst),   pg->generateTime );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		TRACE( traceFile, "  " ITEM_STATE_PREFIX "%d:\n", i );
		ItemSet its = itst_element( pg->itemSets, i );
		Object stateNode = its->stateNode;
		bv_copy ( reduceItems, pg->rightmostItems );
		bv_and  ( reduceItems, its->items );
		if( TRACE( traceFile, "    Items: " ) )
			{
			bv_sendFormattedTo( its->items, traceFile, sendBitNumber, "i%d" );
			TRACE( traceFile, "\n" );
			}
		if( TRACE( traceFile, "    ReduceItems: " ) )
			{
			bv_sendFormattedTo( reduceItems, traceFile, sendBitNumber, "i%d" );
			TRACE( traceFile, "\n" );
			}
		for( j = bv_firstBit( reduceItems ); j != bv_END; j = bv_nextBit( reduceItems, j ) )
			{
			Item it = ita_element( pg->items, j );
			Production pn = it->pn;
			if( TRACE( traceFile, "    Item i%d: ", j ) )
				{
				pn_sendItemTo( pn, it->dot, traceFile, pg->gr, pg->st );
				TRACE( traceFile, "\n" );
				}
			bv_copy( reduceSymbols, it->lookahead );
			if( TRACE( traceFile, "      Original ReduceSymbols: " ) )
				{
				bv_sendFormattedTo( reduceSymbols, traceFile, sendSymBySSTIndex, pg );
				TRACE( traceFile, "\n" );
				}

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
			// In this loop, "continue" means to ignore the competitor.
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
					if( TRACE( traceFile, "      Checking reduce item i%d: ", k ) )
						{
						pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
						TRACE( traceFile, "\n" );
						}
					it_getFollow( competitor, competitorSymbols, pg, NULL );
					if( TRACE( traceFile, "        follow: " ) )
						{
						bv_sendFormattedTo( competitorSymbols, traceFile, sendSymBySSTIndex, pg );
						TRACE( traceFile, "\n" );
						}
					}
				else
					{
					if( TRACE( traceFile, "      Checking shift item i%d: ", k ) )
						{
						pn_sendItemTo( competitor->pn, competitor->dot, traceFile, pg->gr, pg->st );
						TRACE( traceFile, "\n" );
						}
					SymbolSideTableEntry expected = pg_sideTableEntry( pg, pn_token( competitor->pn, competitor->dot, pg->gr ), traceFile );
					bv_or( competitorSymbols, expected->first );
					if( TRACE( traceFile, "        first: " ) )
						{
						bv_sendFormattedTo( competitorSymbols, traceFile, sendSymBySSTIndex, pg );
						TRACE( traceFile, "\n" );
						}
					}
				if( !bv_intersects( reduceSymbols, competitorSymbols ) )
					{
					TRACE( traceFile, "        No conflict\n" );
					continue;
					}
				if( pn_conflictResolution( competitor->pn, pg->gr ) == CR_ABSTRACT )
					{
					TRACE( traceFile, "        Competitor is abstract\n" );
					continue;
					}
				int winningMargin = pn_nestDepth( competitor->pn, pg->gr ) - pn_nestDepth( it->pn, pg->gr );
				if( winningMargin > 0 )
					{
					TRACE( traceFile, "        Competitor has lower priority\n" );
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
							TRACE( traceFile, "        CR_ARBITRARY_REDUCE -- ignoring competitor\n" );
							}
						else
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Reduce-reduce" );
							TRACE( traceFile, "        BAD!\n" );
							}
						continue;
						}
					else
						{
						if( both( it, competitor, CR_SHIFT_BEATS_REDUCE, pg ) )
							{
							TRACE( traceFile, "        CR_SHIFT_BEATS_REDUCE\n" );
							}
						else if( both( it, competitor, CR_REDUCE_BEATS_SHIFT, pg ) )
							{
							TRACE( traceFile, "        CR_REDUCE_BEATS_SHIFT\n" );
							continue;
							}
						else if( it->pn == competitor->pn )
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Self shift-reduce" );
							TRACE( traceFile, "        Self-left-associativity favours reduce\n" );
							continue;
							}
						else
							{
							pg_reportConflict( pg, its, it, competitor, conflictingSymbols, conflictLog, traceFile, "Shift-reduce" );
							TRACE( traceFile, "        Favouring reduce over shift until I figure out something better\n" );
							continue;
							}
						}
					}
				else
					{
					TRACE( traceFile, "        Competitor has higher priority\n" );
					}

				bv_minus( reduceSymbols, competitorSymbols );
				if( TRACE( traceFile, "        Removed lookaheads for i%d: ", j ) )
					{
					bv_sendFormattedTo( competitorSymbols, traceFile, sendSymBySSTIndex, pg );
					TRACE( traceFile, "\n" );
					}
				}
			if( TRACE( traceFile, "      Filtered ReduceSymbols: " ) )
				{
				bv_sendFormattedTo( reduceSymbols, traceFile, sendSymBySSTIndex, pg );
				TRACE( traceFile, "\n" );
				}

			// Add reduce edges
			//
			for( k = bv_firstBit( reduceSymbols ); k != bv_END; k = bv_nextBit( reduceSymbols, k ) )
				{
				SymbolSideTableEntry reduceOn = sst_element( pg->sst, k );
				Symbol pnSymbol = pn_symbol( pn, gr );
				// Ideally: check( pnSymbol ); // TODO: Null symbol = abstract?  Perhaps I should have some default abstract production symbol?
				if( pnSymbol ) // can be null for parser.t
					ob_setField( stateNode, reduceOn->sy, oh_symbolToken( heap, pnSymbol ), heap );
				}
			}
		}

	}

struct ps_struct
	{
	Automaton au;
	Stack stateStack;
	Stack operandStack;
	File diagnostics;
	File detailedDiagnostics;
	};

#ifdef NDEBUG
	typedef struct psa_struct *ParserArray;
#else
	typedef Array ParserArray;
#endif
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
			// LR0 has no lookahead, so this is a conflict.
			return ReduceReduceConflict;
		}
	}

static int pg_sendDotTo( ParserGenerator pg, File dotFile )
	{
	if( !dotFile )
		return 0;
	int charsSent = fl_write( dotFile, "digraph \"Automaton\" { overlap=false \n" );
	int i;
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		charsSent += ob_sendTo( its->stateNode, dotFile, pg->heap );
		charsSent += fl_write( dotFile, "[label=\"%d %s\\n", i, LR0StateKindNames[ its_LR0StateKind( its, pg ) ] );
#ifdef REDUCE_CONTEXT_LENGTH
		charsSent += fl_write( dotFile, "(reduce context: %d)\\n", ob_getIntFieldX( its->stateNode, SYM_REDUCE_CONTEXT_LENGTH, pg->heap ) );
#endif
#if INCLUDE_ITEMS_IN_DOT_NODES
		int j;
		for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
			{
			Item it = ita_element( pg->items, j );
			charsSent += pn_sendItemTo( it->pn, it->dot, dotFile, pg->gr, pg->st );
			charsSent += fl_write( dotFile, "\\n" );
			}
#endif
		charsSent += fl_write( dotFile, "\"]\n" );
		}
	Object startNode = itst_element( pg->itemSets, 0 )->stateNode;
	charsSent += ob_sendDotEdgesTo( startNode, dotFile, pg->heap );
	charsSent += fl_write( dotFile, "}\n" );
	return charsSent;
	}

static char *LR0StatePythonNames[] =
	{
	"SHIFT",
	"REDUCE0",
	"ACCEPT",
	"SRCONFLICT",
	"REDUCE0", // HACK: should be RRCONFLICT, except the whole way we determine state types is wrong.  REDUCE0/RRCONFLICT should be determined by whether all out-edges are the same, not by how many items there are in the item set.
	};

static int pg_sendPythonTo( ParserGenerator pg, File outFile )
	{
	if( !outFile )
		return 0;
	int i;
	int charsSent = fl_write( outFile, "\nfrom sheppard_object import *\n" );
	charsSent += fl_write( outFile, "\ndef generated_automaton():\n" );
	charsSent += fl_write( outFile, "\t# States\n" );
	CheckList states = cl_open( pg->heap );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		Object state = its->stateNode;
		cl_check( states, state );
		charsSent += fl_write( outFile, "\t" );
		charsSent += ob_sendTo( state, outFile, pg->heap );
		charsSent += fl_write( outFile, " = Object( '%s' ); ", LR0StatePythonNames[ its_LR0StateKind( its, pg ) ] );
		charsSent += ob_sendTo( state, outFile, pg->heap );
		charsSent += fl_write( outFile, "._name = '" );
		charsSent += ob_sendTo( state, outFile, pg->heap );
		charsSent += fl_write( outFile, "'\n" );
		}
	charsSent += fl_write( outFile, "\n\t# Edges\n" );
	MemoryLifetime ml = ml_begin( 20, ml_undecided() );
	BitVector edges = bv_new( 100, ml );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		Object state = its->stateNode;
		bv_clear( edges );
		ob_getFieldSymbols( state, edges, pg->heap );
		int s;
		for( s = bv_firstBit( edges ); s != bv_END; s = bv_nextBit( edges, s ) )
			{
			Symbol edge = sy_byIndex( s, pg->st );
			Object target = ob_getField( state, edge, pg->heap );
			if( ob_isToken( target, pg->heap ) )
				{
				// LR0 automaton -- we ignore the lookahead and require that all
				// reduces from the same state are for the same production.  Hence,
				// only need to consult the first one.
				//
				Production firstProduction = ita_element( pg->items, bv_firstBit( its->items ) )->pn;
				charsSent += fl_write( outFile, "\t" );
				charsSent += ob_sendTo( state, outFile, pg->heap );
				charsSent += fl_write( outFile, ".action = 'ACTION_" );
				charsSent += ob_sendTo( target, outFile, pg->heap );
				charsSent += fl_write( outFile, "' # " );
				charsSent += pn_sendTo( firstProduction, outFile, pg->gr, pg->st );
				charsSent += fl_write( outFile, "\n" );
				break;
				}
			else if( cl_isChecked( states, target ) )
				{
				charsSent += fl_write( outFile, "\t" );
				charsSent += ob_sendTo( state, outFile, pg->heap );
				charsSent += fl_write( outFile, "['%s'] = ", sy_name( edge, pg->st ) );
				charsSent += ob_sendTo( target, outFile, pg->heap );
				charsSent += fl_write( outFile, "\n" );
				}
			}
		}
	charsSent += fl_write( outFile, "\treturn " );
	charsSent += ob_sendTo( itst_element( pg->itemSets, 0 )->stateNode, outFile, pg->heap );
	charsSent += fl_write( outFile, "\n\n" );
	ml_end( ml );
	return charsSent;
	}

static int pg_sendTableTo( ParserGenerator pg, File outFile )
	{
	int i;
	int charsSent = sy_sendHeaderTo( pg->stateNodeTag, outFile, pg->heap );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		Object state = its->stateNode;
		charsSent += ob_sendRowTo( state, outFile, pg->heap );
		}
	return charsSent;
	}

typedef struct printer_struct
	{
	File fl;
	InheritanceRelation ir;
	int charsSent;
	} *Printer;

static void printSubtags( void *printerArg, Object node )
	{
	Printer p = (Printer)printerArg; File fl = p->fl; InheritanceRelation ir = p->ir;
	if( ob_tag( node, ir_nodeHeap(ir) ) != ir_nodeTag(ir) )
		return;

	Object subArray = ob_getFieldX( node, SYM_SUBTAGS, ir_nodeHeap(ir) );
	if( !subArray )
		return;

	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, ir_symbolTable(ir) );
	int subtagIndex;
	Object subnode;
	const char *superName = sy_name( ob_getTokenField( node, symSymbol, ir_nodeHeap(ir) ), ir_symbolTable(ir) );
	fl_write( fl, "    %s >", superName );
	for( subtagIndex = IR_START_INDEX; NULL != ( subnode = ob_getElement( subArray, subtagIndex, ir_nodeHeap(ir) ) ); subtagIndex++ )
		{
		Symbol subTag = ob_getTokenField( subnode, symSymbol, ir_nodeHeap(ir) );
		p->charsSent += fl_write( fl, " %s", sy_name( subTag, ir_symbolTable(ir) ) );
		}
	p->charsSent += fl_write( fl, "\n" );
	}

static bool subtagPredicate( void *printerArg, Object xxxx, Symbol edgeSymbol, int edgeIndex, Object head )
	{
	Printer p = (Printer)printerArg; InheritanceRelation ir = p->ir;
	if( ob_tag( head, ir_nodeHeap(ir) ) == ir_nodeTag(ir) )
		return true;
	if( sy_index( edgeSymbol, ir_symbolTable(ir) ) == SYM_SUBTAGS )
		return true;
	return false;
	}

FUNC int ir_sendTo( InheritanceRelation ir, File fl )
	{
	MemoryLifetime sendTime = ml_begin( 100, ml_undecided() );
	Stack rootSet = sk_new( sendTime );
	sk_push( rootSet, ir_index(ir) );
	struct printer_struct printer = { fl, ir, 0 };
	postorderWalk( rootSet, subtagPredicate, printSubtags, ir_nodeHeap(ir), &printer );
	ml_end( sendTime );
	return printer.charsSent;
	}

static void au_augment( Automaton au, InheritanceRelation ir, SymbolTable st, File diagnostics )
	{
	TRACE( diagnostics, "Augmenting automaton %p:\n", PH( au ) );
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
	MemoryLifetime augmentTime = ml_begin( 10000, ml_undecided() );
	CheckList pushedStates = cl_open( au->stateHeap );
	BitVector originalEdges = bv_new( st_count(st), augmentTime );
	Stack states = sk_new( augmentTime ); // automaton states
	Stack nodes  = sk_new( augmentTime ); // nodes in the inheritance relation
	sk_push( states, au->startState );
	cl_check( pushedStates, au->startState );
	while( sk_depth( states ) != 0 )
		{
		Object state = sk_pop( states );
		bv_clear( originalEdges );
		ob_getFieldSymbols( state, originalEdges, au->stateHeap );
		if( diagnostics )
			{
			TRACE( diagnostics, "  State: " );
			ob_sendTo( state, diagnostics, au->stateHeap );
			TRACE( diagnostics, " originalEdges: " );
			bv_sendFormattedTo( originalEdges, diagnostics, sendSymByNumber, st );
			TRACE( diagnostics, "\n" );
			}
		int edge;
		for( edge = bv_firstBit( originalEdges ); edge != bv_END; edge = bv_nextBit( originalEdges, edge ) )
			{
			Symbol edgeSymbol = sy_byIndex( edge, st );
			Object targetState = ob_getField( state, edgeSymbol, au->stateHeap );
			if( diagnostics )
				{
				TRACE( diagnostics, "    Edge '%s' -> ", sy_name( edgeSymbol, st ) );
				ob_sendTo( targetState, diagnostics, au->stateHeap );
				TRACE( diagnostics, "\n" );
				}

			if( !ob_isInt( targetState, au->stateHeap ) && !cl_isChecked( pushedStates, targetState ) )
				{
				cl_check( pushedStates, targetState );
				sk_push( states, targetState );
				TRACE( diagnostics, "    - Pushed\n" );
				}

			Object node = ob_getField( ir_index(ir), edgeSymbol, ir_nodeHeap(ir) );
			if( node )
				{
				CheckList pushedNodes = cl_open( ir_nodeHeap(ir) );
				sk_push( nodes, node );
				cl_check( pushedNodes, node );
				while( sk_depth( nodes ) != 0 )
					{
					node = sk_pop( nodes );
					if( diagnostics )
						{
						TRACE( diagnostics, "      Inheritance node: " );
						ob_sendTo( node, diagnostics, ir_nodeHeap(ir) );
						TRACE( diagnostics, "\n" );
						}
					Object subArray = ob_getField( node, sy_byIndex( SYM_SUBTAGS, st ), ir_nodeHeap(ir) );
					if( subArray )
						{
						int subtagIndex;
						Object subnode;
						for( subtagIndex = IR_START_INDEX; NULL != ( subnode = ob_getElement( subArray, subtagIndex, ir_nodeHeap(ir) ) ); subtagIndex++ )
							{
							Symbol subtag = ob_getTokenField( subnode, symSymbol, ir_nodeHeap(ir) );
							TRACE( diagnostics, "        Subtag '%s' ", sy_name( subtag, st ) );
							if( ob_getField( state, subtag, au->stateHeap ) )
								{
								TRACE( diagnostics, "Already present\n" );
								check( bv_isSet( originalEdges, sy_index( subtag, st ) ) ); // Otherwise it's a conflict
								}
							else
								{
								TRACE( diagnostics, "Copying from '%s'\n", sy_name( edgeSymbol, st ) );
								ob_setField( state, subtag, targetState, au->stateHeap );
								if( !cl_isChecked( pushedNodes, subnode ) )
									{
									cl_check( pushedNodes, subnode );
									sk_push( nodes, subnode );
									TRACE( diagnostics, "        - Pushed\n" );
									}
								}
							}
						}
					Object superArray = ob_getFieldX( node, SYM_SUPERTAGS, ir_nodeHeap(ir) );
					if( superArray )
						{
						Object abstractState = targetState;
						if( ob_isInt( abstractState, au->stateHeap ) )
							{
							// Abstract reduce actions are representative by negative production index.
							// (Shift actions are just normal edges.)
							//
							abstractState = ob_fromInt( -ob_toInt( abstractState, au->stateHeap ), au->stateHeap );
							}
						int supertagIndex;
						Object supernode;
						for( supertagIndex = IR_START_INDEX; NULL != ( supernode = ob_getElement( superArray, supertagIndex, ir_nodeHeap(ir) ) ); supertagIndex++ )
							{
							Symbol supertag = ob_getTokenField( supernode, symSymbol, ir_nodeHeap(ir) );
							TRACE( diagnostics, "        Supertag '%s' ", sy_name( supertag, st ) );
							if( ob_getField( state, supertag, au->stateHeap ) )
								{
								TRACE( diagnostics, "Already present\n" );
								// TODO: CHECK Either supertag must be in originalEdges, or it must be a reduce that's compatible with the existing reduce, or it's a conflict
								//check( bv_isSet( originalEdges, sy_index( supertag, st ) ) ); // Otherwise it's a conflict
								}
							else
								{
								TRACE( diagnostics, "Abstracting from '%s'\n", sy_name( edgeSymbol, st ) );
								ob_setField( state, supertag, abstractState, au->stateHeap );
								if( !cl_isChecked( pushedNodes, supernode ) )
									{
									cl_check( pushedNodes, supernode );
									sk_push( nodes, supernode );
									TRACE( diagnostics, "        - Pushed\n" );
									}
								}
							}
						}
					}
				cl_close( pushedNodes );
				}
			}
		}
	cl_close( pushedStates );

	// Note: it would be possible to build a bitvector of all the symbols in
	// ir_index(ir), and filter originalEdges against that before iterating over
	// it, because nothing interesting will happen to unindexed edges anyway.
	// However, building that bitvector would be O(n) in the size of the subtype
	// graph, and thus far we have avoided a dependence on that.  The subtype
	// graph could contain many tags that don't appear in our automaton, in
	// which case it can be cheaper just to do the pointless index lookups for
	// edges that aren't there, rather than try to avoid them with filtering.
	}

#if 0
static int pushNewElements( Object array, Stack sk, CheckList alreadyPushed, ObjectHeap heap, ParserGenerator pg, File traceFile )
	{
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, pg->st );
	int numNodesPushed = 0;
	if( array )
		{
		int i;
		Object element;
		for( i = IR_START_INDEX; NULL != ( element = ob_getElement( array, i, heap ) ); i++ )
			{
			if( !cl_isChecked( alreadyPushed, element ) )
				{
				cl_check( alreadyPushed, element );
				sk_push( sk, element );
				numNodesPushed++;
				TRACE( traceFile, "        - Pushed '%s'\n", sy_name( ob_getTokenField( element, symSymbol, heap ), pg->st ) );
				}
			}
		}
	return numNodesPushed;
	}

static ObjectArray postOrder( InheritanceRelation ir, Symbol direction, BitVector rootSet, ParserGenerator pg, File diagnostics )
	{
	SymbolTable st = pg->st;
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
	ObjectHeap heap = ir_nodeHeap(ir)
	CheckList alreadyPushed = cl_open( heap );
	ObjectArray result = oba_new( 10, pg->generateTime );

	TRACE( diagnostics, "    Computing postorder '%s'\n", sy_name( direction, pg->st ) );

	// Initialize worklist to contain ir nodes indicated by rootSet
	TRACE( diagnostics, "      Pushing root set\n" );
	MemoryLifetime worklistTime = ml_begin( 10, pg->generateTime );
	Stack worklist = sk_new( pg->generateTime );
	int i;
	for( i = bv_firstBit( rootSet ); i != bv_END; i = bv_nextBit( rootSet, i ) )
		{
		Object root = ob_getField( ir_index(ir), sy_byIndex( i, st ), heap );
		if( root )
			{
			sk_push( worklist, root );
			cl_check( alreadyPushed, root );
			TRACE( diagnostics, "        - Pushed '%s'\n", sy_name( ob_getTokenField( root, symSymbol, heap ), st ) );
			}
		}

	// Generate post order
	TRACE( diagnostics, "      Processing work list\n" );
	while( !sk_isEmpty( worklist ) )
		{
		Object top = sk_top( worklist );
		Object array = ob_getField( top, direction, heap );
		if( 0 == pushNewElements( array, worklist, alreadyPushed, heap, pg, diagnostics ) )
			{
			// top's children have already been processed, so append it to the post order
			oba_append( result, top );
			sk_pop( worklist );
			TRACE( diagnostics, "        - Popped '%s'\n", sy_name( ob_getTokenField( top, symSymbol, heap ), st ) );
			}
		}

	cl_close( alreadyPushed );
	ml_end( worklistTime );
	return result;
	}
#endif

static void bv_propagate( BitVector *target, BitVector source, ParserGenerator pg )
	{
	// Tolerate null bitvectors -- treat them as empty
	if( source )
		{
		if( !*target )
			*target = bv_new( 0, pg->generateTime );
		bv_or( *target, source );
		}
	}

static void sste_propagate( SymbolSideTableEntry target, SymbolSideTableEntry source, ParserGenerator pg, File diagnostics )
	{
	bv_propagate( &target->leftmostItems,  source->leftmostItems,  pg );
	bv_propagate( &target->expectingItems, source->expectingItems, pg );
	bv_propagate( &target->first,          source->first,          pg );
	bv_propagate( &target->follow,         source->follow,         pg );
	TRACE( diagnostics, "      sste_propagate '%s' <- '%s'\n", sy_name( target->sy, pg->st ), sy_name( source->sy, pg->st ) );
	}

static void propagateFromPreds( Symbol sourceArraySymbol, Object targetIRNode, SymbolSideTableEntry targetEntry, CheckList relatedNodes, ObjectHeap irNodeHeap, ParserGenerator pg, File diagnostics )
	{
	SymbolTable st = pg->st;
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
	Symbol targetSym = ob_getTokenField( targetIRNode, symSymbol, irNodeHeap );
	TRACE( diagnostics, "    propagateFromPreds( '%s', '%s' )\n", sy_name( sourceArraySymbol, st ), sy_name( targetSym, st ) );

	Object sourceArray = ob_getField( targetIRNode, sourceArraySymbol, irNodeHeap );
	if( !sourceArray )
		return;
	Object sourceIRNode; int i;
	for( i = IR_START_INDEX; NULL != ( sourceIRNode = ob_getElement( sourceArray, i, irNodeHeap ) ); i++ )
		{
		Symbol sourceSym = ob_getTokenField( sourceIRNode, symSymbol, irNodeHeap );
		SymbolSideTableEntry sourceEntry = pg_sideTableEntry( pg, sourceSym, diagnostics );
		if( cl_isChecked( relatedNodes, sourceIRNode ) )
			sste_propagate( targetEntry, sourceEntry, pg, diagnostics );
		else
			TRACE( diagnostics, "      ('%s' not related)\n", sy_name( sourceSym, pg->st ) );
		}
	}

#if 0
static void traceSymbolVector( File diagnostics, const char *name, SymbolVector syv, ParserGenerator pg )
	{
	TRACE( diagnostics, "  | %s:", name );
	int i; char *sep = " ";
	for( i = bv_firstBit( syv ); i != bv_END; i = bv_nextBit( syv, i ) )
		{
		Symbol sy = sy_byIndex( i, pg->st );
		TRACE( diagnostics, "%s'%s'", sep, sy_name( sy, pg->st ) );
		sep = ", ";
		}
	TRACE( diagnostics, "\n" );
	}

static void tracePostOrder( File diagnostics, const char *name, ObjectArray oba, ObjectHeap nodeHeap, ParserGenerator pg )
	{
	TRACE( diagnostics, "  | %s:", name );
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, pg->st );
	int i; char *sep = " ";
	for( i = 0; i < oba_count( oba ); i++ )
		{
		Object node = oba_get( oba, i );
		Symbol sym = ob_getTokenField( node, symSymbol, nodeHeap );
		TRACE( diagnostics, "%s'%s'", sep, sy_name( sym, pg->st ) );
		sep = ", ";
		}
	TRACE( diagnostics, "\n" );
	}
#endif

typedef struct aug_struct *Augmenter;
struct aug_struct {
	MemoryLifetime ml;
	ParserGenerator pg;
	InheritanceRelation ir;
	File diagnostics;
	Stack top;
	Stack bottom;
	CheckList related;
	Stack topDownStack; // Popping nodes from this gives them in top-down order (which a reverse postorder starting from top)
	bool onlyProcessRelatedNodes;
	// State that depends on direction of walk
	Symbol direction;
	Stack terminalNodes;
};

static Augmenter aug_begin( ParserGenerator pg, InheritanceRelation ir, File diagnostics )
	{
	MemoryLifetime ml = ml_begin( 1000, ml_undecided(/*cheat!*/) );
	Augmenter result = ml_alloc( ml, sizeof( *result ) );
	result->ml = ml;
	result->pg = pg;
	result->ir = ir;
	result->diagnostics = diagnostics;
	result->top    = sk_new( result->ml );
	result->bottom = sk_new( result->ml );
	result->related    = cl_open( ir_nodeHeap(ir) );
	result->topDownStack = sk_new( result->ml );
	result->onlyProcessRelatedNodes = false;
	return result;
	}

static void aug_end( Augmenter aug )
	{
	cl_close( aug->related );
	ml_end( aug->ml );
	}

static Symbol irNodeSymbol( Object node, InheritanceRelation ir )
	{
	return ob_getTokenFieldX( node, SYM_SYMBOL, ir_nodeHeap(ir) );
	}

static int irNodeSideTableIndex( Object node, InheritanceRelation ir, ParserGenerator pg )
	{
	return pg_symbolSideTableIndex( pg, irNodeSymbol( node, ir ) );
	}

static void findTerminalNodes( void *augArg, Object node )
	{
	Augmenter aug = (Augmenter)augArg;
	ObjectHeap heap = ir_nodeHeap(aug->ir);
	if( ob_tag( node, heap ) != ir_nodeTag(aug->ir) )
		return;

	// This is a node "reachable" from the root set, so it's interesting
	//
	cl_check( aug->related, node );

	// Some symbols in the IR might not appear in the SST yet, so make sure they do
	//
	pg_sideTableEntry( aug->pg, irNodeSymbol( node, aug->ir ), aug->diagnostics );

	// If the node has no successors, it's a terminal node
	//
	Object successors = ob_getField( node, aug->direction, ir_nodeHeap( aug->ir ) );
	if( !successors || !ob_getElement( successors, IR_START_INDEX, ir_nodeHeap( aug->ir ) ) )
		sk_push( aug->terminalNodes, node );
	}

static bool propagationPredicate( void *augArg, Object tail, Symbol edgeSymbol, int edgeIndex, Object head )
	{
	bool result;
	Augmenter aug = (Augmenter)augArg;
	ObjectHeap heap = ir_nodeHeap( aug->ir );
	char *reason;
	if( ob_tag( tail, heap ) == ir_nodeTag(aug->ir) ) // Coming from an IR node
		{
		result = edgeSymbol == aug->direction; // Proceed only with the successor array that points the right way
		reason = "successor array";
		}
	else if( aug->onlyProcessRelatedNodes )
		{
		result = cl_isChecked( aug->related, head ); // Proceed only with related nodes
		reason = "related node";
		}
	else 
		{
		result = ob_tag( head, heap ) == ir_nodeTag(aug->ir); // Proceed with any IR node
		reason = "successor node";
		}
	File diagnostics = aug->diagnostics;
	if( 0 && diagnostics )
		{
		TRACE( diagnostics, "      propagationPredicate( " );
		ob_sendTo( tail, diagnostics, heap );
		TRACE( diagnostics, ", '%s', %d, ", edgeSymbol?( sy_name( edgeSymbol, theSymbolTable( theObjectHeap() /*cheat!*/) ) ): "(null)", edgeIndex );
		ob_sendTo( head, diagnostics, heap );
		TRACE( diagnostics, " ) = %s %s\n", result? "is":"is not", reason );
		}
	return result;
	}

static void pushOntoTopDownStack( void *augArg, Object node )
	{
	Augmenter aug = (Augmenter)augArg;
	ObjectHeap heap = ir_nodeHeap( aug->ir );
	if( ob_tag( node, heap ) != ir_nodeTag(aug->ir) )
		return;

	sk_push( aug->topDownStack, node );
	}

static int printObjectSymbolField( void *heapArg, Object ob, File fl )
	{
	ObjectHeap heap = (ObjectHeap)heapArg;
	return fl_write( fl, "'%s'", sy_name( ob_getTokenFieldX( ob, SYM_SYMBOL, heap ), oh_symbolTable( heap ) ) );
	}

static void sst_augment( InheritanceRelation ir, ParserGenerator pg, File diagnostics )
	{
	SymbolTable st = pg->st;
	Symbol superTags = sy_byIndex( SYM_SUPERTAGS, st );
	Symbol subTags   = sy_byIndex( SYM_SUBTAGS,   st );
	Augmenter aug = aug_begin( pg, ir, diagnostics );
	ObjectHeap heap = ir_nodeHeap( aug->ir );
	Stack rootSet = sk_new( aug->ml );

	if( diagnostics )
		{
		TRACE( diagnostics, "Augmenting symbol side table\n" );
		TRACE( diagnostics, "  Inheritance relation: {\n" );
		ir_sendTo( ir, diagnostics );
		TRACE( diagnostics, "  }\n" );
		/*
		TRACE( diagnostics, "  Inheritance relation object graph: {\n" );
		ob_sendDeepTo( ir_index(ir), diagnostics, heap );
		TRACE( diagnostics, "}\n" );
		*/
		}

	int i;
	for( i=1; i < sst_count( pg->sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		Object node = ob_getField( ir_index(ir), sste->sy, heap );
		if( node )
			sk_push( rootSet, node );
		}

	if( diagnostics )
		{
		TRACE( diagnostics, "  Root set: { " );
		sk_sendFormattedToX( rootSet, diagnostics, printObjectSymbolField, ir_nodeHeap(ir), ", " );
		TRACE( diagnostics, "} \n" );
		}

	TRACE( diagnostics, "  Walking upward to find top nodes and related nodes\n" );
	aug->direction     = superTags;
	aug->terminalNodes = aug->top;
	postorderWalk( sk_dup( rootSet, aug->ml ), propagationPredicate, findTerminalNodes, ir_nodeHeap(ir), aug );

	if( diagnostics )
		{
		TRACE( diagnostics, "    Top: { " );
		sk_sendFormattedToX( aug->top, diagnostics, printObjectSymbolField, ir_nodeHeap(ir), ", " );
		TRACE( diagnostics, " }\n" );
		}

	TRACE( diagnostics, "  Walking downward to find bottom nodes and related nodes\n" );
	aug->direction     = subTags;
	aug->terminalNodes = aug->bottom;
	postorderWalk( sk_dup( rootSet, aug->ml ), propagationPredicate, findTerminalNodes, ir_nodeHeap(ir), aug );

	if( diagnostics )
		{
		TRACE( diagnostics, "    Bottom: { " );
		sk_sendFormattedToX( aug->bottom, diagnostics, printObjectSymbolField, ir_nodeHeap(ir), ", " );
		TRACE( diagnostics, " }\n" );
		}

	TRACE( diagnostics, "  Computing top-down stack\n" );
	aug->direction = subTags;
	aug->onlyProcessRelatedNodes = true;
	postorderWalk( sk_dup( aug->top, aug->ml ), propagationPredicate, pushOntoTopDownStack, ir_nodeHeap(ir), aug );
	Stack topDown = aug->topDownStack; // alias for convenience

	if( diagnostics )
		{
		TRACE( diagnostics, "    Top-down: { " );
		sk_sendFormattedToX( topDown, diagnostics, printObjectSymbolField, ir_nodeHeap(ir), ", " );
		TRACE( diagnostics, " } \n" );
		}

	TRACE( diagnostics, "  Initializing newly-added side-table entries\n" );
	for( i=0; i < sk_depth( topDown ); i++ )
		{
		// Make sure every entry exists and contains itself in its first set
		SymbolSideTableEntry sste = pg_sideTableEntry( pg, irNodeSymbol( sk_item( topDown, i ), ir ), diagnostics );
		if( !sste->first )
			{
			TRACE( diagnostics, "    '%s'\n", sy_name( sste->sy, pg->st ) );
			sste->first = bv_new( sst_count( pg->sst ), aug->ml );
			bv_set( sste->first, pg_symbolSideTableIndex( pg, sste->sy ) );
			}
		}

	CheckList relatedNodes = aug->related;
	TRACE( diagnostics, "  Propagating side table info downward into temp table\n" );
	SymbolSideTable tempTable = sst_new( sst_count( pg->sst ), aug->ml );
	sst_clear( tempTable, sst_count( pg->sst ) );

	for( i=0; i < sk_depth( topDown ); i++ )
		{
		Object node = sk_item( topDown, i );
		int sstIndex = irNodeSideTableIndex( node, ir, pg );
		SymbolSideTableEntry sste = sst_element( tempTable, sstIndex );

		// Initialize entry
		//
		sste->sy = irNodeSymbol( node, ir );

		// Propagate info from all preds into this entry
		//
		propagateFromPreds( superTags, node, sste, relatedNodes, heap, pg, aug->diagnostics );
		}

	TRACE( diagnostics, "  Propagating side table info upward\n" );
	for( i = sk_depth( topDown )-1; i >= 0; i-- )
		{
		Object node = sk_item( topDown, i );
		SymbolSideTableEntry sste = pg_sideTableEntry( pg, irNodeSymbol( node, ir ), diagnostics );
		propagateFromPreds( subTags, node, sste, relatedNodes, heap, pg, aug->diagnostics );
		}

	TRACE( diagnostics, "  Merging temp table into main table\n" );
	for( i=0; i < sk_depth( topDown ); i++ )
		{
		Object node = sk_item( topDown, i );
		int sstIndex = irNodeSideTableIndex( node, ir, pg );
		SymbolSideTableEntry realEntry = sst_element( pg->sst,   sstIndex );
		SymbolSideTableEntry tempEntry = sst_element( tempTable, sstIndex );
		sste_propagate( realEntry, tempEntry, pg, aug->diagnostics );
		}

	aug_end( aug );

	TRACE( diagnostics, "Augmented SymbolSideTable:\n" );
	pg_sendSymbolSideTableTo( pg, diagnostics );
	}

#define INHERIT_DIRECTION SYM_SUBTAGS

static Stack getInheritingTags( Augmenter aug, Symbol tag, InheritanceRelation ir, int direction )
	{
	SymbolTable st = oh_symbolTable( ir_nodeHeap(ir) );
	Stack worklist = sk_new( aug->ml );
	aug->topDownStack = sk_new( aug->ml );
	Object curIRNode = ob_getField( ir_index(ir), tag, ir_nodeHeap(ir) );
	if( curIRNode )
		{
		sk_push( worklist, curIRNode );
		aug->direction = sy_byIndex( direction, st );
		postorderWalk( worklist, propagationPredicate, pushOntoTopDownStack, ir_nodeHeap(ir), aug );
		}
	return aug->topDownStack;
	}

static void addAllProductionCombos(
	Grammar newGrammar, Production newProduction,
	Grammar oldGrammar, Production oldProduction,
	int tokenIndex, InheritanceRelation ir, Symbol abstractSymbol, File diagnostics, int recursionDepth, bool unchangedSoFar )
	{
	SymbolTable st = oh_symbolTable( ir_nodeHeap(ir) );
	if( tokenIndex >= pn_length( oldProduction, oldGrammar ) )
		{
		// We're done with this production
		if( unchangedSoFar && optionalDetail("Abort new production %d because it matches one already in the original grammar", pn_index( newProduction, newGrammar ) ) )
			{
			if( diagnostics )
				{
				TRACE( diagnostics, "      %*sdiscarding %d already present: ", recursionDepth, "", pn_index( newProduction, newGrammar ) );
				pn_sendTo( newProduction, diagnostics, newGrammar, st );
				TRACE( diagnostics, "\n" );
				}
			pn_abort( newProduction, newGrammar );
			}
		else
			{
			pn_setConflictResolution( newProduction, CR_ABSTRACT, newGrammar );
			pn_setSymbol( newProduction, pn_symbol( oldProduction, oldGrammar ), newGrammar );
			pn_stopAppending( newProduction, newGrammar );
			if( diagnostics )
				{
				TRACE( diagnostics, "      %*sadding %d: ", recursionDepth, "", pn_index( newProduction, newGrammar ) );
				pn_sendTo( newProduction, diagnostics, newGrammar, st );
				TRACE( diagnostics, "\n" );
				}
			}
		}
	else
		{
		Symbol curToken = pn_token( oldProduction, tokenIndex, oldGrammar );
		SymbolTable st = oh_symbolTable( ir_nodeHeap(ir) );
		Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
		Augmenter aug = aug_begin( NULL, ir, diagnostics );

		Stack tags = getInheritingTags( aug, curToken, ir, SYM_SUBTAGS );
		while( sk_depth( tags ) )
			{
			Object tagNode = sk_pop( tags );
			Symbol subToken = ob_getTokenField( tagNode, symSymbol, ir_nodeHeap(ir) );
			if( subToken != curToken )
				{
				Production subProduction = newProduction;
				newProduction = pn_copy( newGrammar, newProduction, newGrammar, pn_lhs( newProduction, newGrammar ), tokenIndex );
				TRACE( diagnostics, "     %*s%d:%d sub %s for %s\n", recursionDepth, "", pn_index( subProduction, newGrammar ), tokenIndex, sy_name( subToken, st ), sy_name( curToken, st ) );
				pn_appendWithName( subProduction,
					pn_name  ( oldProduction, tokenIndex, oldGrammar ),
					subToken,
					newGrammar );
				addAllProductionCombos( newGrammar, subProduction, oldGrammar, oldProduction, tokenIndex+1, ir, abstractSymbol, diagnostics, recursionDepth+1, false );
				}
			}

		aug_end( aug );

		TRACE( diagnostics, "      %*s%d:%d use %s\n", recursionDepth, "", pn_index( newProduction, newGrammar ), tokenIndex, sy_name( curToken, st ) );
		pn_appendWithName( newProduction,
			pn_name( oldProduction, tokenIndex, oldGrammar ),
			curToken,
			newGrammar );
		addAllProductionCombos( newGrammar, newProduction, oldGrammar, oldProduction, tokenIndex+1, ir, abstractSymbol, diagnostics, recursionDepth+1, unchangedSoFar );
		}
	}

FUNC Grammar gr_augmentedRecursive( Grammar original, InheritanceRelation ir, Symbol abstractSymbol, MemoryLifetime ml, File diagnostics, bool recursive )
	{
	// TODO: Improve these # production estimates
	// TODO: Move to grammar.c

	Grammar result = NULL;
	if( gr_nestDepth( original ) > 0 )
		{
		Grammar outer = gr_outer( original );
		if( recursive )
			outer = gr_augmentedRecursive( outer, ir, abstractSymbol, ml, diagnostics, recursive );
		result = gr_nested( outer, gr_numProductions( original ) - gr_numOuterProductions( original ), ml );
		}
	else
		{
		result = gr_new( gr_goal( original ), gr_numProductions( original ), ml );
		}

	SymbolTable st = oh_symbolTable( ir_nodeHeap(ir) );
	if( diagnostics )
		{
		TRACE( diagnostics, "Augmenting grammar %p {\n", PH( original ) );
		gr_sendTo( original, diagnostics, st );
		TRACE( diagnostics, "}\n" );
		}

	TRACE( diagnostics, "  Adding implied productions at nest depth %d\n", gr_nestDepth( result ) );
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
	int productionIndex;
	for( productionIndex = gr_numOuterProductions( original ); productionIndex < gr_numProductions( original ); productionIndex++ )
		{
		Production pn = gr_production( original, productionIndex );
		if( diagnostics )
			{
			TRACE( diagnostics, "    Processing " );
			pn_sendTo( pn, diagnostics, original, st );
			TRACE( diagnostics, "\n" );
			}
		Production newProduction = pn_copy( original, pn, result, pn_lhs( pn, original ), 0 );
		addAllProductionCombos( result, newProduction, original, pn, 0, ir, abstractSymbol, diagnostics, 0, true );

		// Subtags of the lhs
		Augmenter aug = aug_begin( NULL, ir, diagnostics );

		Stack tags = getInheritingTags( aug, pn_lhs( pn, original ), ir, SYM_SUBTAGS );
		while( sk_depth( tags ) )
			{
			Object tagNode = sk_pop( tags );
			Symbol subLhs = ob_getTokenField( tagNode, symSymbol, ir_nodeHeap(ir) );
			if( subLhs != pn_lhs( pn, original ) )
				{
				TRACE( diagnostics, "    Now with lhs=%s\n", sy_name( subLhs, st ) );
				newProduction = pn_copy( original, pn, result, subLhs, 0 );
				addAllProductionCombos( result, newProduction, original, pn, 0, ir, abstractSymbol, diagnostics, 0, false );
				}
			}

		aug_end( aug );
		}

	gr_stopAdding( result );
	if( gr_numProductions( result ) == 0 )
		{
		TRACE( diagnostics, "  No implied productions -- returning original grammar\n" );
		result = original;
		}
	else
		{
		result = gr_nested( result, gr_numProductions( original ), ml );
		TRACE( diagnostics, "  Adding original productions at nest depth %d\n", gr_nestDepth( result ) );
		for( productionIndex = gr_numOuterProductions( original ); productionIndex < gr_numProductions( original ); productionIndex++ )
			pn_dup( original, gr_production( original, productionIndex ), result );

		gr_stopAdding( result );

		if( diagnostics )
			{
			TRACE( diagnostics, "Augmented grammar %p from %p {\n", PH( result ), PH( original ) );
			gr_sendTo( result, diagnostics, st );
			TRACE( diagnostics, "}\n\n" );
			}
		}
	return result;
	}

static int au_sendReportTo( Automaton au, File fl, ParserGenerator pg )
	{
	if( !fl )
		return 0;

	int charsSent = 0;
#ifdef ITEM_SET_NUMS
	ObjectHeap heap = pg->heap;
	SymbolTable st  = pg->st;
	MemoryLifetime reportTime  = ml_begin( 1000, ml_indefinite() );
	BitVector      edgeSymbols = bv_new( 0, reportTime );

	fl_write( fl, "start state%d\n", ob_getIntFieldX( au->startState, SYM_ITEM_SET_NUM, heap ) );

	int itemSetNum;
	for( itemSetNum=0; itemSetNum < itst_count( pg->itemSets ); itemSetNum++ )
		{
		fl_write( fl, "  state%d\n", itemSetNum );

		// Items
		//
		ItemSet its = itst_element( pg->itemSets, itemSetNum );
		bv_sendFormattedToX( its->items, fl, sendItem, pg, "", "", "" );

		// Edges
		//
		Object state = its->stateNode;
		bv_clear( edgeSymbols );
		ob_getFieldSymbols( state, edgeSymbols, heap );
		int edge;
		for( edge = bv_firstBit( edgeSymbols ); edge != bv_END; edge = bv_nextBit( edgeSymbols, edge ) )
			{
			Symbol edgeSymbol = sy_byIndex( edge, st );
			Object nextState  = ob_getField( state, edgeSymbol, heap );
			if( ob_isInt( nextState, heap ) )
				{
				// This is a special field of some sort, like SYM_ITEM_SET_NUM.  Ignore.
				bv_unset( edgeSymbols, edge );
				}
			else if( ob_isToken( nextState, heap ) )
				{
				// This is a reduce node.  Leave it in edgeSymbols and process it on the next loop.
				}
			else
				{
				// %14s is enough for PARAMETER_LIST
				fl_write( fl, "    %14s -> state%d\n", sy_name( edgeSymbol, st ), ob_getIntFieldX( nextState, SYM_ITEM_SET_NUM, heap ) );
				bv_unset( edgeSymbols, edge );
				}
			}
		for( edge = bv_firstBit( edgeSymbols ); edge != bv_END; edge = bv_nextBit( edgeSymbols, edge ) )
			{
			Symbol reduceSymbol = ob_getTokenFieldX( state, edge, heap );
			fl_write( fl, "    Reduce %s:", sy_name( reduceSymbol, st ) );
			int equivalentEdge;
			for( equivalentEdge = edge; equivalentEdge != bv_END; equivalentEdge = bv_nextBit( edgeSymbols, equivalentEdge ) )
				{
				Symbol equivalentEdgeSymbol = sy_byIndex( equivalentEdge, st );
				if( ob_getTokenField( state, equivalentEdgeSymbol, heap ) == reduceSymbol )
					{
					fl_write( fl, " %s", sy_name( equivalentEdgeSymbol, st ));
					bv_unset( edgeSymbols, equivalentEdge );
					}
				}
			fl_write( fl, "\n" );
			}
		}
	ml_end( reportTime );
#endif
	return charsSent;
	}

FUNC Automaton au_new( Grammar gr, SymbolTable st, ObjectHeap heap, MemoryLifetime ml, OptionSet os, File conflictLog, File diagnostics )
	{
	TRACE( diagnostics, "Generating automaton for {\n" );
	gr_sendTo( gr, diagnostics, st );
#ifdef NDEBUG
	MemoryLifetime generateTime = ml_begin( 100000, ml );
#else
	MemoryLifetime generateTime = ml;
#endif
	TRACE( diagnostics, "}\n" );

	if( os_log( os, on_PARSER_GEN, "Generating automaton\n\n" ) )
		{
		gr_sendTo( gr, os_logFile( os, on_PARSER_GEN ), st );
		os_log( os, on_PARSER_GEN, "\n" );
		}

	Automaton result = (Automaton)ml_alloc( ml, sizeof(*result) );
	char stateTagName[50];
	sprintf( stateTagName, "SN%d", st_count( st ) );
	Symbol stateNodeTag = sy_byName( stateTagName, st ); // TODO: Use some smart instance shape

	ParserGenerator pg = pg_new( gr, st, stateNodeTag, generateTime, ml, theObjectHeap() );
	pg_populateItemTable( pg, diagnostics );
	pg_populateSymbolSideTable( pg, diagnostics );
	if(0) sst_augment( oh_inheritanceRelation( pg->heap ), pg, diagnostics );
	Object startState = pg_computeLR0StateNodes( pg, diagnostics );
	pg_computeFirstSets( pg, diagnostics );
	if(0) sst_augment( oh_inheritanceRelation( pg->heap ), pg, diagnostics );
	pg_computeFollowSets( pg, diagnostics );
	if(0) sst_augment( oh_inheritanceRelation( pg->heap ), pg, diagnostics );
	pg_computeSLRLookaheads( pg, diagnostics );
	pg_computeReduceActions( pg, conflictLog, diagnostics );

	result->gr = gr;
	result->stateHeap  = theObjectHeap();
	result->startState = startState;
	result->parsers    = psa_new( 2, ml );
	if (diagnostics)
		{
		TRACE( diagnostics, "Finished generating automaton %p:\n\n", PH( result ) );
		au_sendTo( result, diagnostics, theObjectHeap(), st );
		if( 0 )
			pg_sendDotTo( pg, diagnostics );
		TRACE( diagnostics, "\n\n" );
		}
	if( os_log( os, on_PARSER_GEN, "Finished automaton %p ", PH( result ) ) )
		{
		au_sendReportTo( result, os_logFile( os, on_PARSER_GEN ), pg );
		os_log( os, on_PARSER_GEN, "\n" );
		}

	if( generateTime == ml )
		result->pg = pg;
	else
		{
		ml_end( generateTime );
		result->pg = NULL;
		}

	if(0) au_augment( result, oh_inheritanceRelation( pg->heap ), st, diagnostics );
	return result;
	}

FUNC Grammar au_grammar( Automaton au )
	{
	return au->gr;
	}

FUNC Parser ps_new( Automaton au, MemoryLifetime ml, File diagnostics )
	{
	Parser result = NULL;
	// Note that recycling parsers doesn't violate ALWAYS_NEW because we only
	// reuse parsers that should never again be used by the program, and so if
	// this function returns an alias to a parser that's still in use, then
	// that's not the only bug in the program.
	int i;
	TRACE( diagnostics, "PARSER Scanning %d existing parsers on %p\n", psa_count( au->parsers ), PH( au ) );
	for( i = 0; i < psa_count( au->parsers ) && !result; i++ )
		{
		Parser ps = psa_get( au->parsers, i );
		if( sk_depth( ps->stateStack ) == 0 )
			{
			result = ps;
			TRACE( diagnostics, "PARSER Found existing parser %p on %p @ %d\n", PH( result ), PH( au ), i );
			}
		else
			{
			TRACE( diagnostics, "PARSER Existing parser %p on %p @ %d has depth %d\n", PH( ps ), PH( au ), i, sk_depth( ps->stateStack ) );
			}
		}
	if( !result )
		{
		result = (Parser)ml_alloc( ml, sizeof(*result) );
		result->au = au;
		result->stateStack   = sk_new( ml );
		result->operandStack = sk_new( ml );
		TRACE( diagnostics, "PARSER Allocated new parser %p on %p @ %d\n", PH( result ), PH( au ), psa_count( au->parsers ) );
		psa_append( au->parsers, result );
		}
	sk_push( result->stateStack, au->startState );
	result->diagnostics = diagnostics;
	result->detailedDiagnostics = NULL;
	return result;
	}

FUNC void ps_close( Parser ps )
	{
	TRACE( ps->diagnostics, "PARSER Freed parser %p\n", PH( ps ) );
	sk_popAll( ps->stateStack );
	sk_popAll( ps->operandStack );
	}

FUNC Automaton ps_automaton( Parser ps )
	{
	return ps->au;
	}

FUNC Stack ps_operandStack( Parser ps )
	{
	return ps->operandStack;
	}

#if 0
FUNC Stack ps_stateStack( Parser ps )
	{
	return ps->stateStack;
	}
#endif

#ifdef ITEM_SET_NUMS
static int ps_itemSetNum( Parser ps, int depth, Symbol isn )
	{
	ObjectHeap heap = theObjectHeap(); // Cheating a bit
	return ob_getIntField( sk_item( ps->stateStack, depth ), isn, heap );
	}

static int ps_itemSetNumX( Parser ps, int depth, SymbolIndex isnIndex )
	{
	ObjectHeap heap = theObjectHeap(); // Cheating a bit
	return ob_getIntFieldX( sk_item( ps->stateStack, depth ), isnIndex, heap );
	}
#endif

static Object ps_nextState( Parser ps, Object ob )
	{
	ObjectHeap oh = ps->au->stateHeap;
	Object curState = sk_top( ps->stateStack );
	Symbol tag = ob_tag( ob, oh );
	if( ps->detailedDiagnostics )
		{
		TRACE( ps->detailedDiagnostics, "NEXT STATE from %d ob: ", ps_itemSetNumX( ps, 0, SYM_ITEM_SET_NUM ) );
		ob_sendTo( ob, ps->detailedDiagnostics, theObjectHeap() );
		TRACE( ps->detailedDiagnostics, "\n" );
		}
	Object result = NULL;
	if( ob_isToken( ob, oh ) )
		{
		// If the symbol represented by this token object is expected, use that in preference to SYM_TOKEN
		result = ob_getField( curState, ob_toSymbol( ob, oh ), oh );
		}
	if( !result )
		result = ob_getField( curState, tag, oh );
	if( !result && ob_isPlaceholder( ob, oh ) )
		{
		// If SYM_PLACEHOLDER is not expected, try the placeholder's TAG field
		result = ob_getField( curState, ob_getTokenFieldX( ob, SYM_TAG, oh ), oh );
		}
	if( ps->detailedDiagnostics )
		{
		TRACE( ps->detailedDiagnostics, "  result: " );
		ob_sendTo( result, ps->detailedDiagnostics, theObjectHeap() );
		TRACE( ps->detailedDiagnostics, "\n" );
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
	sk_push( ps->operandStack, ob );
	Object nextState = ps_nextState( ps, ob );
	if( !nextState || ob_isInt( nextState, oh ) )
		{
		ParserGenerator pg = ps_automaton(ps)->pg;
		if( pg )
			{
			// Extended debug info
			int i,j,k;
			int charsSent = 0;
			File fl = ps->diagnostics;
			if( !fl )
				fl = stderr;
			charsSent += fl_write( fl, "\nUnexpected '%s': ", sy_name( ob_tag( ob, oh ), pg->st ) );
			charsSent += ob_sendTo( ob, fl, oh );
			charsSent += fl_write( fl, "\nState:\n" );
			Symbol isn = sy_byIndex( SYM_ITEM_SET_NUM, pg->st );
			SymbolVector  follow = bv_new( sst_count(pg->sst),  ml_undecided() );
			ItemVector curItems  = bv_new( gr_numItems(pg->gr), ml_undecided() );
			ItemVector nextItems = bv_new( gr_numItems(pg->gr), ml_undecided() );
			bool firstIteration = true; // easiest to initialize during first iteration of loop below
			// Too bad the nextItems logic requires us to print the innermost frame first...
			for( i = 0; i < sk_depth( ps->stateStack ); i++ )
				{
				charsSent += fl_write( fl, "  -- " );
				charsSent += ob_sendTo( sk_item( ps_operandStack( ps ), i ), fl, oh );
				charsSent += fl_write( fl, " --\n" );

				bv_clear( nextItems );
				int itemSetNum = ps_itemSetNum( ps, i, isn );
				charsSent += fl_write( fl, "     " ITEM_STATE_PREFIX "%d:\n", itemSetNum );
				ItemSet its = itst_element( pg->itemSets, itemSetNum );
				for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
					{
					Item it = ita_element( pg->items, j );
					// Figure out what stuff to print
#if 1
					if( !bv_isSet( curItems, j ) && !firstIteration ) // TODO: Why doesn't this logic work with inheritance?
						continue;
#endif
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
							if( lhs->expectingItems )
								bv_or( nextItems, lhs->expectingItems );
							}
							// fall through
						default:
							bv_set( nextItems, j-1 );
							break;
						}

					// Print it
					charsSent += fl_write( fl, "    " );
					charsSent += pn_sendItemTo( it->pn, it->dot, fl, pg->gr, pg->st );
					charsSent += fl_write( fl, "  [ " );
					char *sep = "";
					bv_clear( follow );
					it_getFollow( it, follow, pg, NULL );
					for( k = bv_firstBit( follow ); k != bv_END; k = bv_nextBit( follow, k ) )
						{
						SymbolSideTableEntry sste = sst_element( pg->sst, k );
						charsSent += fl_write( fl, "%s'%s'", sep, sy_name( sste->sy, pg->st ) );
						sep = " ";
						}
					charsSent += fl_write( fl, " ]\n" );
					}
				firstIteration = false;
				bv_copy( curItems, nextItems );
				}
			fl_write( fl, "\n" );
			}
		}
	check( nextState );
	sk_push( ps->stateStack, nextState );
	}

FUNC Symbol ps_handle( Parser ps, Object lookahead )
	{
	ObjectHeap oh = ps->au->stateHeap;
	Object nextState = ps_nextState( ps, lookahead );
	if( nextState && ob_isToken( nextState, oh ) )
		return ob_toSymbol( nextState, oh );
	else
		return NULL;
	}

FUNC void ps_popN( Parser ps, int objectCount )
	{
	assert( sk_depth( ps->stateStack ) >= objectCount+1 ); // must always leave the startState on the stack
	sk_popN( ps->stateStack, objectCount );
	sk_popN( ps->operandStack, objectCount );
	}

FUNC int au_sendTo( Automaton au, File fl, ObjectHeap heap, SymbolTable st )
	{
	return ob_sendDeepTo( au->startState, fl, heap );
	}

FUNC int ps_sendTo( Parser ps, File fl, ObjectHeap heap, SymbolTable st )
	{
	if( !fl )
		return 0;

	int charsSent = 0; //fl_write( fl, "%p(%p): ", PH( ps ), PH( ps_automaton(ps) ) );
	//sk_sendTo( ps->stateStack, fl, heap );
#ifdef ITEM_SET_NUMS
	int i;
	char *sep = ITEM_STATE_PREFIX;
	for( i=0; i < sk_depth( ps->stateStack ); i++ )
		{
		charsSent += fl_write( fl, "%s%d", sep, ps_itemSetNumX( ps, i, SYM_ITEM_SET_NUM ) );
		sep = " " ITEM_STATE_PREFIX;
		}
#endif
	return charsSent;
	}

#ifdef REDUCE_CONTEXT_LENGTH
FUNC int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st )
	{
	return ob_getIntField( sk_top( ps->stateStack ), sy_byIndex( SYM_REDUCE_CONTEXT_LENGTH, st ), heap );
	}
#endif

#ifdef PARSER_T

typedef char *TestGrammarLine[10];

#if 0
static TestGrammarLine grammar[] =
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
static TestGrammarLine grammar[] =
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
static TestGrammarLine grammar[] =
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
static TestGrammarLine grammar[] =
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

static TestGrammarLine grammar2[] =
	{
	{ ":INT",    ":INT", "*", ":INT" },
	{ ":INT",    ":INT", "/", ":INT" },
	};
#endif

#if 0
static TestGrammarLine grammar[] =
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

static TestGrammarLine grammar2[] =
	{
	{ ":INT",         ":INT", "*", ":INT" },
	{ ":INT",         ":INT", "/", ":INT" },
	{ ":INT",         "(", ":INT", ")" },
	};

static TestGrammarLine subtags[] =
	{
	{ ":NUMBER",      ":INT" },
	{ ":INT",         ":NATURAL" },
	};
#endif

#if 0
static TestGrammarLine grammar[] =
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
static TestGrammarLine grammar[] =
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
static TestGrammarLine grammar[] = // http://www.scribd.com/doc/7185137/First-and-Follow-Set
	{
	{ "S", "a", "A", "B", "e" },
	{ "A", "A", "b", "c" },
	{ "A", "b" },
	{ "B", "d" },
	};
#endif

#if 0
static TestGrammarLine grammar[] = // LR0
	{
	{ "S", "E", "#" },
	{ "E", "E", "-", "T" },
	{ "E", "T" },
	{ "T", "n" },
	{ "T", "(", "E", ")" },
	};
#endif

#if 0
static TestGrammarLine grammar[] =
	{
	{ "GOAL",   "ITEM" },
	{ "ITEM",   "letter" },
	{ "ITEM",   "LIST" },
	{ "LIST",   "(", ")" },
	{ "LIST",   "(", "ITEMS", ")" },
	{ "ITEMS",  "ITEM" },
	{ "ITEMS",  "ITEMS", "ITEM" },
	};
#endif

#if 0
static TestGrammarLine grammar[] =
	{
	//{ "GOAL",   "LIST" },
	{ "LIST",   "(", "ITEMS", ")" },
	{ "LIST",   "(", ")" },
	{ "ITEMS",  "ITEM" },
	{ "ITEMS",  "ITEMS", "ITEM" },
	{ "ITEM",   "var" },
	{ "ITEM",   "LIST" },
	};
#endif

#if 1

typedef struct
	{
	char *tokens[10];
	char *response;
	} SheppardGrammarLine;

// Sheppard
TestGrammarLine grammar_hello[] =
	{
	{ "GOAL",   "hello", "world" },
	};

SheppardGrammarLine grammar[] =
	{
	// Builtins
	{{ ":PROGRAM",      "STATEMENTS",     ":EOF" }},
	{{  "STATEMENTS",   "STATEMENTS",      "STATEMENT" },            "eat2" },
	{{  "STATEMENTS",   "STATEMENT" },                               "eat1" },
	{{ "BEGIN_MARKER",  "begin" }},
	{{ ":OBJECT",       "BEGIN_MARKER", "STATEMENTS", "return", "result:OBJECT",  "end" },  "compound_expr" }, // This one doesn't need to be fully polymorphic.  That's wasteful.
	{{ ":BOOLEAN",      "BEGIN_MARKER", "STATEMENTS", "return", "result:BOOLEAN", "end" },  "compound_expr" },
	{{  "STATEMENT",    "BEGIN_MARKER", "STATEMENTS",                             "end" },  "compound_stmt" },
	{{ ":ENVIRONMENT",  "current_environment" }},
	{{ ":OBJECT",       "base:OBJECT",     "field:SYMBOL", "get" }}, // Syntactic sugar for "base field ERROR take"
	{{ ":OBJECT",       "base:OBJECT",     "field:OBJECT", "TAKE_FAILED@default", "take" }}, // If field is a SYMBOL and base has that field, get it; otherwise return default
	{{ ":OBJECT",       "base:OBJECT",     "field:OBJECT", "default:EOF",         "take" }}, // If field is a SYMBOL and base has that field, get it; otherwise return default
	{{  "STATEMENT",    "value:OBJECT",    "base:OBJECT",  "field:SYMBOL", "put" }}, // "set" is a python keyword which is awkward
	{{ ":SYMBOL",       "obj:OBJECT", "tag" }},
	{{ ":SYMBOL",       "obj:OBJECT", "tag_edge_symbol" }},
	{{ ":ENVIRONMENT",  "outer:ENVIRONMENT", "Environment" }},
	{{ ":LIST",         "head:OBJECT",     "tail:LIST", "Cons" }},
	{{ ":PROCEDURE",    "tokens:LIST",     "dialect:STATE", "environment:ENVIRONMENT", "Procedure" }},
	{{ ":DIGRESSION",   "tokens:LIST",     "bindings:ENVIRONMENT", "prev:DIGRESSION", "Digression" }},
	{{ ":ACTIVATION",   "cursor:DIGRESSION",  "operands:LIST", "history:LIST", "scope:ENVIRONMENT", "caller:ACTIVATION", "Activation" }},
	{{ ":THREAD",       "activation:ACTIVATION", "Thread" }},
	// Syntactic sugar
	{{  "STATEMENT",    "value:OBJECT", "symbol:SYMBOL", "bind" }},

	// Procedures from the self-interpreter

	{{ ":OBJECT",       "base:OBJECT", "field:SYMBOL", "pop_list" }},

	{{  "STATEMENT",    "th:THREAD", "remaining_tokens:NULL", "finish_digression" },            "finish_digression_NULL" },
	{{  "STATEMENT",    "th:THREAD", "remaining_tokens:LIST", "finish_digression" },            "finish_digression_LIST" },

	{{  "STATEMENT",    "formal_arg:SYMBOL", "actual_arg:OBJECT", "arg_bindings:BINDINGS", "bind_arg" },   "bind_arg_SYMBOL" },
	{{  "STATEMENT",    "formal_arg:NULL",   "actual_arg:OBJECT", "arg_bindings:BINDINGS", "bind_arg" },   "bind_arg_NULL" },
	{{  "STATEMENT",    "th:THREAD", "formal_args:NULL", "arg_bindings:BINDINGS", "bind_args" },     "bind_args_NULL" },
	{{  "STATEMENT",    "th:THREAD", "formal_args:LIST", "arg_bindings:BINDINGS", "bind_args" },     "bind_args_LIST" },
	{{ ":OBJECT",       "obj:OBJECT", "environment:ENVIRONMENT", "probe:OBJECT",      "bound2" },     "bound2_OBJECT" },
	{{ ":OBJECT",       "obj:OBJECT", "environment:ENVIRONMENT", "TAKE_FAILED",       "bound2" },     "bound2_TAKE_FAILED" },
	{{ ":OBJECT",       "obj:OBJECT", "environment:NULL",        "bound" },     "bound_NULL" },
	{{ ":OBJECT",       "obj:OBJECT", "environment:ENVIRONMENT", "bound" },     "bound_ENVIRONMENT" },

	{{  "STATEMENT",    "state:STATE", "obj:OBJECT", "probe:STATE",   "next_state2"},  "next_state2_STATE" },
	{{  "STATEMENT",    "state:STATE", "obj:OBJECT", "TAKE_FAILED",   "next_state2"},  "next_state2_TAKE_FAILED" },
	{{  "STATEMENT",    "state:STATE", "obj:OBJECT", "next_state"}},

	{{  "STATEMENT",    "th:THREAD", "action:PRIMITIVE", "environment:ENVIRONMENT", "do_action" }, "do_action_PRIMITIVE" },
	{{  "STATEMENT",    "th:THREAD", "action:MACRO",     "environment:ENVIRONMENT", "do_action" }, "do_action_MACRO" },

	{{ ":BOOLEAN",      "th:THREAD", "ACCEPT",  "perform" },   "perform_ACCEPT" },
	{{ ":BOOLEAN",      "th:THREAD", "SHIFT",   "perform" },   "perform_SHIFT" },
	{{ ":BOOLEAN",      "th:THREAD", "REDUCE0", "perform" },   "perform_REDUCE0" },

	{{ ":BOOLEAN",      "th:THREAD", "probe:FALSE", "execute2" },   "execute2_FALSE" },
	{{ ":BOOLEAN",      "th:THREAD", "probe:TRUE",  "execute2" },   "execute2_TRUE" },

	{{ "STATEMENT",    "procedure:PROCEDURE", "environment:ENVIRONMENT", "scope:ENVIRONMENT", "execute" }},

	{{ "STATEMENT",    ":OBJECT",  "pop" }},
	{{ "STATEMENT",    ":BOOLEAN", "pop" }},

	};

static TestGrammarLine subtags[] =
	{
	{ ":OBJECT",      ":LIST", ":PROCEDURE", ":DIGRESSION", ":ENVIRONMENT", ":BINDINGS", ":SYMBOL", ":STATE", ":ACTIVATION", ":THREAD" },
	{ ":OBJECT",      ":MACRO", ":PRIMITIVE", ":EOF" },
	{ ":LIST",        ":NULL" },
	{ ":ENVIRONMENT", ":NULL" },
	{ ":ACTIVATION",  ":NULL" },
	{ ":DIGRESSION",  ":NOTHING" },
	{ ":STATE",       ":SHIFT", ":REDUCE0", ":ACCEPT" },
	{ ":BOOLEAN",     ":FALSE", ":TRUE" },
	};

TestGrammarLine grammar_old2[] =
	{
	{ "STATEMENTS",   "STATEMENTS", "STATEMENT" },
	{ "STATEMENTS",   "STATEMENT" },
	{ "STATEMENT",    "bind", "name:SYMBOL", "to", "value:OBJECT" },
	//{ "OBJECT",       "get", "name:SYMBOL" },
	{ "OBJECT",       "field", "field:SYMBOL", "in", "base:OBJECT" },
	{ "NULL",         "null" },
	{ "LIST",         "Cons", "head:OBJECT", "tail:LIST" },
	{ "PROCEDURE",    "Procedure", "tokens:LIST", "in", "dialect:STATE" },
	{ "DIGRESSION",   "Digression", "tokens:LIST", "with", "bindings:CONTEXT", "before", "prev:DIGRESSION" },
	{ "STATEMENT",    "execute", "SHIFT" },
	{ "STATEMENT",    "execute", "REDUCE" },
	{ "STATEMENT",    "execute", "ACCEPT" },
	//{ "LIST",         "list", "name:SYMBOL" },
	//{ "PROCEDURE",    "procedure", "name:SYMBOL" },
	//{ "DIGRESSION",   "digression", "name:SYMBOL" },
	//{ "CONTINUATION", "CONTINUATION", "cursor:OBJECT", "value_stack:LIST", "state_stack:LIST" },
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
		bv_sendFormattedTo( it->lookahead, traceFile, sendSymBySSTIndex, pg );
		fl_write( traceFile, "\n" );
		}
	}

static char *dup( char *str, int maxLength )
	{
	int length = min( maxLength, strlen(str) );
	char *result = ml_alloc( ml_undecided(), length+1 );
	memcpy( result, str, length );
	result[ length ] = 0;
	return result;
	}

int main( int argc, char *argv[] )
	{
	int i, j; SymbolTable st; Symbol goal; Grammar gr; ParserGenerator pg; ObjectHeap heap;
	File traceFile = fdopen( 3, "wt" );
	File outFile   = stdout;
	File dotFile = fdopen( 4, "wt" );
	File tabFile = fdopen( 5, "wt" );

	heap = theObjectHeap();
	st = theSymbolTable( heap );
	goal = sy_byName( grammar[0].tokens[0], st );
	gr = gr_new( goal, asizeof( grammar ), ml_indefinite() );
	for( i=0; i < asizeof( grammar ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( grammar[i].tokens[0], st ), 10 );
		for( j=1; grammar[i].tokens[j]; j++ )
			{
			char *token = grammar[i].tokens[j];
			char *colon = strchr( token, ':' );
			char *at_sign = strchr( token, '@' );
			if( colon )
				{
				char *name = dup( token, colon-token );
				char *tag  = colon; // tag includes colon!
				pn_appendWithName( pn, sy_byName( name, st ), sy_byName( tag, st ), gr );
				}
			else if( at_sign )
				{
				char *name = at_sign+1;
				char *tag  = dup( token, at_sign-token );
				pn_appendWithName( pn, sy_byName( name, st ), sy_byName( tag, st ), gr );
				}
			else
				{
				pn_append( pn, sy_byName( token, st ), gr );
				}
			}
		pn_stopAppending( pn, gr );
		pn_setConflictResolution( pn, CR_SHIFT_BEATS_REDUCE, gr );
		char *response = grammar[i].response;
		if( !response )
			response = grammar[i].tokens[j-1];
		//pn_autoSymbol( pn, st, gr );
		pn_setSymbol( pn, sy_byName( response, st ), gr );
		}
	gr_stopAdding( gr );
#if 0 // grammar2
	gr = gr_nested( gr, asizeof( grammar2 ), ml_indefinite() );
	for( i=0; i < asizeof( grammar2 ); i++ )
		{
		Production pn = pn_new( gr, sy_byName( grammar2[i][0], st ), 10 );
		for( j=1; grammar2[i][j]; j++ )
			pn_append( pn, sy_byName( grammar2[i][j], st ), gr );
		pn_stopAppending( pn, gr );
		pn_setConflictResolution( pn, CR_SHIFT_BEATS_REDUCE, gr );
		pn_autoSymbol( pn, st, gr );
		}
	gr_stopAdding( gr );
#endif
	//gr_sendTo( gr, traceFile, st );

#if 1 // subtags
	InheritanceRelation ir = oh_inheritanceRelation( theObjectHeap() );
	for( i=0; i < asizeof( subtags ); i++ )
		{
		char **line = subtags[ i ];
		Symbol superSym = sy_byName( line[0], st );
		int subIndex;
		char *subName;
		for( subIndex = 1; NULL != ( subName = line[ subIndex ] ); subIndex++ )
			{
			Symbol subSym = sy_byName( subName, st );
			ir_add( ir, superSym, subSym );
			}
		}
	fl_write( traceFile, "Inheritance relation:\n" );
	ob_sendDeepTo( ir_index(ir), traceFile, heap );

	if(1)
		{
		Symbol sym_abstract = sy_byName( "ABSTRACT_PRODUCTION", st );
		gr = gr_augmented( gr, ir, sym_abstract, ml_indefinite(), traceFile );
		fl_write( traceFile, "Augmented grammar:\n" );
		gr_sendTo( gr, traceFile, st );
		}

#endif

	if( true )
		{
		char stateTagName[50];
		sprintf( stateTagName, "SN_TEST_%d", st_count( st ) );
		Symbol stateNodeTag = sy_byName( stateTagName, st );
		pg = pg_new( gr, st, stateNodeTag, ml_begin( 10000, ml_indefinite() ), ml_indefinite(), theObjectHeap() );

		pg_populateItemTable( pg, traceFile );
		if(0)
			{
			fl_write( traceFile, "Items:\n" );
			for( i=0; i < ita_count( pg->items ); i++ )
				{
				Item it = ita_element( pg->items, i );
				fl_write( traceFile, "  %3d: ", i );
				pn_sendItemTo( it->pn, it->dot, traceFile, gr, st );
				fl_write( traceFile, "\n" );
				}
			}
		fl_write( traceFile, "  rightmostItems: " );
		bv_sendFormattedTo( pg->rightmostItems, traceFile, sendBitNumber, "i%d" );
		fl_write( traceFile, "\n" );

		pg_populateSymbolSideTable( pg, traceFile );
		fl_write( traceFile, "SymbolSideTable:\n" );
		for( i=1; i < sst_count( pg->sst ); i++ )
			{
			SymbolSideTableEntry sste = sst_element( pg->sst, i );
			int symbolIndex = sy_index( sste->sy, st );
			if( pg->sstIndexes[ symbolIndex ] == i )
				{
				fl_write( traceFile, "  %3d: '%s'\n", i, sy_name( sste->sy, st ) );
				if( sste->leftmostItems )
					{
					fl_write( traceFile, "     leftmostItems: " );
					bv_sendFormattedTo( sste->leftmostItems, traceFile, sendBitNumber, "i%d" );
					fl_write( traceFile, "\n" );
					}
				if( sste->expectingItems )
					{
					fl_write( traceFile, "    expectingItems: " );
					bv_sendFormattedTo( sste->expectingItems, traceFile, sendBitNumber, "i%d" );
					fl_write( traceFile, "\n" );
					}
				}
			else
				{
				fl_write( traceFile, "INDEX MISMATCH: entry %d is symbol %s. index %d, which has entry %d\n",
					i, sy_name( sste->sy, st ), symbolIndex, pg->sstIndexes[ symbolIndex ] );
				}
			}

		pg_computeLR0StateNodes( pg, traceFile );

		pg_computeFirstSets( pg, traceFile );
		pg_computeFollowSets( pg, traceFile );
		fl_write( traceFile, "Symbol sets:\n" );
		for( i=1; i < sst_count( pg->sst ); i++ )
			{
			SymbolSideTableEntry sste = sst_element( pg->sst, i );
			fl_write( traceFile, "  %3d: '%s'\n", i, sy_name( sste->sy, st ) );
			fl_write( traceFile, "     first: " );
			bv_sendFormattedTo( sste->first, traceFile, sendSymBySSTIndex, pg );
			fl_write( traceFile, "\n" );
			fl_write( traceFile, "     follow: " );
			bv_sendFormattedTo( sste->follow, traceFile, sendSymBySSTIndex, pg );
			fl_write( traceFile, "\n" );
			}

		pg_computeSLRLookaheads( pg, traceFile );
		dumpItemLookaheads( pg, traceFile );
		pg_computeReduceActions( pg, traceFile, traceFile );
		ob_sendDeepTo( itst_element( pg->itemSets, 0 )->stateNode, traceFile, pg->heap );

		pg_sendPythonTo ( pg, outFile );
		pg_sendDotTo    ( pg, dotFile );
		pg_sendTableTo  ( pg, tabFile );
		}

	Automaton au = au_new( gr, st, heap, ml_indefinite(), NULL, traceFile, traceFile );
	fl_write( traceFile, "Automaton:\n" );
	au_sendTo( au, traceFile, heap, st );

#if 0
	fl_write( traceFile, "Parsing...\n" );
	Parser ps = ps_new( gr, st, ml_indefinite(), traceFile );
	ObjectHeap heap = theObjectHeap();
	static char *sentence[] = { "1", "+", "2", "*", "2", "/", "2", "+", "3" };
	int stop = asizeof( sentence );
	for( i=0; i <= stop; i++ )
		{
#ifdef ITEM_SET_NUMS
		fl_write( traceFile, "State is %d\n", ob_getIntField(
			sk_top( ps->stateStack ),
			sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ) );
#endif
		char *cur  = (i >= stop)?   ":END_OF_INPUT" : sentence[i];
		char *next = (i+1 >= stop)? ":END_OF_INPUT" : sentence[i+1];
		fl_write( traceFile, "  Token: %s\n", cur );
		Symbol sy = sy_byName( cur, st );
		ps_push( ps, oh_symbolToken( heap, sy ) );
#ifdef ITEM_SET_NUMS
		fl_write( traceFile, " -- new state is %d\n", ob_getIntField(
			sk_top( ps->stateStack ),
			sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ) );
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
			fl_write( traceFile, " -- new state is %d\n", ob_getIntField(
				sk_top( ps->stateStack ),
				sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ) );
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

