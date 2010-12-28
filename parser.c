
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include <string.h>

typedef BitVector ItemVector;   // BitVectors of item indexes
typedef BitVector SymbolVector; // BitVectors of symbol side-table indexes

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
	ItemVector leftmostItems;  // items with pn_lhs( pn ) == sy && dot == 0
	ItemVector expectingItems; // items with dot immediately before sy
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
	bool       isExpanded;
	};

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
	ItemSetTable     itemSets;
	} *ParserGenerator;

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

static int its_index( ItemSet its, ParserGenerator pg )
	{
	return its - itst_element( pg->itemSets, 0 );
	}

static void pg_closeItemVector( ParserGenerator pg, ItemVector itemVector )
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
			if( pn_length( pn, pg->gr ) == 0 )
				assert(0); // TODO: empty productions?
			if( !bv_isSet( pg->rightmostItems, i ) )
				{
				Symbol nextToken = pn_token( pn, it->dot, pg->gr );
				SymbolSideTableEntry lhs = pg_sideTableEntry( pg, nextToken );
				ItemVector itemsToAdd = lhs->leftmostItems;
				if( itemsToAdd )
					bv_or( itemVector, itemsToAdd );
				}
			}
		curPopulation = bv_population( itemVector );
		}
	}

static void pg_computeItemsExpectingToken( ParserGenerator pg, ItemVector result, ItemVector itemSet, Symbol token )
	{
	ItemVector expectingItems = pg_sideTableEntry( pg, token )->expectingItems;
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
	ob_setField(
		result->stateNode,
		sy_byIndex( SYM_ITEM_SET_NUM, pg->st ),
		ob_fromInt( its_index( result, pg ), pg->heap ),
		pg->heap );
	result->isExpanded = false;
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

static void pg_populateItemTable( ParserGenerator pg )
	{
	int i,j;
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	pg->items = ita_new( 1000, ml );
	pg->rightmostItems = bv_new( gr_numProductions(gr), ml );
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
	MemoryLifetime ml = pg->generateTime;
	Grammar gr = pg->gr;
	SymbolTable st = pg->st;
	int itemIndex;
	const int sstIndexesSize = st_count(st) * sizeof(pg->sstIndexes[0]);
	pg->sstIndexes = (int*)ml_alloc( ml, sstIndexesSize );
	memset( pg->sstIndexes, 0, sstIndexesSize );
	pg->sst = sst_new( 100, ml );
	sst_incCount( pg->sst ); // sst index zero is used for "null" so skip that one
	itemIndex = 0;
	for( i=0; i < gr_numProductions(gr); i++ )
		{
		Production pn = gr_production( gr, i );
		Symbol lhs = pn_lhs( pn, gr );
		SymbolSideTableEntry entry = pg_sideTableEntry( pg, lhs );
		Item it = ita_element( pg->items, itemIndex );
		if( !entry->leftmostItems )
			entry->leftmostItems = bv_new( ita_count( pg->items ), ml );
		assert( it->pn == pn && it->dot == 0 );
		assert( pn_lhs( it->pn, gr ) == lhs );
		bv_set( entry->leftmostItems, itemIndex );
		for( j=0; j < pn_length( pn, gr ); j++ )
			{
			it = ita_element( pg->items, itemIndex );
			entry = pg_sideTableEntry( pg, pn_token( it->pn, j, gr ) );
			if( !entry->expectingItems )
				entry->expectingItems = bv_new( ita_count( pg->items ), ml );
			bv_set( entry->expectingItems, itemIndex );
			itemIndex++;
			}
		// now the item where j == pn_length
		itemIndex++;
		}
	assert( itemIndex == gr_numItems( pg->gr ) );
	}

static void pg_computeAutomaton( ParserGenerator pg, File traceFile )
	{
	MemoryLifetime ml = pg->generateTime;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = gr_numItems( pg->gr );
	ItemVector nextItems = bv_new( itemCount, ml );
	pg->itemSets = itst_new( itemCount * itemCount, ml ); // guesstimate of number of item sets
	startItemSet = curItemSet = pg_createItemSet( pg, pg_sideTableEntry( pg, gr_goal(pg->gr) )->leftmostItems );
	pg_closeItemVector( pg, curItemSet->items );
	st_count( st ); // just to use the variable and silence a warning
	while( curItemSet )
		{
		int i;
		ItemVector itemsLeft = bv_new( itemCount, ml );

		bv_copy( itemsLeft, curItemSet->items );
		trace( traceFile, "  Expanding ItemSet_%d\n    stateNode: %s_%p\n    items left: ",
			its_index( curItemSet, pg ), sy_name( ob_tag( curItemSet->stateNode, pg->heap ), st ), curItemSet->stateNode );
		traceBV( itemsLeft, traceFile );
		trace( traceFile, "\n" );

		bv_minus( itemsLeft, pg->rightmostItems );
		traceBVX( itemsLeft, traceFile, "    minus rightmost: %d", ", %d" );
		trace( traceFile, "\n" );

		assert( !curItemSet->isExpanded );
		curItemSet->isExpanded = true;
		for( i = bv_firstBit( itemsLeft ); i != bv_END; i = bv_nextBit( itemsLeft, i ) )
			{
			Item it = ita_element( pg->items, i ); ItemSet nextItemSet;
			Symbol expected = pn_token( it->pn, it->dot, pg->gr );
			trace( traceFile, "    Item %d is expecting %s\n", i, sy_name( expected, st ) );

			pg_computeItemsExpectingToken( pg, nextItems, itemsLeft, expected );
			traceBVX( nextItems, traceFile, "      similar items: %d", ", %d" );
			trace( traceFile, "\n" );

			bv_minus( itemsLeft, nextItems );
			traceBVX( itemsLeft, traceFile, "          itemsLeft: %d", ", %d" );
			trace( traceFile, "\n" );

			bv_shift( nextItems );
			traceBVX( nextItems, traceFile, "            shifted: %d", ", %d" );
			trace( traceFile, "\n" );

			pg_closeItemVector( pg, nextItems );
			traceBVX( nextItems, traceFile, "             closed: %d", ", %d" );
			trace( traceFile, "\n" );

			nextItemSet = pg_findItemSet( pg, nextItems );
			if( nextItemSet )
				{
				trace( traceFile, "  Found existing ItemSet_%d with items: ", its_index( nextItemSet, pg ) );
				traceBV( nextItems, traceFile );
				trace( traceFile, "\n" );
				}
			else
				{
				// Use the nextItems bitvector we created, and allocate a new one for the next guy
				nextItemSet = pg_createItemSet( pg, nextItems );
				nextItems = bv_new( itemCount, ml );
				trace( traceFile, "    Created new itemSet %p\n", nextItems );
				}
			ob_setField( curItemSet->stateNode, expected, nextItemSet->stateNode, pg->heap );
			}
		for( i = its_index( curItemSet, pg ); i < itst_count( pg->itemSets ); i++ )
			{
			curItemSet = itst_element( pg->itemSets, i );
			if( !curItemSet->isExpanded )
				break;
			}
		if( curItemSet->isExpanded )
			{
			curItemSet = NULL;
			trace( traceFile, "All ItemSets are expanded\n" );
			}
		else
			{
			trace( traceFile, "    Skipped to %p with items: ", curItemSet );
			traceBV( curItemSet->items, traceFile );
			trace( traceFile, "\n" );
			}
		}
	}

FUNC Parser ps_new( Grammar gr, SymbolTable st, MemoryLifetime ml )
	{
	MemoryLifetime generateTime = ml_begin( 10000, ml );
	ParserGenerator pg = pg_new( gr, st, generateTime, ml, theObjectHeap() ); // TODO: allocate objects in the proper lifetime
	// TODO: generate the parser
	pg_populateItemTable( pg );
	pg_populateSymbolSideTable( pg );
	pg_computeAutomaton( pg, NULL );
	ml_end( generateTime );
	return pg? NULL: NULL;
	}

#ifdef PARSER_T

enum
	{
	Shift,
	Reduce,
	Accept,
	ShiftReduceConflict,
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

static int its_stateKind( ItemSet its, ParserGenerator pg )
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
	{ "S",  "E", "$" },
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
	{ "S",  "E", "$" },
	{ "E",  "E", "+", "T" },
	{ "E",  "E", "-", "T" },
	{ "E",  "T" },
	{ "T",  "T", "*", "F" },
	{ "T",  "T", "/", "F" },
	{ "T",  "F" },
	{ "F",  "num" },
	{ "F",  "id" },
	};
#endif

#if 0
static GrammarLine grammar[] =
	{
	{ "S",  "E", "$" },
	{ "E",  "E", "*", "B" },
	{ "E",  "E", "+", "B" },
	{ "E",  "B" },
	{ "B",  "0" },
	{ "B",  "1" },
	};
#endif

static GrammarLine grammar[] =
	{
	{ "program",    "statements", "$" },
	{ "statements", "statement" },
	{ "statements", "statements", "statement" },

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
		}
	gr_sendTo( gr, traceFile, st );

	pg = pg_new( gr, st, ml_begin( 10000, ml_indefinite() ), ml_indefinite(), theObjectHeap() );

	pg_populateItemTable( pg );
	fl_write( traceFile, "Items:\n" );
	for( i=0; i < ita_count( pg->items ); i++ )
		{
		Item it = ita_element( pg->items, i );
		fl_write( traceFile, "  %3d: ", i );
		pn_sendItemTo( it->pn, it->dot, traceFile, gr, st );
		fl_write( traceFile, "\n" );
		}
	fl_write( traceFile, "  rightmostItems: " );
	bv_sendTo( pg->rightmostItems, traceFile );
	fl_write( traceFile, "\n" );

	pg_populateSymbolSideTable( pg );
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
				bv_sendTo( sste->leftmostItems, traceFile );
				fl_write( traceFile, "\n" );
				}
			if( sste->expectingItems )
				{
				fl_write( traceFile, "    expectingItems: " );
				bv_sendTo( sste->expectingItems, traceFile );
				fl_write( traceFile, "\n" );
				}
			}
		else
			{
			fl_write( traceFile, "INDEX MISMATCH: entry %d is symbol %s. index %d, which has entry %d\n",
				i, sste->sy, symbolIndex, pg->sstIndexes[ symbolIndex ] );
			}
		}

	pg_computeAutomaton( pg, traceFile );
	//ob_sendDeepTo( itst_element( pg->itemSets, 0 )->stateNode, traceFile, pg->heap );
	fprintf( dotFile, "digraph \"G\" {\n" );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		fprintf( dotFile, "n%p [label=\"%d %s\\n", its->stateNode, i, LR0StateKindNames[ its_stateKind( its, pg ) ] );
		for( j = bv_firstBit( its->items ); j != bv_END; j = bv_nextBit( its->items, j ) )
			{
			Item it = ita_element( pg->items, j );
			pn_sendItemTo( it->pn, it->dot, dotFile, pg->gr, pg->st );
			fprintf( dotFile, "\\n" );
			}
		fprintf( dotFile, "\"]\n" );
		}
	ob_sendDotEdgesTo( itst_element( pg->itemSets, 0 )->stateNode, dotFile, pg->heap );
	fprintf( dotFile, "}\n" );

	return 0;
	}

#endif


// MERGE: 55

