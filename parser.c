
#include "parser.h"
#include "bitvector.h"
#include "memory.h"
#include "objects.h"
#include "stack.h"
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
	SymbolVector     nullableSymbols;
	ItemSetTable     itemSets;
	} *ParserGenerator;

static int pg_symbolSideTableIndex( ParserGenerator pg, Symbol sy )
	{
	return pg->sstIndexes[ sy_index( sy, pg->st ) ];
	}

static SymbolSideTableEntry pg_sideTableEntry( ParserGenerator pg, Symbol sy )
	{
	SymbolSideTableEntry result;
	int sstIndex = pg_symbolSideTableIndex( pg, sy );
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
			if( !bv_isSet( pg->rightmostItems, i ) ) // Note: this skips empty productions
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
#if 0
	ob_setField(
		result->stateNode,
		sy_byIndex( SYM_ITEM_SET_NUM, pg->st ),
		ob_fromInt( its_index( result, pg ), pg->heap ),
		pg->heap );
#endif
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
	pg->nullableSymbols = bv_new( 100, ml );
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

static Object pg_computeStateNodes( ParserGenerator pg, File traceFile )
	{
	MemoryLifetime ml = pg->generateTime;
	SymbolTable st = pg->st;
	ItemSet curItemSet, startItemSet;
	int itemCount = gr_numItems( pg->gr );
	ItemVector nextItems = bv_new( itemCount, ml );
	pg->itemSets = itst_new( itemCount * itemCount, ml ); // guesstimate of number of item sets
	startItemSet = curItemSet = pg_createItemSet( pg, pg_sideTableEntry( pg, gr_goal(pg->gr) )->leftmostItems );
	Object startState = startItemSet->stateNode;
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
	while( somethingChanged )
		{
		somethingChanged = false;
		for( pnNum=0; pnNum < gr_numProductions( pg->gr ); pnNum++ )
			{
			Production pn = gr_production( pg->gr, pnNum );
			int lhsIndex  = pg_symbolSideTableIndex( pg, pn_lhs( pn, pg->gr ) );
			SymbolSideTableEntry lhs = sst_element( pg->sst, lhsIndex );
			int i = pn_length( pn, pg->gr ) - 1;
			// Last token has no successor so treat it specially
			if( i >= 1 )
				{
				int lastIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i, pg->gr ) );
				SymbolSideTableEntry last = sst_element( pg->sst, lastIndex );
				somethingChanged |= bv_orChanged( last->follow, lhs->follow );
				}
			// Loop through the other tokens right-to-left propagating follow info across nullable tokens
			for( i--; i >= 0; i-- )
				{
				int curIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i, pg->gr ) );
				SymbolSideTableEntry cur = sst_element( pg->sst, curIndex );
				int nextIndex = pg_symbolSideTableIndex( pg, pn_token( pn, i+1, pg->gr ) );
				SymbolSideTableEntry next = sst_element( pg->sst, nextIndex );
				somethingChanged |= bv_orChanged( cur->follow, next->first );
				if( bv_isSet( pg->nullableSymbols, nextIndex ) )
					somethingChanged |= bv_orChanged( cur->follow, next->follow );
				}
			}
		}
	}

static void pg_computeReduceNodes( ParserGenerator pg, File traceFile )
	{
	// For now we're doing SLR
	Grammar gr = pg->gr; ObjectHeap heap = pg->heap;
	int i,j,k;
	ItemVector iv = bv_new( ita_count(pg->items), pg->generateTime );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		Object stateNode = its->stateNode;
		bv_copy ( iv, pg->rightmostItems );
		bv_and  ( iv, its->items );
		for( j = bv_firstBit( iv ); j != bv_END; j = bv_nextBit( iv, j ) )
			{
			Item it = ita_element( pg->items, j );
			Production pn = it->pn;
			SymbolSideTableEntry lhs = pg_sideTableEntry( pg, pn_lhs( pn, gr ) );
			SymbolVector follow = lhs->follow;
			for( k = bv_firstBit( follow ); k != bv_END; k = bv_nextBit( follow, k ) )
				{
				SymbolSideTableEntry reduceOn = sst_element( pg->sst, k );
				check( !ob_hasField( stateNode, reduceOn->sy, heap ) );
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

FUNC Parser ps_new( Grammar gr, SymbolTable st, MemoryLifetime ml )
	{
	MemoryLifetime generateTime = ml_begin( 10000, ml );
	ParserGenerator pg = pg_new( gr, st, generateTime, ml, theObjectHeap() );
	pg_populateItemTable( pg );
	pg_populateSymbolSideTable( pg );
	Object startState = pg_computeStateNodes( pg, NULL );
	pg_computeFirstSets( pg, NULL );
	pg_computeFollowSets( pg, NULL );
	pg_computeReduceNodes( pg, NULL );
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
		token = ob_toSymbol( ob, oh );
	if( !ob_hasField( curState, token, oh ) )
		return NULL;
	else
		return ob_getField( curState, token, oh );
	}

FUNC void ps_push( Parser ps, Object ob )
	{
	Object nextState = ps_nextState( ps, ob );
	check( nextState );
	sk_push( ps->stateStack, nextState );
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

static char *sentence[] =
	{
	"1", "+", "2", "*", "2", "/", "2", "+", "3"
	};

#endif

#if 1
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

static char *sentence[] =
	{
	"1", "+", "2", "*", "2", "/", "2", "+", "3"
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

	pg_computeStateNodes( pg, traceFile );

	pg_computeFirstSets( pg, traceFile );
	pg_computeFollowSets( pg, traceFile );
	fl_write( traceFile, "Symbol sets:\n" );
	for( i=1; i < sst_count( pg->sst ); i++ )
		{
		SymbolSideTableEntry sste = sst_element( pg->sst, i );
		fl_write( traceFile, "  %3d: %s\n", i, sy_name( sste->sy, st ) );
		fl_write( traceFile, "     first: " );
		bv_sendTo( sste->first, traceFile );
		fl_write( traceFile, "\n" );
		fl_write( traceFile, "     follow: " );
		bv_sendTo( sste->follow, traceFile );
		fl_write( traceFile, "\n" );
		}

	pg_computeReduceNodes( pg, traceFile );
	ob_sendDeepTo( itst_element( pg->itemSets, 0 )->stateNode, traceFile, pg->heap );

	fprintf( dotFile, "digraph \"G\" { overlap=false \n" );
	for( i=0; i < itst_count( pg->itemSets ); i++ )
		{
		ItemSet its = itst_element( pg->itemSets, i );
		fprintf( dotFile, "n%p [label=\"%d %s\\n", its->stateNode, i, LR0StateKindNames[ its_LR0StateKind( its, pg ) ] );
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

	fprintf( traceFile, "Parsing...\n" );
	Parser ps = ps_new( gr, st, ml_indefinite() );
	ObjectHeap heap = theObjectHeap();
	int stop = asizeof( sentence );
	for( i=0; i <= stop; i++ )
		{
		char *cur  = (i >= stop)?   ":END_OF_INPUT" : sentence[i];
		char *next = (i+1 >= stop)? ":END_OF_INPUT" : sentence[i+1];
		fprintf( traceFile, "  Token: %s\n", cur );
		Symbol sy = sy_byName( cur, st );
		ps_push( ps, oh_symbolToken( heap, sy ) );
		Object lookahead = oh_symbolToken( heap, sy_byName( next, st ) );
		Production handle = ps_handle( ps, lookahead );
		while( handle )
			{
			fprintf( traceFile, "    Reduce: " );
			pn_sendTo( handle, traceFile, gr, st );
			fprintf( traceFile, " -- new state is %d\n", ob_toInt( ob_getField(
				sk_top( ps->stateStack ),
				sy_byIndex( SYM_ITEM_SET_NUM, st ), heap ), heap ) );
			ps_popN( ps, pn_length( handle, gr ) );
			ps_push( ps, oh_symbolToken( heap, pn_lhs( handle, gr ) ) );
			handle = ps_handle( ps, lookahead );
			}
		}
	fprintf( traceFile, "...done\n" );

	return 0;
	}

#endif


// MERGE: 55

