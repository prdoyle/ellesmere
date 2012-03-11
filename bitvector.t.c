
#include "bitvector.h"
#include "memory.h"
#include <stdio.h>

static BitVector populate( int *entries, int numEntries )
	{
	BitVector bv = bv_new( entries[ numEntries-1 ], ml_indefinite() );
	bv_populate( bv, entries, numEntries );
	return bv;
	}

static int compare( BitVector bv, int *entries, int numEntries )
	{
	int i,j;
	for( i=0, j = bv_firstBit( bv ); j != bv_END; j = bv_nextBit( bv, j ), i++ )
		if( entries[i] != j )
			return printf( "Mismatched bit #%d: %d != %d\n", i, entries[i], j );
	if( i != numEntries )
		return printf( "Wrong number of entries: %d != %d\n", i, numEntries );
	return 0;
	}

static int clear( BitVector bv, int *entries, int numEntries )
	{
	int i;
	for( i=0; i < numEntries; i++ )
		bv_unset( bv, entries[i] );
	if( bv_firstBit( bv ) != bv_END )
		return printf( "Bitvector has bit %d set\n", bv_firstBit( bv ) );
	return 0;
	}

static int testIteration( int *entries, int numEntries )
	{
	BitVector bv = populate( entries, numEntries );
	return compare( bv, entries, numEntries ) || clear( bv, entries, numEntries );
	}

static int test1[] = { 0, 1, 2, 29, 30, 31, 32, 33, 62, 63, 64, 65 };
static int test2[] = { 0, 1, 3, 11, 12 };
static int test3[] = { 0, 1 }; // and
static int test4[] = { 0, 1, 2, 3, 11, 12, 29, 30, 31, 32, 33, 62, 63, 64, 65 }; // or
static int test5[] = { 2, 3, 11, 12, 29, 30, 31, 32, 33, 62, 63, 64, 65 }; // xor
static int test6[] = { 2, 29, 30, 31, 32, 33, 62, 63, 64, 65 }; // minus
static int test7[] = { 3, 11, 12 }; // b minus a
static int test8[] = { 1, 3, 5, 150 };
static int test9[] = { 1, 3, 5 };

static int test10[] = { 1, 2, 3, 4, 12, 13, 30, 31, 32, 33, 34, 63, 64, 65, 66 }; // test4 shifted
static int test11[] = { 63 };
static int test12[] = { 64 }; // test11 shifted

int main( int argc, char **argv )
	{
	BitVector a, b;
	int errorOccurred = testIteration( test1, asizeof( test1 ) );
	errorOccurred    |= testIteration( test2, asizeof( test2 ) );

	a = populate( test1, asizeof( test1 ) );
	b = populate( test2, asizeof( test2 ) );
	bv_and( a, b );
	errorOccurred |= compare( a, test3, asizeof( test3 ) );

	a = populate( test1, asizeof( test1 ) );
	bv_or( a, b );
	errorOccurred |= compare( a, test4, asizeof( test4 ) );

	a = populate( test1, asizeof( test1 ) );
	bv_copy( a, b );
	errorOccurred |= !bv_equals( a, b );
	errorOccurred |= !bv_equals( b, a );
	errorOccurred |= compare( a, test2, asizeof( test2 ) );

	a = populate( test1, asizeof( test1 ) );
	bv_xor( a, b );
	errorOccurred |= compare( a, test5, asizeof( test5 ) );

	a = populate( test1, asizeof( test1 ) );
	bv_minus( a, b );
	errorOccurred |= compare( a, test6, asizeof( test6 ) );

	a = populate( test2, asizeof( test2 ) );
	b = populate( test1, asizeof( test1 ) );
	bv_and( a, b );
	errorOccurred |= compare( a, test3, asizeof( test3 ) );

	a = populate( test2, asizeof( test2 ) );
	bv_or( a, b );
	errorOccurred |= compare( a, test4, asizeof( test4 ) );

	a = populate( test2, asizeof( test2 ) );
	bv_copy( a, b );
	errorOccurred |= !bv_equals( a, b );
	errorOccurred |= !bv_equals( b, a );
	errorOccurred |= compare( a, test1, asizeof( test1 ) );

	a = populate( test2, asizeof( test2 ) );
	bv_xor( a, b );
	errorOccurred |= compare( a, test5, asizeof( test5 ) );

	a = populate( test2, asizeof( test2 ) );
	bv_minus( a, b );
	errorOccurred |= compare( a, test7, asizeof( test7 ) );

	a = populate( test8, asizeof( test8 ) );
	bv_unset( a, 150 );
	errorOccurred |= compare( a, test9, asizeof( test9 ) );
	bv_shrinkWrap( a );
	errorOccurred |= compare( a, test9, asizeof( test9 ) );
	bv_unset( a, 1 );
	bv_unset( a, 3 );
	bv_unset( a, 5 );
	errorOccurred |= ( bv_firstBit(a) != bv_END );
	bv_shrinkWrap( a );
	errorOccurred |= ( bv_firstBit(a) != bv_END );
	bv_set( a, 1 );
	bv_set( a, 3 );
	bv_set( a, 5 );
	errorOccurred |= compare( a, test9, asizeof( test9 ) );

	a = populate( test4, asizeof( test4 ) );
	bv_shift( a );
	errorOccurred |= compare( a, test10, asizeof( test10 ) );

	a = populate( test11, asizeof( test11 ) );
	bv_shift( a );
	errorOccurred |= compare( a, test12, asizeof( test12 ) );

	a = populate( test8, asizeof( test8 ) );
	b = populate( test9, asizeof( test9 ) );
	errorOccurred |= bv_equals( a, b );
	errorOccurred |= !bv_contains( a, b );
	errorOccurred |= bv_contains( b, a );
	errorOccurred |= 150    != bv_lastBit( a );
	errorOccurred |= 5      != bv_prevBit( a, 150 );
	errorOccurred |= 3      != bv_prevBit( a, 5 );
	errorOccurred |= 1      != bv_prevBit( a, 3 );
	errorOccurred |= bv_END != bv_prevBit( a, 1 );

	return errorOccurred? 1 : 0;
	}

