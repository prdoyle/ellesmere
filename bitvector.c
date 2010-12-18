
#include "bitvector.h"
#include "memory.h"
#include <stdint.h>
#include <string.h>

typedef uint64_t Word;

enum { BITS_PER_WORD = sizeof(Word) * 8 };

struct bv_struct
	{
	int         numWords;
	Word       *words;
	MemoryBatch mb;
	};

static int min(int a, int b)
	{ return a<b? a : b; }

static int bit2word( int bitNum )
	{
	return bitNum / BITS_PER_WORD;
	}

static int bit2shift( int bitNum )
	{
	return bitNum % BITS_PER_WORD;
	}

static Word bit2mask( int bitNum )
	{
	return ((Word)1) << bit2shift( bitNum );
	}

FUNC BitVector bv_newInMB( int numBits, MemoryBatch mb )
	{
	int numWords = bit2word( numBits-1 ) + 1;
	int numBytes = numWords * sizeof( Word );
	BitVector result;
	if( mb )
		{
		result    = (BitVector)mb_alloc( mb, sizeof(*result) );
		result->words = (Word*)mb_alloc( mb, numBytes );
		}
	else
		{
		result    = (BitVector)mem_alloc( sizeof(*result) );
		result->words = (Word*)mem_alloc( numBytes );
		}
	result->numWords = numWords;
	result->mb       = mb;
	memset( result->words, 0, numBytes );
	return result;
	}

FUNC BitVector bv_new( int numBits )
	{
	return bv_newInMB( numBits, NULL );
	}

static void bv_ensure( BitVector bv, int numWords, BitVector fillFrom )
	{
	assert( numWords >= 0 );
	if( numWords > bv->numWords )
		{
		int firstWordToFill = bv->numWords;
		if( bv->mb )
			bv->words = (Word*)mb_realloc( bv->mb, bv->words, bv->numWords * sizeof( Word ), numWords * sizeof( Word ) );
		else
			bv->words = (Word*)mem_realloc( bv->words, numWords * sizeof( Word ) );
		if( fillFrom )
			{
			int wordsToCopy = min( numWords, fillFrom->numWords ) - firstWordToFill;
			memcpy( bv->words + firstWordToFill, fillFrom->words + firstWordToFill, wordsToCopy * sizeof( Word ) );
			firstWordToFill += wordsToCopy;
			}
		if( firstWordToFill < numWords )
			memset( bv->words + firstWordToFill, 0, ( numWords - firstWordToFill ) * sizeof( Word ) );
		bv->numWords = numWords;
		}
	}

FUNC bool bv_isSet( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex );
	if( wordIndex < bv->numWords )
		return ( bv->words[ wordIndex ] & bit2mask( bitIndex ) ) != 0;
	else
		return false;
	}

FUNC void bv_set( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex );
	bv_ensure( bv, wordIndex+1, NULL );
	bv->words[ wordIndex ] |= bit2mask( bitIndex );
	}

FUNC void bv_unset( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex );
	if( wordIndex < bv->numWords )
		bv->words[ wordIndex ] &= ~bit2mask( bitIndex );
	}

FUNC int bv_nextBit( BitVector bv, int prevBit )
	{
	int bitIndex = bit2shift( prevBit+1 );
	int wordIndex, i;
	int numWords = bv->numWords;
	for( wordIndex = bit2word ( prevBit+1 ); wordIndex < numWords; wordIndex++ )
		{
		Word w = bv->words[ wordIndex ];
		if( w )
			for( i = bitIndex; i < BITS_PER_WORD; i++ )
				if( (w>>i) & 1 )
					return wordIndex * BITS_PER_WORD + i;
		bitIndex = 0;
		}
	return bv_END;
	}

FUNC int bv_firstBit( BitVector bv )
	{
	return bv_nextBit( bv, -1 );
	}

FUNC void bv_and( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	bv_ensure( target, source->numWords, NULL );
	for( i=0; i < stop; i++ )
		target->words[ i ] &= source->words[ i ];
	for( i = stop; i < target->numWords; i++ )
		target->words[ i ] = 0;
	}

FUNC void bv_or( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	bv_ensure( target, source->numWords, source );
	for( i=0; i < stop; i++ )
		target->words[ i ] |= source->words[ i ];
	}

FUNC void bv_xor( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	bv_ensure( target, source->numWords, source );
	for( i=0; i < stop; i++ )
		target->words[ i ] ^= source->words[ i ];
	}

FUNC void bv_minus( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	bv_ensure( target, source->numWords, NULL );
	for( i=0; i < stop; i++ )
		target->words[ i ] &= ~source->words[ i ];
	}

FUNC void bv_shrinkWrap( BitVector bv )
	{
	int i, newNumWords=0;
	for( i=0; i < bv->numWords; i++ )
		if( bv->words[i] )
			newNumWords = i+1;
	if( bv->mb )
		bv->words = (Word*)mb_realloc( bv->mb, bv->words, bv->numWords * sizeof( Word ), newNumWords * sizeof( Word ) );
	else
		bv->words = (Word*)mem_realloc( bv->words, newNumWords * sizeof( Word ) );
	bv->numWords = newNumWords;
	}

#ifdef UNIT_TEST

#include <stdio.h>

static BitVector populate( int *entries, int numEntries )
	{
	int i;
	BitVector bv = bv_new( entries[ numEntries-1 ] );
	for( i=0; i < numEntries; i++ )
		bv_set( bv, entries[i] );
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

	return errorOccurred? 1 : 0;
	}

#endif

// MERGE:17

