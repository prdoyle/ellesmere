
#include "bitvector.h"
#include "memory.h"
#include <stdint.h>
#include <string.h>

typedef uint64_t Word;

enum { BITS_PER_WORD = sizeof(Word) * 8 };

struct bv_struct
	{
	int   numWords;
	Word *words;
	};

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

FUNC BitVector bv_new( int numBits )
	{
	int numWords = bit2word( numBits-1 ) + 1;
	int numBytes = numWords * sizeof( Word );
	BitVector result = (BitVector)mem_alloc( sizeof(*result) );
	result->numWords = numWords;
	result->words    = (Word*)mem_alloc( numBytes );
	memset( result->words, 0, numBytes );
	return result;
	}

FUNC bool bv_isSet( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex );
	if( wordIndex >= bv->numWords )
		return ( bv->words[ wordIndex ] & bit2mask( bitIndex ) ) != 0;
	else
		return false;
	}

FUNC void bv_set( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex && wordIndex < bv->numWords );
	bv->words[ wordIndex ] |= bit2mask( bitIndex );
	}

FUNC void bv_unset( BitVector bv, int bitIndex )
	{
	int wordIndex = bit2word( bitIndex );
	assert( 0 <= wordIndex && wordIndex < bv->numWords );
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

static int min(int a, int b)
	{ return a<b? a : b; }

FUNC void bv_and( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	for( i=0; i < stop; i++ )
		target->words[ i ] &= source->words[ i ];
	for( i = stop; i < target->numWords; i++ )
		target->words = 0;
	}

FUNC void bv_or( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	for( i=0; i < stop; i++ )
		target->words[ i ] |= source->words[ i ];
	}

FUNC void bv_xor( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	for( i=0; i < stop; i++ )
		target->words[ i ] ^= source->words[ i ];
	}

FUNC void bv_minus( BitVector target, BitVector source )
	{
	int i, stop = min( target->numWords, source->numWords );
	for( i=0; i < stop; i++ )
		target->words[ i ] &= ~source->words[ i ];
	}

#ifdef UNIT_TEST

#include <stdio.h>

static int try( int *entries, int numEntries )
	{
	int i,j;
	BitVector bv = bv_new( entries[ numEntries-1 ] );
	for( i=0; i < numEntries; i++ )
		bv_set( bv, entries[i] );
	for( i=0, j = bv_firstBit( bv ); j != bv_END; j = bv_nextBit( bv, j ), i++ )
		if( entries[i] == j )
			bv_unset( bv, j );
		else
			return printf( "Mismatched bit #%d: %d != %d\n", i, entries[i], j );
	if( i != numEntries )
		return printf( "Wrong number of entries: %d != %d\n", i, numEntries );
	if( bv_firstBit( bv ) != bv_END )
		return printf( "Bitvector has bit %d set\n", bv_firstBit( bv ) );
	return 0;
	}

static int test1[] = { 0, 1, 2, 29, 30, 31, 32, 33, 62, 63, 64, 65 };

int main( int argc, char **argv )
	{
	int errorOccurred = try( test1, sizeof( test1 ) / sizeof( test1[0] ) );
	return errorOccurred? 1 : 0;
	}

#endif

// MERGE:17

