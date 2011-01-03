
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
	MemoryLifetime ml;
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

FUNC BitVector bv_new( int numBits, MemoryLifetime ml )
	{
	int numWords = bit2word( numBits-1 ) + 1;
	int numBytes = numWords * sizeof( Word );
	BitVector result;
	result    = (BitVector)ml_alloc( ml, sizeof(*result) );
	//result->words = (Word*)ml_allocZeros( ml, numBytes );
	result->words = (Word*)ml_alloc( ml, numBytes );
	memset( result->words, 0, numBytes );
	result->numWords = numWords;
	result->ml       = ml;
	return result;
	}

static void bv_reallocWords( BitVector bv, int newNumWords )
	{
	bv->words = (Word*)ml_realloc( bv->ml, bv->words, bv->numWords * sizeof( Word ), newNumWords * sizeof( Word ) );
	bv->numWords = newNumWords;
	}

static void bv_ensure( BitVector bv, int numWords, BitVector fillFrom )
	{
	assert( numWords >= 0 );
	if( numWords > bv->numWords )
		{
		int firstWordToFill = bv->numWords;
		bv_reallocWords( bv, numWords );
		if( fillFrom )
			{
			int wordsToCopy = min( numWords, fillFrom->numWords ) - firstWordToFill;
			memcpy( bv->words + firstWordToFill, fillFrom->words + firstWordToFill, wordsToCopy * sizeof( Word ) );
			firstWordToFill += wordsToCopy;
			}
		if( firstWordToFill < numWords )
			memset( bv->words + firstWordToFill, 0, ( numWords - firstWordToFill ) * sizeof( Word ) );
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

FUNC int bv_prevBit( BitVector bv, int nextBit )
	{
	int bitIndex = bit2shift( nextBit-1 );
	int wordIndex, i;
	for( wordIndex = bit2word ( nextBit-1 ); wordIndex >= 0; wordIndex-- )
		{
		Word w = bv->words[ wordIndex ];
		if( w )
			for( i = bitIndex; i >= 0; i-- )
				if( (w>>i) & 1 )
					return wordIndex * BITS_PER_WORD + i;
		bitIndex = BITS_PER_WORD;
		}
	return bv_END;
	}

FUNC int bv_firstBit( BitVector bv )
	{
	return bv_nextBit( bv, -1 );
	}

FUNC int bv_lastBit( BitVector bv )
	{
	return bv_prevBit( bv, bv->numWords * BITS_PER_WORD );
	}

FUNC int bv_population( BitVector bv )
	{
	int result = 0;  int i;
	for( i = bv_firstBit( bv ); i != bv_END; i = bv_nextBit( bv, i ) )
		result++;
	return result;
	}

FUNC int bv_hash( BitVector bv )
	{
	int result = 1; int i;
	for( i=0; i < bv->numWords; i++ )
		if( bv->words[i] ) // make sure trailing zeros don't affect the hash
			result = ( result ^ bv->words[i] ) * 1103515245 ^ i;
	return result;
	}

FUNC bool bv_isEmpty( BitVector bv )
	{
	return bv_firstBit( bv ) == bv_END;
	}

FUNC void bv_clear( BitVector bv )
	{
#if 0 // This works too, but it is counterproductive if the user has pre-allocated the proper number of words
	if( bv->numWords > 0 )
		bv_reallocWords( bv, 0 );
#else
	int i;
	for( i=0; i < bv->numWords; i++ )
		bv->words[i] = 0;
#endif
	}

FUNC bool bv_equals( BitVector bv, BitVector other )
	{
	BitVector smaller, bigger; int i, stop;
	if( bv->numWords < other->numWords )
		{
		smaller = bv;
		bigger  = other;
		}
	else
		{
		smaller = other;
		bigger  = bv;
		}
	stop = smaller->numWords;
	for( i=0; i < stop; i++ )
		if( bv->words[i] != other->words[i] )
			return false;
	return bv_nextBit( bigger, stop * BITS_PER_WORD ) == bv_END;
	}

FUNC bool bv_contains( BitVector bv, BitVector other )
	{
	// TODO: Do this without temporaries
	MemoryLifetime tempML = ml_begin( sizeof(*bv) + bv->numWords * sizeof(Word), bv->ml );
	BitVector tempBV = bv_new( bv->numWords * BITS_PER_WORD, tempML );
	bv_copy( tempBV, bv );
	bv_and( tempBV, other );
	bool result = bv_equals( tempBV, other );
	ml_end( tempML );
	return result;
	}

FUNC bool bv_intersects( BitVector target, BitVector source )
	{
	bool result = false;
	int i, stop = min( target->numWords, source->numWords );
	for( i=0; i < stop && !result; i++ )
		result = result || ( target->words[ i ] & source->words[ i ] );
	return result;
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

FUNC void bv_copy( BitVector target, BitVector source )
	{
	int i;
	bv_reallocWords( target, source->numWords );
	for( i=0; i < source->numWords; i++ )
		target->words[ i ] = source->words[ i ];
	}

FUNC bool bv_orChanged( BitVector target, BitVector source )
	{
	bool somethingChanged = false;
	int i, stop = min( target->numWords, source->numWords );
	bv_ensure( target, source->numWords, source );
	for( i=0; i < stop; i++ )
		{
		Word newWord = target->words[ i ] | source->words[ i ];
		if( newWord != target->words[ i ] )
			{
			target->words[ i ] = newWord;
			somethingChanged = true;
			}
		}
	return somethingChanged;
	}

FUNC void bv_or( BitVector target, BitVector source )
	{
	bv_orChanged( target, source );
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

static int shiftedOff( Word w )
	{
	return (w >> (BITS_PER_WORD-1) ) & 1;
	}

FUNC void bv_shift( BitVector bv )
	{
	int i;
	if( shiftedOff( bv->words[ bv->numWords-1 ] ) )
		bv_ensure( bv, bv->numWords+1, NULL );
	for( i = bv->numWords-1; i > 0; i-- )
		bv->words[ i ] = ( bv->words[ i ] << 1 ) | shiftedOff( bv->words[ i-1 ] );
	bv->words[0] <<= 1;
	}

FUNC void bv_shrinkWrap( BitVector bv )
	{
	int i, newNumWords=0;
	for( i=0; i < bv->numWords; i++ )
		if( bv->words[i] )
			newNumWords = i+1;
	bv->words = (Word*)ml_realloc( bv->ml, bv->words, bv->numWords * sizeof( Word ), newNumWords * sizeof( Word ) );
	bv->numWords = newNumWords;
	}

FUNC int bv_sendFormattedTo( BitVector bv, File fl, const char *firstFormat, const char *subsequentFormat )
	{
	int i; int charsSent = fl_write( fl, "{ " ); const char *format = firstFormat;
	for( i = bv_firstBit( bv ); i != bv_END; i = bv_nextBit( bv, i ) )
		{
		charsSent += fl_write( fl, format, i );
		format = subsequentFormat;
		}
	charsSent += fl_write( fl, " }" );
	return charsSent;
	}

#ifdef BITVECTOR_T

#include <stdio.h>

static BitVector populate( int *entries, int numEntries )
	{
	int i;
	BitVector bv = bv_new( entries[ numEntries-1 ], ml_indefinite() );
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

#endif

// MERGE:17

