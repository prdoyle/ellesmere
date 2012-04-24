
#include "bitvector.h"
#include "memory.h"
#include <string.h>

typedef uint64_t Word;

enum { BITS_PER_WORD = sizeof(Word) * 8 };

struct bv_struct
	{
	int   numWords;
	Word *words;
	MemoryLifetime ml;
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

// These macros are for users of bitvector.h, not the implementation
#undef bv_new
#undef bv_newAnnotated

FUNC BitVector bv_new( int numBits, MemoryLifetime ml )
	{
	int numWords = bit2word( numBits-1 ) + 1;
	int numBytes = numWords * sizeof( Word );
	BitVector result;
	result    = (BitVector)ml_alloc( ml, sizeof(*result) );
	result->words = (Word*)ml_allocZeros( ml, numBytes );
	result->numWords = numWords;
	result->ml       = ml;
	return result;
	}

#ifndef NDEBUG
FUNC BitVector bv_newAnnotated( int numBits, MemoryLifetime ml, const char *file, int line )
	{
	int numWords = bit2word( numBits-1 ) + 1;
	int numBytes = numWords * sizeof( Word );
	BitVector result;
	result    = (BitVector)ml_allocAnnotated( ml, sizeof(*result), file, line );
	result->words = (Word*)ml_allocZerosAnnotated( ml, numBytes, file, line );
	result->numWords = numWords;
	result->ml       = ml;
	return result;
	}
#endif

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

FUNC void bv_populate( BitVector bv, int *entries, int numEntries )
	{
	int i;
	for( i=0; i < numEntries; i++ )
		bv_set( bv, entries[i] );
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

FUNC int bv_sendFormattedToX( BitVector bv, File fl, BitFormat format, void *context, char *prefix, char *separator, char *suffix )
	{
	int i;
	int charsSent = fl_write( fl, "%s", prefix );
	const char *sep = "";
	for( i = bv_firstBit( bv ); i != bv_END; i = bv_nextBit( bv, i ) )
		{
		charsSent += fl_write( fl, "%s", sep );
		charsSent += format( context, i, fl );
		sep = separator;
		}
	charsSent += fl_write( fl, "%s", suffix );
	return charsSent;
	}

FUNC int sendBitNumber( void *formatStr, int bitIndex, File fl )
	{
	return fl_write( fl, (char*)formatStr, bitIndex );
	}

// MERGE:17

