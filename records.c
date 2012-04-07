
#include "records.h"
#include "memory.h"
#include "symbols.h"
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>

enum { BUCKET_SIZE=4 };

typedef struct hb_struct *HashBucket;
struct hb_struct
	{
	int     keys    [ BUCKET_SIZE ];
	uint8_t indexes [ BUCKET_SIZE ];
	};

struct rd_struct
	{
	int maxIndex;
	int hashParameter;
	int8_t log2numBuckets;
	struct hb_struct *buckets;
	BitVector fieldIDs;
	};

static int flog2( int arg ) // floor of log base 2
	{
	int result = 0;
	int bit;
	for( bit = 4; bit >= 0; bit-- )
		{
		int bitValue = 1 << bit;
		if( arg >> ( result | bitValue ) )
			result |= bitValue;
		}
	return result;
	}

static inline long long power( long long base, int exp )
	{
	long long result = 1;
	for( ; exp >= 1; exp-- )
		result *= base;
	return result;
	}

static int computeLog2numBuckets( int numFields )
	{
	// Get an approximate result, erring on the low side
	int result = flog2( numFields ) * ( BUCKET_SIZE+1 ) / BUCKET_SIZE;
	//TRACE( stdout, "computeLog2numBuckets( %d ): initial result: %d\n", numFields, result );
	// Increment until it's big enough
	long long threshold = power( numFields, BUCKET_SIZE+1 );
	while( power( 1LL<<result, BUCKET_SIZE ) < threshold )
		result++;
	//TRACE( stdout, "computeLog2numBuckets( %d ): final result: %d, 2^%d^%d == %lld >= %lld\n", numFields, result, result, BUCKET_SIZE, power( 1LL<<result, BUCKET_SIZE ), threshold );
	return result;
	}

static int rd_hash( Record rd, int key )
	{
	key = key * rd->hashParameter | 1; // Should be odd -> relatively prime to power-of-two table size
	int result = 0;
	const int8_t log2numBuckets = rd->log2numBuckets;
	const int chunkMask = ( 1 << log2numBuckets ) - 1;
	while( key ) // TODO: It's likely we don't need to exhaust all bits in the key to achieve perfect hashing.  Exit early if possible.
		{
		result ^= key & chunkMask;
		key = (int)( ((unsigned)key) >> log2numBuckets );
		}
	return result;
	}

static int hb_subBucketIndex( HashBucket hb, int key )
	{
	int i;
	for( i = BUCKET_SIZE-1; i >= 0; i-- )
		if( hb->keys[i] == key )
			break;
	return i;
	}

FUNC Record rd_new( BitVector fieldIDs, MemoryLifetime ml )
	{
	// Allocate the result struct
	Record result;
	int numFields = bv_population( fieldIDs );
	if( numFields <= 1 || numFields > ( 1 << (8*sizeof( result->buckets[0].indexes[0] )) ) )
		return NULL; // TODO: Do the first N and then quit?  TODO: Handle 1-field records -- they should be most efficient of all!
	result = (Record)ml_alloc( ml, sizeof(*result) );
	result->maxIndex = numFields;
	result->fieldIDs = bv_new( bv_lastBit( fieldIDs ), ml );
	bv_copy( result->fieldIDs, fieldIDs );

	// Try pseudorandom (ok not all that random) hash parameters until one is perfect.
	// Shouldn't take many tries.
	int8_t log2numBuckets = computeLog2numBuckets( numFields );
	result->log2numBuckets = log2numBuckets;
	int numBuckets = 1 << log2numBuckets;
	int parameter;
	srandom( 123 );
	while(1)
		{
		parameter = random();
		int size = numBuckets * sizeof( result->buckets[0] );
		result->buckets = (struct hb_struct*)ml_allocZeros( ml, size );
		result->hashParameter = parameter;
		int nextIndex = 1;
		int fieldID;
		for( fieldID = bv_firstBit( fieldIDs ); fieldID != bv_END; fieldID = bv_nextBit( fieldIDs, fieldID ) )
			{
			int hash = rd_hash( result, fieldID );
			HashBucket hb = result->buckets + hash;
			int freeSubBucket = hb_subBucketIndex( hb, 0 );
			if( freeSubBucket >= 0 )
				{
				hb->keys    [ freeSubBucket ] = fieldID;
				hb->indexes [ freeSubBucket ] = nextIndex++;
				}
			else
				break; // too many collisions in this bucket; proceed to next hash parameter
			}
		if( fieldID == bv_END )
			break;
		else
			result->buckets = ml_realloc( ml, result->buckets, size, 0 ); // free storage and try again
		}
	return result;
	}

FUNC int rd_maxIndex( Record rd )
	{
	return rd? rd->maxIndex : 0;
	}

FUNC int rd_indexOf( Record rd, int fieldID )
	{
	if( !rd )
		return 0;

	int hash = rd_hash( rd, fieldID );
	HashBucket hb = rd->buckets + hash;
	int subBucket = hb_subBucketIndex( hb, fieldID );
	if( subBucket >= 0 && hb->keys[ subBucket ] == fieldID )
		return hb->indexes[ subBucket ];
	else
		return 0;
	}

FUNC int rd_firstField( Record rd )
	{
	return rd? bv_firstBit( rd->fieldIDs ) : rd_NONE;
	}

FUNC int rd_nextField( Record rd, int prevField )
	{
	if( !rd )
		return rd_NONE;

	int result = bv_nextBit( rd->fieldIDs, prevField );
	if( result == bv_END )
		return rd_NONE;
	else
		return result;
	}

FUNC int rd_sendTo( Record rd, File fl, SymbolTable st )
	{
	int charsSent = fl_write( fl, "Record %p {\n", rd );
	int fieldID;
	for( fieldID = rd_firstField( rd ); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
		{
		Symbol field = sy_byIndex( fieldID, st );
		// TODO: Sort by index
		charsSent += fl_write( fl, "  %4d: %s\n", rd_indexOf( rd, fieldID ), sy_name( field, st ) );
		}
	charsSent += fl_write( fl, "}\n" );
	return charsSent;
	}

//MERGE:35

