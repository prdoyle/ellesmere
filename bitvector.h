
#ifndef BITVECTOR_H
#define BITVECTOR_H

#include "base.h"
#include "file.h"

FUNC BitVector bv_new     ( int numBits, MemoryLifetime ml ) ALWAYS_NEW;
FUNC bool      bv_isSet   ( BitVector bv, int bitIndex );
FUNC void      bv_set     ( BitVector bv, int bitIndex );
FUNC void      bv_unset   ( BitVector bv, int bitIndex );
FUNC void      bv_populate( BitVector bv, int *entries, int numEntries );

enum { bv_END=-1 }; // indicates there are no more bits
FUNC int       bv_firstBit   ( BitVector bv );
FUNC int       bv_lastBit    ( BitVector bv );
FUNC int       bv_nextBit    ( BitVector bv, int prevBit );
FUNC int       bv_prevBit    ( BitVector bv, int nextBit );
FUNC int       bv_population ( BitVector bv );
FUNC int       bv_hash       ( BitVector bv );
FUNC bool      bv_isEmpty    ( BitVector bv );
FUNC bool      bv_equals     ( BitVector bv, BitVector other );
FUNC bool      bv_contains   ( BitVector bv, BitVector other );
FUNC bool      bv_intersects ( BitVector bv, BitVector other );

FUNC void      bv_clear ( BitVector target );
FUNC void      bv_copy  ( BitVector target, BitVector source );
FUNC void      bv_and   ( BitVector target, BitVector source );
FUNC void      bv_or    ( BitVector target, BitVector source );
FUNC void      bv_xor   ( BitVector target, BitVector source );
FUNC void      bv_minus ( BitVector target, BitVector source );

// These are like the corresponding operations above but they return "true" if
// the operation changed the target vector
FUNC bool      bv_orChanged( BitVector target, BitVector source );

static inline BitVector bv_dup( BitVector source, MemoryLifetime ml )
	{
	BitVector result = bv_new( 0, ml );
	bv_copy( result, source );
	return result;
	}

FUNC void      bv_shift( BitVector bv );
FUNC void      bv_shrinkWrap( BitVector bv );

typedef int ( *BitFormat )( void *context, int bitIndex, File fl );
FUNC int       bv_sendFormattedToX( BitVector bv, File fl, BitFormat format, void *context, char *prefix, char *separator, char *suffix );
static inline int bv_sendFormattedTo( BitVector bv, File fl, BitFormat format, void *context )
	{ return bv_sendFormattedToX( bv, fl, format, context, "{ ", ", ", " }" ); }

FUNC int sendBitNumber( void *formatStr, int bitIndex, File fl );
static inline int bv_sendTo( BitVector bv, File fl )
	{ return bv_sendFormattedTo( bv, fl, sendBitNumber, "%d" ); }

static inline bool bv_isUnset( BitVector bv, int bitIndex ){ return !bv_isSet( bv, bitIndex ); }

#if 1
	#define traceBV bv_sendTo
	#define traceBVX bv_sendFormattedTo
#else
	#define traceBV(...)
	#define traceBVX(...)
#endif

#ifdef NDEBUG
	#define bv_newAnnotated( numBits, ml, f, l )  bv_new( numBits, ml )
#else
	FUNC BitVector bv_newAnnotated( int numBits, MemoryLifetime ml, const char *file, int line ) ALWAYS_NEW;
	#define bv_new( numBits, ml )  bv_newAnnotated( numBits, ml, __FILE__, __LINE__ )
#endif

#endif

