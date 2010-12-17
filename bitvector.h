
#ifndef BITVECTOR_H
#define BITVECTOR_H

#include "base.h"

FUNC BitVector bv_new( int sizeEstimate );
FUNC bool      bv_isSet ( BitVector bv, int bitIndex );
FUNC void      bv_set   ( BitVector bv, int bitIndex );
FUNC void      bv_unset ( BitVector bv, int bitIndex );

FUNC int       bv_firstBit ( BitVector bv );
FUNC int       bv_nextBit  ( BitVector bv, int prevBit );
enum { bv_END=-1 }; // indicates there are no more bits

FUNC void      bv_and   ( BitVector target, BitVector source );
FUNC void      bv_or    ( BitVector target, BitVector source );
FUNC void      bv_xor   ( BitVector target, BitVector source );
FUNC void      bv_minus ( BitVector target, BitVector source );

#endif

