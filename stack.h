
#ifndef STACK_H
#define STACK_H

#include "objects.h"

FUNC Stack  sk_new( MemoryLifetime ml );
FUNC int    sk_depth  ( Stack sk );
FUNC void   sk_push   ( Stack sk, Object ob );
FUNC Object sk_item   ( Stack sk, int depth );
FUNC void   sk_popN   ( Stack sk, int count );
FUNC Stack  sk_dup( Stack other, MemoryLifetime ml );

static inline Object sk_top    ( Stack sk ){ return sk_item( sk, 0 ); }
static inline Object sk_pop    ( Stack sk ){ Object result = sk_top(sk); sk_popN( sk, 1 ); return result; }
static inline void   sk_popAll ( Stack sk ){ sk_popN( sk, sk_depth( sk ) ); }
static inline bool   sk_isEmpty( Stack sk ){ return sk_depth( sk ) == 0; }

FUNC int sk_sendNTo( Stack sk, int numElements, File fl, ObjectHeap heap );

static inline int sk_sendTo( Stack sk, File fl, ObjectHeap heap )
	{ return sk_sendNTo( sk, sk_depth(sk), fl, heap ); }

#endif

