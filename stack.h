
#ifndef STACK_H
#define STACK_H

#include "objects.h"

FUNC Stack  sk_new( MemoryLifetime ml );
FUNC int    sk_depth  ( Stack sk );
FUNC void   sk_push   ( Stack sk, Object ob );
FUNC Object sk_item   ( Stack sk, int depth );
FUNC void   sk_popN   ( Stack sk, int count );
FUNC void   sk_dupN   ( Stack target, int count, Stack source );
FUNC void   sk_mirrorN( Stack target, int count, Stack source );

static inline Object sk_top    ( Stack sk ){ return sk_item( sk, 0 ); }
static inline Object sk_pop    ( Stack sk ){ Object result = sk_top(sk); sk_popN( sk, 1 ); return result; }
static inline void   sk_popAll ( Stack sk ){ sk_popN( sk, sk_depth( sk ) ); }
static inline bool   sk_isEmpty( Stack sk ){ return sk_depth( sk ) == 0; }
static inline Stack  sk_dup    ( Stack sk, MemoryLifetime ml ){ Stack result = sk_new( ml ); sk_dupN    ( result, sk_depth( sk ), sk ); return result; }
static inline Stack  sk_mirror ( Stack sk, MemoryLifetime ml ){ Stack result = sk_new( ml ); sk_mirrorN ( result, sk_depth( sk ), sk ); return result; }

FUNC int sk_sendNTo( Stack sk, int numElements, File fl, ObjectHeap heap );

static inline int sk_sendTo( Stack sk, File fl, ObjectHeap heap )
	{ return sk_sendNTo( sk, sk_depth(sk), fl, heap ); }

FUNC int sk_sendNFormattedToX( Stack sk, int numElements, File fl, ObjectFormat format, void *context, char *separator );
static inline int sk_sendFormattedToX( Stack sk, File fl, ObjectFormat format, void *context, char *separator )
	{ return sk_sendNFormattedToX( sk, sk_depth(sk), fl, format, context, separator ); }
static inline int sk_sendNFormattedTo( Stack sk, int numElements, File fl, ObjectFormat format, void *context )
	{ return sk_sendNFormattedToX( sk, numElements, fl, format, context, ", " ); }
static inline int sk_sendFormattedTo( Stack sk, File fl, ObjectFormat format, void *context )
	{ return sk_sendNFormattedTo( sk, sk_depth(sk), fl, format, context ); }

#endif

