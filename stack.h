
#ifndef STACK_H
#define STACK_H

#include "objects.h"

typedef struct sk_struct *Stack;

FUNC Stack  sk_new();
FUNC int    sk_depth  ( Stack sk );
FUNC void   sk_push   ( Stack sk, Object ob );
FUNC Object sk_item   ( Stack sk, int depth );
FUNC void   sk_popN   ( Stack sk, int count );
static inline Object sk_top( Stack sk ){ return sk_item( sk, 0 ); }
static inline Object sk_pop( Stack sk ){ Object result = sk_top(sk); sk_popN( sk, 1 ); return result; }

FUNC int sk_sendTo( Stack sk, Stream sm, ObjectHeap heap );

#endif

