
#ifndef ARRAY_H
#define ARRAY_H

#include "base.h"

FUNC Array ar_new      ( int capacity, int elementSize );
FUNC Array ar_newInMB  ( int capacity, int elementSize, MemoryBatch mb );
FUNC int   ar_count    ( Array ar );
FUNC int   ar_capacity ( Array ar );
FUNC void *ar_element  ( Array ar, int index, int elementSize );
FUNC void *ar_last     ( Array ar, int indexFromEnd, int elementSize );

FUNC void  ar_store       ( Array ar, int index, void *newValue, int elementSize );
FUNC int   ar_incCount    ( Array ar, int elementSize );
FUNC int   ar_incCountBy  ( Array ar, int delta, int elementSize );
FUNC void  ar_setCount    ( Array ar, int newCount, int elementSize );
FUNC void  ar_setCapacity ( Array ar, int newCapacity, int elementSize );
FUNC void  ar_clear       ( Array ar, int newCount, int elementSize );

#endif

