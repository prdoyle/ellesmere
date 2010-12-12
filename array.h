
#ifndef ARRAY_H
#define ARRAY_H

#include "base.h"

FUNC Array ar_new      ( int capacity, int elementSize );
FUNC int   ar_count    ( Array ar );
FUNC int   ar_capacity ( Array ar );
FUNC void *ar_element  ( Array ar, int index, int elementSize );

FUNC void  ar_store       ( Array ar, int index, void *newValue, int elementSize );
FUNC int   ar_incCount    ( Array ar, int elementSize );
FUNC void  ar_setCount    ( Array ar, int newCount, int elementSize );
FUNC void  ar_setCapacity ( Array ar, int newCapacity, int elementSize );

#endif
