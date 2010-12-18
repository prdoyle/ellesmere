
#include "array.h"
#include "memory.h"
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

struct ar_struct
	{
	int   count;
	int   capacity;
	void *storage;
	MemoryBatch mb;
	};

FUNC Array ar_new( int capacity, int elementSize )
	{
	Array result = (Array)mem_alloc( sizeof(*result) );
	result->count = 0;
	result->capacity = capacity;
	result->mb = NULL;
	result->storage = mem_alloc( result->capacity * elementSize );
	return result;
	}

FUNC Array ar_newInMB( int capacity, int elementSize, MemoryBatch mb )
	{
	Array result = (Array)mb_alloc( mb, sizeof(*result) );
	result->count = 0;
	result->capacity = capacity;
	result->mb = mb;
	result->storage = mb_alloc( mb, result->capacity * elementSize );
	return result;
	}

FUNC int ar_count( Array ar )
	{
	return ar->count;
	}

FUNC int ar_capacity( Array ar )
	{
	return ar->capacity;
	}

FUNC void *ar_element( Array ar, int index, int elementSize )
	{
	assert( 0 <= index && index < ar->count );
	return (void*)( (intptr_t)ar->storage + index * elementSize );
	}

FUNC void *ar_last( Array ar, int indexFromEnd, int elementSize )
	{
	assert( 0 <= indexFromEnd && indexFromEnd < ar->count );
	return ar_element( ar, (ar->count-1 - indexFromEnd), elementSize );
	}

FUNC void ar_store( Array ar, int index, void *newValue, int elementSize )
	{
	assert( 0 <= index && index < ar->count );
	memcpy( ar_element( ar, index, elementSize ), newValue, elementSize );
	}

static void ar_changeCapacity( Array ar, int newCapacity, int elementSize ) __attribute__((noinline));
static void ar_changeCapacity( Array ar, int newCapacity, int elementSize )
	{
	assert( newCapacity != ar->capacity );
	if( ar->mb )
		ar->storage = mb_realloc( ar->mb, ar->storage, ar->capacity * elementSize, newCapacity * elementSize );
	else
		ar->storage = mem_realloc( ar->storage, newCapacity * elementSize );
	ar->capacity = newCapacity;
	if( newCapacity < ar->count )
		ar->count = newCapacity;
	}

FUNC void ar_setCapacity( Array ar, int newCapacity, int elementSize ) __attribute__((always_inline));
FUNC void ar_setCapacity( Array ar, int newCapacity, int elementSize )
	{
	assert( newCapacity >= 0 );
	if( newCapacity != ar->capacity )
		ar_changeCapacity( ar, newCapacity, elementSize );
	assert( ar_capacity(ar) == newCapacity );
	}

FUNC void ar_setCount( Array ar, int newCount, int elementSize ) __attribute__((always_inline));
FUNC void ar_setCount( Array ar, int newCount, int elementSize )
	{ 
	if( newCount > ar_capacity( ar ) )
		ar_changeCapacity( ar, 2 * ( newCount-1 ), elementSize );
	ar->count = newCount;
	assert( ar_count(ar) == newCount );
	}

FUNC int ar_incCount( Array ar, int elementSize ) __attribute__((always_inline));
FUNC int ar_incCount( Array ar, int elementSize )
	{ 
	ar_setCount( ar, ar_count(ar) + 1, elementSize );
	return ar->count;
	}

FUNC int ar_incCountBy( Array ar, int delta, int elementSize ) __attribute__((always_inline));
FUNC int ar_incCountBy( Array ar, int delta, int elementSize )
	{ 
	ar_setCount( ar, ar_count(ar) + delta, elementSize );
	return ar->count;
	}

/*
typedef Array IntArray;
#define AR_PREFIX  ia
#define AR_TYPE    IntArray
#define AR_ELEMENT int
#define AR_BYVALUE
#include "array_template.h"

static void test()
	{
	IntArray ia = ia_new( 123 );
	ia_append( ia, 456 );
	}
*/

//MERGE:15

