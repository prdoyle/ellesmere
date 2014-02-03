
#include "array.h"
#include "memory.h"
#include <stdlib.h>
#include <string.h>

// These macros are for users of memory.h, not the implementation
#undef ar_new

struct ar_struct
	{
	int   count;
	int   capacity;
	void *storage;
	MemoryLifetime ml;
	};

static Array ar_init( int capacity, int elementSize, MemoryLifetime ml, void *arrayStruct, void *storage )
	{
	Array result = arrayStruct;
	result->count = 0;
	result->capacity = capacity;
	result->ml = ml;
	result->storage = storage;
	return result;
	}

FUNC Array ar_new( int capacity, int elementSize, MemoryLifetime ml )
	{
	return ar_init(
		capacity, elementSize, ml,
		ml_alloc( ml, sizeof(struct ar_struct) ),
		ml_alloc( ml, capacity * elementSize )
		);
	}

#ifndef NDEBUG
FUNC Array ar_newAnnotated( int capacity, int elementSize, MemoryLifetime ml, const char *file, int line )
	{
	return ar_init(
		capacity, elementSize, ml,
		ml_allocAnnotated( ml, sizeof(struct ar_struct), file, line ),
		ml_allocAnnotated( ml, capacity * elementSize, file, line )
		);
	}
#endif

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
	if( newCapacity * elementSize > 30000 )
		printf( "# Hey!  Allocated big array: %d bytes\n", newCapacity * elementSize );
	ar->storage = ml_realloc( ar->ml, ar->storage, ar->capacity * elementSize, newCapacity * elementSize );
	ar->capacity = newCapacity;
	if( newCapacity < ar->count )
		ar->count = newCapacity;
	}

//FUNC void ar_setCapacity( Array ar, int newCapacity, int elementSize ) __attribute__((always_inline));
FUNC void ar_setCapacity( Array ar, int newCapacity, int elementSize )
	{
	assert( newCapacity >= 0 );
	if( newCapacity != ar->capacity )
		ar_changeCapacity( ar, newCapacity, elementSize );
	assert( ar_capacity(ar) == newCapacity );
	}

//FUNC void ar_setCount( Array ar, int newCount, int elementSize ) __attribute__((always_inline));
FUNC void ar_setCount( Array ar, int newCount, int elementSize )
	{ 
	if( newCount > ar_capacity( ar ) )
		ar_changeCapacity( ar, 2 * newCount, elementSize );
	ar->count = newCount;
	assert( ar_count(ar) == newCount );
	}

//FUNC int ar_incCountBy( Array ar, int delta, int elementSize ) __attribute__((always_inline));
FUNC int ar_incCountBy( Array ar, int delta, int elementSize )
	{ 
	ar_setCount( ar, ar_count(ar) + delta, elementSize );
	return ar->count;
	}

FUNC void ar_clear( Array ar, int newCount, int elementSize )
	{
	ar_setCount( ar, newCount, elementSize );
	memset( ar->storage, 0, newCount * elementSize );
	}

//MERGE:15

