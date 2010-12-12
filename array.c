
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
	};

FUNC Array ar_new( int capacity, int elementSize )
	{
	Array result = (Array)mem_alloc( sizeof(*result) );
	result->count = 0;
	result->capacity = capacity;
	result->storage = mem_alloc( result->capacity * elementSize );
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

FUNC void ar_store( Array ar, int index, void *newValue, int elementSize )
	{
	assert( 0 <= index && index < ar->count );
	memcpy( ar_element( ar, index, elementSize ), newValue, elementSize );
	}

FUNC void ar_setCapacity( Array ar, int newCapacity, int elementSize )
	{
	assert( newCapacity >= 0 );
	if( newCapacity != ar->capacity )
		{
		ar->capacity = newCapacity;
		ar->storage = mem_realloc( ar->storage, ar->capacity * elementSize );
		if( newCapacity < ar->count )
			ar->count = newCapacity;
		}
	assert( ar_capacity(ar) == newCapacity );
	}

FUNC void ar_setCount( Array ar, int newCount, int elementSize )
	{ 
	if( newCount > ar_capacity( ar ) )
		ar_setCapacity( ar, 2 * ( newCount-1 ), elementSize );
	ar->count = newCount;
	assert( ar_count(ar) == newCount );
	}

FUNC int ar_incCount( Array ar, int elementSize )
	{ 
	ar_setCount( ar, ar_count(ar) + 1, elementSize );
	return ar->count;
	}

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

//MERGE:15

