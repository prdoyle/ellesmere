
#include "stack.h"
#include "memory.h"

#define AR_PREFIX  ska
#define AR_TYPE    Stack
#define AR_ELEMENT Object
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define ska_new( size, ml ) ska_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

#define INITIAL_CAPACITY 19

FUNC Stack sk_new( MemoryLifetime ml )
	{
	return ska_new( INITIAL_CAPACITY, ml );
	}

FUNC int sk_depth( Stack sk )
	{
	return ska_count( sk );
	}

FUNC void sk_push( Stack sk, Object ob )
	{
	ska_append( sk, ob );
	}

FUNC Object sk_item( Stack sk, int depth )
	{
	assert( depth < sk_depth(sk) );
	return ska_getLast( sk, depth );
	}

FUNC void sk_popN( Stack sk, int count )
	{
	assert( count <= sk_depth(sk) );
	ska_incCountBy( sk, -count );
	}

FUNC Stack sk_dup( Stack other, MemoryLifetime ml )
	{
	Stack result = sk_new( ml );
	int i;
	for( i = sk_depth( other ) - 1; i >= 0; i-- )
		sk_push( result, sk_item( other, i ) );
	return result;
	}

FUNC Stack sk_mirror( Stack other, MemoryLifetime ml )
	{
	Stack result = sk_new( ml );
	int i;
	for( i=0; i < sk_depth( other ); i++ )
		sk_push( result, sk_item( other, i ) );
	return result;
	}

FUNC int sk_sendNTo( Stack sk, int numElements, File fl, ObjectHeap heap )
	{
	if( !fl )
		return 0;
	else
		{
		int i;
		int charsSent = fl_write( fl, "{ " );
		char *sep = "..., ";
		if( numElements >= sk_depth(sk) )
			{
			sep = "";
			numElements = sk_depth(sk);
			}
		for( i = numElements - 1; i >= 0; i-- )
			{
			charsSent += fl_write( fl, "%s", sep );
			sep = ", ";
			charsSent += ob_sendTo( sk_item( sk, i ), fl, heap );
			}
		charsSent += fl_write( fl, " }" );
		return charsSent;
		}
	}

FUNC int sk_sendNFormattedToX( Stack sk, int numElements, File fl, ObjectFormat format, void *context, char *separator )
	{
	if( !fl )
		return 0;
	else
		{
		int i;
		int charsSent = 0;
		if( numElements >= sk_depth(sk) )
			numElements = sk_depth(sk);
		char *sep = "";
		for( i = numElements - 1; i >= 0; i-- )
			{
			charsSent += fl_write( fl, "%s", sep );
			sep = ", ";
			charsSent += ob_sendFormattedTo( sk_item( sk, i ), fl, format, context );
			}
		return charsSent;
		}
	}

//MERGE:50

