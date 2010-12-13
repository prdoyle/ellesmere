
#include "stack.h"
#include "memory.h"

#define AR_PREFIX  ska
#define AR_TYPE    Stack
#define AR_ELEMENT Object
#define AR_BYVALUE
#include "array_template.h"

#define INITIAL_CAPACITY 19

FUNC Stack sk_new()
	{
	return ska_new( INITIAL_CAPACITY );
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

FUNC int sk_sendTo( Stack sk, File fl, ObjectHeap heap )
	{
	if( !fl )
		return 0;
	else
		{
		int i;
		int charsSent = fl_write( fl, "_Stack_%p{ ", sk );
		char *sep = "";
		for( i = sk_depth(sk) - 1; i >= 0; i-- )
			{
			charsSent += fl_write( fl, "%s", sep );
			sep = ", ";
			charsSent += ob_sendTo( sk_item( sk, i ), fl, heap );
			}
		charsSent += fl_write( fl, " }" );
		return charsSent;
		}
	}

//MERGE:50

