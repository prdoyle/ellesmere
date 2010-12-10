
#include "stack.h"
#include "memory.h"

struct sk_struct
	{
	int depth;
	int capacity;
	Object *items;
	};

#define INITIAL_CAPACITY 19

FUNC Stack sk_new()
	{
	Stack result = (Stack)mem_alloc( sizeof(*result) );
	result->depth = 0;
	result->capacity = INITIAL_CAPACITY;
	result->items = (Object*)mem_alloc( result->capacity * sizeof( result->items[0] ) );
	return result;
	}

FUNC int sk_depth( Stack sk )
	{
	return sk->depth;
	}

FUNC void sk_push( Stack sk, Object ob )
	{
	if( sk->depth >= sk->capacity )
		{
		sk->capacity *= 2;
		sk->items = mem_realloc( sk->items, sk->capacity * sizeof( sk->items[0] ) );
		}
	sk->items[ sk->depth++ ] = ob;
	}

FUNC Object sk_item( Stack sk, int depth )
	{
	assert( depth < sk_depth(sk) );
	return sk->items[ sk->depth-1 - depth ];
	}

FUNC void sk_popN( Stack sk, int count )
	{
	assert( count <= sk_depth(sk) );
	sk->depth -= count;
	if( sk->depth < sk->capacity / 16 && (sk->capacity/8) >= INITIAL_CAPACITY )
		{
		sk->capacity /= 8;
		sk->items = mem_realloc( sk->items, sk->capacity * sizeof( sk->items[0] ) );
		}
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

