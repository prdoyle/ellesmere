
#include "stack.h"

typedef struct ski_struct
	{
	Object value;
	struct ski_struct *next;
	} *StackItem;

static StackItem ski_new( Object value, StackItem next )
	{
	StackItem result = (StackItem)malloc( sizeof(*result) );
	result->value = value;
	result->next = next;
	return result;
	}

struct sk_struct
	{
	StackItem top;
	};

FUNC Stack sk_new()
	{
	Stack result = (Stack)malloc( sizeof(*result) );
	result->top = NULL;
	return result;
	}

FUNC int sk_depth( Stack sk )
	{
	int result=0;
	for( StackItem i = sk->top; i; i = i->next )
		result += 1;
	return result;
	}

FUNC void sk_push( Stack sk, Object ob )
	{
	sk->top = ski_new( ob, sk->top );
	}

FUNC Object sk_item( Stack sk, int depth )
	{
	assert( depth < sk_depth(sk) );
	StackItem ski = sk->top;
	while( depth-- > 0 )
		ski = ski->next;
	return ski->value;
	}

FUNC void sk_popN( Stack sk, int count )
	{
	assert( count <= sk_depth(sk) );
	while( count-- > 0 )
		sk->top = sk->top->next;
	}

FUNC int sk_sendTo( Stack sk, Stream sm, ObjectHeap heap )
	{
	int i;
	int charsSent = sm_write( sm, "_Stack_%p{ ", sk );
	char *sep = "";
	for( i=0; i < sk_depth(sk); i++ )
		{
		charsSent += sm_write( sm, "%s", sep );
		sep = ", ";
		charsSent += ob_sendTo( sk_item( sk, i ), sm, heap );
		}
	charsSent += sm_write( sm, " }" );
	return charsSent;
	}

//MERGE:25
