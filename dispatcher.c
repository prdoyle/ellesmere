
#include "dispatcher.h"

typedef struct ss_struct
	{
	Symbol sy;
	struct ss_struct *next;
	} *SymbolStack;

static SymbolStack ss_new( Symbol sy, SymbolStack next )
	{
	SymbolStack result = (SymbolStack)malloc( sizeof(*result) );
	result->sy = sy;
	result->next = next;
	return result;
	}

struct di_struct
	{
	SymbolTable st;
	SymbolStack stack;
	Action      undecidedAction;
	};

FUNC Dispatcher di_new( SymbolTable st, Action undecidedAction )
	{
	Dispatcher result = (Dispatcher)malloc( sizeof(*result) );
	result->st = st;
	result->stack = NULL;
	result->undecidedAction = undecidedAction;
	return result;
	}

FUNC Action di_action( Dispatcher di, Symbol sy )
	{
	if( sy_immediateAction( sy, di->st ) )
		return sy_immediateAction( sy, di->st );
	else
		{
		di->stack = ss_new( sy, di->stack );
		return di->undecidedAction;
		}
	}

FUNC void di_discard( Dispatcher di, int numSymbols )
	{
	while( numSymbols-- >= 1 )
		{
		SymbolStack old = di->stack;
		assert( old );
		di->stack = old->next;
		free( old );
		}
	}

static int ss_sendTo( SymbolStack ss, SymbolTable st, Stream sm )
	{
	int charsSent = 0;
	if( ss->next )
		{
		charsSent += ss_sendTo( ss->next, st, sm );
		charsSent += sm_write( sm, ", " );
		}
	charsSent += sm_write( sm, "%s", sy_name( ss->sy, st ) );
	return charsSent;
	}

FUNC int di_sendTo( Dispatcher di, Stream sm )
	{
	int charsSent = sm_write( sm, "_Dispatcher_%p{ ", di );
	if( di->stack )
		charsSent += ss_sendTo( di->stack, di->st, sm );
	charsSent += sm_write( sm, " }" );
	return charsSent;
	}

//MERGE:15

