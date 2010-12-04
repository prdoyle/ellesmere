
#include "dispatcher.h"

typedef struct ss_struct
	{
	Symbol sy;
	int argsRemaining;
	struct ss_struct *next;
	} *StateStack;

static StateStack ss_new( Symbol sy, int argsRemaining, StateStack next )
	{
	assert( argsRemaining >= 1 );
	StateStack result = (StateStack)malloc( sizeof(*result) );
	result->sy = sy;
	result->argsRemaining = argsRemaining;
	result->next = next;
	return result;
	}

struct di_struct
	{
	SymbolTable st;
	StateStack  stack;
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
		{
		if( sy_arity( sy, di->st ) == 0 )
			return sy_immediateAction( sy, di->st );
		else
			di->stack = ss_new( sy, sy_arity( sy, di->st ), di->stack );
		}
	else if( di->stack && ( --di->stack->argsRemaining == 0 ) )
		{
		Action result = sy_immediateAction( di->stack->sy, di->st );
		di->stack = di->stack->next;
		return result;
		}
	return di->undecidedAction;
	}

static int ss_sendTo( StateStack ss, SymbolTable st, File fl )
	{
	int charsSent = 0;
	if( ss->next )
		{
		charsSent += ss_sendTo( ss->next, st, fl );
		charsSent += fl_write( fl, ", " );
		}
	charsSent += fl_write( fl, "%s-%d", sy_name( ss->sy, st ), ss->argsRemaining );
	return charsSent;
	}

FUNC int di_sendTo( Dispatcher di, File fl )
	{
	if( !fl )
		return 0;
	else
		{
		int charsSent = fl_write( fl, "_Dispatcher_%p{ ", di );
		if( di->stack )
			charsSent += ss_sendTo( di->stack, di->st, fl );
		charsSent += fl_write( fl, " }" );
		return charsSent;
		}
	}

//MERGE:15

