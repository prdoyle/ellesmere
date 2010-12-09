
#include "dispatcher.h"
#include "memory.h"

typedef struct ss_struct
	{
	struct ss_struct *next;
	Symbol  dispatchee;
	int argsRemaining;
	int tokensRemaining;
	} *StateStack;

static StateStack ss_new( Symbol dispatchee, int argsRemaining, int tokensRemaining, StateStack next )
	{
	assert( argsRemaining >= 1 );
	StateStack result = (StateStack)mem_alloc( sizeof(*result) );
	result->dispatchee = dispatchee;
	result->argsRemaining = argsRemaining;
	result->tokensRemaining = tokensRemaining;
	result->next = next;
	return result;
	}

struct di_struct
	{
	ObjectHeap  heap;
	SymbolTable st;
	StateStack  stack;
	Action      shiftAction;
	File        diagnostics;
	};

FUNC Dispatcher di_new( ObjectHeap heap, SymbolTable st, Action shiftAction, File diagnostics )
	{
	Dispatcher result = (Dispatcher)mem_alloc( sizeof(*result) );
	result->heap = heap;
	result->st = st;
	result->stack = NULL;
	result->shiftAction = shiftAction;
	result->diagnostics = diagnostics;
	return result;
	}

static int instrumented_if( int cond, char *condStr, char *file, int line, Dispatcher di )
	{
	fl_write( di->diagnostics, "    if(%d) %s %s:%d\n", cond, condStr, file, line );
	return cond;
	}

#define if(c) if(instrumented_if(!!(c), #c, __FILE__, __LINE__, di))

FUNC Action di_action( Dispatcher di, Object ob, Scope sc )
	{
	bool tryRunning = true;
	if( ob_isToken( ob, di->heap ) )
		{
		Symbol token = ob_toSymbol( ob, di->heap );
		if( di->stack )
			{
			if( di->stack->tokensRemaining >= 1 )
				{
				// Dispatcher is expecting this token
				di->stack->tokensRemaining--;
				di->stack->argsRemaining--;
				tryRunning = false;
				}
			else if( sy_immediateAction( token, sc ) == NULL )
				{
				// Dispatchee didn't ask for a token but it's getting one because this token has no action
				di->stack->argsRemaining--;
				tryRunning = false;
				}
			}
		else if( sy_immediateAction( token, sc ) == NULL )
			tryRunning = false; // No action to run
		if( tryRunning )
			{
			if( sy_arity( token, sc ) == 0 )
				{
				// Token has a known action we can take immediately
				return sy_immediateAction( token, sc );
				}
			else
				{
				// Token has a known action we can take after parsing its arguments
				di->stack = ss_new( token, sy_arity(token,sc), sy_isSymbolic(token,sc)?1:0, di->stack );
				}
			}
		}
	else
		{
		if( di->stack )
			di->stack->argsRemaining--;
		}

	if( di->stack )
		{
		if( di->stack->argsRemaining == 0 )
			{
			Action result = sy_immediateAction( di->stack->dispatchee, sc );
			di->stack = di->stack->next;
			return result;
			}
		}

	// Returning shiftAction tells the caller we're done with this object.
	// Otherwise, caller will call di_action again with the same object.
	//
	return di->shiftAction;
	}

#undef if

static int ss_sendTo( StateStack ss, SymbolTable st, File fl )
	{
	int charsSent = 0;
	if( ss->next )
		{
		charsSent += ss_sendTo( ss->next, st, fl );
		charsSent += fl_write( fl, ", " );
		}
	charsSent += fl_write( fl, "%s+%d", sy_name( ss->dispatchee, st ), ss->argsRemaining );
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

