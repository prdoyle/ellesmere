
#include "dispatcher.h"
#include "memory.h"

typedef struct sf_struct
	{
	Symbol  dispatchee;
	int argsRemaining;
	int tokensRemaining;
	} *StackFrame;

static void sf_init( StackFrame frame, Symbol dispatchee, int argsRemaining, int tokensRemaining )
	{
	assert( argsRemaining >= 1 );
	frame->dispatchee = dispatchee;
	frame->argsRemaining = argsRemaining;
	frame->tokensRemaining = tokensRemaining;
	}

typedef struct ds_struct
	{
	int depth;
	int capacity;
	struct sf_struct *frames;
	} *DispatcherStack;

#define INITIAL_STACK_CAPCITY 23

static void ds_init( DispatcherStack ds )
	{
	ds->depth = 0;
	ds->capacity = INITIAL_STACK_CAPCITY;
	ds->frames = (struct sf_struct*)mem_alloc( ds->capacity * sizeof( ds->frames[0] ) );
	}

static StackFrame ds_frame( DispatcherStack ds, int depth )
	{
	assert( ds->depth >= 1 );
	return ds->frames + ds->depth-1 - depth;
	}

static StackFrame ds_topFrame( DispatcherStack ds )
	{
	StackFrame result;
	assert( ds->depth >= 1 );
	result = ds->frames + ds->depth-1;
	assert( result == ds_frame( ds, 0 ) );
	return result;
	}

static StackFrame ds_push( DispatcherStack ds )
	{
	if( ds->depth == ds->capacity )
		{
		ds->capacity *= 2;
		ds->frames = (struct sf_struct*)mem_realloc( ds->frames, ds->capacity * sizeof( ds->frames[0] ) );
		}
	return ds->frames + ds->depth++;
	}

static void ds_pop( DispatcherStack ds )
	{
	assert( ds->depth >= 1 );
	ds->depth--;
	// TODO: realloc if depth << capacity
	}

struct di_struct
	{
	ObjectHeap       heap;
	struct ds_struct stack;
	SymbolTable      st;
	Action           shiftAction;
	File             diagnostics;
	};

FUNC Dispatcher di_new( ObjectHeap heap, SymbolTable st, Action shiftAction, File diagnostics )
	{
	Dispatcher result = (Dispatcher)mem_alloc( sizeof(*result) );
	result->heap = heap;
	result->st = st;
	result->shiftAction = shiftAction;
	result->diagnostics = diagnostics;
	ds_init( &result->stack );
	return result;
	}

#if 0
static int instrumented_if( int cond, char *condStr, char *file, int line, Dispatcher di )
	{
	fl_write( di->diagnostics, "    if(%d) %s %s:%d\n", cond, condStr, file, line );
	return cond;
	}

#define if(c) if(instrumented_if(!!(c), #c, __FILE__, __LINE__, di))
#endif

FUNC Action di_action( Dispatcher di, Object ob, Context cx )
	{
	bool tryRunning = true;
	if( ob_isToken( ob, di->heap ) )
		{
		Symbol token = ob_toSymbol( ob, di->heap );
		if( di->stack.depth >= 1 )
			{
			StackFrame frame = ds_topFrame( &di->stack );
			if( frame->tokensRemaining >= 1 )
				{
				// Dispatcher is expecting this token
				frame->tokensRemaining--;
				frame->argsRemaining--;
				tryRunning = false;
				}
			else if( sy_immediateAction( token, cx ) == NULL )
				{
				// Dispatchee didn't ask for a token but it's getting one because this token has no action
				frame->argsRemaining--;
				tryRunning = false;
				}
			}
		else if( sy_immediateAction( token, cx ) == NULL )
			tryRunning = false; // No action to run
		if( tryRunning )
			{
			if( sy_arity( token, cx ) == 0 )
				{
				// Token has a known action we can take immediately
				return sy_immediateAction( token, cx );
				}
			else
				{
				// Token has a known action we can take after parsing its arguments
				StackFrame newFrame = ds_push( &di->stack );
				sf_init( newFrame, token, sy_arity(token,cx), sy_isSymbolic(token,cx)?1:0 );
				}
			}
		}
	else
		{
		if( di->stack.depth >= 1 )
			ds_topFrame( &di->stack )->argsRemaining--;
		}

	if( di->stack.depth >= 1 )
		{
		StackFrame frame = ds_topFrame( &di->stack );
		if( frame->argsRemaining == 0 )
			{
			Action result = sy_immediateAction( frame->dispatchee, cx );
			ds_pop( &di->stack );
			return result;
			}
		}

	// Returning shiftAction tells the caller we're done with this object.
	// Otherwise, caller will call di_action again with the same object.
	//
	return di->shiftAction;
	}

#if 0
#undef if
#endif

FUNC int di_sendTo( Dispatcher di, File fl )
	{
	if( !fl )
		return 0;
	else
		{
		int i;
		char *sep = "";
		int charsSent = fl_write( fl, "_Dispatcher_%p{ ", di );
		for( i = 0; i < di->stack.depth; i++ )
			{
			StackFrame sf = ds_frame( &di->stack, i );
			charsSent += fl_write( fl, "%s%s+%d", sep, sy_name( sf->dispatchee, di->st ), sf->argsRemaining );
			sep = ", ";
			}
		charsSent += fl_write( fl, " }" );
		return charsSent;
		}
	}

//MERGE:15

