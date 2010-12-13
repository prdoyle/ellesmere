
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

typedef struct sfa_struct *StackFrameArray;
#define AR_PREFIX  sfa
#define AR_TYPE    StackFrameArray
#define AR_ELEMENT struct sf_struct
#undef AR_BYVALUE
#include "array_template.h"

static StackFrame sfa_frame( StackFrameArray sfa, int depth )
	{
	assert( sfa_count(sfa) > depth );
	return sfa_last( sfa, depth );
	}

static StackFrame sfa_topFrame( StackFrameArray sfa )
	{
	assert( sfa_count(sfa) >= 1 );
	return sfa_last( sfa, 0 );
	}

static StackFrame sfa_push( StackFrameArray sfa )
	{
	return sfa_element( sfa, sfa_incCount( sfa ) - 1 );
	}

static void sfa_pop( StackFrameArray sfa )
	{
	assert( sfa_count(sfa) >= 1 );
	sfa_incCountBy( sfa, -1 );
	}

struct di_struct
	{
	ObjectHeap       heap;
	StackFrameArray  stack;
	SymbolTable      st;
	Action           shiftAction;
	File             diagnostics;
	};

FUNC Dispatcher di_new( ObjectHeap heap, SymbolTable st, Action shiftAction, File diagnostics )
	{
	Dispatcher result = (Dispatcher)mem_alloc( sizeof(*result) );
	result->heap = heap;
	result->stack = sfa_new( 19 );
	result->st = st;
	result->shiftAction = shiftAction;
	result->diagnostics = diagnostics;
	return result;
	}

FUNC Action di_action( Dispatcher di, Object ob, Context cx )
	{
	bool tryRunning = true;
	if( ob_isToken( ob, di->heap ) )
		{
		Symbol token = ob_toSymbol( ob, di->heap );
		if( sfa_count( di->stack ) >= 1 )
			{
			StackFrame frame = sfa_topFrame( di->stack );
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
				StackFrame newFrame = sfa_push( di->stack );
				sf_init( newFrame, token, sy_arity(token,cx), sy_isSymbolic(token,cx)?1:0 );
				}
			}
		}
	else
		{
		if( sfa_count( di->stack ) >= 1 )
			sfa_topFrame( di->stack )->argsRemaining--;
		}

	if( sfa_count( di->stack ) >= 1 )
		{
		StackFrame frame = sfa_topFrame( di->stack );
		if( frame->argsRemaining == 0 )
			{
			Action result = sy_immediateAction( frame->dispatchee, cx );
			sfa_pop( di->stack );
			return result;
			}
		}

	// Returning shiftAction tells the caller we're done with this object.
	// Otherwise, caller will call di_action again with the same object.
	//
	return di->shiftAction;
	}

FUNC int di_sendTo( Dispatcher di, File fl )
	{
	if( !fl )
		return 0;
	else
		{
		int i;
		char *sep = "";
		int charsSent = fl_write( fl, "_Dispatcher_%p{ ", di );
		for( i = 0; i < sfa_count( di->stack ); i++ )
			{
			StackFrame sf = sfa_frame( di->stack, i );
			charsSent += fl_write( fl, "%s%s+%d", sep, sy_name( sf->dispatchee, di->st ), sf->argsRemaining );
			sep = ", ";
			}
		charsSent += fl_write( fl, " }" );
		return charsSent;
		}
	}

//MERGE:60

