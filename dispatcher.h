
#ifndef DISPATCHER_H
#define DISPATCHER_H

#include "symbols.h"
#include "stream.h"

typedef struct di_struct *Dispatcher;

FUNC Dispatcher di_new( SymbolTable st, Action undecidedAction );
FUNC Action di_action( Dispatcher di, Symbol sy );
FUNC void di_discard( Dispatcher di, int numSymbols ); // pops them from the stack

static inline Actor      di_actor( Dispatcher di ){ return (Actor)di; }
static inline Dispatcher di_fromActor( Actor ar ){ return (Dispatcher)ar; }

FUNC int di_sendTo( Dispatcher di, Stream sm );

#endif

