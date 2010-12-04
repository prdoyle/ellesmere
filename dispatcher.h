
#ifndef DISPATCHER_H
#define DISPATCHER_H

#include "symbols.h"
#include "file.h"

FUNC Dispatcher di_new( SymbolTable st, Action undecidedAction );
FUNC Action di_action( Dispatcher di, Symbol sy );
FUNC void di_discard( Dispatcher di, int numSymbols ); // pops them from the stack

FUNC int di_sendTo( Dispatcher di, File fl );

#endif

