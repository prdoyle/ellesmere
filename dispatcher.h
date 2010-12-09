
#ifndef DISPATCHER_H
#define DISPATCHER_H

#include "objects.h"
#include "file.h"

FUNC Dispatcher di_new( ObjectHeap heap, SymbolTable st, Action shiftAction, File diagnostics );
FUNC Action di_action( Dispatcher di, Object ob, Context cx );

FUNC int di_sendTo( Dispatcher di, File fl );

#endif

