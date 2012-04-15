
#ifndef TOKENS_H
#define TOKENS_H

#include "objects.h"

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st );

FUNC Object      ts_current    ( TokenStream ts );
FUNC Object      ts_currentRaw ( TokenStream ts ); // Doesn't apply bindings.  TODO: Is this necessary?
FUNC void        ts_advance    ( TokenStream ts );
FUNC void        ts_digress    ( TokenStream ts, TokenBlock tb, Object bindings ); // Start a new digression
FUNC int         ts_depth      ( TokenStream ts ); // Current number of digressions
FUNC TokenBlock  ts_curBlock   ( TokenStream ts );
FUNC Object      ts_getBindings( TokenStream ts );
FUNC void        ts_setBindings( TokenStream ts, Object bindings );
FUNC TokenBlock  ts_beginBlock ( TokenStream ts ); // Return a block begun at the current token
FUNC TokenBlock  ts_skipBlock  ( TokenStream ts ); // Return a block begun at the current token if it exists, and advance past it
FUNC ObjectHeap  ts_heap       ( TokenStream ts );
FUNC int         ts_sendTo     ( TokenStream ts, File fl );
FUNC void        ts_cancelDigression ( TokenStream ts ); // Abort an unfinished digression and resume original stream

FUNC int        tb_length        ( TokenBlock tb );
FUNC void       tb_append        ( TokenBlock tb, Object token );
FUNC void       tb_stopAppending ( TokenBlock tb );
FUNC int        tb_sendTo        ( TokenBlock tb, File fl, ObjectHeap heap );
FUNC int        tb_sendNTo       ( TokenBlock tb, int tokenLimit, File fl, ObjectHeap heap );


#endif

