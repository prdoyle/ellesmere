
#ifndef TOKENS_H
#define TOKENS_H

#include "objects.h"

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st );

FUNC Object      ts_current    ( TokenStream ts );
FUNC Object      ts_next       ( TokenStream ts );
FUNC void        ts_advance    ( TokenStream ts );
FUNC void        ts_push       ( TokenStream ts, TokenBlock tb );
FUNC TokenBlock  ts_curBlock   ( TokenStream ts );
FUNC TokenBlock  ts_pop        ( TokenStream ts );
FUNC TokenBlock  ts_beginBlock ( TokenStream ts ); // Begin a block at the current token
FUNC TokenBlock  ts_getBlock   ( TokenStream ts ); // Return a block begun at the current token, if any
FUNC ObjectHeap  ts_heap       ( TokenStream ts );
FUNC int         ts_sendTo     ( TokenStream ts, File fl );

FUNC void       tb_append        ( TokenBlock tb, Object token );
FUNC void       tb_stopAppending ( TokenBlock tb );
FUNC int        tb_sendTo        ( TokenBlock tb, File fl, ObjectHeap heap );


#endif

