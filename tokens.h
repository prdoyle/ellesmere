
#ifndef TOKENS_H
#define TOKENS_H

#include "objects.h"

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st );
FUNC TokenStream ts_fromBlock( TokenBlock block, ObjectHeap heap, TokenStream caller );

FUNC Object      ts_current ( TokenStream ts );
FUNC Object      ts_next    ( TokenStream ts );
FUNC void        ts_advance ( TokenStream ts );
FUNC TokenStream ts_caller  ( TokenStream ts );
FUNC ObjectHeap  ts_heap    ( TokenStream ts );

FUNC TokenBlock tb_new           ( MemoryLifetime ml );
FUNC void       tb_append        ( TokenBlock tb, Object token );
FUNC void       tb_stopAppending ( TokenBlock tb );

FUNC TokenStream ts_close( TokenStream ts ); // Returns ts_caller

#endif

