
#ifndef TOKENS_H
#define TOKENS_H

#include "objects.h"

FUNC TokenStream theLexTokenStream( ObjectHeap heap, SymbolTable st );
FUNC TokenStream ts_fromBlock( TokenBlock block, ObjectHeap heap, TokenStream caller );

FUNC Object      ts_next   ( TokenStream ts );
FUNC TokenStream ts_caller ( TokenStream ts );
FUNC ObjectHeap  ts_heap   ( TokenStream ts );

FUNC TokenBlock ts_recordUntil( TokenStream ts, Symbol terminator );

FUNC TokenStream ts_close( TokenStream ts ); // Returns ts_caller

#endif

