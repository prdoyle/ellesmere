
#ifndef TOKENS_H
#define TOKENS_H

#include "objects.h"

FUNC TokenStream theLexTokenStream( ObjectHeap heap );
FUNC TokenStream ts_new           ( TokenBlock tb, ObjectHeap heap );

FUNC Object      ts_current    ( TokenStream ts );
FUNC void        ts_advance    ( TokenStream ts );
FUNC void        ts_digress    ( TokenStream ts, TokenBlock tb, Object bindings ); // Start a new digression
FUNC int         ts_depth      ( TokenStream ts ); // Current number of digressions
FUNC TokenBlock  ts_tokenBlock ( TokenStream ts ); // NULL if the tream doesn't come from a TokenBlock
FUNC ObjectHeap  ts_heap       ( TokenStream ts );
FUNC int         ts_sendTo     ( TokenStream ts, File fl );
FUNC int         ts_sendNTo    ( TokenStream ts, int tokenLimit, File fl );

FUNC TokenBlock tb_new           ( MemoryLifetime ml ) ALWAYS_NEW;  // Also consider ts_beginBlock
FUNC int        tb_length        ( TokenBlock tb );
FUNC void       tb_append        ( TokenBlock tb, Object token );
FUNC void       tb_appendBlock   ( TokenBlock tb, TokenBlock suffix );
FUNC void       tb_stopAppending ( TokenBlock tb );
FUNC int        tb_sendTo        ( TokenBlock tb, File fl, ObjectHeap heap );
FUNC int        tb_sendNTo       ( TokenBlock tb, int startIndex, int tokenLimit, File fl, ObjectHeap heap );


#endif

