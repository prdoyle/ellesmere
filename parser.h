
#ifndef PARSER_H
#define PARSER_H

#include "grammar.h"
#include "file.h"

FUNC Parser  ps_new( Grammar gr, SymbolTable st, MemoryLifetime ml, File conflictLog, File diagnostics );
FUNC Grammar ps_grammar( Parser ps );

FUNC bool       ps_expects ( Parser ps, Object ob ); // False if pushing ob would cause a parse error
FUNC void       ps_push    ( Parser ps, Object ob );
FUNC int        ps_depth   ( Parser ps );
FUNC Production ps_handle  ( Parser ps, Object lookahead ); // OK, the "handle" is actually the symbols, not the production.  So sue me.
FUNC void       ps_popN    ( Parser ps, int count );

FUNC int        ps_sendTo      ( Parser ps, File fl, ObjectHeap heap, SymbolTable st );
FUNC int        ps_sendStateTo ( Parser ps, File fl, ObjectHeap heap, SymbolTable st );

#endif

