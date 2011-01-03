
#ifndef PARSER_H
#define PARSER_H

#include "grammar.h"
#include "file.h"

FUNC Parser  ps_new( Grammar gr, SymbolTable st, MemoryLifetime ml, File diagnostics );
FUNC Grammar ps_grammar( Parser ps );

FUNC bool       ps_expects ( Parser ps, Object ob ); // False if pushing ob would cause a parse error
FUNC void       ps_push    ( Parser ps, Object ob );
FUNC Production ps_handle  ( Parser ps, Object lookahead ); // OK, the "handle" is actually the symbols, not the production.  So sue me.
FUNC void       ps_popN    ( Parser ps, int count );

#endif

