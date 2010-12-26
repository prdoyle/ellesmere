
#ifndef PARSER_H
#define PARSER_H

#include "grammar.h"

FUNC Parser  ps_new( Grammar gr, SymbolTable st );
FUNC Grammar ps_grammar( Parser ps );

FUNC void       ps_push   ( Parser ps, Object ob );
FUNC Production ps_handle ( Parser ps );
FUNC void       ps_popN   ( Parser ps, int count );

#endif

