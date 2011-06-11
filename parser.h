
#ifndef PARSER_H
#define PARSER_H

#include "grammar.h"
#include "file.h"

#define ITEM_SET_NUMS
#define REDUCE_CONTEXT_LENGTH // max tokens that could be involved in a reduce from the given state

FUNC Automaton  au_new ( Grammar gr, SymbolTable st, MemoryLifetime ml, File conflictLog, File diagnostics );
FUNC Grammar    au_grammar ( Automaton au );
FUNC void       au_augment ( Automaton au, Object inheritanceRelationIndex, ObjectHeap heap, SymbolTable st, File diagnostics );

FUNC Parser     ps_new ( Automaton au, MemoryLifetime ml, File diagnostics );
FUNC Automaton  ps_automaton ( Parser ps );

FUNC bool       ps_expects ( Parser ps, Object ob ); // False if pushing ob would cause a parse error
FUNC void       ps_push    ( Parser ps, Object ob );
FUNC int        ps_depth   ( Parser ps );
FUNC Production ps_handle  ( Parser ps, Object lookahead ); // OK, the "handle" is actually the symbols, not the production.  So sue me.
FUNC void       ps_popN    ( Parser ps, int count );
FUNC void       ps_close   ( Parser ps ); // Can't use ps anymore after this

FUNC int        au_sendTo  ( Automaton au, File fl, ObjectHeap heap, SymbolTable st );
FUNC int        ps_sendTo  ( Parser ps,    File fl, ObjectHeap heap, SymbolTable st );

#ifdef REDUCE_CONTEXT_LENGTH
FUNC          int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st );
#else
static inline int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st ) { return ps_depth(ps); }
#endif

static inline Grammar ps_grammar( Parser ps )
	{ return au_grammar( ps_automaton( ps ) ); }

#endif

