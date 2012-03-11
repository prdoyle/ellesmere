
#ifndef PARSER_H
#define PARSER_H

#include "grammar.h"
#include "stack.h"
#include "file.h"

#define ITEM_SET_NUMS
#define REDUCE_CONTEXT_LENGTH // max tokens that could be involved in a reduce from the given state

FUNC Automaton  au_new ( Grammar gr, SymbolTable st, ObjectHeap heap, MemoryLifetime ml, File conflictLog, File diagnostics );
FUNC Grammar    au_grammar ( Automaton au );

FUNC Parser     ps_new ( Automaton au, MemoryLifetime ml, File diagnostics );
FUNC Automaton  ps_automaton ( Parser ps );

FUNC bool       ps_expects ( Parser ps, Object ob ); // False if pushing ob would cause a parse error
FUNC void       ps_push    ( Parser ps, Object ob );
FUNC void       ps_popN    ( Parser ps, int objectCount );
FUNC Symbol     ps_handle  ( Parser ps, Object lookahead ); // OK, the "handle" is actually the tokens, not the production.  So sue me.
FUNC void       ps_close   ( Parser ps ); // Can't use ps anymore after this

// Modify this at your own risk.  You could make the parser's internal state inconsistent.
FUNC Stack      ps_operandStack ( Parser ps );

FUNC int        au_sendTo  ( Automaton au, File fl, ObjectHeap heap, SymbolTable st );
FUNC int        ps_sendTo  ( Parser ps,    File fl, ObjectHeap heap, SymbolTable st );

#ifdef REDUCE_CONTEXT_LENGTH
FUNC          int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st );
#else
static inline int ps_reduceContextLength( Parser ps, ObjectHeap heap, SymbolTable st ) { return ps_depth(ps); }
#endif

static inline Grammar ps_grammar( Parser ps )
	{ return au_grammar( ps_automaton( ps ) ); }

static inline Object ps_pop( Parser ps )
	{
	Object result = sk_top( ps_operandStack( ps ) );
	ps_popN( ps, 1 );
	return result;
	}

#endif

