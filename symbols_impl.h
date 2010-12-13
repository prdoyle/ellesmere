
#ifndef SYMBOLS_IMPL_H
#define SYMBOLS_IMPL_H

#include "symbols.h"

struct sy_scopedDefs
	{
	Action immediateAction;
	int    arity;
	bool   isSymbolic;
	Object value;
	};

struct sy_struct
	{
	const char *name;
	Object token;
	struct sy_scopedDefs scopedDefs;
	// cache of a couple of things to reduce malloc
	SymbolDefList freeSDL;
	Action        recentAction;
	};


#endif
