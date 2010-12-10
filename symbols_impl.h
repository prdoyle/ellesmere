
#ifndef SYMBOLS_IMPL_H
#define SYMBOLS_IMPL_H

#include "symbols.h"

enum{ MAX_SYMBOLS=500 };

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
	};


#endif
