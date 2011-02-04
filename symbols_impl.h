
#ifndef SYMBOLS_IMPL_H
#define SYMBOLS_IMPL_H

#include "symbols.h"

struct sy_scopedDefs
	{
	Object value;
	};

struct sy_struct
	{
	const char *name;
	Object token;
	Record instanceShape;
	struct sy_scopedDefs scopedDefs;
	};


#endif
