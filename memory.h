
#ifndef MEMORY_H
#define MEMORY_H

#include "base.h"

#ifdef NDEBUG
	#include <stdlib.h>
	#define mem_alloc malloc
	#define mem_realloc realloc
	#define mem_report()
#else
	FUNC void *mem_allocAnnotated(int size, const char *file, int line);
	FUNC void *mem_reallocAnnotated(void *old, int size, const char *file, int line);
	FUNC void mem_report();
	#define mem_alloc(s)     mem_allocAnnotated   ((s),     __FILE__, __LINE__)
	#define mem_realloc(p,s) mem_reallocAnnotated ((p),(s), __FILE__, __LINE__)
#endif

FUNC MemoryLifetime ml_new( int numBytesEstimate );
FUNC void *ml_alloc   ( MemoryLifetime ml, int numBytes );
FUNC void *ml_realloc ( MemoryLifetime ml, void *oldStorage, int oldNumBytes, int newNumBytes );
FUNC void  ml_free    ( MemoryLifetime ml );

#endif

