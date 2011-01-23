
#ifndef MEMORY_H
#define MEMORY_H

#include "base.h"
#include "file.h"

FUNC MemoryLifetime ml_begin ( int numBytesEstimate, MemoryLifetime parent );
FUNC void           ml_end   ( MemoryLifetime ml );
FUNC MemoryLifetime ml_indefinite(); // Never ends

#define ml_undecided ml_indefinite // To mark uses I probably want to change later
#define ml_singleton ml_indefinite // To mark uses for singletons that might not always be singletons

FUNC void *ml_alloc      ( MemoryLifetime ml, int numBytes );
FUNC void *ml_allocZeros ( MemoryLifetime ml, int numBytes );
FUNC void *ml_realloc    ( MemoryLifetime ml, void *oldStorage, int oldNumBytes, int newNumBytes );

#ifndef NDEBUG
	// These are inside #ifndef NDEBUG to make sure we never call them in optimized builds

	FUNC void *ml_allocAnnotated      ( MemoryLifetime ml, int numBytes, const char *file, int line );
	FUNC void *ml_allocZerosAnnotated ( MemoryLifetime ml, int numBytes, const char *file, int line );
	FUNC void *ml_reallocAnnotated    ( MemoryLifetime ml, void *oldStorage, int oldNumBytes, int newNumBytes, const char *file, int line );
	FUNC int   ml_sendReportTo        ( File fl );

	#define ml_alloc(ml,s)       ml_allocAnnotated       ((ml),(s),         __FILE__, __LINE__)
	#define ml_allocZeros(ml,s)  ml_allocZerosAnnotated  ((ml),(s),         __FILE__, __LINE__)
	#define ml_realloc(ml,p,o,n) ml_reallocAnnotated     ((ml),(p),(o),(n), __FILE__, __LINE__)
#endif

#endif

