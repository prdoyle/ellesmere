
#ifndef BASE_H
#define BASE_H

#include <stdlib.h>
#include <assert.h>

typedef char bool;
#define false 0
#define true  1

// assert is used to catch internal errors
// check is used to catch user errors
static inline void check( int condition ){ assert( condition ); }

#ifndef FUNC
#define FUNC
#endif

#endif

