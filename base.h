
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

typedef struct an_struct *Action;
typedef struct ar_struct *Actor;
typedef struct di_struct *Dispatcher;
typedef struct ob_struct *Object;
typedef struct oh_struct *ObjectHeap;
typedef struct sk_struct *Stack;
typedef struct st_struct *SymbolTable;
typedef struct sy_struct *Symbol;
typedef struct tb_struct *TokenBlock;
typedef struct ts_struct *TokenStream;

#endif
