
#ifndef BASE_H
#define BASE_H

#include <assert.h>

typedef char bool;
#define false 0
#define true  1

// assert is used to catch internal errors
// check is used to catch user errors
#define check assert

#define asizeof(x) ( sizeof(x) / sizeof((x)[0]) )

#ifndef FUNC
#define FUNC
#endif

typedef struct ar_struct *Array;
typedef struct au_struct *Automaton;
typedef struct bv_struct *BitVector;
typedef struct cl_struct *CheckList;
typedef struct cx_struct *Context;
typedef struct di_struct *Dispatcher;
typedef struct fn_struct *Function;
typedef struct gr_struct *Grammar;
typedef struct ml_struct *MemoryLifetime;
typedef struct ob_struct *Object;
typedef struct oh_struct *ObjectHeap;
typedef struct ps_struct *Parser;
typedef struct pn_struct *Production;
typedef struct sk_struct *Stack;
typedef struct st_struct *SymbolTable;
typedef struct sy_struct *Symbol;
typedef struct tb_struct *TokenBlock;
typedef struct ts_struct *TokenStream;

#endif
