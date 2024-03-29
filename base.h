
#ifndef BASE_H
#define BASE_H

#include <assert.h>
#include <stdint.h>

typedef char bool;
#define false 0
#define true  1

// assert is used to catch internal errors
// check is used to catch user errors
#define check assert

#define xassert(...) // Expensive assertions: consistency checks that can increase algorithmic complexity

#define asizeof(x) ( sizeof(x) / sizeof((x)[0]) )

#ifndef FUNC
#define FUNC
#endif

#ifndef NATIVE_ACTION
#define NATIVE_ACTION
#endif

typedef struct ar_struct *Array;
typedef struct au_struct *Automaton;
typedef struct bv_struct *BitVector;
typedef struct cl_struct *CheckList;
typedef struct di_struct *Dispatcher;
typedef struct fn_struct *Function;
typedef struct gr_struct *Grammar;
typedef struct gl_struct *GrammarLine;
typedef struct ir_struct *InheritanceRelation;
typedef struct ml_struct *MemoryLifetime;
typedef struct ps_struct *Parser;
typedef struct pn_struct *Production;
typedef struct rd_struct *Record;
typedef struct sk_struct *Stack;
typedef struct tb_struct *TokenBlock;
typedef struct ts_struct *TokenStream;

static inline int min(int a, int b)
	{ return a<b? a : b; }

static inline int max(int a, int b)
	{ return a>b? a : b; }

static inline void *PH(void *pointer) // pointer hash -- makes %p smaller
	{
	uintptr_t result = (uintptr_t)pointer;
	result ^= ( result>>12 );
	result ^= ( result>>24 );
	return (void*)((uintptr_t)result & 0xfff);
	}

#define LIKE_PRINTF( FORMAT, FIRST_ARG ) __attribute__ ((format (printf, FORMAT, FIRST_ARG)))
#define LIKE_VPRINTF( FORMAT )           __attribute__ ((format (printf, FORMAT, 0)))
#define ALWAYS_NEW                       __attribute__ ((malloc))

#endif
