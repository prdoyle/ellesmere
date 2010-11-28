
#ifndef LEX_H
#define LEX_H

#include "base.h"

typedef enum {
	ERROR    =-1,
	NO_TOKEN = 0,
	INT,
	STRING,
	WORD,
	NUM_TOKENS
} token_t;

int         currentLineNumber();
const char *lastString();
int         lastInt();
const char *lastWord();

#endif

