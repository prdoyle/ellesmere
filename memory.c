
#include "memory.h"
#undef mem_alloc // that's for users of memory.h, not the implementation!
#include <stdlib.h>
#include <stdio.h>

typedef struct header_struct
	{
	struct header_struct *prev;
	struct header_struct *next;
	const char *file;
	int line;
	int size;
	} Header;

static Header *lastHeader;

FUNC void *mem_allocAnnotated(int size, const char *file, int line)
	{
	Header *result = (Header*)malloc(size + sizeof(Header));
	check( result );
	result->file = file;
	result->line = line;
	result->size = size;
	result->prev = lastHeader;
	if( lastHeader )
		lastHeader->next = result;
	result->next = NULL;
	lastHeader = result;
	return result+1;
	}

FUNC void *mem_reallocAnnotated(void *old, int size, const char *file, int line)
	{
	Header *oldHeader, *newHeader, *result;
	oldHeader = ((Header*)old) - 1;
	// Make a "naked header" to record the original alloc info
	newHeader = ((Header*)mem_allocAnnotated( 1, oldHeader->file, oldHeader->line )) - 1;
	newHeader->size = oldHeader->size;
	// Realloc the new block
	result = (Header*)realloc(oldHeader, size + sizeof(Header));
	check( result );
	// Update new header to record new alloc info
	result->file = file;
	result->line = line;
	result->size = size;
	// Fix up links
	if( result->prev )
		result->prev->next = result;
	else
		lastHeader = result;
	if( result->next )
		result->next->prev = result;
	return result+1;
	}

FUNC void mem_report()
	{
	Header *h;
	for( h = lastHeader; h; h = h->prev )
		printf("%d %s %d\n", h->size, h->file, h->line);
	}

