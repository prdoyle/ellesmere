
#include "memory.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef NDEBUG

#undef mem_alloc // that's for users of memory.h, not the implementation!

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

#endif

typedef struct mh_struct *MemoryHunk;
struct mh_struct
	{
	MemoryHunk prev;
	int8_t *base, *alloc, *limit;
	};

static MemoryHunk mh_new( int size, MemoryHunk prev )
	{
	// if you want to use mem_alloc here, be careful freeing it
	MemoryHunk result = (MemoryHunk)malloc( sizeof(*result) );
	result->base  = result->alloc = (int8_t*)malloc( size );
	result->limit = result->base + size;
	result->prev  = prev;
	return result;
	}

struct ml_struct
	{
	MemoryHunk curHunk;
	};

#define BASIC_HUNK_SIZE 1000

FUNC MemoryLifetime ml_new( int numBytesEstimate )
	{
	MemoryLifetime result = (MemoryLifetime)malloc( sizeof(*result) );
	result->curHunk = (MemoryHunk)mh_new( numBytesEstimate + BASIC_HUNK_SIZE, NULL );
	return result;
	}

FUNC void *ml_alloc( MemoryLifetime ml, int numBytes )
	{
	MemoryHunk hunk = ml->curHunk; void *result;
	if( hunk->limit - hunk->alloc < numBytes )
		{
		int newSize = 2 * ( hunk->limit - hunk->base );
		if( newSize <  numBytes )
			{
			// Big allocation - give it its own hunk and keep using the existing one
			hunk->prev = mh_new( numBytes, hunk->prev );
			hunk = hunk->prev;
			}
		else
			hunk = ml->curHunk = mh_new( newSize, hunk );
		}
	result = hunk->alloc;
	hunk->alloc += numBytes;
	return result;
	}

static bool mh_reallocInPlace( MemoryHunk mh, int8_t *oldEnd, int delta )
	{
	if( mh->alloc == oldEnd )
		{
		// This was the last allocation from mh.  Just pretend it was the modified size.
		mh->alloc += delta;
		return true;
		}
	else if( mh->prev && mh_reallocInPlace( mh->prev, oldEnd, delta ) )
		{
		// This was the last allocation from some previous mh.  It has already been resized.
		return true;
		}
	else if( delta <= 0 )
		{
		// Whatever mh it's in, ignore the extra bytes allocated (thereby causing fragmentation)
		return true;
		}

	// Nothing we can do within this MemoryHunk
	return false;
	}

FUNC void *ml_realloc( MemoryLifetime ml, void *oldStorage, int oldNumBytes, int newNumBytes )
	{
	int8_t *oldEnd = ((int8_t*)oldStorage) + oldNumBytes;
	int      delta = newNumBytes - oldNumBytes;
	if( mh_reallocInPlace( ml->curHunk, oldEnd, delta ) )
		return oldStorage;
	else
		{
		void *result = ml_alloc( ml, newNumBytes );
		memcpy( result, oldStorage, oldNumBytes );
		return result;
		}
	}

FUNC void ml_free( MemoryLifetime ml )
	{
	MemoryHunk hunk, prevHunk;
	for( hunk = ml->curHunk; hunk; hunk = prevHunk )
		{
		prevHunk = hunk->prev;
		free( hunk->base );
		free( hunk );
		}
	ml->curHunk = (MemoryHunk)0xdead;
	free( ml );
	}

// MERGE:5

