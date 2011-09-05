
#include "memory.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct mh_struct *MemoryHunk;
struct mh_struct
	{
	MemoryHunk prev;
	int8_t *base, *alloc, *limit;
	};

#define BASIC_HUNK_SIZE 1000

static MemoryHunk mh_new( int size, MemoryHunk prev, MemoryLifetime parent )
	{
	MemoryHunk result = (MemoryHunk)ml_alloc( parent, sizeof(*result) );
	assert( size >= BASIC_HUNK_SIZE );
	result->base  = result->alloc = (int8_t*)ml_alloc( parent, size );
	result->limit = result->base + size;
	result->prev  = prev;
	return result;
	}

typedef struct header_struct *Header;

struct header_struct
	{
	Header prev;
	Header next;
	Header reallocatedFrom;
	const char *file;
	int line;
	int size;
	};

struct ml_struct
	{
	MemoryLifetime parent;
	MemoryHunk     curHunk;
	};

// These macros are for users of memory.h, not the implementation
#undef ml_alloc
#undef ml_allocZeros
#undef ml_realloc
#undef ml_allocAnnotated
#undef ml_allocZerosAnnotated
#undef ml_reallocAnnotated

#ifndef NDEBUG

static Header lastHeader = NULL;

FUNC void *ml_allocAnnotated(MemoryLifetime ml, int size, const char *file, int line)
	{
	Header result = (Header)ml_alloc(ml, size + sizeof(*result));
	check( result );
	result->file = file;
	result->line = line;
	result->size = size;
	result->prev = lastHeader;
	if( lastHeader )
		lastHeader->next = result;
	result->next = NULL;
	result->reallocatedFrom = NULL;
	lastHeader = result;
	return result+1;
	}

FUNC void *ml_allocZerosAnnotated(MemoryLifetime ml, int size, const char *file, int line)
	{
	void *result = ml_allocAnnotated( ml, size, file, line );
	memset( result, 0, size );
	return result;
	}

FUNC void *ml_reallocAnnotated(MemoryLifetime ml, void *oldStorage, int oldSize, int newSize, const char *file, int line)
	{
	if( oldSize == newSize )
		return oldStorage; // Pretend this never happened

	Header oldHeader, newHeader, result;
	oldHeader = ((Header)oldStorage) - 1;
	// Make a "naked header" to record the original size and the realloc file/line
	newHeader = ((Header)ml_allocAnnotated( ml, 1, file, line )) - 1;
	newHeader->size = oldHeader->size;
	// Realloc the new block
	result = ml_realloc( ml, oldHeader, oldSize + sizeof(*oldHeader), newSize + sizeof(*newHeader) );
	check( result );
	// Update new header to record new size
	result->size = newSize;
	// Fix up links
	if( result->prev )
		result->prev->next = result;
	else
		lastHeader = result;
	if( result->next )
		result->next->prev = result;
	result->reallocatedFrom = newHeader;
	return result+1;
	}

FUNC int ml_sendReportTo( File fl )
	{
	Header h; int charsSent = 0;
	for( h = lastHeader; h; h = h->prev )
		{
		char *sep = "";
		Header cur;
		for( cur = h; cur; cur = cur->reallocatedFrom )
			{
			charsSent += fl_write( fl, "%s%d bytes %s line %d", sep, cur->size, cur->file, cur->line );
			sep = " from ";
			}
		charsSent += fl_write( fl, "\n" );
		}
	return charsSent;
	}

#endif

FUNC MemoryLifetime ml_indefinite()
	{
	static struct ml_struct result = { (MemoryLifetime)0xdead4 };
	return &result;
	}

FUNC MemoryLifetime ml_begin( int numBytesEstimate, MemoryLifetime parent )
	{
	MemoryLifetime result = (MemoryLifetime)ml_alloc( parent, sizeof(*result) );
	result->parent  = parent;
	result->curHunk = (MemoryHunk)mh_new( numBytesEstimate + BASIC_HUNK_SIZE, NULL, parent );
	return result;
	}

FUNC void *ml_alloc( MemoryLifetime ml, int numBytes )
	{
	void *result;
	if( ml == ml_indefinite() )
		result = malloc( numBytes );
	else
		{
		MemoryHunk hunk = ml->curHunk;
		if( hunk->limit - hunk->alloc < numBytes )
			{
			int newSize = 2 * ( hunk->limit - hunk->base );
			if( newSize <  numBytes )
				{
				// Big allocation - give it its own hunk and keep using the existing one
				hunk->prev = mh_new( numBytes, hunk->prev, ml->parent );
				hunk = hunk->prev;
				}
			else
				hunk = ml->curHunk = mh_new( newSize, hunk, ml->parent );
			}
		result = hunk->alloc;
		hunk->alloc += numBytes;
		}
	return result;
	}

FUNC void *ml_allocZeros( MemoryLifetime ml, int numBytes )
	{
	void *result = ml_alloc( ml, numBytes );
	memset( result, 0, numBytes );
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
	if( ml == ml_indefinite() )
		return realloc( oldStorage, newNumBytes );
	else
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
	}

FUNC void ml_end( MemoryLifetime ml )
	{
	MemoryHunk hunk, prevHunk;
	assert( ml != ml_indefinite() );
	if( ml->parent != ml_indefinite() )
		return; // No way to free these yet
	return; // No way to tell annotated from non-annotated blocks yet
	for( hunk = ml->curHunk; hunk; hunk = prevHunk )
		{
		prevHunk = hunk->prev;
		memset( hunk->base, 0xde, sizeof(void*) );
		free( hunk->base );
		hunk->prev = (MemoryHunk)0xdead2;
		free( hunk );
		}
	ml->curHunk = (MemoryHunk)0xdead3;
	free( ml );
	}

// MERGE:5

