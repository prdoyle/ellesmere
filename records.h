
#ifndef RECORDS_H
#define RECORDS_H

#include "bitvector.h"
#include "symbols.h"

FUNC Record rd_new( BitVector fieldIDs, MemoryLifetime ml ); // NULL is a valid Record (the empty record) and other functions can cope with it

enum { rd_NONE=0 };                  // Indicates the given field ID is not present in the Records
FUNC int rd_maxIndex ( Record rd );  // Max field index
FUNC int rd_indexOf  ( Record rd, int fieldID ); // numbering starts from 1.  0 indicates no such field.

FUNC int rd_firstField ( Record rd );
FUNC int rd_nextField  ( Record rd, int prevField ); // returns rd_NONE when finished

FUNC int rd_sendTo( Record rd, File fl, SymbolTable st );

#endif

