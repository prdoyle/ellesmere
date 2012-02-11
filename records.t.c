
#include "records.h"
#include "memory.h"
#include <stdio.h>

static int test1[] = { 0, 1, 2, 29, 30, 31, 32, 33, 62, 63, 64, 65 };

int main( int argc, char *argv[] )
	{
	BitVector fields = bv_new( test1[ asizeof(test1) - 1 ], ml_indefinite() );
	bv_populate( fields, test1, asizeof( test1 ) );
	Record rd = rd_new( fields, ml_indefinite() );
	BitVector fieldMap = bv_new( asizeof(test1), ml_indefinite() );
	int i;
	for( i=0; i < asizeof(test1); i++ )
		{
		int index = rd_indexOf( rd, test1[i] );
		if( bv_isSet( fieldMap, index ) )
			{
			fprintf( stderr, "ERROR: Field %d got index %d already in use\n", i, index );
			return 1;
			}
		else
			{
			bv_set( fieldMap, index );
			}
		}
	if( rd_indexOf( rd, 10 ) )
		{
		fprintf( stderr, "ERROR: Nonexistent field %d got index %d\n", 10, rd_indexOf( rd, 10 ) );
		return 1;
		}
	return 0;
	}

