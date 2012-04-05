
#include "file.h"
#include <stdlib.h>

FUNC int fl_write( File fl, const char *format, ... )
	{
	if( !fl )
		return 0;
	else
		{
		int result;
		va_list args;
		va_start( args, format );
		result = fl_vwrite( fl, format, args );
		va_end( args );
		return result;
		}
	}

FUNC int fl_vwrite( File fl, const char *format, va_list args )
	{
	if( !fl )
		return 0;
	else
		return vfprintf( fl, format, args );
	}

// Mark optimizations with this
FUNC int optional(char *format, ...)
	{
	static bool  initialized = false;
	static char *optLimitStr;
	static int   optLimit;
	static int   optCount = 0;
	if( !initialized )
		{
		optLimitStr = getenv( "EL_optLimit" );
		if( optLimitStr )
			optLimit = atoi( optLimitStr );
		}
	if( !optLimitStr )
		return true;
	if( optCount >= optLimit ) // haven't incremented it yet, so this is actually checking the previous opt's number
		return false;
	optCount++;
	va_list args;
	va_start( args, format );
	fl_write( stderr, "OPT %d: ", optCount );
	fl_vwrite( stderr, format, args );
	fl_write( stderr, "\n" );
	va_end( args );
	return true;
	}

//MERGE:10

