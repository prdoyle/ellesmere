
#include "file.h"
#include "options.h"
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

//MERGE:10

