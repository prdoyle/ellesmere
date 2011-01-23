
#ifndef FILE_H
#define FILE_H

#include "base.h"
#include <stdio.h>
#include <stdarg.h>

typedef FILE *File;

FUNC int fl_write( File fl, const char *format, ... );
FUNC int fl_vwrite( File fl, const char *format, va_list args );

#if 1
	#define trace   fl_write
#else
	#define trace(...)
#endif

#endif

