
#ifndef FILE_H
#define FILE_H

#include "base.h"
#include <stdio.h>
#include <stdarg.h>

typedef FILE *File;

FUNC int fl_write( File fl, const char *format, ... )           LIKE_PRINTF(2,3);
FUNC int fl_vwrite( File fl, const char *format, va_list args ) LIKE_VPRINTF(2);

#endif

