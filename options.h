
#ifndef OPTIONS_H
#define OPTIONS_H

#include "base.h"
#include "file.h"

typedef struct os_struct *OptionSet;
typedef struct od_struct *OptionDelta;

typedef enum
	{
	oq_NULL,

	oq_REPORT_DETAIL,
	oq_DISABLED,

	oq_NUM_OPTION_QUERIES
	} OptionQuery;

typedef enum
	{
	on_NULL,

	on_EXECUTION,
	on_INTERPRETER,
	on_PARSER_GEN,
	on_PARSER_CONFLICT,
	on_INHERITANCE,

	on_NUM_OPTION_NOUNS
	} OptionNoun;

typedef signed char OptionLevel;

FUNC OptionSet os_new ( MemoryLifetime ml );

FUNC OptionDelta od_parse   ( char *start, char *stop, MemoryLifetime ml );
FUNC void        od_applyTo ( OptionDelta od, OptionSet os, MemoryLifetime ml );

FUNC OptionLevel os_get( OptionSet, OptionQuery query, OptionNoun noun );
FUNC File os_getLogFile ( OptionSet os );
FUNC void os_setLogFile ( OptionSet os, File newLogFile );

static inline File os_logFile ( OptionSet os, OptionNoun noun )
	{ return ( os_get( os, oq_REPORT_DETAIL, noun ) >= 1 )? os_getLogFile( os ) : NULL; }

static inline File os_traceFile ( OptionSet os, OptionNoun noun )
	{ return ( os_get( os, oq_REPORT_DETAIL, noun ) >= 2 )? os_getLogFile( os ) : NULL; }

FUNC int os_log   ( OptionSet os, OptionNoun noun, const char *format, ... );
FUNC int os_trace ( OptionSet os, OptionNoun noun, const char *format, ... );

static inline bool os_disable( OptionSet os, OptionNoun noun )
	{ return os_get( os, oq_DISABLED, noun ) >= 1; }

static inline bool os_enable( OptionSet os, OptionNoun noun )
	{ return !os_disable( os, noun ); }

#endif

