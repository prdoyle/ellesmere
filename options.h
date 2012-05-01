
#ifndef OPTIONS_H
#define OPTIONS_H

#include "base.h"
#include "file.h"

typedef struct os_struct *OptionSet;
typedef struct od_struct *OptionDelta;

typedef enum
	{
	oq_NULL,

	oq_TRACING,
	oq_LAST_SLOW_OPTION_QUERY = oq_TRACING,

	oq_DISABLED,
	oq_LOGGING,

	oq_NUM_OPTION_QUERIES
	} OptionQuery;

typedef enum
	{
	on_NULL,

	on_CONCRETIFICATION,
	on_EXECUTION,
	on_GRAMMAR_AUGMENTATION,
	on_HACK,
	on_INHERITANCE,
	on_INTERPRETER,
	on_OPTIMIZATIONS,
	on_OPTIONS,
	on_PARSER_CONFLICT,
	on_PARSER_GEN,

	on_NUM_OPTION_NOUNS
	} OptionNoun;

FUNC OptionSet os_new ( MemoryLifetime ml ) ALWAYS_NEW;
FUNC OptionSet os_global();

FUNC OptionDelta od_parse   ( char *start, char *stop, MemoryLifetime ml );
FUNC void        od_applyTo ( OptionDelta od, OptionSet os, MemoryLifetime ml );

FUNC bool os_isSet( OptionSet os, OptionQuery query, OptionNoun noun );

// These take a series of query+noun pairs followed by a 0
FUNC bool os_areAllSet ( OptionSet os, ... );
FUNC bool os_isAnySet  ( OptionSet os, ... );

FUNC File os_getLogFile ( OptionSet os );
FUNC void os_setLogFile ( OptionSet os, File newLogFile );

static inline bool os_logging  ( OptionSet os, OptionNoun noun ) { return  os_isSet( os, oq_LOGGING,  noun ); }
static inline bool os_tracing  ( OptionSet os, OptionNoun noun ) { return  os_isSet( os, oq_TRACING,  noun ); }
static inline bool os_disabled ( OptionSet os, OptionNoun noun ) { return  os_isSet( os, oq_DISABLED, noun ); }
static inline bool os_enabled  ( OptionSet os, OptionNoun noun ) { return !os_isSet( os, oq_DISABLED, noun ); }

#if 0
static inline bool os_loggingAny  ( OptionSet os, ... );
static inline bool os_tracingAny  ( OptionSet os, ... );
static inline bool os_disabledAny ( OptionSet os, ... );
static inline bool os_enabledAny  ( OptionSet os, ... );

static inline bool os_loggingAll  ( OptionSet os, ... );
static inline bool os_tracingAll  ( OptionSet os, ... );
static inline bool os_disabledAll ( OptionSet os, ... );
static inline bool os_enabledAll  ( OptionSet os, ... );
#endif

static inline File os_logFile   ( OptionSet os, OptionNoun noun ){ return os_logging( os, noun ) ? os_getLogFile(os) : NULL; }
static inline File os_traceFile ( OptionSet os, OptionNoun noun ){ return os_tracing( os, noun ) ? os_getLogFile(os) : NULL; }

FUNC int os_log   ( OptionSet os, OptionNoun noun, const char *format, ... ) LIKE_PRINTF(3,4);
FUNC int os_trace ( OptionSet os, OptionNoun noun, const char *format, ... ) LIKE_PRINTF(3,4);

#if 1
	#define TRACE    fl_write
	FUNC int optional(char *format, ...) LIKE_PRINTF(1,2);
#else
	#define TRACE(...)
	static inline int optional(char *format, ...) { return true; }
#endif
static inline int optionalDetail(char *format, ...) { return true; }

#endif

