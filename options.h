
#ifndef OPTIONS_H
#define OPTIONS_H

#include "base.h"
#include "file.h"

typedef struct os_struct *OptionSet;
typedef struct od_struct *OptionDelta;

typedef enum
	{
	ov_NULL,

	ov_LOG,
	ov_TRACE,

	ov_NUM_OPTION_VERBS
	} OptionVerb;

typedef enum
	{
	on_NULL,

	on_EXECUTION,
	on_INTERPRETER,
	on_PARSER_GEN,
	on_PARSER_CONFLICT,

	on_NUM_OPTION_NOUNS
	} OptionNoun;

FUNC OptionSet os_new ( MemoryLifetime ml );

FUNC OptionDelta od_parse   ( char *start, char *stop, MemoryLifetime ml );
FUNC void        od_applyTo ( OptionDelta od, OptionSet os, MemoryLifetime ml );

FUNC bool os_do      ( OptionSet os, OptionVerb verb, OptionNoun noun );
FUNC File os_logFile ( OptionSet os );

#endif

