
#include "options.h"
#include "memory.h"
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

static const struct
	{
	OptionVerb id;
	char      *abbreviation;
	char      *name;
	char      *description;
	} verbs[] = {
	{ ov_LOG,       "l", "log",    "Record events at a high level"                       },
	{ ov_TRACE,     "t", "trace",  "Record implementation steps to aid in debugging"     },
	{ 0 }
	};

static const struct
	{
	OptionNoun id;
	char      *abbreviation;
	char      *name;
	char      *description;
	} nouns[] = {
	{ on_EXECUTION,       "x", "execution",       "Operations performed by the user program"                                     },
	{ on_INTERPRETER,     "i", "interpreter",     "Reading tokens, walking automata, and computing which operations to perform"  },
	{ on_PARSER_GEN,      "g", "parsergen",       "Construction of an automaton from a grammar"                                  },
	{ on_PARSER_CONFLICT, "c", "parserconflict",  "Situations where the automaton to generate is ambiguous"                      },
	{ 0 }
	};

typedef struct oc_struct
	{
	OptionVerb verb;
	OptionNoun noun;
	bool       invert;
	} OptionClause;

#ifdef NDEBUG
	typedef struct oca_struct *OptionClauseArray; // type-safe phony struct
#else
	typedef Array OptionClauseArray; // give the debugger some symbol info it can use
#endif
#define AR_PREFIX  oca
#define AR_TYPE    OptionClauseArray
#define AR_ELEMENT struct oc_struct
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define oca_new( size, ml ) oca_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct od_struct
	{
	OptionClauseArray clauses;
	};

struct os_struct
	{
	File logFile;
	int  verbsByNoun[ on_NUM_OPTION_NOUNS ];
	};

FUNC File os_getLogFile( OptionSet os )
	{
	if( !os )
		return NULL;

	return os->logFile;
	}

FUNC void os_setLogFile ( OptionSet os, File newLogFile )
	{
	if( !os )
		return;

	assert( newLogFile != NULL );
	os->logFile = newLogFile;
	}

static bool os_getFlag( OptionSet os, OptionVerb verb, OptionNoun noun ) __attribute__((always_inline));
static bool os_getFlag( OptionSet os, OptionVerb verb, OptionNoun noun )
	{
	return ( os->verbsByNoun[ noun ] & ( 1<<verb ) ) != 0;
	}

static void os_setFlagTo( OptionSet os, OptionVerb verb, OptionNoun noun, bool newValue )
	{
	if( newValue )
		os->verbsByNoun[ noun ] |= ( 1<<verb );
	else
		os->verbsByNoun[ noun ] &= ~( 1<<verb );
	assert( os_getFlag( os, verb, noun ) == newValue );
	}

#if 0
static void os_setFlag( OptionSet os, OptionVerb verb, OptionNoun noun )
	{
	os_setFlagTo( os, verb, noun, true );
	}
#endif

FUNC bool os_do( OptionSet os, OptionVerb verb, OptionNoun noun ) __attribute__((always_inline));
FUNC bool os_do( OptionSet os, OptionVerb verb, OptionNoun noun )
	{
	return os_getFlag( os, verb, noun );
	}

FUNC void od_applyTo( OptionDelta od, OptionSet os, MemoryLifetime ml )
	{
	int i;
	OptionClauseArray clauses = od->clauses;
	for( i=0; i < oca_count( clauses ); i++)
		{
		OptionClause *clause = oca_element( clauses, i );
		os_setFlagTo( os, clause->verb, clause->noun, !clause->invert );
		}
	}

static OptionClause defaultSettings[] =
	{
	//{ ov_LOG, on_EXECUTION },
	{ 0 }
	};

FUNC OptionSet os_new( MemoryLifetime ml )
	{
	OptionSet result = ml_alloc( ml, sizeof(*result) );
	memset( result, 0, sizeof(*result) );
	int i;
	for( i=0; i < sizeof(defaultSettings)/sizeof(defaultSettings[0]); i++ )
		{
		OptionClause *clause = defaultSettings + i;
		if( !clause->verb )
			break;
		os_setFlagTo( result, clause->verb, clause->noun, !clause->invert );
		}
	result->logFile = stderr;
	return result;
	}

#if 0
static bool prefixMatches( char *prefix, char *start, char *stop )
	{
	int len = strlen( prefix );
	if( stop - start < len )
		return false;
	else
		return !strncmp( prefix, start, len );
	}

static char *optionStop( char *start, char *stop )
	{
	for( char *c = start; c < stop; c++ )
		if( c[0] == ',' )
			break;
	return c;
	}

static bool matches( char *pattern, char *start, char *stop )
	{
	int len = strlen( pattern );
	if( stop - start < len )
		return false;
	else if( stop - start > len && start[len] != ',')
		return false;
	else
		return !strncmp( pattern, start, len );
	}
#endif

FUNC OptionDelta od_parse( char *start, char *stop, MemoryLifetime ml )
	{
	int numClauses = 1;
	char *opt;
	for( opt = start; opt != stop; opt++ )
		if( opt[0] == ',' )
			numClauses++;

	OptionDelta result = ml_alloc( ml, sizeof(*result) );
	OptionClauseArray clauses = oca_new( numClauses, ml );
	result->clauses = clauses;

	opt = start;
	while( opt < stop )
		{
		// TODO: Cope with full option names
		int verbIndex=0, nounIndex=0;
		while( verbs[ verbIndex ].abbreviation[0] != opt[0] )
			{
			verbIndex++;
			if( verbs[ verbIndex ].id == 0 )
				{
				fprintf( stderr, "Unrecognized verb: '%c'\n", opt[0] );
				exit(1);
				}
			}
		while( nouns[ nounIndex ].abbreviation[0] != opt[1] )
			{
			nounIndex++;
			if( nouns[ nounIndex ].id == 0 )
				{
				fprintf( stderr, "Unrecognized noun: '%c'\n", opt[1] );
				exit(1);
				}
			}
		OptionClause oc = { verbs[ verbIndex ].id, nouns[ nounIndex ].id };
		oca_append( clauses, oc );
		opt += 2;
		}

	return result;
	}

FUNC int os_log( OptionSet os, OptionNoun noun, const char *format, ... )
	{
	if( !os )
		return 0;

	va_list args;
	va_start( args, format );
	int result = fl_vwrite( os_logFile( os, noun ), format, args );
	va_end( args );
	return result;
	}

FUNC int os_trace( OptionSet os, OptionNoun noun, const char *format, ... )
	{
	if( !os )
		return 0;

	va_list args;
	va_start( args, format );
	int result = fl_vwrite( os_traceFile( os, noun ), format, args );
	va_end( args );
	return result;
	}


//MERGE:65

