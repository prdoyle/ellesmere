
#include "options.h"
#include "memory.h"
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct oc_struct
	{
	OptionQuery query;
	OptionLevel level;
	OptionNoun  noun;
	} OptionClause;

static const struct verb_struct
	{
	char         *abbreviation;
	char         *name;
	OptionClause  clause;
	char         *description1;
	char         *description2;
	} verbs[] = {
	{ "l", "log",      { oq_REPORT_DETAIL,  1 }, "record",    "at a coarse granularity to aid understanding"  },
	{ "t", "trace",    { oq_REPORT_DETAIL,  2 }, "record",    "at a fine granularity to aid debugging"        },
	{ "d", "disable",  { oq_DISABLED,       1 }, "prevent",   "from occurring"                                },
	{ "e", "enable",   { oq_DISABLED,      -1 }, "allow",     "to occur"                                      },
	{ 0 }
	};

static const struct noun_struct
	{
	OptionNoun id;
	char      *abbreviation;
	char      *name;
	char      *description;
	} nouns[] = {
	{ on_EXECUTION,       "exec", "execution",       "operations performed by the user program"                                     },
	{ on_INHERITANCE,     "inh",  "inheritance",     "substitution of one symbol for another"                                       },
	{ on_INTERPRETER,     "int",  "interpreter",     "reading tokens, walking automata, and computing which operations to perform"  },
	{ on_OPTIONS,         "ops",  "options",         "option parsing and processing"                                                },
	{ on_PARSER_CONFLICT, "pc",   "parserconflict",  "situations where the automaton to generate is ambiguous"                      },
	{ on_PARSER_GEN,      "pgen", "parsergen",       "construction of an automaton from a grammar"                                  },
	{ 0 }
	};

static const struct query_struct
	{
	OptionQuery  id;
	char        *name;
	} queries[] = {
	{ oq_DISABLED,        "disabled"      },
	{ oq_REPORT_DETAIL,   "reportDetail"  },
	{ 0 }
	};

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
	int  optionLevels[ oq_NUM_OPTION_QUERIES ][ on_NUM_OPTION_NOUNS ];
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

FUNC OptionLevel os_get( OptionSet os, OptionQuery query, OptionNoun noun ) __attribute__((always_inline));
FUNC OptionLevel os_get( OptionSet os, OptionQuery query, OptionNoun noun )
	{
	if ( os )
		return os->optionLevels[ query ][ noun ];
	else
		return 0;
	}

static void os_set( OptionSet os, OptionQuery query, OptionNoun noun, OptionLevel level )
	{
	assert( os );
	if( abs( level ) >= abs( os_get( os, query, noun ) ) )
		{
		os_log( os, on_OPTIONS, "%s.%s %d -> %d\n", nouns[ noun-1 ].name, queries[ query-1 ].name, os->optionLevels[ query ][ noun ], level );
		os->optionLevels[ query ][ noun ] = level;
		}
	else
		{
		os_trace( os, on_OPTIONS, "%s.%s %d takes precedence over %d\n", nouns[ noun-1 ].name, queries[ query-1 ].name, os->optionLevels[ query ][ noun ], level );
		}
	}

FUNC void od_applyTo( OptionDelta od, OptionSet os, MemoryLifetime ml )
	{
	int i;
	OptionClauseArray clauses = od->clauses;
	for( i=0; i < oca_count( clauses ); i++)
		{
		OptionClause *clause = oca_element( clauses, i );
		os_set( os, clause->query, clause->noun, clause->level );
		}
	}

static OptionClause defaultSettings[] =
	{
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
		if( !clause->query )
			break;
		os_set( result, clause->query, clause->noun, clause->level );
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
#endif

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

static void dumpHelpAndTerminate()
	{
	fprintf( stderr, "Options are of the format 'VN', where the 'V' ('verb') is one of the following:\n\n" );
	const struct verb_struct *verb;
	for( verb = verbs; verb->abbreviation; verb++)
		fprintf( stderr, "  %5s: %s %s ('%s')\n", verb->abbreviation, verb->description1, verb->description2, verb->name );
	fprintf( stderr, "\n...and the 'N' ('noun') is one of the following:\n\n" );
	const struct noun_struct *noun;
	for( noun = nouns; noun->abbreviation; noun++)
		fprintf( stderr, "  %5s: %s ('%s')\n", noun->abbreviation, noun->description, noun->name );
	const struct verb_struct *exampleVerb = verbs;
	const struct noun_struct *exampleNoun = nouns;
	fprintf( stderr, "\nFor example, '%s%s' means '%s %s'", exampleVerb->abbreviation, exampleNoun->abbreviation, exampleVerb->name, exampleNoun->name );
	fprintf( stderr, ", which would %s\n%s %s", exampleVerb->description1, exampleNoun->description, exampleVerb->description2 );
	fprintf( stderr, "\n" );
	exit(1);
	}

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
		const struct verb_struct *verb;
		for( verb = verbs; ; verb++ )
			{
			if( verb->abbreviation == 0 )
				{
				fprintf( stderr, "Unrecognized verb: '%c'\n", opt[0] );
				dumpHelpAndTerminate();
				}
			if( verb->abbreviation[0] == opt[0] )
				break;
			}
		const struct noun_struct *noun;
		int length = stop - (opt+1);
		for( noun = nouns; ; noun++ )
			{
			if( noun->id == 0 )
				{
				fprintf( stderr, "Unrecognized noun: '%.*s'\n", length, opt+1 );
				dumpHelpAndTerminate();
				}
			assert( noun - nouns == noun->id - 1 ); // nouns table should be in order
			if( matches( noun->name, opt+1, stop ) )
				break;
			if( matches( noun->abbreviation, opt+1, stop ) )
				break;
			}
		OptionClause oc = verb->clause;
		oc.noun = noun->id;
		oca_append( clauses, oc );
		opt += 1 + length + 1; // verb + noun + comma
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

