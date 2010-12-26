
#include "array.h"

/*
	Before including this, define the following macros:
   - AR_PREFIX
	- AR_TYPE
	- AR_ELEMENT
	- AR_BYVALUE
*/

#define AR_PASTE0( left, right ) left ## right
#define AR_PASTE1( left, right ) AR_PASTE0( left, right )
#define AR_PREFIXED( str ) AR_PASTE1( AR_PREFIX, str )

static inline AR_TYPE AR_PREFIXED( _new )( int capacity )
	{ return (AR_TYPE)ar_new( capacity, sizeof(AR_ELEMENT) ); }

static inline AR_TYPE AR_PREFIXED( _newInMB )( int capacity, MemoryLifetime ml )
	{ return (AR_TYPE)ar_newInMB( capacity, sizeof(AR_ELEMENT), ml ); }

static inline int AR_PREFIXED( _count )( AR_TYPE ar )
	{ return ar_count( (Array)ar ); }

static inline int AR_PREFIXED( _capacity )( AR_TYPE ar )
	{ return ar_capacity( (Array)ar ); }

static inline AR_ELEMENT *AR_PREFIXED( _element )( AR_TYPE ar, int index )
	{ return (AR_ELEMENT*)ar_element( (Array)ar, index, sizeof(AR_ELEMENT) ); }

static inline AR_ELEMENT *AR_PREFIXED( _last )( AR_TYPE ar, int indexFromEnd )
	{ return (AR_ELEMENT*)ar_last( (Array)ar, indexFromEnd, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _store )( AR_TYPE ar, int index, AR_ELEMENT *newValue )
	{ ar_store( (Array)ar, index, newValue, sizeof(AR_ELEMENT) ); }

static inline int AR_PREFIXED( _incCount )( AR_TYPE ar )
	{ return ar_incCount( (Array)ar, sizeof(AR_ELEMENT) ); }

static inline int AR_PREFIXED( _incCountBy )( AR_TYPE ar, int delta )
	{ return ar_incCountBy( (Array)ar, delta, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _setCount )( AR_TYPE ar, int newCount )
	{ ar_setCount( (Array)ar, newCount, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _setCapacity )( AR_TYPE ar, int newCapacity )
	{ ar_setCapacity( (Array)ar, newCapacity, sizeof(AR_ELEMENT) ); }

static inline AR_ELEMENT *AR_PREFIXED( _nextElement )( AR_TYPE ar )
	{ return (AR_ELEMENT*)ar_element( (Array)ar, ar_incCount( (Array)ar, sizeof(AR_ELEMENT) ) - 1, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _shrinkWrap )( AR_TYPE ar )
	{ ar_setCapacity( (Array)ar, ar_count( (Array)ar ), sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _clear )( AR_TYPE ar, int newCount )
	{ ar_clear( (Array)ar, newCount, sizeof(AR_ELEMENT) ); }

#if defined( AR_BYVALUE )
// a few bonus functions to get/set by value if desired
static inline AR_ELEMENT AR_PREFIXED( _get )( AR_TYPE ar, int index )
	{ return *AR_PREFIXED( _element )( ar, index ); }

static inline AR_ELEMENT AR_PREFIXED( _getLast )( AR_TYPE ar, int indexFromEnd )
	{ return *AR_PREFIXED( _last )( ar, indexFromEnd ); }

static inline void AR_PREFIXED( _set )( AR_TYPE ar, int index, AR_ELEMENT newValue )
	{ ar_store( (Array)ar, index, &newValue, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _append )( AR_TYPE ar, AR_ELEMENT newValue )
	{ ar_store( (Array)ar, ar_incCount( (Array)ar, sizeof(AR_ELEMENT) ) - 1, &newValue, sizeof(AR_ELEMENT) ); }
#endif

#undef AR_PREFIX
#undef AR_TYPE
#undef AR_ELEMENT
#undef AR_BYVALUE

