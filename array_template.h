
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
	{ return ar_new( capacity, sizeof(AR_ELEMENT) ); }

static inline int AR_PREFIXED( _count )( AR_TYPE ar )
	{ return ar_count( ar ); }

static inline int AR_PREFIXED( _capacity )( AR_TYPE ar )
	{ return ar_capacity( ar ); }

static inline AR_ELEMENT *AR_PREFIXED( _element )( AR_TYPE ar, int index )
	{ return (AR_ELEMENT*)ar_element( ar, index, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _store )( AR_TYPE ar, int index, AR_ELEMENT *newValue )
	{ ar_store( ar, index, newValue, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _incCount )( AR_TYPE ar )
	{ ar_incCount( ar, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _setCount )( AR_TYPE ar, int newCount )
	{ ar_setCount( ar, newCount, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _setCapacity )( AR_TYPE ar, int newCapacity )
	{ ar_setCapacity( ar, newCapacity, sizeof(AR_ELEMENT) ); }

#if defined( AR_BYVALUE )
// a few bonus functions to get/set by value if desired
static inline AR_ELEMENT AR_PREFIXED( _get )( AR_TYPE ar, int index )
	{ return *AR_PREFIXED( _element )( ar, index ); }

static inline void AR_PREFIXED( _set )( AR_TYPE ar, int index, AR_ELEMENT newValue )
	{ ar_store( ar, index, &newValue, sizeof(AR_ELEMENT) ); }

static inline void AR_PREFIXED( _append )( AR_TYPE ar, AR_ELEMENT newValue )
	{ ar_store( ar, ar_incCount( ar, sizeof(AR_ELEMENT) ) - 1, &newValue, sizeof(AR_ELEMENT) ); }
#endif

#undef AR_PREFIX
#undef AR_TYPE
#undef AR_ELEMENT
#undef AR_BYVALUE

