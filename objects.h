
#ifndef OBJECTS_H
#define OBJECTS_H

#include "base.h"
#include "symbols.h"
#include "file.h"

FUNC ObjectHeap theObjectHeap();

FUNC SymbolTable oh_tagSymbolTable( ObjectHeap heap );

FUNC Object ob_create( Symbol tag, ObjectHeap heap );
FUNC Symbol ob_tag( Object ob, ObjectHeap heap );

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap );
FUNC void   ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap );
FUNC void   ob_getFieldSymbols( Object ob, BitVector result, ObjectHeap heap ); // TODO: Not necessarily the most efficient interface, but handy

FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap );
FUNC void   ob_setElement( Object ob, int index, Object value, ObjectHeap heap );

FUNC CheckList cl_open  ( ObjectHeap heap );
FUNC void      cl_close ( CheckList cl );
FUNC void      cl_check     ( CheckList cl, Object ob ); // Beware large int objects.  This may perform very poorly.
FUNC void      cl_checkAll  ( CheckList target, CheckList source );
FUNC void      cl_uncheck   ( CheckList cl, Object ob );
FUNC bool      cl_isChecked ( CheckList cl, Object ob );

FUNC int ob_sendTo         ( Object ob, File fl, ObjectHeap heap );
FUNC int ob_sendDeepTo     ( Object ob, File fl, ObjectHeap heap );
FUNC int ob_sendDotEdgesTo ( Object ob, File fl, ObjectHeap heap );

typedef struct oba_struct *ObjectArray;
#define AR_PREFIX  oba
#define AR_TYPE    ObjectArray
#define AR_ELEMENT Object
#define AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define oba_new( capacity, ml ) oba_newAnnotated( capacity, ml, __FILE__, __LINE__ )
#endif

// Primitive types

FUNC Object      ob_fromInt( int value, ObjectHeap heap );
FUNC bool        ob_isInt( Object ob, ObjectHeap heap );
FUNC int         ob_toInt( Object ob, ObjectHeap heap );
static inline int   ob_getIntField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toInt( ob_getField( ob, field, heap ), heap ); }
static inline void  ob_setIntField( Object ob, Symbol field, int value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromInt( value, heap ), heap ); }

FUNC Object      ob_fromString( const char *value, ObjectHeap heap );
FUNC bool        ob_isString( Object ob, ObjectHeap heap );
FUNC const char *ob_toString( Object ob, ObjectHeap heap );
static inline const char *ob_getStringField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toString( ob_getField( ob, field, heap ), heap ); }
static inline void        ob_setStringField( Object ob, Symbol field, const char *value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromString( value, heap ), heap ); }

FUNC Object      oh_symbolToken( ObjectHeap heap, Symbol sy );
FUNC bool        ob_isToken( Object ob, ObjectHeap heap );
FUNC Symbol      ob_toSymbol( Object ob, ObjectHeap heap );
static inline Symbol ob_getTokenField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toSymbol( ob_getField( ob, field, heap ), heap ); }
static inline void   ob_setTokenField( Object ob, Symbol field, Symbol value, ObjectHeap heap ){ ob_setField( ob, field, oh_symbolToken( heap, value ), heap ); }

// Reflected data structures (mostly temporary until parts of the interpreter can be rewritten in Ellesmere)

FUNC Object       ob_fromFunction( Function fn, ObjectHeap heap );
FUNC bool         ob_isFunction( Object ob, ObjectHeap heap );
FUNC Function     ob_toFunction( Object ob, ObjectHeap heap );
static inline Function ob_getFunctionField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toFunction( ob_getField( ob, field, heap ), heap ); }
static inline void     ob_setFunctionField( Object ob, Symbol field, Function value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromFunction( value, heap ), heap ); }

FUNC Object       ob_fromTokenBlock( TokenBlock tb, ObjectHeap heap );
FUNC bool         ob_isTokenBlock( Object ob, ObjectHeap heap );
FUNC TokenBlock   ob_toTokenBlock( Object ob, ObjectHeap heap );
static inline TokenBlock ob_getTokenBlockField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toTokenBlock( ob_getField( ob, field, heap ), heap ); }
static inline void       ob_setTokenBlockField( Object ob, Symbol field, TokenBlock value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromTokenBlock( value, heap ), heap ); }

FUNC Object       ob_fromTokenStream( TokenStream ts, ObjectHeap heap );
FUNC bool         ob_isTokenStream( Object ob, ObjectHeap heap );
FUNC TokenStream  ob_toTokenStream( Object ob, ObjectHeap heap );
static inline TokenStream ob_getTokenStreamField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toTokenStream( ob_getField( ob, field, heap ), heap ); }
static inline void        ob_setTokenStreamField( Object ob, Symbol field, TokenStream value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromTokenStream( value, heap ), heap ); }

FUNC Object       ob_fromGrammar( Grammar gr, ObjectHeap heap );
FUNC bool         ob_isGrammar( Object ob, ObjectHeap heap );
FUNC Grammar      ob_toGrammar( Object ob, ObjectHeap heap );
static inline Grammar ob_getGrammarField( Object ob, Symbol field, ObjectHeap heap ){ return ob_toGrammar( ob_getField( ob, field, heap ), heap ); }
static inline void    ob_setGrammarField( Object ob, Symbol field, Grammar value, ObjectHeap heap ){ ob_setField( ob, field, ob_fromGrammar( value, heap ), heap ); }

static inline Object ob_getOrCreateField( Object ob, Symbol field, Symbol tag, ObjectHeap heap )
	{
	Object result = ob_getField( ob, field, heap );
	if( !result )
		{
		result = ob_create( tag, heap );
		ob_setField( ob, field, result, heap );
		}
	return result;
	}

static inline int ob_getIfIntField( Object ob, Symbol field, int defaultValue, ObjectHeap heap )
	{
	Object result = ob_getField( ob, field, heap );
	if( result )
		return ob_toInt( result, heap );
	else
		return defaultValue;
	}

static inline Object ob_getFieldIfPresent( Object ob, Symbol field, Object defaultValue, ObjectHeap heap )
	{
	Object result = ob_getField( ob, field, heap );
	if( result )
		return result;
	else
		return defaultValue;
	}

typedef bool ( *EdgePredicate )( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail );
typedef void ( *VertexProcedure )( void *context, Object vertex );

FUNC void postorderWalk( Stack workList, EdgePredicate recurseIntoEdge, VertexProcedure processVertex, ObjectHeap heap, void *context );
bool everyEdge( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail );

#endif

