
#ifndef OBJECTS_H
#define OBJECTS_H

#include "base.h"
#include "symbols.h"
#include "file.h"

FUNC ObjectHeap theObjectHeap();

FUNC Object ob_create( Symbol tag, ObjectHeap heap );
FUNC Symbol ob_tag( Object ob, ObjectHeap heap );

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap );
FUNC void   ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap );

FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap );
FUNC void   ob_setElement( Object ob, int index, Object value, ObjectHeap heap );

FUNC CheckList cl_open  ( ObjectHeap heap );
FUNC void      cl_close ( CheckList cl );
FUNC void      cl_check     ( CheckList cl, Object ob ); // Beware large int objects.  This may perform very poorly.
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
	#define oba_new( size, ml ) oba_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

// Primitive types

FUNC Object      ob_fromInt( int value, ObjectHeap heap );
FUNC bool        ob_isInt( Object ob, ObjectHeap heap );
FUNC int         ob_toInt( Object ob, ObjectHeap heap );

FUNC Object      ob_fromString( const char *value, ObjectHeap heap );
FUNC bool        ob_isString( Object ob, ObjectHeap heap );
FUNC const char *ob_toString( Object ob, ObjectHeap heap );

FUNC Object      oh_symbolToken( ObjectHeap heap, Symbol sy );
FUNC bool        ob_isToken( Object ob, ObjectHeap heap );
FUNC Symbol      ob_toSymbol( Object ob, ObjectHeap heap );

// Reflected data structures (mostly temporary until parts of the interpreter can be rewritten in Ellesmere)

FUNC Object       ob_fromFunction( Function fn, ObjectHeap heap );
FUNC bool         ob_isFunction( Object ob, ObjectHeap heap );
FUNC Function     ob_toFunction( Object ob, ObjectHeap heap );

FUNC Object       ob_fromTokenBlock( TokenBlock tb, ObjectHeap heap );
FUNC bool         ob_isTokenBlock( Object ob, ObjectHeap heap );
FUNC TokenBlock   ob_toTokenBlock( Object ob, ObjectHeap heap );

FUNC Object       ob_fromTokenStream( TokenStream ts, ObjectHeap heap );
FUNC bool         ob_isTokenStream( Object ob, ObjectHeap heap );
FUNC TokenStream  ob_toTokenStream( Object ob, ObjectHeap heap );

#endif

