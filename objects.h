
#ifndef OBJECTS_H
#define OBJECTS_H

#include "base.h"
#include "symbols.h"
#include "stream.h"

typedef struct oh_struct *ObjectHeap;
typedef struct ob_struct *Object;

FUNC ObjectHeap theObjectHeap();

FUNC Object ob_create( Symbol tag, ObjectHeap heap );
FUNC Symbol ob_tag( Object ob, ObjectHeap heap );

FUNC Object ob_fromInt( int value, ObjectHeap heap );
FUNC bool   ob_isInt( Object ob, ObjectHeap heap );
FUNC int    ob_toInt( Object ob, ObjectHeap heap );

FUNC Object      ob_fromString( const char *value, ObjectHeap heap );
FUNC bool        ob_isString( Object ob, ObjectHeap heap );
FUNC const char *ob_toString( Object ob, ObjectHeap heap );

FUNC Object oh_symbolToken( ObjectHeap heap, Symbol sy );
FUNC bool   ob_isToken( Object ob, ObjectHeap heap );
FUNC Symbol ob_toSymbol( Object ob, ObjectHeap heap );

FUNC bool   ob_hasField( Object ob, Symbol field, ObjectHeap heap );
FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap );
FUNC void   ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap );

FUNC bool   ob_hasElement( Object ob, int index, ObjectHeap heap );
FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap );
FUNC void   ob_setElement( Object ob, int index, Object value, ObjectHeap heap );

FUNC int ob_sendTo( Object ob, Stream sm, ObjectHeap heap );

#endif

