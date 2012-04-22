
#ifndef OBJECTS_H
#define OBJECTS_H

#include "symbols.h"
#include "records.h"
#include "file.h"

typedef struct ob_struct *Object;
typedef struct oh_struct *ObjectHeap;

FUNC ObjectHeap theObjectHeap();

FUNC SymbolTable          oh_symbolTable         ( ObjectHeap heap );
FUNC InheritanceRelation  oh_inheritanceRelation ( ObjectHeap heap );

FUNC Record  sy_instanceShape    ( Symbol sy, ObjectHeap heap );
FUNC void    sy_setInstanceShape ( Symbol sy, Record rd, ObjectHeap heap );

FUNC InheritanceRelation ir_new ( ObjectHeap heap, MemoryLifetime ml ); // TODO: Get rid of this if each heap has exactly one inheritance relation
FUNC Object              ir_index    ( InheritanceRelation ir );
FUNC Symbol              ir_nodeTag  ( InheritanceRelation ir );
FUNC ObjectHeap          ir_nodeHeap ( InheritanceRelation ir );
FUNC void                ir_add      ( InheritanceRelation ir, Symbol super, Symbol sub );
FUNC int                 ir_sendTo   ( InheritanceRelation ir, File fl );
static inline SymbolTable ir_symbolTable( InheritanceRelation ir ){ return oh_symbolTable( ir_nodeHeap(ir) ); }

enum { IR_START_INDEX=1 };

FUNC Object ob_create( Symbol tag, ObjectHeap heap );
FUNC Symbol ob_tag( Object ob, ObjectHeap heap );

FUNC bool   ob_hasFields( Object ob ); // TODO: Should we be required to answer this question without a heap?
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

typedef int ( *ObjectFormat )( void *context, Object object, File fl );
static inline int ob_sendFormattedTo( Object ob, File fl, ObjectFormat format, void *context )
	{ return format( context, ob, fl ); }

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

FUNC Object      oh_valuePlaceholder( ObjectHeap heap, Symbol tag, Object value );
FUNC Object      oh_recordedPlaceholder( ObjectHeap heap, Symbol tag );
FUNC bool        ob_isPlaceholder( Object ob, ObjectHeap heap );

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

// Handy functions taking symbol indexes instead of symbols

static inline Object ob_createX( SymbolIndex tagIndex, ObjectHeap heap )
	{ return ob_create( sy_byIndex( tagIndex, oh_symbolTable( heap ) ), heap ); }

static inline Object ob_getFieldX( Object ob, SymbolIndex fieldIndex, ObjectHeap heap )
	{ return ob_getField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), heap ); }

static inline int ob_getIntFieldX( Object ob, SymbolIndex fieldIndex, ObjectHeap heap )
	{ return ob_getIntField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), heap ); }

static inline void ob_setFieldX( Object ob, SymbolIndex fieldIndex, Object value, ObjectHeap heap )
	{ ob_setField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), value, heap ); }

static inline Symbol ob_getTokenFieldX( Object ob, SymbolIndex fieldIndex, ObjectHeap heap )
	{ return ob_getTokenField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), heap ); }

static inline void ob_setTokenFieldX( Object ob, SymbolIndex fieldIndex, Symbol value, ObjectHeap heap )
	{ ob_setTokenField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), value, heap ); }

static inline TokenBlock ob_getTokenBlockFieldX( Object ob, SymbolIndex fieldIndex, ObjectHeap heap )
	{ return ob_getTokenBlockField( ob, sy_byIndex( fieldIndex, oh_symbolTable( heap ) ), heap ); }

FUNC SymbolIndex ob_tagX( Object ob, ObjectHeap heap );

// Miscellaneous handy functions

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

static inline Object ob_getFieldRecursively( Object ob, Symbol field, Symbol delegateSymbol, ObjectHeap heap )
	{
	for( ; ob; ob = ob_getField( ob, delegateSymbol, heap ) )
		{
		Object result = ob_getField( ob, field, heap );
		if( result )
			return result;
		}
	return NULL;
	}

static inline Object ob_getFieldRecursivelyIfPresent( Object ob, Symbol field, Symbol delegateSymbol, Object defaultValue, ObjectHeap heap )
	{
	Object result = ob_getFieldRecursively( ob, field, delegateSymbol, heap );
	if( result )
		return result;
	else
		return defaultValue;
	}

#endif

