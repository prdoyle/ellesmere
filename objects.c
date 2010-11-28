
#include "objects.h"
#include <stdint.h>

struct oh_struct
	{
	SymbolTable symbolTable;
	};

typedef enum
	{
	OB_STRUCT =0,
	OB_INT    =1,
	} ObjectKind;

const intptr_t OB_KIND_BITS = 1;
const intptr_t OB_KIND_MASK = 1;

static ObjectKind ob_kind( Object ob )
	{
	intptr_t address = (intptr_t)ob;
	return (ObjectKind)(address & OB_KIND_MASK);
	}

FUNC ObjectHeap theObjectHeap()
	{
	static struct oh_struct _theObjectHeap = { 0 };
	if ( !_theObjectHeap.symbolTable )
		{
		_theObjectHeap.symbolTable = theSymbolTable();
		}
	return &_theObjectHeap;
	}

typedef struct fl_struct
	{
	SymbolIndex       si;
	Object            value;
	struct fl_struct *tail;
	} *FieldList;

static FieldList fl_new( SymbolIndex si, Object value, FieldList tail )
	{
	FieldList result = (FieldList)malloc( sizeof(*result) );
	assert(( (intptr_t)result & OB_KIND_MASK ) == 0);
	result->si    = si;
	result->value = value;
	result->tail  = tail;
	return result;
	}

static FieldList fl_bySymbol( SymbolIndex si, FieldList fl )
	{
	if ( fl && fl->si != si )
		return fl_bySymbol( si, fl->tail );
	else
		return fl;
	}

struct ob_struct
	{
	SymbolIndex tag;
	union
		{
		FieldList   fields;
		const char *characters;
		} data;
	};

FUNC Object ob_create( Symbol tag, ObjectHeap heap )
	{
	Object result = (Object)malloc( (sizeof(*result)) );
	result->tag = sy_index( tag, heap->symbolTable );
	result->data.fields = NULL;
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
	}

FUNC Object ob_fromInt( int value, ObjectHeap heap )
	{
	intptr_t packed = ((intptr_t)value << OB_KIND_BITS) | OB_INT;
	return (Object)packed;
	}

FUNC bool ob_isInt( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_INT;
	}

FUNC int ob_toInt( Object ob, ObjectHeap heap )
	{
	assert( ob_isInt( ob, heap ) );
	return (intptr_t)ob >> OB_KIND_BITS;
	}

FUNC Object ob_fromString( const char *value, ObjectHeap heap )
	{
	assert( ((intptr_t)value & OB_KIND_MASK) == 0 );
	intptr_t packed = ((intptr_t)value << OB_KIND_BITS) | OB_INT;
	return (Object)packed;
	}

FUNC bool ob_isString( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_STRING;
	}

FUNC const char *ob_toString( Object ob, ObjectHeap heap )
	{
	assert( ob_isString( ob, heap ) );
	return ob->data.characters;
	}

FUNC Symbol ob_tag( Object ob, ObjectHeap heap )
	{
	switch ( ob_kind( ob ) )
		{
		case OB_INT:
			return st_int( heap->symbolTable );
		case OB_STRUCT:
			break;
		}
	return st_byIndex( ob->tag, heap->symbolTable );
	}

static bool ob_hasItems( Object ob )
	{
	return ob_kind(ob) != OB_INT && ob->tag != SYM_STRING;
	}

static bool ob_hasItem( Object ob, SymbolIndex si )
	{
	if ( ob_hasItems(ob) )
		return fl_bySymbol( si, ob->data.fields ) != NULL;
	else
		return false;
	}

static Object ob_getItem( Object ob, SymbolIndex si )
	{
	assert( ob_hasItem( ob, si ) );
	FieldList fl = fl_bySymbol( si, ob->data.fields );
	assert( fl );
	return fl->value;
	}

static void ob_setItem( Object ob, SymbolIndex si, Object value )
	{
	assert( ob_hasItems( ob ) );
	FieldList fl = fl_bySymbol( si, ob->data.fields );
	if ( fl )
		fl->value = value;
	else
		ob->data.fields = fl_new( si, value, ob->data.fields );
	assert( ob_hasItem( ob, si ) );
	assert( ob_getItem( ob, si ) == value );
	}

FUNC bool ob_hasField( Object ob, Symbol field, ObjectHeap heap )
	{
	return ob_hasItem( ob, sy_index( field, heap->symbolTable ) );
	}

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap )
	{
	check( ob_hasField( ob, field, heap ) );
	return ob_getItem( ob, sy_index( field, heap->symbolTable ) );
	}

FUNC void ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap )
	{
	check( ob_hasItems( ob ) );
	ob_setItem( ob, sy_index( field, heap->symbolTable ), value );
	assert( ob_hasField( ob, field, heap ) );
	assert( ob_getField( ob, field, heap ) == value );
	}

FUNC bool ob_hasElement( Object ob, int index, ObjectHeap heap )
	{
	return ob_hasItem( ob, (SymbolIndex)index );
	}

FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap )
	{
	check( ob_hasElement( ob, index, heap ) );
	return ob_getItem( ob, (SymbolIndex)index );
	}

FUNC void ob_setElement( Object ob, int index, Object value, ObjectHeap heap )
	{
	check( ob_hasItems(ob) );
	ob_setItem( ob, (SymbolIndex)index, value );
	assert( ob_hasElement( ob, index, heap ) );
	assert( ob_getElement( ob, index, heap ) == value );
	}

//MERGE:20

