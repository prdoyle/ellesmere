
#include "objects.h"
#include "memory.h"
#include <stdint.h>
#include <string.h>

struct oh_struct
	{
	SymbolTable    st;
	MemoryLifetime ml;
	int curCheckListIndex;
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
	if( !_theObjectHeap.st )
		{
		_theObjectHeap.st = theSymbolTable();
		_theObjectHeap.ml = ml_singleton();
		_theObjectHeap.curCheckListIndex = 1;
		}
	return &_theObjectHeap;
	}

typedef struct fl_struct
	{
	SymbolIndex       si;
	Object            value;
	struct fl_struct *tail;
	} *FieldList;

static FieldList fl_new( SymbolIndex si, Object value, FieldList tail, ObjectHeap heap )
	{
	FieldList result = (FieldList)ml_alloc( heap->ml, sizeof(*result) );
	assert(( (intptr_t)result & OB_KIND_MASK ) == 0);
	result->si    = si;
	result->value = value;
	result->tail  = tail;
	return result;
	}

static FieldList fl_bySymbol( SymbolIndex si, FieldList fl )
	{
	if( fl && fl->si != si )
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
		Symbol      symbol;
		Function    function;
		TokenStream tokenStream;
		} data;
	int checkListIndex;
	};

FUNC Object ob_create( Symbol tag, ObjectHeap heap )
	{
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = sy_index( tag, heap->st );
	result->data.fields = NULL;
	result->checkListIndex = heap->curCheckListIndex;
	heap->curCheckListIndex += 2;
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
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = SYM_STRING;
	result->data.characters = strdup( value );
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
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

FUNC bool ob_isToken( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_TOKEN;
	}

FUNC Symbol ob_toSymbol( Object ob, ObjectHeap heap )
	{
	assert( ob_isToken( ob, heap ) );
	return ob->data.symbol;
	}

FUNC Object ob_fromFunction( Function fn, ObjectHeap heap )
	{
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = SYM_FUNCTION;
	result->data.function = fn;
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
	}

FUNC bool ob_isFunction( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_FUNCTION;
	}

FUNC Function ob_toFunction( Object ob, ObjectHeap heap )
	{
	assert( ob_isFunction( ob, heap ) );
	return ob->data.function;
	}

FUNC Object ob_fromTokenStream( TokenStream ts, ObjectHeap heap )
	{
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = SYM_TOKEN_STREAM;
	result->data.tokenStream = ts;
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
	}

FUNC bool ob_isTokenStream( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_TOKEN_STREAM;
	}

FUNC TokenStream ob_toTokenStream( Object ob, ObjectHeap heap )
	{
	assert( ob_isTokenStream( ob, heap ) );
	return ob->data.tokenStream;
	}

FUNC Symbol ob_tag( Object ob, ObjectHeap heap )
	{
	switch ( ob_kind( ob ) )
		{
		case OB_INT:
			return sy_byIndex( SYM_INT, heap->st );
		case OB_STRUCT:
			break;
		}
	return sy_byIndex( ob->tag, heap->st );
	}

static bool ob_hasItems( Object ob )
	{
	return ob_kind(ob) != OB_INT && ob->tag >= NUM_SPECIAL_OBJECT_TAGS;
	}

static bool ob_hasItem( Object ob, SymbolIndex si )
	{
	if( ob_hasItems(ob) )
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

static void ob_setItem( Object ob, SymbolIndex si, Object value, ObjectHeap heap )
	{
	assert( ob_hasItems( ob ) );
	FieldList fl = fl_bySymbol( si, ob->data.fields );
	if( fl )
		fl->value = value;
	else
		ob->data.fields = fl_new( si, value, ob->data.fields, heap );
	assert( ob_hasItem( ob, si ) );
	assert( ob_getItem( ob, si ) == value );
	}

FUNC bool ob_hasField( Object ob, Symbol field, ObjectHeap heap )
	{
	return ob_hasItem( ob, sy_index( field, heap->st ) );
	}

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap )
	{
	check( ob_hasField( ob, field, heap ) );
	return ob_getItem( ob, sy_index( field, heap->st ) );
	}

FUNC void ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap )
	{
	check( ob_hasItems( ob ) );
	ob_setItem( ob, sy_index( field, heap->st ), value, heap );
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
	ob_setItem( ob, (SymbolIndex)index, value, heap );
	assert( ob_hasElement( ob, index, heap ) );
	assert( ob_getElement( ob, index, heap ) == value );
	}

#include "bitvector.h"

struct cl_struct
	{
	ObjectHeap heap;
	BitVector  checkMarks;
	MemoryLifetime ml;
	};

FUNC CheckList cl_open( ObjectHeap heap )
	{
	MemoryLifetime checklistLifetime = ml_begin( 1000, heap->ml );
	CheckList result = (CheckList)ml_alloc( checklistLifetime, sizeof(*result) );
	result->heap = heap;
	result->ml = checklistLifetime;
	result->checkMarks = bv_new( 50, checklistLifetime );
	return result;
	}

FUNC void cl_close( CheckList cl )
	{
	ml_end( cl->ml );
	}

FUNC void cl_check( CheckList cl, Object ob )
	{
	if( ob_isInt( ob, cl->heap ) )
		bv_set( cl->checkMarks, 2*ob_toInt( ob, cl->heap ) );
	else
		bv_set( cl->checkMarks, ob->checkListIndex );
	}

FUNC void cl_uncheck( CheckList cl, Object ob )
	{
	if( ob_isInt( ob, cl->heap ) )
		bv_unset( cl->checkMarks, 2*ob_toInt( ob, cl->heap ) );
	else
		bv_unset( cl->checkMarks, ob->checkListIndex );
	}

FUNC bool cl_isChecked( CheckList cl, Object ob )
	{
	if( ob_isInt( ob, cl->heap ) )
		return bv_isSet( cl->checkMarks, 2*ob_toInt( ob, cl->heap ) );
	else
		return bv_isSet( cl->checkMarks, ob->checkListIndex );
	}

FUNC int ob_sendTo( Object ob, File fl, ObjectHeap heap )
	{
	int charsSent=0;
	if( fl )
		{
		if( ob == NULL )
			return fl_write( fl, "(null)" );
		else switch( sy_index( ob_tag(ob,heap), heap->st ) )
			{
			case SYM_INT:
				{
				fl_write( fl, "%d", ob_toInt(ob,heap) );
				break;
				}
			case SYM_STRING:
				{
				fl_write( fl, "\"%s\"", ob_toString(ob,heap) );
				break;
				}
			case SYM_TOKEN:
				{
				Symbol sy = ob_toSymbol( ob, heap );
				fl_write( fl, "{%s#%d}", sy_name( sy, heap->st ), sy_index( sy, heap->st ) );
				break;
				}
			case SYM_FUNCTION:
				{
				Function fn = ob_toFunction( ob, heap );
				fl_write( fl, ":FUNCTION_%p", fn );
				break;
				}
			case SYM_TOKEN_STREAM:
				{
				TokenStream ts = ob_toTokenStream( ob, heap );
				fl_write( fl, ":TOKEN_STREAM_%p", ts );
				break;
				}
			default:
				{
				fl_write( fl, "%s_%p", sy_name( ob_tag(ob,heap), heap->st ), ob );
				break;
				}
			}
		}
	return charsSent;
	}

static int sendDeepTo( Object ob, File fl, ObjectHeap heap, CheckList cl )
	{
	int charsSent = 0;
	if( !cl_isChecked( cl, ob ) )
		{
		FieldList field;
		cl_check( cl, ob );
		if( ob_hasItems( ob ) )
			{
			ob_sendTo( ob, fl, heap );
			fl_write( fl, "\n  {\n" );
			for( field = ob->data.fields; field; field = field->tail )
				{
				fl_write( fl, "  " );
				sy_sendTo( sy_byIndex( field->si, heap->st ), fl, heap->st );
				fl_write( fl, "->" );
				ob_sendTo( field->value, fl, heap );
				fl_write( fl, "\n" );
				}
			fl_write( fl, "  }\n" );
			for( field = ob->data.fields; field; field = field->tail )
				sendDeepTo( field->value, fl, heap, cl );
			}
		else
			{
			// Don't bother printing objects with no items
			//ob_sendTo( ob, fl, heap );
			//fl_write( fl, "\n" );
			}
		}
	return charsSent;
	}

FUNC int ob_sendDeepTo( Object ob, File fl, ObjectHeap heap )
	{
	CheckList cl = cl_open( heap );
	int result = sendDeepTo( ob, fl, heap, cl );
	cl_close( cl );
	return result;
	}

static int sendDotEdgesTo( Object ob, File fl, ObjectHeap heap, CheckList cl )
	{
	int charsSent = 0;
	if( !cl_isChecked( cl, ob ) )
		{
		FieldList field;
		cl_check( cl, ob );
		if( ob_hasItems( ob ) )
			{
			for( field = ob->data.fields; field; field = field->tail )
				{
				Symbol sy = sy_byIndex( field->si, heap->st );
				if( ob_hasItems( field->value ) )
					{
					charsSent += fl_write( fl,
						"n%p -> n%p [label=\"%s\"]\n",
						ob, field->value, sy_name( sy, heap->st ) );
					charsSent += sendDotEdgesTo( field->value, fl, heap, cl );
					}
				else if( ob_isInt( field->value, heap ) )
					{
					charsSent += fl_write( fl,
						"n%p -> %d [label=\"%s\"]\n",
						ob, ob_toInt( field->value, heap ), sy_name( sy, heap->st ) );
					}
				}
			}
		}
	return charsSent;
	}

FUNC int ob_sendDotEdgesTo( Object ob, File fl, ObjectHeap heap )
	{
	CheckList cl = cl_open( heap );
	int result = sendDotEdgesTo( ob, fl, heap, cl );
	cl_close( cl );
	return result;
	}

#include "symbols_impl.h"

FUNC Object oh_symbolToken( ObjectHeap heap, Symbol sy )
	{
	if( !sy->token )
		{
		sy->token = ob_create( sy_byIndex( SYM_TOKEN, heap->st ), heap );
		sy->token->data.symbol = sy;
		}
	return sy->token;
	}

//MERGE:40

