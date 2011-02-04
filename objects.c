
#include "objects.h"
#include "records.h"
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

#undef WRAP_TOKEN_BLOCKS

struct ob_struct
	{
	SymbolIndex tag;
	union
		{
		FieldList   listFields;
		const char *characters;
		Symbol      symbol;
		Function    function;
#ifdef WRAP_TOKEN_BLOCKS
		TokenBlock  tokenBlock;
#endif
		TokenStream tokenStream;
		} data;
	int checkListIndex;
	Object recordFields[1]; // actually numRecordFields(tag) elements
	};

static int numRecordFields( Symbol tag, SymbolTable st )
	{
	if( sy_instanceShape( tag, st ) )
		return rd_maxIndex( sy_instanceShape( tag, st ) );
	else
		return 0;
	}

FUNC Object ob_create( Symbol tag, ObjectHeap heap )
	{
	Object result;
	int recordFieldBytes = numRecordFields( tag, heap->st ) * sizeof( result->recordFields[0] );
	result = (Object)ml_allocZeros( heap->ml, sizeof(*result) - sizeof(result->recordFields) + recordFieldBytes );
	result->tag = sy_index( tag, heap->st );
	result->data.listFields = NULL;
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

FUNC Object ob_fromTokenBlock( TokenBlock tb, ObjectHeap heap )
	{
#ifdef WRAP_TOKEN_BLOCKS
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = SYM_TOKEN_BLOCK;
	result->data.tokenBlock = tb;
#else
	Object result = (Object)tb;
#endif
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
	}

FUNC bool ob_isTokenBlock( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_TOKEN_BLOCK;
	}

FUNC TokenBlock ob_toTokenBlock( Object ob, ObjectHeap heap )
	{
	assert( ob_isTokenBlock( ob, heap ) );
#ifdef WRAP_TOKEN_BLOCKS
	return ob->data.tokenBlock;
#else
	return (TokenBlock)ob;
#endif
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

static Object ob_getItem( Object ob, SymbolIndex si, ObjectHeap heap )
	{
	int index = rd_indexOf( sy_instanceShape( ob_tag(ob,heap), heap->st ), si );
	if( index )
		return ob->recordFields[ index-1 ];
	else
		{
		FieldList fl = fl_bySymbol( si, ob->data.listFields );
		if( fl )
			return fl->value;
		}
	return NULL;
	}

static void ob_setItem( Object ob, SymbolIndex si, Object value, ObjectHeap heap )
	{
	assert( ob_hasItems( ob ) );
	int index = rd_indexOf( sy_instanceShape( ob_tag(ob,heap), heap->st ), si );
	if( index )
		ob->recordFields[ index-1 ] = value;
	else
		{
		FieldList fl = fl_bySymbol( si, ob->data.listFields );
		if( fl )
			fl->value = value;
		else
			ob->data.listFields = fl_new( si, value, ob->data.listFields, heap );
		}
	assert( ob_getItem( ob, si, heap ) == value );
	}

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap )
	{
	check( ob_hasItems( ob ) );
	return ob_getItem( ob, sy_index( field, heap->st ), heap );
	}

FUNC void ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap )
	{
	check( ob_hasItems( ob ) );
	ob_setItem( ob, sy_index( field, heap->st ), value, heap );
	assert( ob_getField( ob, field, heap ) == value );
	}

FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap )
	{
	check( ob_hasItems( ob ) );
	return ob_getItem( ob, (SymbolIndex)index, heap );
	}

FUNC void ob_setElement( Object ob, int index, Object value, ObjectHeap heap )
	{
	check( ob_hasItems(ob) );
	ob_setItem( ob, (SymbolIndex)index, value, heap );
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
				fl_write( fl, "%s", sy_name( sy, heap->st ), sy_index( sy, heap->st ) );
				break;
				}
			case SYM_FUNCTION:
				{
				Function fn = ob_toFunction( ob, heap );
				fl_write( fl, "FUNCTION_%p", fn );
				break;
				}
			case SYM_TOKEN_BLOCK:
				{
				TokenBlock tb = ob_toTokenBlock( ob, heap );
				fl_write( fl, "TOKEN_BLOCK_%p", tb );
				break;
				}
			case SYM_TOKEN_STREAM:
				{
				TokenStream ts = ob_toTokenStream( ob, heap );
				fl_write( fl, "TOKEN_STREAM_%p", ts );
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

static int sendEdgeTo( Symbol sy, Object value, ObjectHeap heap, File fl )
	{
	int charsSent = 0;
	charsSent += fl_write( fl, "  " );
	charsSent += sy_sendTo( sy, fl, heap->st );
	charsSent += fl_write( fl, "->" );
	charsSent += ob_sendTo( value, fl, heap );
	charsSent += fl_write( fl, "\n" );
	return charsSent;
	}

static int sendDeepTo( Object ob, File fl, ObjectHeap heap, CheckList cl )
	{
	int charsSent = 0;
	if( !cl_isChecked( cl, ob ) && fl )
		{
		FieldList field;
		int fieldID;
		cl_check( cl, ob );
		if( ob_hasItems( ob ) )
			{
			ob_sendTo( ob, fl, heap );
			fl_write( fl, "\n  {\n" );
			Record rd = sy_instanceShape( ob_tag(ob,heap), heap->st );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				charsSent += sendEdgeTo( sy, ob_getField( ob, sy, heap ), heap, fl );
				}
			for( field = ob->data.listFields; field; field = field->tail )
				charsSent += sendEdgeTo( sy_byIndex( field->si, heap->st ), field->value, heap, fl );
			fl_write( fl, "  }\n" );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				charsSent += sendDeepTo( ob_getField( ob, sy, heap ), fl, heap, cl );
				}
			for( field = ob->data.listFields; field; field = field->tail )
				charsSent += sendDeepTo( field->value, fl, heap, cl );
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

static int sendDotEdgesTo( Object ob, File fl, ObjectHeap heap, CheckList cl );

static int sendDotEdgeTo( Object ob, Symbol sy, Object value, ObjectHeap heap, File fl, CheckList cl )
	{
	if( !value )
		return 0;

	int charsSent = 0;
	if( ob_hasItems( value ) )
		{
		charsSent += fl_write( fl,
			"n%p -> n%p [label=\"%s\"]\n",
			ob, value, sy_name( sy, heap->st ) );
		charsSent += sendDotEdgesTo( value, fl, heap, cl );
		}
#if 0
	else if( ob_isInt( value, heap ) )
		{
		charsSent += fl_write( fl,
			"n%p -> %d [label=\"%s\"]\n",
			ob, ob_toInt( value, heap ), sy_name( sy, heap->st ) );
		}
#endif
	return charsSent;
	}

static int sendDotEdgesTo( Object ob, File fl, ObjectHeap heap, CheckList cl )
	{
	int charsSent = 0;
	if( !cl_isChecked( cl, ob ) )
		{
		FieldList field;
		int fieldID;
		cl_check( cl, ob );
		if( ob_hasItems( ob ) )
			{
			Record rd = sy_instanceShape( ob_tag(ob,heap), heap->st );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				charsSent += sendDotEdgeTo( ob, sy, ob_getField( ob, sy, heap ), heap, fl, cl );
				}
			if( ob->data.listFields )
				fl_write( fl, "// listFields for n%p begin here\n", ob );
			for( field = ob->data.listFields; field; field = field->tail )
				{
				Symbol sy = sy_byIndex( field->si, heap->st );
				charsSent += sendDotEdgeTo( ob, sy, field->value, heap, fl, cl );
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

