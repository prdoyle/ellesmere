
#include "objects.h"
#include "memory.h"
//#include "stack.h"
#include <stdint.h>
#include <string.h>

typedef struct syd_struct
	{
	Object  token;
	Object  placeholder; // A simple information-free placeholder.  Nothing more than a token really.
	Record  instanceShape;
	} *SymbolDescriptor;

#ifdef NDEBUG
	typedef struct syda_struct *SymbolDescriptorArray; // type-safe phony struct
#else
	typedef Array SymbolDescriptorArray; // give the debugger some symbol info it can use
#endif
#define AR_PREFIX  syda
#define AR_TYPE    SymbolDescriptorArray
#define AR_ELEMENT struct syd_struct
#undef AR_BYVALUE
#include "array_template.h"
#ifndef NDEBUG
	#define syda_new( size, ml ) syda_newAnnotated( size, ml, __FILE__, __LINE__ )
#endif

struct oh_struct
	{
	MemoryLifetime         ml;
	SymbolTable            st;
	InheritanceRelation    ir;
	SymbolDescriptorArray  symbolDescriptors;
	int                    curCheckListIndex;
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

struct ir_struct
	{
	Object      index;
	Symbol      nodeTag;
	ObjectHeap  nodeHeap;
	};

FUNC InheritanceRelation ir_new( ObjectHeap heap, MemoryLifetime ml )
	{
	SymbolTable st = heap->st;

	char indexTagName[30], nodeTagName[30];
	sprintf( indexTagName, "IR_INDEX_%d", st_count( st ) );
	sprintf( nodeTagName,  "IR_NODE_%d",  st_count( st ) );

	InheritanceRelation result = (InheritanceRelation)ml_alloc( ml, sizeof(*result) );
	result->index    = ob_create( sy_byName( indexTagName, st ), heap );
	result->nodeTag  = sy_byName( nodeTagName, st );
	result->nodeHeap = heap;
	return result;
	}

FUNC Object ir_index( InheritanceRelation ir )
	{
	return ir->index;
	}

FUNC Symbol ir_nodeTag( InheritanceRelation ir )
	{
	return ir->nodeTag;
	}

FUNC ObjectHeap ir_nodeHeap( InheritanceRelation ir )
	{
	return ir->nodeHeap;
	}

static void appendToArray( Object ob, int whichArray, Object value, InheritanceRelation ir )
	{
	ObjectHeap heap = ir->nodeHeap;
	SymbolTable st  = heap->st;
	Symbol countSymbol  = sy_byIndex( SYM_ELEMENT_COUNT, st );
	Symbol arraySymbol  = sy_byIndex( SYM_ARRAY, st );
	Object array = ob_getOrCreateField( ob, sy_byIndex( whichArray, st ), arraySymbol, heap );
	int nextIndex = 1 + ob_getIfIntField( array, countSymbol, IR_START_INDEX-1, heap );
	ob_setIntField( array, countSymbol, nextIndex, heap );
	ob_setElement( array, nextIndex, value, heap );
	}

FUNC void ir_add( InheritanceRelation ir, Symbol super, Symbol sub )
	{
	SymbolTable st = ir->nodeHeap->st;
	Symbol symSymbol = sy_byIndex( SYM_SYMBOL, st );
	Object superNode = ob_getOrCreateField( ir->index, super, ir->nodeTag, ir->nodeHeap );
	Object subNode   = ob_getOrCreateField( ir->index, sub,   ir->nodeTag, ir->nodeHeap );
	ob_setField( superNode, symSymbol, oh_symbolToken( ir->nodeHeap, super ), ir->nodeHeap );
	ob_setField( subNode,   symSymbol, oh_symbolToken( ir->nodeHeap, sub   ), ir->nodeHeap );
	appendToArray( superNode, SYM_SUBTAGS,   subNode, ir );
	appendToArray( subNode,   SYM_SUPERTAGS, superNode, ir );
	}

FUNC ObjectHeap theObjectHeap()
	{
	static struct oh_struct _theObjectHeap = { 0 };
	if( !_theObjectHeap.ml )
		{
		MemoryLifetime ml = _theObjectHeap.ml = ml_singleton();
		_theObjectHeap.curCheckListIndex = 1;
		SymbolTable st = _theObjectHeap.st = theSymbolTable();

		// Symbol descriptor table
		int initialDescriptorCount = st_count(st);
		SymbolDescriptorArray syda = _theObjectHeap.symbolDescriptors = syda_new( 100 + initialDescriptorCount, ml );
		syda_setCount( syda, initialDescriptorCount );
		SymbolDescriptor firstDescriptor = syda_element( syda, 0 );
		memset( firstDescriptor, 0, initialDescriptorCount * sizeof( *firstDescriptor ) );

		// Initialize inheritance relation
		InheritanceRelation ir = _theObjectHeap.ir = ir_new( &_theObjectHeap, ml_singleton() );
		int i;
		Symbol any = sy_byIndex( SYM_ANY, st );
		for( i=SYM_ANY+1; i < st_count(st); i++ )
			ir_add( ir, any, sy_byIndex( i, st ) );
		}
	return &_theObjectHeap;
	}

FUNC SymbolTable oh_symbolTable( ObjectHeap heap )
	{
	return heap->st;
	}

FUNC InheritanceRelation oh_inheritanceRelation ( ObjectHeap heap )
	{
	return heap->ir;
	}

#include "objects_walk_backdoor.h"

static FieldList fdl_new( SymbolIndex si, Object value, FieldList tail, ObjectHeap heap )
	{
	FieldList result = (FieldList)ml_alloc( heap->ml, sizeof(*result) );
	assert(( (intptr_t)result & OB_KIND_MASK ) == 0);
	result->si    = si;
	result->value = value;
	result->tail  = tail;
	return result;
	}

static FieldList fdl_bySymbol( SymbolIndex si, FieldList fdl )
	{
	FieldList i;
	for( i = fdl; i && i->si != si; i = i->tail );
	return i;
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
		Grammar     grammar;
		} data;
	int checkListIndex;
	Object recordFields[1]; // actually numRecordFields(tag) elements
	};

static int numRecordFields( Symbol tag, ObjectHeap heap )
	{
	if( sy_instanceShape( tag, heap ) )
		return rd_maxIndex( sy_instanceShape( tag, heap ) );
	else
		return 0;
	}

FUNC Object ob_create( Symbol tag, ObjectHeap heap )
	{
	Object result;
	int recordFieldBytes = numRecordFields( tag, heap ) * sizeof( result->recordFields[0] );
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

FUNC Object ob_fromGrammar( Grammar gr, ObjectHeap heap )
	{
	Object result = (Object)ml_alloc( heap->ml, (sizeof(*result)) );
	result->tag = SYM_GRAMMAR;
	result->data.grammar = gr;
	assert( ob_kind( result ) == OB_STRUCT );
	return result;
	}

FUNC bool ob_isGrammar( Object ob, ObjectHeap heap )
	{
	return ob_kind( ob ) == OB_STRUCT && ob->tag == SYM_GRAMMAR;
	}

FUNC Grammar ob_toGrammar( Object ob, ObjectHeap heap )
	{
	assert( ob_isGrammar( ob, heap ) );
	return ob->data.grammar;
	}

FUNC SymbolIndex ob_tagX( Object ob, ObjectHeap heap )
	{
	switch ( ob_kind( ob ) )
		{
		case OB_INT:
			return SYM_INT;
		case OB_STRUCT:
			break;
		}
	return ob->tag;
	}

FUNC Symbol ob_tag( Object ob, ObjectHeap heap )
	{
	return sy_byIndex( ob_tagX( ob, heap ), heap->st );
	}

FUNC bool ob_hasFields( Object ob )
	{
	return ob_kind(ob) != OB_INT && ob->tag >= NUM_FIELDLESS_OBJECT_TAGS;
	}

FUNC FieldList ob_listFields( Object ob )
	{
	assert( ob_hasFields( ob ) );
	return ob->data.listFields;
	}

static Object ob_getItem( Object ob, SymbolIndex si, ObjectHeap heap )
	{
	int index = rd_indexOf( sy_instanceShape( ob_tag(ob,heap), heap ), si );
	if( index )
		return ob->recordFields[ index-1 ];
	else
		{
		FieldList fdl = fdl_bySymbol( si, ob->data.listFields );
		if( fdl )
			return fdl->value;
		}
	return NULL;
	}

static void ob_setItem( Object ob, SymbolIndex si, Object value, ObjectHeap heap )
	{
	assert( ob_hasFields( ob ) );
	int index = rd_indexOf( sy_instanceShape( ob_tag(ob,heap), heap ), si );
	if( index )
		ob->recordFields[ index-1 ] = value;
	else
		{
		FieldList fdl = fdl_bySymbol( si, ob->data.listFields );
		if( fdl )
			fdl->value = value;
		else
			ob->data.listFields = fdl_new( si, value, ob->data.listFields, heap );
		}
	assert( ob_getItem( ob, si, heap ) == value );
	}

FUNC Object ob_getField( Object ob, Symbol field, ObjectHeap heap )
	{
	check( ob_hasFields( ob ) );
	return ob_getItem( ob, sy_index( field, heap->st ), heap );
	}

FUNC void ob_setField( Object ob, Symbol field, Object value, ObjectHeap heap )
	{
	check( ob_hasFields( ob ) );
	ob_setItem( ob, sy_index( field, heap->st ), value, heap );
	assert( ob_getField( ob, field, heap ) == value );
	}

FUNC void ob_getFieldSymbols( Object ob, BitVector result, ObjectHeap heap )
	{
	if( ob_hasFields( ob ) )
		{
		Record rd = sy_instanceShape( ob_tag(ob,heap), heap );
		int fieldID;
		for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
			if( ob_getItem( ob, fieldID, heap ) )
				bv_set( result, fieldID );
		FieldList field;
		for( field = ob->data.listFields; field; field = field->tail )
			if( field->value )
				bv_set( result, field->si );
		}
	}

static SymbolIndex elementIndex2SymbolIndex( int index )
	{
	// A real SymbolIndex is >= 0.  We choose these fake SymbolIndexes to be negative
	// numbers that are small if the given index is near zero.
	//
	//  0  ->  -1
	// -1  ->  -2
	//  1  ->  -3
	// -2  ->  -4
	//  2  ->  -5
	//    ...
	//
	int numBits = 8*sizeof(index);
	int nonNegativeMask = (-index) >> (numBits-1); // All ones if index >= 0
	return (SymbolIndex)( (index<<1) ^ nonNegativeMask );
	}

FUNC int symbolIndex2ElementIndex( SymbolIndex siArg )
	{
	int si = (int)siArg; // enums can be unsigned
	assert( si < 0 ); // otherwise it's not an element index at all, but rather a bona fide symbol index
	int signMask = - ( si & 1 );
	int result = signMask ^ ( si >> 1 );
	assert( elementIndex2SymbolIndex( result ) == si );
	return result;
	}

FUNC Object ob_getElement( Object ob, int index, ObjectHeap heap )
	{
	check( ob_hasFields( ob ) );
	return ob_getItem( ob, elementIndex2SymbolIndex( index ), heap );
	}

FUNC void ob_setElement( Object ob, int index, Object value, ObjectHeap heap )
	{
	check( ob_hasFields(ob) );
	ob_setItem( ob, elementIndex2SymbolIndex( index ), value, heap );
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

FUNC void cl_checkAll( CheckList target, CheckList source )
	{
	bv_or( target->checkMarks, source->checkMarks );
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
				charsSent += fl_write( fl, "%d", ob_toInt(ob,heap) );
				break;
				}
			case SYM_STRING:
				{
				charsSent += fl_write( fl, "\"%s\"", ob_toString(ob,heap) );
				break;
				}
			case SYM_TOKEN:
				{
				Symbol sy = ob_toSymbol( ob, heap );
				charsSent += fl_write( fl, "%s", sy_name( sy, heap->st ) ); //, sy_index( sy, heap->st ) );
				break;
				}
			case SYM_FUNCTION:
				{
				Function fn = ob_toFunction( ob, heap );
				charsSent += fl_write( fl, "FUNCTION_%p", fn );
				break;
				}
			case SYM_TOKEN_BLOCK:
				{
				TokenBlock tb = ob_toTokenBlock( ob, heap );
				charsSent += fl_write( fl, "TOKEN_BLOCK_%p", tb );
				break;
				}
			case SYM_TOKEN_STREAM:
				{
				TokenStream ts = ob_toTokenStream( ob, heap );
				charsSent += fl_write( fl, "TOKEN_STREAM_%p", ts );
				break;
				}
			case SYM_PLACEHOLDER:
				{
				//charsSent += fl_write( fl, "%s_%p:%s", sy_name( ob_tag(ob,heap), heap->st ), ob, sy_name( ob_getTokenFieldX( ob, SYM_TAG, heap ), heap->st ) );
				charsSent += fl_write( fl, "?%s?", sy_name( ob_getTokenFieldX( ob, SYM_TAG, heap ), heap->st ) );
				break;
				}
			case SYM_VALUE_PLACEHOLDER:
				{
				charsSent += fl_write( fl, "?%s@", sy_name( ob_getTokenFieldX( ob, SYM_TAG, heap ), heap->st ) );
				charsSent += ob_sendTo( ob_getFieldX( ob, SYM_VALUE, heap ), fl, heap );
				charsSent += fl_write( fl, "?" );
				break;
				}
			default:
				{
				charsSent += fl_write( fl, "%s_x%x", sy_name( ob_tag(ob,heap), heap->st ), ob->checkListIndex );
				break;
				}
			}
		}
	return charsSent;
	}

static int sendEdgeTo( SymbolIndex siArg, Object value, ObjectHeap heap, File fl )
	{
	int si = (int)siArg;
	int charsSent = 0;
	charsSent += fl_write( fl, "  " );
	if( si < 0 )
		charsSent += fl_write( fl, "%d", symbolIndex2ElementIndex( si ) );
	else
		charsSent += sy_sendTo( sy_byIndex( si, heap->st ), fl, heap->st );
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
		if( ob_hasFields( ob ) )
			{
			ob_sendTo( ob, fl, heap );
			fl_write( fl, "\n  {\n" );
			Record rd = sy_instanceShape( ob_tag(ob,heap), heap );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Object target = ob_getFieldX( ob, fieldID, heap );
				if( target )
					charsSent += sendEdgeTo( fieldID, target, heap, fl );
				}
			for( field = ob->data.listFields; field; field = field->tail )
				charsSent += sendEdgeTo( field->si, field->value, heap, fl );
			fl_write( fl, "  }\n" );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Object target = ob_getFieldX( ob, fieldID, heap );
				if( target )
					charsSent += sendDeepTo( target, fl, heap, cl );
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
	if( ob_hasFields( value ) )
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
		if( ob_hasFields( ob ) )
			{
			Record rd = sy_instanceShape( ob_tag(ob,heap), heap );
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

static SymbolDescriptor sy_descriptor( Symbol sy, ObjectHeap heap )
	{
	int index = sy_index( sy, heap->st );
	if( index >= syda_count( heap->symbolDescriptors ) )
		syda_setCount( heap->symbolDescriptors, st_count( heap->st ) );
	return syda_element( heap->symbolDescriptors, sy_index( sy, heap->st ) );
	}

FUNC Record sy_instanceShape( Symbol sy, ObjectHeap heap )
	{
	return sy_descriptor( sy, heap )->instanceShape;
	}

FUNC void sy_setInstanceShape( Symbol sy, Record rd, ObjectHeap heap )
	{
	sy_descriptor( sy, heap )->instanceShape = rd;
	}

FUNC Object oh_symbolToken( ObjectHeap heap, Symbol sy )
	{
	Object result = sy_descriptor( sy, heap )->token;
	if( !result )
		{
		result = ob_createX( SYM_TOKEN, heap );
		result->data.symbol = sy;
		sy_descriptor( sy, heap )->token = result;
		}
	return result;
	}

FUNC Object oh_valuePlaceholder( ObjectHeap heap, Symbol tag, Object value )
	{
	Object result = ob_createX( SYM_VALUE_PLACEHOLDER, heap );
	ob_setFieldX      ( result, SYM_VALUE, value, heap );
	ob_setTokenFieldX ( result, SYM_TAG,   tag,   heap );
	return result;
	}

FUNC Object oh_recordedPlaceholder( ObjectHeap heap, Symbol tag )
	{
	Object result = sy_descriptor( tag, heap )->placeholder;
	if( !result )
		{
		result = ob_createX( SYM_RECORDED_PLACEHOLDER, heap );
		ob_setTokenFieldX ( result, SYM_TAG, tag, heap );
		sy_descriptor( tag, heap )->placeholder = result;
		}
	return result;
	}

FUNC bool ob_isPlaceholder( Object ob, ObjectHeap heap )
	{
	SymbolIndex tag = ob_tagX( ob, heap );
	return SYM_PLACEHOLDER <= tag && tag < NUM_PREDEFINED_SYMBOLS;
	}

//MERGE:40

