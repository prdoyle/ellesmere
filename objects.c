
#include "objects.h"
#include "records.h"
#include "memory.h"
#include "stack.h"
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

FUNC SymbolTable oh_tagSymbolTable( ObjectHeap heap )
	{
	return heap->st;
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
		Grammar     grammar;
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

FUNC void ob_getFieldSymbols( Object ob, BitVector result, ObjectHeap heap )
	{
	if( ob_hasItems( ob ) )
		{
		Record rd = sy_instanceShape( ob_tag(ob,heap), heap->st );
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

static int symbolIndex2ElementIndex( SymbolIndex siArg )
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
	check( ob_hasItems( ob ) );
	return ob_getItem( ob, elementIndex2SymbolIndex( index ), heap );
	}

FUNC void ob_setElement( Object ob, int index, Object value, ObjectHeap heap )
	{
	check( ob_hasItems(ob) );
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

static int recurseIntoField( Object head, Symbol edgeSymbol, Object tail, Stack workList, CheckList alreadyPushed, ObjectHeap heap, EdgePredicate ep, void *context )
	{
	if( !tail )
		return 0;
	if( cl_isChecked( alreadyPushed, tail ) )
		return 0;
	if( !ep( context, head, edgeSymbol, 0, tail ) )
		return 0;
	cl_check( alreadyPushed, tail );
	sk_push( workList, tail );
	return 1;
	}

static int recurseIntoArrayElement( Object head, int index, Object tail, Stack workList, CheckList alreadyPushed, ObjectHeap heap, EdgePredicate ep, void *context )
	{
	if( !tail )
		return 0;
	if( cl_isChecked( alreadyPushed, tail ) )
		return 0;
	if( !ep( context, head, NULL, index, tail ) )
		return 0;
	cl_check( alreadyPushed, tail );
	sk_push( workList, tail );
	return 1;
	}

FUNC void postorderWalk( Stack workList, EdgePredicate recurseIntoEdge, VertexProcedure processVertex, ObjectHeap heap, void *context )
	{
	MemoryLifetime walkTime = ml_begin( 1000, ml_indefinite() );
	CheckList alreadyPushed = cl_open( heap );
	int i;
	for( i=0; i < sk_depth( workList ); i++ )
		cl_check( alreadyPushed, sk_item( workList, i ) );
	while( !sk_isEmpty( workList ) )
		{
		Object curObject = sk_top( workList );
		int numChildrenPushed = 0;
		if( ob_hasItems( curObject ) )
			{
			Record rd = sy_instanceShape( ob_tag(curObject,heap), heap->st );
			int fieldID;
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				numChildrenPushed += recurseIntoField( curObject, sy, ob_getField( curObject, sy, heap ), workList, alreadyPushed, heap, recurseIntoEdge, context );
				}
			FieldList field;
			for( field = curObject->data.listFields; field; field = field->tail )
				{
				int si = (int)field->si;
				if( si < 0 )
					numChildrenPushed += recurseIntoArrayElement( curObject, symbolIndex2ElementIndex( field->si ), field->value, workList, alreadyPushed, heap, recurseIntoEdge, context );
				else
					numChildrenPushed += recurseIntoField( curObject, sy_byIndex( field->si, heap->st ), field->value, workList, alreadyPushed, heap, recurseIntoEdge, context );
				}
			}
		if( numChildrenPushed == 0 )
			{
			sk_pop( workList );
			processVertex( context, curObject );
			}
		}
	cl_close( alreadyPushed );
	ml_end( walkTime );
	}

bool everyEdge( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail ){ return true; }

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
		if( ob_hasItems( ob ) )
			{
			ob_sendTo( ob, fl, heap );
			fl_write( fl, "\n  {\n" );
			Record rd = sy_instanceShape( ob_tag(ob,heap), heap->st );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				Object target = ob_getField( ob, sy, heap );
				if( target )
					charsSent += sendEdgeTo( fieldID, target, heap, fl );
				}
			for( field = ob->data.listFields; field; field = field->tail )
				charsSent += sendEdgeTo( field->si, field->value, heap, fl );
			fl_write( fl, "  }\n" );
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, heap->st );
				Object target = ob_getField( ob, sy, heap );
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

#ifdef OBJECTS_T

static Object create( char *tagName )
	{
	return ob_create( sy_byName( tagName, theSymbolTable() ), theObjectHeap() );
	}

static void field( Object from, char *edgeName, Object to )
	{
	ob_setField( from, sy_byName( edgeName, theSymbolTable() ), to, theObjectHeap() );
	}

static void element( Object from, int index, Object to )
	{
	ob_setElement( from, index, to, theObjectHeap() );
	}

static const char *ob2str( Object ob )
	{
	if( ob )
		return sy_name( ob_tag( ob, theObjectHeap() ), theSymbolTable() );
	else
		return "(NULL)";
	}

static bool printEveryEdge( void *fileArg, Object head, Symbol edgeSymbol, int edgeIndex, Object tail )
	{
	File file = (File)fileArg;
	if( edgeSymbol )
		fl_write( file, "%s -%s-> %s\n", ob2str( head ), sy_name( edgeSymbol, theSymbolTable() ), ob2str( tail ) );
	else
		fl_write( file, "%s[%d] -> %s\n", ob2str( head ), edgeIndex, ob2str( tail ) );
	return true;
	}

static void printNodeTag( void *fileArg, Object node )
	{
	File file = (File)fileArg;
	fl_write( file, "%s\n", ob2str( node ) );
	}

int main( int argc, char **argv )
	{
	Object any             = create( "any" );
	Object physical        = create( "physical" );
	Object furniture       = create( "furniture" );
	Object sofa            = create( "sofa" );
	Object bed             = create( "bed" );
	Object sofaBed         = create( "sofaBed" );
	Object vehicle         = create( "vehicle" );
	Object truck           = create( "truck" );
	Object fireTruck       = create( "fireTruck" );
	Object dumpTruck       = create( "dumpTruck" );
	Object mobileHome      = create( "mobileHome" );
	Object number          = create( "number" );
	Object integer         = create( "integer" );
	Object positive        = create( "positive" );
	Object prime           = create( "prime" );
	Object even            = create( "even" );
	Object odd             = create( "odd" );
	Object mersenne        = create( "mersenne" );
	Object three           = create( "three" );

	field( physical,  "is", any );
	field( furniture, "is", physical );
	field( sofa     , "is", furniture );
	field( bed      , "is", furniture );
	element( sofaBed, 1, sofa );
	element( sofaBed, 2, bed );
	field( vehicle,   "is", physical );
	field( truck,     "is", vehicle );
	field( fireTruck, "is", truck );
	field( dumpTruck, "is", truck );
	element( mobileHome, 60, truck );
	element( mobileHome, 30, bed );
	element( mobileHome, 10, sofa );

	field( number,   "is", any );
	field( integer,  "is", number );
	field( positive, "is", number );
	field( prime,    "is", positive );
	field( even,     "is", number );
	field( odd,      "is", number );
	field( mersenne, "is", prime );
	field( mersenne, "isAlso", odd );
	field( three,    "is", mersenne );
	field( three,    "value", ob_fromInt( 3, theObjectHeap() ) );

	Stack workList = sk_new( ml_indefinite() );
	sk_push( workList, mobileHome );
	sk_push( workList, three );
	postorderWalk( workList, printEveryEdge, printNodeTag, theObjectHeap(), stdout );
	}

#endif

//MERGE:40

