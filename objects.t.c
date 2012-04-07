
#include "objects.h"
#include "stack.h"
#include "memory.h"
#include "walk.h"

static Object create( char *tagName )
	{
	return ob_create( sy_byName( tagName, theSymbolTable( theObjectHeap() ) ), theObjectHeap() );
	}

static void field( Object from, char *edgeName, Object to )
	{
	ob_setField( from, sy_byName( edgeName, theSymbolTable( theObjectHeap() ) ), to, theObjectHeap() );
	}

static void element( Object from, int index, Object to )
	{
	ob_setElement( from, index, to, theObjectHeap() );
	}

static const char *ob2str( Object ob )
	{
	if( ob )
		return sy_name( ob_tag( ob, theObjectHeap() ), theSymbolTable( theObjectHeap() ) );
	else
		return "(NULL)";
	}

static bool printEveryEdge( void *fileArg, Object tail, Symbol edgeSymbol, int edgeIndex, Object head )
	{
	File file = (File)fileArg;
	if( edgeSymbol )
		fl_write( file, "%s -%s-> %s\n", ob2str( tail ), sy_name( edgeSymbol, theSymbolTable( theObjectHeap() ) ), ob2str( head ) );
	else
		fl_write( file, "%s[%d] -> %s\n", ob2str( tail ), edgeIndex, ob2str( head ) );
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

