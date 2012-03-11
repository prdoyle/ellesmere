
#include "walk.h"
#include "records.h"
#include "stack.h"

#include "objects_walk_backdoor.h"

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
	CheckList alreadyPushed = cl_open( heap );
	int i;
	for( i=0; i < sk_depth( workList ); i++ )
		cl_check( alreadyPushed, sk_item( workList, i ) );
	while( !sk_isEmpty( workList ) )
		{
		Object curObject = sk_top( workList );
		int numChildrenPushed = 0;
		if( ob_hasFields( curObject ) )
			{
			SymbolTable st = oh_symbolTable( heap );
			Record rd = sy_instanceShape( ob_tag(curObject,heap), heap );
			int fieldID;
			for( fieldID = rd_firstField(rd); fieldID != rd_NONE && !numChildrenPushed; fieldID = rd_nextField( rd, fieldID ) )
				{
				Symbol sy = sy_byIndex( fieldID, st );
				numChildrenPushed += recurseIntoField( curObject, sy, ob_getField( curObject, sy, heap ), workList, alreadyPushed, heap, recurseIntoEdge, context );
				}
			FieldList field;
			for( field = ob_listFields( curObject ); field && !numChildrenPushed; field = field->tail )
				{
				int si = (int)field->si;
				if( si < 0 )
					numChildrenPushed += recurseIntoArrayElement( curObject, symbolIndex2ElementIndex( field->si ), field->value, workList, alreadyPushed, heap, recurseIntoEdge, context );
				else
					numChildrenPushed += recurseIntoField( curObject, sy_byIndex( field->si, st ), field->value, workList, alreadyPushed, heap, recurseIntoEdge, context );
				}
			}
		if( numChildrenPushed == 0 )
			{
			sk_pop( workList );
			processVertex( context, curObject );
			}
		}
	cl_close( alreadyPushed );
	}

FUNC bool everyEdge( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail ){ return true; }

