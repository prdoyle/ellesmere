
#ifndef WALK_H
#define WALK_H

#include "objects.h"

typedef bool ( *EdgePredicate )( void *context, Object tail, Symbol edgeSymbol, int edgeIndex, Object head );
typedef void ( *VertexProcedure )( void *context, Object vertex );

FUNC void postorderWalk( Stack workList, EdgePredicate recurseIntoEdge, VertexProcedure processVertex, ObjectHeap heap, void *context );
FUNC bool everyEdge( void *context, Object tail, Symbol edgeSymbol, int edgeIndex, Object head );

#endif

