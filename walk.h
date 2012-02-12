
#ifndef WALK_H
#define WALK_H

#include "base.h"

typedef bool ( *EdgePredicate )( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail );
typedef void ( *VertexProcedure )( void *context, Object vertex );

FUNC void postorderWalk( Stack workList, EdgePredicate recurseIntoEdge, VertexProcedure processVertex, ObjectHeap heap, void *context );
FUNC bool everyEdge( void *context, Object head, Symbol edgeSymbol, int edgeIndex, Object tail );

#endif

