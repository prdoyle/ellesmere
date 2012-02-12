
#ifndef OBJECTS_WALK_BACKDOOR_H
#define OBJECTS_WALK_BACKDOOR_H

typedef struct fdl_struct
	{
	SymbolIndex       si;
	Object            value;
	struct fdl_struct *tail;
	} *FieldList;

FUNC FieldList ob_listFields( Object ob );
FUNC int symbolIndex2ElementIndex( SymbolIndex siArg );

#endif

