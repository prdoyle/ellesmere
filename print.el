
def <<
	<< arg:ANY,
as
	{ arg print! << }

def VOID
	<< >>
as {}

<< "Hello.  1+2=", 1 2 add! >>
