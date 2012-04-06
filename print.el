#! ellesmere
# This demonstrates something that doesn't work yet:
# token blocks currently must parse.  That really
# limits the expressive freedom possible.
#
# I need to find a way to make a token block any
# sequence of tokens between curly braces, possibly
# containing nested curly braces. (Though somehow there
# also ought to be a way to have unbalanced curlies
# in a token block!)

def <<
	<< arg:ANY,
as
	{ arg print! << }

def VOID
	<< >>
as {}

<< "Hello.  1+2=", 1 2 add! >>
