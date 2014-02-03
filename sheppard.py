#! /usr/bin/python

import string

object_id = 0

class Object:

	def __init__( self, tag, **edges ):
		global object_id
		self.TAG = tag
		self.ELEMENTS = {}
		self.FIELDS = [ k for k in edges ]
		self.ID = object_id
		object_id = object_id+1
		for ( name, value ) in edges.iteritems():
			setattr( self, name, value )

	def get( self, key, default ):
		if key in self:
			return self[ key ]
		else:
			return default

	# Allow map syntax for convenience

	def __getitem__( self, key ):
		if isinstance( key, int ):
			return self.ELEMENTS[ key ]
		else:
			return getattr( self, key )

	def __setitem__( self, key, value ):
		if isinstance( key, int ):
			self.ELEMENTS[ key ] = value
		else:
			setattr( self, key, value )
			if not key in self.FIELDS:
				self.FIELDS.append( key )

	def __contains__( self, key ):
		try:
			self[ key ]
			return True
		except:
			return False

	def __delitem__( self, key ):
		if key in [ "TAG", "ELEMENTS", "FIELDS", "ID" ]:
			raise KeyError # TODO: Use a Sheppard exception?
		else:
			delattr( self, key )
			self.FIELDS.remove( key )

	def __iter__( self ): # Range over array (index,value) pairs; TODO: do normal fields too?
		for key in self.ELEMENTS.iterkeys():
			yield ( key, self.ELEMENTS[ key ] )

	def __repr__( self ):
		if self is null:
			return "null"
		else:
			return "%s#%d" % ( self.TAG, self.ID )

	def __str__( self ):
		if self is null:
			return "null"
		else:
			return repr( self ) + "{ " + string.join([ "%s:%s" % ( field, self[field] ) for field in self.FIELDS ], ', ') + " }"

	# null and zero are false; all else are true

	def __nonzero__( self ): return self != 0 and self != null

class Null( Object ): # Just to make debugging messages more informative

	def __init__( self ):
		Object.__init__( self, "null" )

def is_int( obj ):    return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_symbol( obj ): return isinstance( obj, str ) # Sheppard symbols are represented by Python strs

def tag( obj ):
	if is_int( obj ):
		return "int"
	elif is_symbol( obj ):
		return "symbol"
	else: # All other Sheppard objects are represented by instances of Object
		return obj.TAG

def pop( stack ):  return ( stack.head, stack.tail )

# Object constructors

null = Null()
def Parser( automaton ): return Object( "parser", state=automaton, stack=null )
def Cons( head, tail ): return Object( "list", head=head, tail=tail, FIELDS=['head','tail'] )
def List( items ):
	if items:
		return Cons( items[0], List( items[1:] ) )
	else:
		return null

def Digression( tokens, bindings, prev ): return Object( "digression", tokens=tokens, bindings=bindings, prev=prev )

def Thread( cursor, value_stack, state_stack ): return Object( "thread", cursor=cursor, value_stack=value_stack, state_stack=state_stack )

# Main execute procedure

def debug( message, *args ):
	if args:
		message = message % args
	print message

def python_list( sheppard_list, head="head", tail="tail" ):
	if sheppard_list:
		return [ sheppard_list[ head ] ] + to_python_list( sheppard_list[ tail ], head, tail )
	else:
		return []

def stack_str( stack ):
	return string.join([ repr(s) for s in python_list( stack )], ", " )

#
# The interpreter
#

def get_raw_token( th ):
	if th.cursor:
		raw_token = th.cursor.tokens.get( "head", "EOF" )
		th.cursor.tokens = th.cursor.tokens.tail
	else:
		raw_token = "EOF"
	return raw_token

def finish_digression( th ):
	if th.cursor and not th.cursor.tokens:
		debug( "  finished digression" )
		th.cursor = th.cursor.prev

def bind_arg( th, formal_arg, actual_arg, arg_bindings ):
	if formal_arg:
		arg_bindings[ formal_arg ] = actual_arg
		debug( "    %s=%s", formal_arg, repr( actual_arg ) )
	else:
		debug( "    pop %s", repr( actual_arg ) )

def bind_args( th, formal_args, arg_bindings ):
	if formal_args:
		th.state_stack = th.state_stack.tail
		( formal_arg, formal_args )    = pop( formal_args )
		( actual_arg, th.value_stack ) = pop( th.value_stack )
		bind_arg( th, formal_arg, actual_arg, arg_bindings )
		return bind_args( th, formal_args, arg_bindings )
	else:
		return arg_bindings

def bound( obj, digression ):
	# This gives dynamic scoping.  TODO: Static scopes would usually be preferable.
	if digression and is_symbol( obj ):
		return digression.bindings.get( obj, obj )
	else:
		return obj

def do_action( th, action, arg_bindings ):
	if tag( action ) == "primitive":
		action.function( th, arg_bindings )
	else:
		th.cursor = Digression( action.tokens, arg_bindings, th.cursor )

def perform_accept( th ):
	return True

def perform_shift( th ):
	debug( "shift" )
	debug( "  cursor: %s", repr( th.cursor ) )
	raw_token = get_raw_token( th )
	debug( "  token: %s", repr( raw_token ) )
	current_token = bound( raw_token, th.cursor )
	debug( "    value: %s", repr( raw_token ) )
	th.value_stack = Cons( current_token, th.value_stack )
	new_state = th.state_stack.head[ current_token ]
	debug( "  new_state: %s", repr( new_state ) )
	th.state_stack = Cons( new_state, th.state_stack )
	finish_digression( th )
	return False

def perform_reduce0( th ):
	debug( "reduce0" )
	action = bound( th.state_stack.head.action, th.cursor )
	debug( "  action: %s", repr( action ) )
	formal_args = action.formal_args
	arg_bindings = Object( "bindings" )
	bind_args( th, formal_args, arg_bindings )
	debug( "  bindings: %s", arg_bindings )
	do_action( th, action, arg_bindings )
	return False

perform = {
	"accept":  perform_accept,
	"shift":   perform_shift,
	"reduce0": perform_reduce0,
	}

def execute( procedure, bindings ):
	th = Thread(
		Digression( procedure.tokens, bindings, null ),
		null,
		Cons( procedure.dialect, null ) )
	debug( "starting thread:\n  %s", th )
	while True:
		debug( "state_stack: %s", stack_str( th.state_stack ) )
		command = tag( th.state_stack.head )
		if perform[ command ]( th ):
			break

def Shift( **edges ): return Object( "shift", **edges )
def Reduce0( action ): return Object( "reduce0", action=action )
def Accept(): return Object( "accept" )
def Macro( tokens, formal_args ): return Object( "macro", tokens=tokens, formal_args=formal_args )
def Procedure( tokens, dialect ): return Object( "procedure", tokens=tokens, dialect=dialect )
def Primitive( function, formal_args ): return Object( "primitive", function=function, formal_args=formal_args )

#
# Testing
#

if 0:
	x = Cons("first", null)
	x = Cons("second", x)

if 1:
	dialect = Shift(
		hello = Shift(
			world = Reduce0( Macro(
				tokens = List([ "go" ]),
				formal_args = List([ "W", "H" ])  # Formal args get popped, so they must appear in reverse order
				))),
		go = Reduce0( Primitive(
			function = ( lambda th, b: debug( "ARGS: %s", b ) ),
			formal_args=Cons("arg", null)
			)),
		EOF = Accept())

	execute( Procedure( List([ "hello", "world" ]), dialect ), Object("bindings") )

