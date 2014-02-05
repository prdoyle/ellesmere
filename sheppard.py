#! /usr/bin/python

import string, re

object_id = 0

class Object:

	def __init__( self, tag, **edges ):
		global object_id
		assert( tag.isupper() )
		self._tag = tag
		self._elements = {}
		self._fields = sorted([ k for k in edges ]) # _fields can be adjusted if we want a particular field ordering
		self._id = object_id
		object_id = object_id+1
		for ( name, value ) in edges.iteritems():
			setattr( self, name, value )

	def take( self, key, default ):
		#debug( "--    %s.take( %s, %s )", repr(self), repr(key), repr(default) )
		if is_symbol( key ) and key in self:
			return self[ key ]
		else:
			return default

	# Allow map syntax for convenience

	def __getitem__( self, key ):
		if isinstance( key, int ):
			return self._elements[ key ]
		else:
			return getattr( self, key )

	def __setitem__( self, key, value ):
		if isinstance( key, int ):
			self._elements[ key ] = value
		else:
			setattr( self, key, value )
			if not key in self._fields:
				self._fields.append( key )

	def __contains__( self, key ):
		try:
			self[ key ]
			return True
		except:
			return False

	def __delitem__( self, key ):
		if key in [ "_tag", "_elements", "_fields", "_id" ]:
			raise KeyError # TODO: Use a Sheppard exception?
		else:
			delattr( self, key )
			self._fields.remove( key )

	def __iter__( self ): # Range over array (index,value) pairs; TODO: do normal fields too?
		for key in self._fields:
			yield ( key, getattr( self, key ) )
		for key in self._elements.iterkeys():
			yield ( key, self._elements[ key ] )

	def __repr__( self ):
		if self is null:
			return "null"
		else:
			return "%s#%d" % ( self._tag, self._id )

	def __str__( self ):
		if self is null:
			return "null"
		else:
			return repr( self ) + "{ " + string.join([ "%s:%s" % ( field, repr( self[field] ) ) for field in self._fields ], ', ') + " }"

	def description( self, already_described=None ):
		if already_described is None:
			already_described = set()
		if self is null:
			return "null"
		elif self in already_described:
			return repr( self )
		else:
			already_described.add( self )
			return repr( self ) + "{ " + string.join([ "%s:%s" % ( field, self._description( self[ field ], already_described ) ) for field in self._fields ], ', ') + " }"

	def _description( self, value, already_described ):
		if isinstance( value, Object ):
			return value.description( already_described )
		else:
			return str( value )

	# null and zero are false; all else are true

	def __nonzero__( self ): return self != 0 and self != null

class Null( Object ): # Just to make debugging messages more informative

	def __init__( self ):
		Object.__init__( self, "NULL" )

def is_int( obj ):    return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_symbol( obj ): return isinstance( obj, str ) # Sheppard symbols are represented by Python strs

def tag( obj ):
	if is_int( obj ):
		result = "INT"
	elif is_symbol( obj ):
		result = "SYMBOL"
	else: # All other Sheppard objects are represented by instances of Object
		result = obj._tag
	assert( result.isupper() )
	return result

def is_a( obj, t ):
	assert( t.isupper() )
	return tag( obj ) == t # TODO: inheritance

def popped( stack ):  return ( stack.head, stack.tail )

# Object constructors

null = Null()
def Parser( automaton ): return Object( "PARSER", state=automaton, stack=null )
def Cons( head, tail ): return Object( "LIST", head=head, tail=tail, _fields=['head','tail'] )
def List( items ):
	if items:
		return Cons( items[0], List( items[1:] ) )
	else:
		return null
def Stack( items ):
	if items:
		return Cons( items[-1], List( items[:-1] ) )
	else:
		return null

def Environment( outer, **bindings ): return Object( "ENVIRONMENT", outer=outer, bindings=Object("BINDINGS", **bindings) )

def Digression( tokens, environment, prev ): return Object( "DIGRESSION", tokens=tokens, environment=environment, prev=prev )

def Eof():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( "EOF", environment=Environment(null) )
	endless_eof = Cons( result, null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.prev   = result
	return result

eof = Eof()
false = Object( "FALSE" )
true  = Object( "TRUE" )
take_failed = Object( "TAKE_FAILED" )

def Thread( cursor, value_stack, state_stack ): return Object( "THREAD", cursor=cursor, value_stack=value_stack, state_stack=state_stack )

# Main execute procedure

def debug( message, *args ):
	if args:
		message = message % args
	print message

def python_list( sheppard_list, head="head", tail="tail" ):
	if sheppard_list:
		return [ sheppard_list[ head ] ] + python_list( sheppard_list[ tail ], head, tail )
	else:
		return []

def stack_str( stack ):
	return string.join([ repr(s) for s in python_list( stack )], ", " )

#
# The interpreter.
# Some rules to make it look more like Sheppard code:
#  - Procedures are only allowed one if statement sequence.  It must be at the
#  top level, and must use is_a.  This represents sheppard automaton-based dispatch.
#  - Loops will be replaced with tail digression.  There's only one loop anyway
#  so that's no big deal.
#

def pop( base, field ):
	result = base[field].head
	base[field] = base[field].tail
	return result

def finish_digression( th ):
	if is_a( th.cursor.tokens, "NULL" ):
		debug( "    finished %s", repr( th.cursor ) )
		th.cursor = th.cursor.prev

def bind_arg( formal_arg, actual_arg, arg_bindings ):
	if is_a( formal_arg, "SYMBOL" ):
		arg_bindings[ formal_arg ] = actual_arg
		debug( "    %s=%s", formal_arg, repr( actual_arg ) )
	else:
		debug( "    pop %s", repr( actual_arg ) )

def bind_args( th, formal_args, arg_bindings ):
	if is_a( formal_args, "NULL" ):
		return arg_bindings
	else:
		th.state_stack = th.state_stack.tail
		bind_arg( formal_args.head, pop( th, "value_stack" ), arg_bindings )
		return bind_args( th, formal_args.tail, arg_bindings )

def bound2( obj, environment, probe ):
	if is_a( probe, "TAKE_FAILED" ):
		return bound( obj, environment.outer )
	else:
		return probe

def bound( obj, environment ):
	if is_a( environment, "NULL" ):
		return obj
	else:
		#debug( "-- looking up %s in: %s", repr(obj), environment.bindings )
		return bound2( obj, environment, environment.bindings.take( obj, take_failed ) )

def next_state2( state, obj, probe ):
	if is_a( probe, "TAKE_FAILED" ): # Need to use TAKE_FAILED to get a short-circuit version of take.  If state[obj] exists and state[ tag(obj) ] does not, we can't evaluate the latter
		return state[ tag(obj) ]
	else:
		return probe

def next_state( state, obj ):
	# Note: this gives priority to SYMBOL over specific symbols, which might make keywords impossible.  We might want to rethink this.
	return next_state2( state, obj, state.take( obj, take_failed ) )

def do_action( th, action, environment ):
	if is_a( action, "PRIMITIVE" ):
		action.function( th, **dict( environment.bindings ) )
	else:
		th.cursor = Digression( action.script, environment, th.cursor )
		debug( "    new_digression: %s", repr( th.cursor.description() ) )

def perform_accept( th ):
	debug( "accept" )
	return false

def perform_shift( th ):
	debug( "shift" )
	debug( "  cursor: %s", repr( th.cursor ) )
	raw_token = th.cursor.tokens.take( "head", eof )
	th.cursor.tokens = th.cursor.tokens.tail
	debug( "  token: %s", repr( raw_token ) )
	current_token = bound( raw_token, th.cursor.environment )
	debug( "    value: %s", repr( current_token ) )
	th.value_stack = Cons( current_token, th.value_stack )
	new_state = next_state( th.state_stack.head, current_token )
	debug( "  new_state: %s", repr( new_state ) )
	th.state_stack = Cons( new_state, th.state_stack )
	return true

def perform_reduce0( th ):
	debug( "reduce0" )
	action = bound( th.state_stack.head.action, th.cursor.environment )
	debug( "  action: %s", repr( action ) )
	finish_digression( th )
	formal_args = action.formal_args
	environment = Environment( action.environment )
	bind_args( th, formal_args, environment.bindings )
	debug( "  environment: %s", environment )
	do_action( th, action, environment )
	return true

perform = {
	"ACCEPT":  perform_accept,
	"SHIFT":   perform_shift,
	"REDUCE0": perform_reduce0,
	}

def execute2( th, probe ):
	if is_a( probe, "TRUE" ):
		debug( "state_stack: %s", stack_str( th.state_stack ) )
		command = tag( th.state_stack.head )
		execute2( th, perform[ command ]( th ) )

def execute( procedure, environment ):
	digression = Digression( procedure.script, environment, eof )
	th = Thread( digression, null, Cons( procedure.dialect, null ) )
	debug( "starting thread: %s with digression:\n\t%s", repr(th), digression )
	execute2( th, true )

# PROBLEM: as I write this, perform_reduce contains a call to finish_digression.
# The motivation for this is tail digression elimination: we want to clean up an
# exhausted digression before starting a new one.  Really, since environments
# are for looking up the values bound to shifted tokens, this could be done
# even earlier, right after the last shift from the digression.  However, we
# also use the environment to look up the reduce actions, and this is where the
# problem comes from.  There could be multiple consectuve reduce actions (with
# no intervening shifts) at the end of a digression, and we may want to use
# that digression's environment to look up the actions to take.  Or, we might
# not want that.  The environment to use for binding reduce actions is, I
# think, ill-defined under the digression model.  That binding operation may
# need to use a separate mechanism entirely.
#
# In the mean time, we finish_digression right after looking up the reduce
# action.  That gets us tail digression elimination, and usually does the right
# thing.  It also makes no difference as long as all the action bindings are in
# the global_scope anyway.  Once I work out how to bind reduce actions, it
# would probably be better and cleaner to finish_digression as soon as that
# digression's last token has been bound.

def Shift( **edges ): return Object( "SHIFT", **edges )
def Reduce0( action ): return Object( "REDUCE0", action=action )
def Accept(): return Object( "ACCEPT" )
def Macro( script, formal_args, environment ): return Object( "MACRO", script=script, formal_args=formal_args, environment=environment )
def Procedure( script, dialect, environment ): return Object( "PROCEDURE", script=script, dialect=dialect, environment=environment )
def Primitive( function, formal_args, environment ): return Object( "PRIMITIVE", function=function, formal_args=formal_args, environment=environment )

#
# Testing
#

class Start_script:

	""" This just permits me to write some Sheppard code without enclosing every word in quotes
	"""

	def __init__( self ):
		self._tokens = []

	def __getattr__( self, key ):
		if key == "_tokens":
			return getattr( self, "_tokens" )
		else:
			self._tokens.append( key )
			return self

	def __getitem__( self, key ):
		return self.__getattr__( key )

	def End_script( self ):
		return List( self._tokens )

	def __call__( self, key ):
		# Without this syntactic sugar, programs are unbearably verbose
		return self["*"][key]

	def __str__( self ):
		return str( self._tokens )

	def __repr__( self ):
		return "Script_builder()" + string.join([ '.' + str(t) for t in self._tokens ])

if 0:
	x = Cons("first", null)
	x = Cons("second", x)

if 0:
	def primitive_hello( th, where ):
		print "!! Called primitive_hello( %s )" % where

	global_scope = Environment( null )
	bindings = global_scope.bindings
	bindings[ "A1" ] = Macro(
		formal_args = Stack([ null, "arg" ]),
		script = List([ "hello", "arg" ]),
		environment = global_scope
		)
	bindings[ "A2" ] = Primitive(
		formal_args= Stack([ null, "where" ]), # ignore the "go" keyword
		function = primitive_hello,
		environment = global_scope
		)
	dialect = Shift(
		go = Shift( SYMBOL = Reduce0( "A1" )),
		hello = Shift( SYMBOL = Reduce0( "A2" )),
		EOF = Accept())

	debug( "Global scope: %s", global_scope )
	debug( "  bindings: %s", global_scope.bindings )
	debug( "  dialect: %s", dialect.description() )
	execute( Procedure(
		Start_script().
			go.world.
		End_script(),
		dialect, global_scope ), Environment( global_scope ) )

if 0:
	scripts = {
		"pop": Start_script()
				("base")("field").get.head.get
			.frame.result.put
				("base")("field").get.tail.get
			("base")("field").put
			("result")
			.End_script(),
		"finish_digression": Start_script()
				("th").cursor.get.prev.get
			("th").cursor.put
			.End_script(),
		"bind_arg_NULL": Start_script()
			.End_script(),
		"bind_arg_SYMBOL": Start_script()
				("actual_arg")
			("arg_bindings")("formal_arg").put
			.End_script(),
		"bind_args_NULL": Start_script()
			("arg_bindings")
			.End_script(),
		"bind_args_LIST": Start_script()
				("th").state_stack.get.tail.get
			("th").state_stack.put
				("th")
				("formal_args").tail.get
					("th")
					.value_stack
				.pop
				("arg_bindings")
			.bind_arg
				("formal_args").tail.get
				("arg_bindings")
			.bind_args
			.End_script(),
		"bound2": Start_script()

			.End_script(),
		}
	print scripts

def parse_script( string ):
	# Start_script is still too cumbersome
	result = {}
	( name, script ) = ( "main", [] )
	for word in re.split( '[^a-zA-Z0-9_:]+', string.strip() ):
		print word
		if word[-1] == ':':
			result[ name ] = List( script )
			( name, script ) = ( word[:-1], [] )
		else:
			script.append( word )
	if script:
		result[ name ] = List( script )
	return result

post_order = parse_script("""
pop:
		base$ field$ get head get
	result setlocal
		base$ field$ get tail get
	base$ field$ set
	result$

finish_digression_NULL:
		th$ cursor get prev get
	th$ cursor set

finish_digression_OBJECT:

bind_arg_SYMBOL:
		$actual_arg
	arg_bindings$ $formal_arg set

bind_arg_NULL:

bind_args_NULL:
	arg_bindings$

bind_args_OBJECT:
		$th state_stack get tail get
	$th state_stack set
		$formal_args head get
		$th value_stack pop
		$arg_bindings
	bind_arg
		$th
		$formal_args tail get
		$arg_bindings
	bind_args

bound2_TAKE_FAILED:
		$obj
		$environment outer get
	bound

bound2_OBJECT:
	$probe

bound_NULL:
	$obj

bound_OBJECT:
		$obj
		$environment
			$environment bindings get
			$obj
			take_failed
		take
	bound2

next_state2_TAKE_FAILED:
	$state $obj tag get

next_state2_OBJECT:
	$probe

next_state:
		$state
		$obj
			$state
			$obj
			take_failed
		take
	next_state2

do_action_PRIMITIVE: REPLACE WITH NATIVE

do_action_MACRO:
			$action script get
			$environment
			$th cursor get
		Digression
	$th cursor set

perform_accept: FALSE

perform_shift:
			$th cursor get tokens get
			head
			Eof
		take
	raw_token setlocal
		$th cursor get tokens get tail get
	$th cursor get tokens set
			raw_token
			$th cursor get environment get
		bound
	current_token setlocal
			$current_token
			$th.value_stack
		Cons
	$th value_stack set
			$th state_stack get head get
			$current_token
		next_state
	new_state setlocal
			$new_state
			$th state_stack get
		Cons
	$th state_stack set
	TRUE

perform_reduce0:
			$th state_stack get head get action get
			$th cursor get environment get
		bound
	action setlocal
	$th finish_digression
		$action formal_args get
	formal_args setlocal
		$action environment get Environment
	environment setlocal
		$th
		$action
		$environment
	do_action
	TRUE

execute2_FALSE:
execute2_TRUE:
		$th state_stack get head get tag
	command setlocal
		$th
		$command $th perform
	execute2

execute:
		$procedure script get
		$environment
		Eof
	Digression digression setlocal
		$digression
		null
		$procedure dialect get null Cons
	Thread th setlocal
		$th
		TRUE
	execute2

""")

print post_order
