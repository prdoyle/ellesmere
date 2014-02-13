#! /usr/bin/python

import string, re
from sheppard_gen import generated_automaton
from sheppard_object import *

def popped( stack ):  return ( stack.head, stack.tail )

# Object constructors

def LIST( head, tail ): return Object( "LIST", head=head, tail=tail, _fields=['head','tail'] )
def List( items ):
	if items:
		return LIST( items[0], List( items[1:] ) )
	else:
		return null
def Stack( items ):
	if items:
		return LIST( items[-1], Stack( items[:-1] ) )
	else:
		return null

def ENVIRONMENT( outer, **bindings ): return Object( "ENVIRONMENT", outer=outer, bindings=Object("BINDINGS", **bindings) )

def DIGRESSION( tokens, environment, prev ): return Object( "DIGRESSION", tokens=tokens, environment=environment, prev=prev )

def Eof():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( "EOF", environment=ENVIRONMENT(null) )
	endless_eof = LIST( result, null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.prev   = result
	return result

eof = Eof()
false = Object( "FALSE" )
true  = Object( "TRUE" )
take_failed = Object( "TAKE_FAILED" )

def THREAD( cursor, value_stack, state_stack ): return Object( "THREAD", cursor=cursor, value_stack=value_stack, state_stack=state_stack )

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

def take( obj, key, default ):
	#debug( "--    %s.take( %s, %s )", repr(obj), repr(key), repr(default) )
	if is_symbol( key ) and key in obj:
		return obj[ key ]
	else:
		return default

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
		return bound2( obj, environment, take( environment.bindings, obj, take_failed ) )

def next_state2( state, obj, probe ):
	if is_a( probe, "TAKE_FAILED" ): # Need to use TAKE_FAILED to get a short-circuit version of take.  If state[obj] exists and state[ tag(obj) ] does not, we can't evaluate the latter
		return state[ tag(obj) ]
	else:
		return probe

def next_state( state, obj ):
	# Note: this gives priority to SYMBOL over specific symbols, which might make keywords impossible.  We might want to rethink this.
	return next_state2( state, obj, take( state, obj, take_failed ) )

def do_action( th, action, environment ):
	if is_a( action, "PRIMITIVE" ):
		action.function( th, **dict( environment.bindings ) )
	else:
		th.cursor = DIGRESSION( action.script, environment, th.cursor )
		debug( "    new_digression: %s", repr( th.cursor ) )

def perform_accept( th ):
	debug( "accept" )
	return false

def perform_shift( th ):
	debug( "shift" )
	debug( "  cursor: %s", repr( th.cursor ) )
	raw_token = take( th.cursor.tokens, "head", eof )
	th.cursor.tokens = th.cursor.tokens.tail
	debug( "  token: %s", repr( raw_token ) )
	current_token = bound( raw_token, th.cursor.environment )
	debug( "    value: %s", repr( current_token ) )
	th.value_stack = LIST( current_token, th.value_stack )
	new_state = next_state( th.state_stack.head, current_token )
	debug( "  new_state: %s", repr( new_state ) )
	th.state_stack = LIST( new_state, th.state_stack )
	return true

def perform_reduce0( th ):
	debug( "reduce0" )
	action = bound( th.state_stack.head.action, th.cursor.environment )
	debug( "  action: %s", repr( action ) )
	finish_digression( th )
	formal_args = action.formal_args
	environment = ENVIRONMENT( action.environment )
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
		debug( "value_stack: %s", stack_str( th.value_stack ) )
		command = tag( th.state_stack.head )
		execute2( th, perform[ command ]( th ) )

def execute( procedure, environment ):
	digression = DIGRESSION( procedure.script, environment, eof )
	th = THREAD( digression, null, LIST( procedure.dialect, null ) )
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
def MACRO( script, formal_args, environment ): return Object( "MACRO", script=script, formal_args=formal_args, environment=environment )
def PROCEDURE( script, dialect, environment ): return Object( "PROCEDURE", script=script, dialect=dialect, environment=environment )
def PRIMITIVE( function, formal_args, environment ): return Object( "PRIMITIVE", function=function, formal_args=formal_args, environment=environment )

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
	x = LIST("first", null)
	x = LIST("second", x)

def go_world():
	def primitive_hello( th, where ):
		print "!! Called primitive_hello( %s )" % where

	global_scope = ENVIRONMENT( null )
	bindings = global_scope.bindings
	bindings[ "A1" ] = MACRO(
		formal_args = Stack([ null, "arg" ]),
		script = List([ "hello", "arg" ]),
		environment = global_scope
		)
	bindings[ "A2" ] = PRIMITIVE(
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

	return PROCEDURE(
		Start_script().
			go.world.
		End_script(),
		dialect, global_scope )

if 0:
	procedure = go_world()
	execute( procedure, ENVIRONMENT( procedure.environment ) )

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

if 0:
	for ( symbol, binding ) in sorted( global_scope.bindings ):
		print "%s: %s" % ( symbol, binding )
		#global_scope[ symbol ] = MACRO

def parse_macros( string, environment ):
	# Start_script is still too cumbersome
	result = Object("BINDINGS")
	( name, args, script ) = ( None, None, [] )
	def done( result, name, args, script ):
		if name != None:
			args.append( null ) # Automatically add a don't-care arg for the macro name
			result[ name ] = MACRO( List( script ), Stack( args ), environment )
	for word in re.findall( r'\w+|\$|\([^)]*\)', string.strip() ):
		if word[0] == '(':
			done( result, name, args, script )
			name = None
			args = word[1:-1].split()
			script = []
		elif name == None:
			name = "ACTION_" + word
		else:
			script.append( word )
	done( result, name, args, script )
	return result

global_scope = ENVIRONMENT( null )
bindings = parse_macros("""
( symbol ) $
		frame
		frame symbol get
	get

( value symbol ) putlocal 
	value frame symbol put

( base field ) pop
		base field get head get
	result putlocal
		base field get tail get
	base field put
	result

( th ) finish_digression_NULL
		th cursor get prev get
	th cursor put

( th ) finish_digression_OBJECT

( formal_arg actual_arg arg_bindings ) bind_arg_SYMBOL
		actual_arg
	arg_bindings formal_arg put

( formal_arg actual_arg arg_bindings ) bind_arg_NULL

( th formal_args arg_bindings ) bind_args_NULL
	arg_bindings

( th formal_args arg_bindings ) bind_args_OBJECT
		th state_stack get tail get
	th state_stack put
		formal_args head get
		th value_stack pop
		arg_bindings
	bind_arg
		th
		formal_args tail get
		arg_bindings
	bind_args

( obj environment probe ) bound2_TAKE_FAILED
		obj
		environment outer get
	bound

( obj environment probe ) bound2_OBJECT
	probe

( obj environment ) bound_NULL
	obj

( obj environment ) bound_OBJECT
		obj
		environment
			environment bindings get
			obj
			take_failed
		take
	bound2

( state obj probe ) next_state2_TAKE_FAILED
	state obj tag get

( state obj probe ) next_state2_OBJECT
	probe

( state obj ) next_state
		state
		obj
			state
			obj
			take_failed
		take
	next_state2

( th action environment ) do_action_PRIMITIVE: REPLACE WITH NATIV

( th action environment ) do_action_MACRO
			action script get
			environment
			th cursor get
		Digression
	th cursor put

( th ) perform_ACCEPT
	FALSE

( th ) perform_SHIFT
			th cursor get tokens get
			head
			Eof
		take
	raw_token putlocal
		th cursor get tokens get tail get
	th cursor get tokens put
			raw_token
			th cursor get environment get
		bound
	current_token putlocal
			current_token
			th.value_stack
		Cons
	th value_stack put
			th state_stack get head get
			current_token
		next_state
	new_state putlocal
			new_state
			th state_stack get
		Cons
	th state_stack put
	TRUE

( th ) perform_REDUCE0
			th state_stack get head get action get
			th cursor get environment get
		bound
	action putlocal
	th finish_digression
		action formal_args get
	formal_args putlocal
		action environment get Environment
	environment putlocal
		th
		action
		environment
	do_action
	TRUE

( th probe ) execute2_FALSE

( th probe ) execute2_TRUE
		th state_stack get head get tag
	command putlocal
		th
		command th perform
	execute2

( procedure environment ) execute
		procedure script get
		environment
		Eof
	Digression digression putlocal
		digression
		Null
		procedure dialect get Null Cons
	Thread th putlocal
		th
		TRUE
	execute2

""", global_scope )

# Sheppard builtins

def define_builtins( bindings, global_scope ):
	def digress( th, *values ):
		th.cursor = DIGRESSION( List( values ), th.cursor.environment, th.cursor )

	def bind( func, *args ):
		bindings[ "ACTION_" + func.func_name ] = PRIMITIVE( func, Stack( list( args ) + [null] ), global_scope )

	def eat1( th ): pass
	bind( eat1 )

	def eat2( th ): pass
	bind( eat2 )

	def frame( th ):
		digress( th, th.cursor.environment )
	bind( frame )

	def get( th, base, field ):
		digress( th, base[ field ] )
	bind( get, 'base', 'field' )

	def take( th, base, field, default ):
		digress( th, take( base, field, default ) )
	bind( take, 'base', 'field', 'default' )

	def put( th, value, base, field ):
		base, [ field ] = value
	bind( put, 'value', 'base', 'field' )

	def Null( th ): return null
	bind( Null )

	def Eof( th ): return eof
	bind( Eof )

	def Cons( th, **args ):
		digress( th, LIST(**args) )
	bind( Cons, 'head', 'tail' )

	def Procedure( th, **args ):
		digress( th, PROCEDURE( **args ) )
	bind( Procedure, 'script', 'dialect', 'environment' )

	def Digression( th, **args ):
		digress( th, DIGRESSION( **args ) )
	bind( Digression, 'tokens', 'environment', 'prev' )

	def Thread( th, **args ):
		digress( th, THREAD( **args ) )
	bind( Thread, 'cursor', 'value_stack', 'state_stack' )

global_scope.bindings = bindings
print "global_scope: " + str( global_scope )
print "  bindings: " + str( global_scope.bindings )
define_builtins( global_scope.bindings, global_scope )

if 1:
	inner_procedure = go_world()
	outer_procedure = PROCEDURE( List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), "execute" ]), generated_automaton(), global_scope )
	execute( outer_procedure, ENVIRONMENT( global_scope ) )
