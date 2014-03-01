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

def Eof(): return Object( "EOF" )

def Nothing():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( "NOTHING", environment=ENVIRONMENT(null) )
	endless_eof = LIST( Eof(), null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.prev   = result
	return result

eof = Eof()
nothing = Nothing()
false = Object( "FALSE" )
true  = Object( "TRUE" )
take_failed = Object( "TAKE_FAILED" )

def CONTINUATION( cursor, operands, history, scope, caller ): return Object( "CONTINUATION", cursor=cursor, operands=operands, history=history, scope=scope, caller=caller )

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

def stack_str( stack, sep=", " ):
	return string.join([ repr(s) for s in python_list( stack )], sep )

def list_str( lst, sep=", " ):
	return string.join([ repr(s) for s in reversed( python_list( lst ) )], sep )

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
		debug( "  (finished %s)", repr( th.cursor ) )
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
		th.history = th.history.tail
		bind_arg( formal_args.head, pop( th, "operands" ), arg_bindings )
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
		debug( "  Primitive bindings: %s", dict( environment.bindings ) )
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
	debug( "    environment: %s", th.cursor.environment )
	current_token = bound( raw_token, th.cursor.environment )
	finish_digression( th )
	debug( "    value: %s", current_token )
	th.operands = LIST( current_token, th.operands )
	new_state = next_state( th.history.head, current_token )
	debug( "  new_state: %s", repr( new_state ) )
	th.history = LIST( new_state, th.history )
	return true

def print_stuff( th ):
	#debug( "stack: %s", zip( python_list( th.history ), python_list( th.operands ) ) )
	debug( "+ PROGRAM: %s ^ %s", list_str( th.operands, "  " ), cursor_description( th.cursor ) )
	debug( "| history: %s", list_str( th.history, ":" ) )
	debug( "|   scope: %s", repr( th.scope ) )

def perform_reduce0( th ):
	print_stuff( th )
	debug( ">-- reduce0 %s --", th.history.head.action )
	action = bound( th.history.head.action, th.scope )
	debug( "  action: %s", repr( action ) )
	if is_a( action, "MACRO" ):
		debug( "    %s", python_list( action.script ) )
	formal_args = action.formal_args
	#environment = ENVIRONMENT( action.environment )  # More appropriate for a procedure call than a macro
	environment = ENVIRONMENT( th.cursor.environment )  # Need this in order to make "put" a macro, or else I can't access the environment I'm trying to bind
	bind_args( th, formal_args, environment.bindings )
	debug( "  environment: %s", environment )
	debug( "    based on: %s", th.cursor )
	do_action( th, action, environment )
	return true

perform = {
	"ACCEPT":  perform_accept,
	"SHIFT":   perform_shift,
	"REDUCE0": perform_reduce0,
	}

def cursor_description( cursor ):
	if cursor == nothing:
		return ""
	else:
		return string.join( [ repr(x) for x in python_list( cursor.tokens ) ], "  " ) + " . " + cursor_description( cursor.prev )

def execute2( th, probe ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while is_a( probe, "TRUE" ):
		print_stuff( th )
		command = tag( th.history.head )
		debug( "-__ execute2 __" )
		#execute2( th, perform[ command ]( th ) )
		probe = perform[ command ]( th )

def execute( procedure, environment, scope ):
	th = CONTINUATION( DIGRESSION( procedure.script, environment, nothing ), null, LIST( procedure.dialect, null ), scope, "CONTINUATION" )
	debug( "starting thread: %s with digression:\n\t%s", repr(th), th.cursor )
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
#
# WORSE PROBLEM: what happens if the digressions ends without doing a reduce?
# Then we need to finish the digression, but if we're only doing that at reduce
# actions, then it never happens.  I really need to figure out how to bind
# reduce actions, and I don't think I can do it using digressions at all.
#
# UPDATE: I created a new environment, the reduce_environment, used for all
# reduces.  Now I can finish_digression after we shift the last token out of
# it.
#
# PROBLEM: I can't figure out how to write a digression that can reliably
# access the environment of its caller.  In other words, I'm having trouble
# making macros that are unsanitary.  I guess that's meant to be a good thing,
# but it's hampering my ability to do things like define "bind" in terms of
# doing a "put" on the caller's environment.  The trouble is: if the call to
# the unsanitary macro shifts the last token from the digression it's in, then
# that digression's environment disappears before the callee can get its hands
# on it.
#
# Maybe this is just how digressions roll.  Maybe I can live with that.

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
	execute( procedure, ENVIRONMENT( procedure.environment ), procedure.environment )

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
	for word in re.findall( r'\w+|[{}]|\$|\([^)]*\)', string.strip() ):
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
( value symbol ) bind 
		value
		scope bindings get
	symbol put

( base field ) pop
	after
			base field get head get
		result bind
			base field get tail get
		base field put
	return
		result
	end

( th remaining_tokens ) finish_digression_NULL
		th cursor get prev get
	th cursor put

( th remaining_tokens ) finish_digression_LIST

( formal_arg actual_arg arg_bindings ) bind_arg_SYMBOL
		actual_arg
	arg_bindings formal_arg put

( formal_arg actual_arg arg_bindings ) bind_arg_NULL

( th formal_args arg_bindings ) bind_args_NULL
	arg_bindings

( th formal_args arg_bindings ) bind_args_LIST
		th history get tail get
	th history put
		formal_args head get
		th operands pop
		arg_bindings
	bind_arg
		th
		formal_args tail get
		arg_bindings
	bind_args

( obj environment probe ) bound2_OBJECT
	probe

( obj environment probe ) bound2_TAKE_FAILED
		obj
		environment outer get
	bound

( obj environment ) bound_NULL
	obj

( obj environment ) bound_ENVIRONMENT
		obj
		environment
			environment bindings get
			obj
			take_failed
		take
	bound2

( state obj probe ) next_state2_TAKE_FAILED
	state obj tag get

( state obj probe ) next_state2_STATE
	probe

( state obj ) next_state
		state
		obj
			state
			obj
			take_failed
		take
	next_state2

( th action environment ) do_action_MACRO
			action script get
			environment
			th cursor get
		Digression
	th cursor put

( th state ) perform_ACCEPT
	false

( th state ) perform_SHIFT
	after
				th cursor get tokens get
				head
				Eof
			take
		raw_token bind
			th cursor get tokens get tail get
		th cursor get tokens put
				raw_token
				th cursor get environment get
			bound
		current_token bind
		th finish_digression
				current_token
				th.operands
			Cons
		th operands put
				th history get head get
				current_token
			next_state
		new_state bind
				new_state
				th history get
			Cons
		th history put
	return
		true
	end

( th state ) perform_REDUCE0
	after
				th history get head get action get
				th reduce_environment get
			bound
		action bind
			action formal_args get
		formal_args bind
			th cursor get environment get Environment
		actual_args bind
			th
			action
			actual_args
		do_action
	return
		true
	end

( th probe ) execute2_FALSE

( th probe ) execute2_TRUE
	after
			th history get head get tag
		command bind
	return
			th
			th command perform
		execute2
	end

( procedure environment lexical_scope ) execute
			procedure script get
			environment
			nothing
		Digression
		null
		procedure dialect get null Cons
		lexical_scope
		CONTINUATION
	Continuation th bind
			th
			true
		execute2
	pop
""", global_scope )

# Sheppard builtins

def define_builtins( bindings, global_scope ):
	def digress( th, *values ):
		th.cursor = DIGRESSION( List( values ), th.cursor.environment, th.cursor )

	def bind_with_name( func, name, *args ):
		bindings[ "ACTION_" + name ] = PRIMITIVE( func, Stack( list( args ) ), global_scope )
		debug( "Binding primitive: %s %s", name, list(args) )

	def bind( func, *args ):
		bind_with_name( func, func.func_name, *args )

	def eat( th ):
		digress( th, "STATEMENTS" )
	bind_with_name( eat, "eat0" )
	bind_with_name( eat, "eat1", null )
	bind_with_name( eat, "eat2", null, null )

	def scope( th ):
		digress( th, th.scope )
	bind( scope, null )

	def get_tag( th, obj ):
		digress( th, tag(obj) )
	bind_with_name( get_tag, "tag", 'obj', null )

	def get( th, base, field ):
		digress( th, base[ field ] )
	bind( get, 'base', 'field', null )

	def do_take( th, base, field, default ):
		digress( th, take( base, field, default ) )
	bind_with_name( do_take, "take", 'base', 'field', 'default', null )

	def put( th, value, base, field ):
		base[ field ] = value
		digress( th, "STATEMENT" )
	bind( put, 'value', 'base', 'field', null )

	def pop( th ):
		pass
	bind( pop, null, null )

	def Null( th ):
		digress( th, null )
	bind( Null, null )

	def Eof( th ):
		digress( th, eof )
	bind( Eof, null )

	def Nothing( th ):
		digress( th, nothing )
	bind( Nothing, null )

	def Cons( th, **args ):
		digress( th, LIST(**args) )
	bind( Cons, 'head', 'tail', null )

	def Procedure( th, **args ):
		digress( th, PROCEDURE( **args ) )
	bind( Procedure, 'script', 'dialect', 'environment', null )

	def Digression( th, **args ):
		digress( th, DIGRESSION( **args ) )
	bind( Digression, 'tokens', 'environment', 'prev', null )

	def Continuation( th, **args ):
		digress( th, CONTINUATION( **args ) )
	bind( Continuation, 'cursor', 'operands', 'history', 'scope', 'caller', null )

	def Environment( th, **args ):
		digress( th, ENVIRONMENT( **args ) )
	bind( Environment, 'outer', null )

	def do_action_PRIMITIVE( th, **args ):
		print "Huh?? What do I do for do_action_PRIMITIVE?"
		digress( th, "STATEMENT" )
	bind( do_action_PRIMITIVE, 'th', 'action', 'environment', null )


global_scope.bindings = bindings
print "global_scope: " + str( global_scope )
define_builtins( global_scope.bindings, global_scope )
print "  bindings: " + str( global_scope.bindings )
dialect = generated_automaton()
#print "  dialect:\n" + dialect.description()
print "\n===================\n"

if 1:
	inner_procedure = go_world()
	nothing.environment = global_scope
	bindings = global_scope.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'nothing' ] = nothing
	# Older stuff
	bindings[ 'Null' ] = null
	bindings[ 'Eof' ] = eof
	bindings[ 'False' ] = false
	bindings[ 'True' ] = true
	bindings[ 'Nothing' ] = nothing
	outer_procedure = PROCEDURE( List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment, "execute" ]), dialect, global_scope )
	execute( outer_procedure, ENVIRONMENT( global_scope ), global_scope )

