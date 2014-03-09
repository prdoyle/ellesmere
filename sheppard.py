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

def DIGRESSION( tokens, environment, resumption ): return Object( "DIGRESSION", tokens=tokens, environment=environment, resumption=resumption )

def Eof(): return Object( "EOF" )

def Nothing():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( "NOTHING", environment=ENVIRONMENT(null) )
	endless_eof = LIST( Eof(), null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.resumption   = result
	return result

eof = Eof()
nothing = Nothing()
false = Object( "FALSE" )
true  = Object( "TRUE" )
take_failed = Object( "TAKE_FAILED" )

def ACTIVATION( cursor, operands, history, scope, caller ): return Object( "ACTIVATION", cursor=cursor, operands=operands, history=history, scope=scope, caller=caller )

def THREAD( activation ): return Object( "THREAD", activation=activation )

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

def list_str( lst, sep=", ", ellision_limit=999 ):
	pl = python_list( lst )
	prefix = ""
	if len( pl ) > ellision_limit:
		pl = pl[ : ellision_limit-2 ]
		prefix = "... "
	return prefix + string.join([ repr(s) for s in reversed( pl )], sep )

def take( obj, key, default ):
	#debug( "--    %s.take( %s, %s )", repr(obj), repr(key), repr(default) )
	if is_symbol( key ) and key in obj:
		return obj[ key ]
	else:
		return default

#
# The interpreter.
# Some rules to make it look more like Sheppard code:
#  - Procedures are only allowed one if statement sequence.  It must be at the
#  top level, and must use is_a based on an argument.  This represents sheppard
#  automaton-based dispatch.
#  - Loops will be replaced with tail digression.  There's only one loop anyway
#  so that's no big deal.
#

def pop_list( base, field ):
	result = base[field].head
	base[field] = base[field].tail
	return result

def finish_digression( th, remaining_tokens ):
	if is_a( remaining_tokens, "NULL" ):
		act = th.activation
		debug( "  (finished %s)", repr( act.cursor ) )
		act.cursor = act.cursor.resumption

def bind_arg( formal_arg, actual_arg, arg_bindings ):
	if is_a( formal_arg, "SYMBOL" ):
		arg_bindings[ formal_arg ] = actual_arg
		debug( "    %s=%s", formal_arg, repr( actual_arg ) )
	else:
		debug( "    pop %s", repr( actual_arg ) )

def bind_args( th, formal_args, arg_bindings ):
	if is_a( formal_args, "NULL" ):
		pass
	else:
		act = th.activation
		act.history = act.history.tail
		bind_arg( formal_args.head, pop_list( th.activation, "operands" ), arg_bindings )
		bind_args( th, formal_args.tail, arg_bindings )

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
		try:
			return state[ tag(obj) ]
		except AttributeError:
			e = Unexpected_token( state, obj )
			debug( "ERROR: %s", e )
			print_stuff( th )
			raise e
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
		act = th.activation
		act.cursor = DIGRESSION( action.script, environment, act.cursor )
		debug( "    new_digression: %s", repr( act.cursor ) )
		finish_digression( th, act.cursor.tokens ) # Just in case the macro is totally empty

def perform_accept( th ):
	debug( "accept" )
	return false

def perform_shift( th ):
	#debug( "shift" )
	act = th.activation
	if act.operands != null and act.operands.head in action_words:
		e = Missed_Action_Word( act.operands.head )
		debug( "ERROR: %s", e )
		print_stuff( th )
		raise e
	#debug( "  cursor: %s", repr( act.cursor ) )
	raw_token = take( act.cursor.tokens, "head", eof )
	act.cursor.tokens = act.cursor.tokens.tail
	#debug( "  token: %s", repr( raw_token ) )
	#debug( "    environment: %s", act.cursor.environment )
	current_token = bound( raw_token, act.cursor.environment )
	finish_digression( th, act.cursor.tokens )
	#debug( "    value: %s", current_token )
	act.operands = LIST( current_token, act.operands )
	new_state = next_state( act.history.head, current_token )
	#debug( "  new_state: %s", repr( new_state ) )
	act.history = LIST( new_state, act.history )
	return true

debug_ellision_limit=999

def print_program( th ):
	act = th.activation
	debug( "+ PROGRAM: %s ^ %s", list_str( act.operands, "  ", debug_ellision_limit ), cursor_description( act.cursor ) )

def print_stuff( th ):
	act = th.activation
	#debug( "stack: %s", zip( python_list( act.history ), python_list( act.operands ) ) )
	print_program( th )
	debug( "| history: %s", list_str( act.history, ":", debug_ellision_limit ) )
	debug( "|  cursor: %s", repr( act.cursor ) )
	debug( "|     env: %s", repr( act.cursor.environment ) )

def perform_reduce0( th ):
	print_stuff( th )
	act = th.activation
	debug( ">-- reduce0 %s --", act.history.head.action )
	action = bound( act.history.head.action, act.scope )
	debug( "  action: %s", repr( action ) )
	if is_a( action, "MACRO" ):
		debug( "    %s", python_list( action.script ) )
	reduce_env = ENVIRONMENT( action.environment )
	reduce_env.digressor = act.cursor.environment  # Need this in order to make "bind" a macro, or else I can't access the environment I'm trying to bind
	bind_args( th, action.formal_args, reduce_env.bindings )
	debug( "  environment: %s", reduce_env )
	debug( "    based on: %s", act.cursor )
	do_action( th, action, reduce_env )
	print_program( th )
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
		return string.join( [ repr(x) for x in python_list( cursor.tokens ) ], "  " ) + " . " + cursor_description( cursor.resumption )

def execute2( th, probe ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while is_a( probe, "TRUE" ):
		#print_stuff( th )
		command = tag( th.activation.history.head )
		#debug( "-__ execute2 __" )
		# if Python had tail call elimination, we could do this:
		# execute2( th, perform[ command ]( th ) )
		probe = perform[ command ]( th )

def execute( procedure, environment, scope ):
	global th # Allow us to print debug info without passing this all over the place
	th = THREAD( ACTIVATION( DIGRESSION( procedure.script, environment, nothing ), null, LIST( procedure.dialect, null ), scope, "ACTIVATION" ) )
	debug( "starting thread: %s with digression:\n\t%s", repr(th), th.activation.cursor )
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

class Missed_Action_Word( BaseException ):

	def __init__( self, word ):
		self._word = word

	def __str__( self ):
		return "Missed action word: " + repr( self._word )

class Unexpected_token( BaseException ):

	def __init__( self, state, token ):
		self._state = state
		self._token = token

	def __str__( self ):
		return "Unexpected token %s in state %s" % ( self._token, self._state )

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

# Sheppard builtins

def define_builtins( bindings, global_scope ):
	def digress( th, *values ):
		th.activation.cursor = DIGRESSION( List( values ), th.activation.cursor.environment, th.activation.cursor )

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

	def begin( th ):
		# One day, this might start some kind of local scope.  But not today.
		# TODO: This could probably be a macro
		digress( th, "BEGIN_MARKER" )
	bind( begin, null )

	def compound_expr( th, result ):
		# TODO: This could probably be a macro
		digress( th, result )
	bind( compound_expr, null, null, null, 'result', null )

	def compound_stmt( th ):
		# TODO: This could probably be a macro
		pass
	bind( compound_stmt, null, null, null )

	def current_environment( th ):
		digress( th, th.activation.cursor.environment )
	bind( current_environment, null )

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
	bind( Digression, 'tokens', 'environment', 'resumption', null )

	def Activation( th, **args ):
		digress( th, ACTIVATION( **args ) )
	bind( Activation, 'cursor', 'operands', 'history', 'scope', 'caller', null )

	def Thread( th, **args ):
		digress( th, THREAD( **args ) )
	bind( Thread, 'activation', null )

	def Environment( th, **args ):
		digress( th, ENVIRONMENT( **args ) )
	bind( Environment, 'outer', null )

	def do_action_PRIMITIVE( th, **args ):
		print "Huh?? What do I do for do_action_PRIMITIVE?"
		raise BaseException
		digress( th, "STATEMENT" )
	bind( do_action_PRIMITIVE, 'th_arg', 'action', 'environment', null ) # Hmm, collision between th in the interpreter and th in the program

# Meta-interpreter

global_scope = ENVIRONMENT( null )
bindings = parse_macros("""
( value symbol ) bind 
		value
		current_environment digressor get bindings get
	symbol put

( base field ) pop_list
	begin
			base field get head get
		result bind
			base field get tail get
		base field put
	return
		result
	end

( th remaining_tokens ) finish_digression_NULL
		th activation get
	act bind
		act cursor get resumption get
	act cursor put

( th remaining_tokens ) finish_digression_LIST

( formal_arg actual_arg arg_bindings ) bind_arg_SYMBOL
		actual_arg
	arg_bindings formal_arg put

( formal_arg actual_arg arg_bindings ) bind_arg_NULL

( th formal_args arg_bindings ) bind_args_NULL

( th formal_args arg_bindings ) bind_args_LIST
		th activation get
	act bind
		act history get tail get
	act history put
		formal_args head get
		act operands pop_list
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
			TAKE_FAILED
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
			TAKE_FAILED
		take
	next_state2

( th action environment ) do_action_MACRO
		th activation get
	act bind
			action script get
			environment
			act cursor get
		Digression
	act cursor put
		th
		act cursor get tokens get
	finish_digression

( th state ) perform_ACCEPT
	false

( th state ) perform_SHIFT
	begin
			th activation get
		act bind
				act cursor get tokens get
				head
				Eof
			take
		raw_token bind
			act cursor get tokens get tail get
		act cursor get tokens put
				raw_token
				act cursor get environment get
			bound
		current_token bind
			th
			act cursor get tokens get
		finish_digression
				current_token
				act operands get
			Cons
		act operands put
				act history get head get
				current_token
			next_state
		new_state bind
				new_state
				act history get
			Cons
		act history put
	return
		true
	end

( th state ) perform_REDUCE0
	begin
			th activation get
		act bind
				act history get head get action get
				act scope get
			bound
		action bind
			action environment get Environment
		reduce_env bind
			act cursor get environment get
		reduce_env digressor put
			th
			action formal_args get
			reduce_env bindings get
		bind_args
			th
			action
			reduce_env
		do_action
	return
		true
	end

( th probe ) execute2_FALSE
	false

( th probe ) execute2_TRUE
	begin
			th activation get history get head get tag
		command bind
	return
			th
			th command perform
		execute2
	end

( procedure environment scope ) execute
			procedure script get
			environment
			nothing
		Digression
		null
		procedure dialect get null Cons
		scope
		ACTIVATION
	Activation Thread th bind
			th
			true
		execute2
	pop
""", global_scope )


global_scope.bindings = bindings
print "global_scope: " + str( global_scope )
define_builtins( global_scope.bindings, global_scope )
print "  bindings: " + str( global_scope.bindings )
action_words = [ s[7:] for s in bindings._fields ]
print "  action words: " + str( action_words )
dialect = generated_automaton()
#print "  dialect:\n" + dialect.description()
print "\n===================\n"

def wrap_procedure( inner_procedure ):
	nothing.environment = global_scope
	bindings = global_scope.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'nothing' ] = nothing
	outer_procedure = PROCEDURE( List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment, "execute" ]), dialect, global_scope )
	return outer_procedure

if 1:
	procedure = go_world()
	execute( procedure, ENVIRONMENT( procedure.environment ), procedure.environment )
	procedure = wrap_procedure( go_world() )
	execute( procedure, ENVIRONMENT( procedure.environment ), procedure.environment )

