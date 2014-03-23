#! /usr/bin/python -O

import string, re, time
from sheppard_gen import generated_automaton
from sheppard_object import *
from itertools import islice
from sys import argv

def popped( stack ):  return ( stack.head, stack.tail )

# Object constructors

def LIST( head, tail ): return Object( 'LIST', head=head, tail=tail, _fields=['head','tail'] )
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

def list_length( arg ):
	if arg is null:
		return 0
	else:
		return 1 + list_length( arg.tail )

def ENVIRONMENT( outer, **bindings ): return Object( 'ENVIRONMENT', outer=outer, bindings=Object('BINDINGS', **bindings) )

def DIGRESSION( tokens, environment, resumption ): return Object( 'DIGRESSION', tokens=tokens, environment=environment, resumption=resumption )

def Eof(): return Object( 'EOF' )

def Nothing():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( 'NOTHING', environment=ENVIRONMENT(null) )
	endless_eof = LIST( Eof(), null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.resumption   = result
	return result

eof = Eof()
nothing = Nothing()
false = Object( 'FALSE' )
true  = Object( 'TRUE' )

def ACTIVATION( cursor, operands, history, scope, caller ): return Object( 'ACTIVATION', cursor=cursor, operands=operands, history=history, scope=scope, caller=caller )

def THREAD( activation, meta_thread ): return Object( 'THREAD', activation=activation, meta_thread=meta_thread )

# Main execute procedure

def debug( message, *args ):
	if args:
		message = message % args
	print message

def silence( message, *args ):
	pass

def error( exception ):
	debug( "!! ERROR: %s", exception )
	print_backtrace( current_thread )
	raise exception

def python_list( sheppard_list, head='head', tail='tail' ):
	if sheppard_list:
		return [ sheppard_list[ head ] ] + python_list( sheppard_list[ tail ], head, tail )
	else:
		return []

def stack_str( stack, sep=", " ):
	return string.join([ repr(s) for s in python_list( stack )], sep )

def list_str( lst, sep=", ", ellision_limit=999 ):
	pl = python_list( lst )
	prefix = ''
	if len( pl ) > ellision_limit:
		pl = pl[ : ellision_limit-2 ]
		prefix = "... "
	return prefix + string.join([ repr(s) for s in reversed( pl )], sep )

def meta_level( th ):
	if th.meta_thread is null:
		return 0
	else:
		return 1 + meta_level( th.meta_thread )

def tag_edge_symbol( obj ):
	return ':' + tag( obj )

# These functions operate in sharp-land, so they're safe to call directly from
# a Sheppard program without interfering with the automaton

give_failed = 'GIVE_FAILED'

def give( value_sharp, obj, key_sharp ):
	if is_symbol( key_sharp ):
		obj[ flat(key_sharp) ] = flat( value_sharp )
	else:
		return give_failed

take_failed = 'TAKE_FAILED'

def take( obj, key_sharp ):
	#debug( "--    %s.take( %s, %s )", repr(obj), repr(key_sharp), repr(default) )
	try:
		return sharp( obj[ flat( key_sharp ) ] )
	except (KeyError, TypeError):
		pass
	return take_failed

def make( tag_sharp ):
	return Object( flat( tag_sharp ) )

def cons( head_sharp, tail ):
	return LIST( flat( head_sharp ), tail )

debug_ellision_limit=999
printing_level_threshold=1

def cursor_description( cursor ):
	if cursor == nothing:
		return ''
	else:
		return string.join( [ repr(x) for x in python_list( cursor.tokens ) ], "  " ) + " . " + cursor_description( cursor.resumption )

def print_program( th ):
	if meta_level( th ) >= printing_level_threshold:
		act = th.activation
		debug( "# PROGRAM%d: %s ^ %s", meta_level(th), list_str( act.operands, "  ", debug_ellision_limit ), cursor_description( act.cursor ) )

def print_stuff( th ):
	if meta_level( th ) >= printing_level_threshold:
		act = th.activation
		#debug( "stack: %s", zip( python_list( act.history ), python_list( act.operands ) ) )
		print_program( th )
		debug( "|  history: %s", list_str( act.history, ':', debug_ellision_limit ) )
		debug( "|   cursor: %s", repr( act.cursor ) )
		debug( "|      env: %s %s", repr( act.cursor.environment ), act.cursor.environment.bindings )

def print_reduce_stuff( th, action, environment ):
	if meta_level( th ) >= printing_level_threshold:
		act = th.activation
		debug( ">+  ACTION: %s %s", action.name, action )
		debug( " |    with: %s %s", repr( environment ), environment.bindings )

def print_backtrace( th ):
	print_stuff( th )
	act = th.activation
	debug( "| backtrace:" )
	history  = python_list( act.history )
	operands = python_list( act.operands )
	for ( state, obj ) in zip( history, operands ):
		debug( "|   |      %s", state )
		debug( "|   | %s",      obj )
	debug( "|   |      %s", history[-1] )

#####################################
#
# The interpreter.
#
#
# Some rules to make it look more like Sheppard code:
#  - Procedures are only allowed one if statement sequence.  It must be at the
#  top level, and must use is_a based on the last argument or compare the last 
#  argument against a specific symbol.
# This represents sheppard automaton-based dispatch.
#  - Loops will be replaced with tail digression.  There's only one loop anyway
#  so that's no big deal.
#
#
# PROBLEM: I think we must generally deal with quoted values, because the moment
# an unquoted value appears on the lowest-level operand stack, it's possible an
# action can be taken.  Hence, values must always be manipulated in quoted form.

def pop_list( base, field_symbol_sharp ):
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	give( take( current, 'tail#' ), base, field_symbol_sharp )
	return result

def finish_digression( act, remaining_tokens ):
	if remaining_tokens is null: #is_a( remaining_tokens, 'NULL' ):
		#debug( "  (finished %s)", repr( act.cursor ) )
		act.cursor = act.cursor.resumption

def bind_arg( arg_value_sharp, arg_bindings, arg_symbol_sharp ):
	debug_bind = silence
	if arg_symbol_sharp is null: # is_a( arg_symbol_sharp, 'NULL' )
		debug_bind( "    pop %s", repr( arg_value_sharp ) )
	else:
		assert( is_a( arg_symbol_sharp, 'SYMBOL' ) )
		give( arg_value_sharp, arg_bindings, arg_symbol_sharp )
		debug_bind( "    %s=%s", arg_symbol_sharp, repr( arg_value_sharp ) )

def bind_args( act, arg_bindings, formal_args ):
	if formal_args is null: #is_a( formal_args, 'NULL' ):
		pass
	else:
		act.history = act.history.tail
		bind_arg( pop_list( act, 'operands#' ), arg_bindings, take( formal_args, 'head#' ) )
		bind_args( act, arg_bindings, formal_args.tail )

def bound2( obj_sharp, environment, probe ):
	if probe is take_failed:
		return bound( obj_sharp, environment.outer )
	else:
		return probe

def bound( obj_sharp, environment ):
	if environment is null: #is_a( environment, 'NULL' ):
		return obj_sharp
	else:
		#debug( "-- looking up %s in: %s", repr(obj_sharp), environment.bindings )
		return bound2( obj_sharp, environment, take( environment.bindings, obj_sharp ) )

def next_state2( state, obj_sharp, probe ):
	if probe is take_failed: # Need to use TAKE_FAILED to get a short-circuit version of take.  If state[obj_sharp] exists and state[ tag_edge_symbol(obj_sharp) ] does not, we can't evaluate the latter
		try:
			return state[ tag_edge_symbol(obj_sharp) ]
		except AttributeError:
			error( Unexpected_token( state, obj_sharp ) )
	else:
		return probe

def next_state( state, obj_sharp ):
	# Note: this gives priority to SYMBOL over specific symbols, which might make keywords impossible.  We might want to rethink this.
	return next_state2( state, obj_sharp, take( state, obj_sharp ) )

debug_do = silence

def do_action_primitive( act, environment, action ):
	debug_do( "  Primitive bindings: %s", dict( environment.bindings ) )
	action.function( act.thread, **dict( environment.bindings ) )

def do_action_macro( act, environment, action ):
	act.cursor = DIGRESSION( action.script, environment, act.cursor )
	debug_do( "    new_digression: %s", repr( act.cursor ) )
	finish_digression( act, act.cursor.tokens ) # Just in case the macro is totally empty

do_action = {
	'PRIMITIVE': do_action_primitive,
	'MACRO':     do_action_macro,
	}

def get_token( probe ):
	if probe is take_failed:
		return eof
	else:
		return probe

def perform_accept( act ):
	debug( 'accept' )
	return false

shift_count = 0
def perform_shift( act ):
	global shift_count
	shift_count += 1
	#debug_shift = silence
	#debug_shift( 'shift' )
	#if act.operands != null and act.operands.head in action_words:
	#	error( Missed_Action_Word( act.operands.head ) )
	#debug_shift( "  cursor: %s", repr( act.cursor ) )
	raw_token_sharp = get_token( take( act.cursor.tokens, 'head#' ) )
	act.cursor.tokens = act.cursor.tokens.tail
	#debug_shift( "  token: %s", repr( flat( raw_token_sharp ) ) )
	#debug_shift( "    environment: %s", act.cursor.environment )
	token_sharp = bound( raw_token_sharp, act.cursor.environment )
	finish_digression( act, act.cursor.tokens )
	#debug_shift( "    value: %s", repr( flat( token_sharp ) ) )
	act.operands = cons( token_sharp, act.operands )
	new_state = next_state( act.history.head, token_sharp )
	#debug_shift( "  new_state: %s", repr( new_state ) )
	act.history = cons( new_state, act.history )
	if list_length( act.operands ) > 50:
		error( RuntimeError( "Operand stack overflow" ) )
	return true

def perform_reduce0( act ):
	if meta_level( act.thread ) >= printing_level_threshold:
		debug_reduce = debug
	else:
		debug_reduce = silence
	print_stuff( act.thread )
	#debug_reduce( ">-- reduce0 %s --", act.history.head.action )
	action = bound( take( act.history.head, 'action#' ), act.scope ) # 'take' here just to get a sharp result
	#debug_reduce( "  action: %s", repr( action ) )
	#if is_a( action, 'MACRO' ):
	#	debug_reduce( "    %s", python_list( action.script ) )
	reduce_env = ENVIRONMENT( action.environment )
	reduce_env.digressor = act.cursor.environment  # Need this in order to make 'bind' a macro, or else I can't access the environment I'm trying to bind
	bind_args( act, reduce_env.bindings, action.formal_args )
	print_reduce_stuff( act.thread, action, reduce_env )
	#debug_reduce( "  environment: %s", reduce_env )
	#debug_reduce( "    based on: %s", act.cursor )
	do_action[ tag( action ) ]( act, reduce_env, action )
	print_program( act.thread )
	return true

perform = {
	'ACCEPT':  perform_accept,
	'SHIFT':   perform_shift,
	'REDUCE0': perform_reduce0,
	}

def execute2( act, probe ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while probe == true: #is_a( probe, 'TRUE' ):
		#print_stuff( act.thread )
		command = tag( act.history.head )
		#debug( "-__ execute2 __" )
		# if Python had tail call elimination, we could do this:
		# execute2( act, perform[ command ]( act ) )
		probe = perform[ command ]( act )

def execute( procedure, environment, scope ):
	act = ACTIVATION( DIGRESSION( procedure.script, environment, nothing ), null, LIST( procedure.dialect, null ), scope, null )
	global current_thread # Allow us to print debug info without passing this all over the place
	current_thread = THREAD( act, null )
	act.thread = current_thread # I don't love this back link, but it's really handy and efficient
	debug( "starting thread: %s with digression:\n\t%s", repr(current_thread), act.cursor )
	execute2( act, true )

#
#
#####################################


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
# but it's hampering my ability to do things like define 'bind' in terms of
# doing a 'put' on the caller's environment.  The trouble is: if the call to
# the unsanitary macro shifts the last token from the digression it's in, then
# that digression's environment disappears before the callee can get its hands
# on it.
#
# Maybe this is just how digressions roll.  Maybe I can live with that.

def MACRO( name, script, formal_args, environment ): return Object( 'MACRO', name=name, script=script, formal_args=formal_args, environment=environment )
def PROCEDURE( name, script, dialect, environment ): return Object( 'PROCEDURE', name=name, script=script, dialect=dialect, environment=environment )
def PRIMITIVE( name, function, formal_args, environment ): return Object( 'PRIMITIVE', name=name, function=function, formal_args=formal_args, environment=environment )
def Reduce0( action ): return Object( 'REDUCE0', action=action )
def Accept(): return Object( 'ACCEPT' )
def Shift( **edges ):
	# For convenience, we stick a colon on the front of each uppercase field name because that's probably what you want
	result = Object( 'SHIFT' )
	for ( name, value ) in edges.iteritems():
		if name.isupper():
			result[ ':'+name ] = value
		else:
			result[     name ] = value
	return result

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
		if key == '_tokens':
			return getattr( self, '_tokens' )
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
	x = LIST('first', null)
	x = LIST('second', x)

def go_world():
	def primitive_hello( th, where ):
		print "!! Called primitive_hello( %s )" % where

	global_scope = ENVIRONMENT( null )
	bindings = global_scope.bindings
	bindings[ "A1" ] = MACRO( "A1",
		formal_args = Stack([ null, 'arg' ]),
		script = List([ 'hello', 'arg' ]),
		environment = global_scope
		)
	bindings[ "A2" ] = PRIMITIVE( "A2",
		formal_args= Stack([ null, 'where' ]), # ignore the 'go' keyword
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

	return PROCEDURE( 'go_world',
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
	debug_parse = silence
	# Start_script is still too cumbersome
	result = Object('BINDINGS')
	( name, args, script ) = ( None, None, [] )
	def done( result, name, args, script ):
		if name != None:
			args.append( null ) # Automatically add a don't-care arg for the macro name
			result[ 'ACTION_' + name ] = MACRO( name, List( script ), Stack( args ), environment )
			debug_parse( "PARSED MACRO[ ACTION_%s ]: %s", name, name )
	for word in re.findall( r'\w+#*(?:/:?\w+#*)?|\([^)]*\)', string.strip() ):
		debug_parse( "WORD: '%s'", word )
		if word[0] == '(':
			done( result, name, args, script )
			name = None
			args = word[1:-1].split()
			script = []
		elif name == None:
			name = word
		else:
			script.append( word )
	done( result, name, args, script )
	return result

# Sheppard builtins

def define_builtins( bindings, global_scope ):
	debug_builtins = silence
	def digress( th, *values ):
		th.activation.cursor = DIGRESSION( List( values ), th.activation.cursor.environment, th.activation.cursor )

	def bind_with_name( func, name, *args ):
		bindings[ 'ACTION_' + name ] = PRIMITIVE( name, func, Stack( list( args ) ), global_scope )
		debug_builtins( "Binding primitive: %s %s", name, list(args) )

	# Cython doesn't support func_name
	#def bind( func, *args ):
	#	bind_with_name( func, func.func_name, *args )

	def eat( th ):
		pass
		#digress( th, 'STATEMENTS' )
	bind_with_name( eat, "eat0" )
	bind_with_name( eat, "eat1", null )
	bind_with_name( eat, "eat2", null, null )

	def begin( th ):
		# One day, this might start some kind of local scope.  But not today.
		# TODO: This could probably be a macro
		digress( th, 'BEGIN_MARKER' )
	bind_with_name( begin, 'begin', null )

	def compound_expr( th, result ):
		# TODO: This could probably be a macro
		digress( th, result )
	bind_with_name( compound_expr, 'compound_expr', null, null, null, 'result', null )

	def compound_stmt( th ):
		# TODO: This could probably be a macro
		pass
	bind_with_name( compound_stmt, 'compound_stmt', null, null, null )

	def current_environment( th ):
		digress( th, th.activation.cursor.environment )
	bind_with_name( current_environment, 'current_environment', null )

	def current_thread( th ):
		digress( th, th )
	bind_with_name( current_thread, 'current_thread', null )

	def _tag( th, obj ):
		digress( th, tag(obj) )
	bind_with_name( _tag, 'tag', 'obj', null )

	def _tag_edge_symbol( th, obj ):
		digress( th, tag_edge_symbol(obj) )
	bind_with_name( _tag_edge_symbol, 'tag_edge_symbol', 'obj', null )

	def get( th, base, field ):
		digress( th, base[ field ] )
	bind_with_name( get, 'get', 'base', 'field', null )

	def _give( th, value, base, field ):
		give( value, base, field )
		#digress( th, 'STATEMENT' )
	bind_with_name( _give, 'give', 'value', 'base', 'field', null )

	def _take( th, base, field ):
		digress( th, take( base, field ) )
	bind_with_name( _take, 'take', 'base', 'field', null )

	def put( th, value, base, field ):
		base[ field ] = value
		#digress( th, 'STATEMENT' )
	bind_with_name( put, 'put', 'value', 'base', 'field', null )

	def pop( th ):
		pass
	bind_with_name( pop, 'pop', null, null )

	def Null( th ):
		digress( th, null )
	bind_with_name( Null, 'Null', null )

	def Nothing( th ):
		digress( th, nothing )
	bind_with_name( Nothing, 'Nothing', null )

	def _cons( th, **args ):
		digress( th, cons(**args) )
	bind_with_name( _cons, 'cons', 'head_sharp', 'tail', null )

	def Procedure( th, **args ):
		digress( th, PROCEDURE( **args ) )
	bind_with_name( Procedure, 'Procedure', 'name', 'script', 'dialect', 'environment', null )

	def Digression( th, **args ):
		digress( th, DIGRESSION( **args ) )
	bind_with_name( Digression, 'Digression', 'tokens', 'environment', 'resumption', null )

	def Activation( th, **args ):
		digress( th, ACTIVATION( **args ) )
	bind_with_name( Activation, 'Activation', 'cursor', 'operands', 'history', 'scope', 'caller', null )

	def Thread( th, **args ):
		digress( th, THREAD( **args ) )
	bind_with_name( Thread, 'Thread', 'activation', 'meta_thread', null )

	def Environment( th, **args ):
		digress( th, ENVIRONMENT( **args ) )
	bind_with_name( Environment, 'Environment', 'outer', null )

	def do_primitive( th, th_arg, environment, action ):
		action.function( th_arg, **dict( environment.bindings ) )
	bind_with_name( do_primitive, 'do_primitive', 'th_arg', 'environment', 'action', null ) # Hmm, collision between th in the interpreter and th in the program

	def _print_stuff( th, th_arg ):
		print_stuff( th_arg )
		#digress( th, 'STATEMENT' )
	bind_with_name( _print_stuff, 'print_stuff', 'th_arg', null )

	def _print_reduce_stuff( th, th_arg, action, env ):
		print_reduce_stuff( th_arg, action, env )
		#digress( th, 'STATEMENT' )
	bind_with_name( _print_reduce_stuff, 'print_reduce_stuff', 'th_arg', 'action', 'env', null )

	def _print_program( th, th_arg ):
		print_program( th_arg )
		#digress( th, 'STATEMENT' )
	bind_with_name( _print_program, 'print_program', 'th_arg', null )

	def _sharp( th, **args ):
		digress( th, sharp( **args ) )
	bind_with_name( _sharp, 'sharp', 'arg' )

	def _flat( th, **args ):
		digress( th, flat( **args ) )
	bind_with_name( _flat, 'flat', 'arg' )

#####################################
#
# Meta-interpreter
#

global_scope = ENVIRONMENT( null )
bindings = parse_macros("""
( value symbol ) bind 
		value
		current_environment digressor get bindings get
	symbol put

( base field_symbol_sharp ) pop_list
		base field_symbol_sharp take
	current bind
		current head# take
	result bind
		current tail# take
	base field_symbol_sharp give
	result

( act remaining_tokens ) finish_digression/:NULL
		act cursor get resumption get
	act cursor put

( act remaining_tokens ) finish_digression/:LIST

( arg_value_sharp arg_bindings arg_symbol_sharp ) bind_arg/:SYMBOL
		arg_value_sharp
	arg_bindings arg_symbol_sharp give

( arg_value_sharp arg_bindings arg_symbol_sharp ) bind_arg/:NULL

( act arg_bindings formal_args ) bind_args/:NULL

( act arg_bindings formal_args ) bind_args/:LIST
		act history get tail get
	act history put
		act operands# pop_list
		arg_bindings
		formal_args head# take
	bind_arg
		act
		arg_bindings
		formal_args tail get
	bind_args

( obj_sharp environment probe ) bound2
	probe

( obj_sharp environment probe ) bound2/TAKE_FAILED
		obj_sharp
		environment outer get
	bound

( obj_sharp environment ) bound/:NULL
	obj_sharp

( obj_sharp environment ) bound/:ENVIRONMENT
		obj_sharp
		environment
			environment bindings get
			obj_sharp
		take
	bound2

( state obj_sharp probe ) next_state2/TAKE_FAILED
	state obj_sharp tag_edge_symbol get

( state obj_sharp probe ) next_state2
	probe

( state obj_sharp ) next_state
		state
		obj_sharp
			state
			obj_sharp
		take
	next_state2

( act environment action ) do_action/:PRIMITIVE
	act thread get environment action do_primitive

( act environment action ) do_action/:MACRO
			action script get
			environment
			act cursor get
		Digression
	act cursor put
		act
		act cursor get tokens get
	finish_digression

( act state ) perform/:ACCEPT
	false

( probe ) get_token/TAKE_FAILED
	eof

( probe ) get_token
	probe

( act state ) perform/:SHIFT
			act cursor get tokens get
			head#
		take get_token
	raw_token_sharp bind
		act cursor get tokens get tail get
	act cursor get tokens put
			raw_token_sharp
			act cursor get environment get
		bound
	token_sharp bind
		act
		act cursor get tokens get
	finish_digression
			token_sharp
			act operands get
		cons
	act operands put
			act history get head get
			token_sharp
		next_state
	new_state bind
			new_state
			act history get
		cons
	act history put
	true

( act state ) perform/:REDUCE0
	act thread get print_stuff
			act history get head get action# take
			act scope get
		bound
	action bind
		action environment get Environment
	reduce_env bind
		act cursor get environment get
	reduce_env digressor put
		act
		reduce_env bindings get
		action formal_args get
	bind_args
		act thread get
		action
		reduce_env
	print_reduce_stuff
		act
		reduce_env
		action
	do_action
	act thread get print_program
	true

( act probe ) execute2/:FALSE
	false

( act probe ) execute2/:TRUE
		act
			act
			act history get head get
		perform
	execute2

( procedure environment scope ) execute
			procedure script get
			environment
			nothing
		Digression
		null
		procedure dialect get null cons
		scope
		null
	Activation act bind
		act current_thread Thread
	act thread put
		act
		true
	execute2
""", global_scope )

def meta_automaton( action_bindings ):
	debug_ma = silence
	symbols = { 
		':ACCEPT', ':ACTIVATION', ':BINDINGS', ':BOOLEAN', ':DIGRESSION', ':ENVIRONMENT', ':EOF', ':FALSE', ':LIST', ':MACRO', ':NOTHING', ':NULL', ':OBJECT', ':PRIMITIVE', ':PROCEDURE', ':PROGRAM', ':SHIFT', ':STATE', ':SYMBOL', ':THREAD', ':TRUE',
		'ACCEPT', 'Activation', 'BEGIN_MARKER', 'cons', 'Digression', 'Environment', 'Procedure', 'SHIFT', 'STATEMENT', 'STATEMENTS', 'TAKE_FAILED',
		'Thread', 'begin', 'bind', 'bind_arg', 'bind_args', 'bound', 'bound2', 'compound_expr', 'compound_stmt', 'current_environment',
		'default', 'do_action', 'end', 'execute', 'finish_digression', 'get', 'next_state', 'perform', 'pop', 'pop_list', 'put',
		'return', 'set', 'tag', 'tag_edge_symbol', 'take',
		}
	dispatch_symbols = { ':FALSE', ':TRUE', ':NULL', ':SHIFT', ':ACCEPT', ':REDUCE0', 'TAKE_FAILED', ':ENVIRONMENT', ':LIST', ':SYMBOL', ':MACRO', ':PRIMITIVE', 'STATEMENT', 'STATEMENTS' }
	default_state = Shift()
	def shifty():
		while 1:
			yield Shift()
	dispatch_states = {}
	for symbol, state in zip( dispatch_symbols, shifty() ):
		dispatch_states[ symbol ] = state # dispatch_states[ x ] is the state we're in if the last item shifted was x
	debug_ma( "dispatch_states: %s", dispatch_states )
	for state in dispatch_states.values() + [ default_state ]:
		# The default shift action
		for symbol in symbols-dispatch_symbols:
			state[ symbol ] = default_state
		# Symbol-specific shift actions
		for symbol in dispatch_symbols:
			state[ symbol ] = dispatch_states[ symbol ]
		# Monomorphic reduce actions
		for ( symbol, action ) in action_bindings:
			#try:
			#	[ name, dispatch_symbol ] = action.name.split( '/' )
			#except ValueError: # No slash in the name
			#	[ name, dispatch_symbol ] = [ action.name, None ]
			if not '/' in action.name:
				name = action.name
				state[ name ] = Reduce0( symbol )
				debug_ma( 'Monomorphic: %s[ %s ] = Reduce0( %s )', repr(state), name, symbol )
	# Polymorphic reduce actions
	for ( symbol, action ) in action_bindings:
		try:
			[ name, dispatch_symbol ] = action.name.split( '/' )
			dispatch_states[ dispatch_symbol ][ name ] = Reduce0( symbol )
			debug_ma( 'Polymorphic: state[ %s ][ %s ] = Reduce0( %s ) # %s', dispatch_symbol, name, symbol, repr(dispatch_states[ dispatch_symbol ]) )
		except ValueError: # No slash in the name
			pass
	default_state[ ':EOF' ] = Accept()
	return default_state

#
#
#####################################

global_scope.bindings = bindings
#print "global_scope: " + str( global_scope )
define_builtins( global_scope.bindings, global_scope )
#print "  bindings: " + str( global_scope.bindings )
action_words = [ s[7:] for s in bindings._fields ]
#print "  action words: " + str( action_words )
#dialect = generated_automaton()
dialect = meta_automaton( bindings )
#print "  dialect:\n" + dialect.description()
print "\n#===================\n"

def wrap_procedure( inner_procedure ):
	nothing.environment = global_scope
	bindings = global_scope.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'nothing' ] = nothing
	outer_procedure = PROCEDURE( 'meta_' + inner_procedure.name, List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment, 'execute' ]), dialect, global_scope )
	return outer_procedure

def test( depth, plt ):
	global printing_level_threshold
	procedure = go_world()
	printing_level_threshold = plt
	for _ in range(depth):
		procedure = wrap_procedure( procedure )
	execute( procedure, ENVIRONMENT( procedure.environment ), procedure.environment )

def main():
	try:
		depth = int( argv[1] )
	except IndexError:
		depth = 0
	try:
		printing_level_threshold = int( argv[2] )
	except IndexError:
		printing_level_threshold = depth
	test( depth, printing_level_threshold )

def pretty_time( t ):
	if t < 1:
		return "%.2fms" % (t*1000)
	elif t < 60:
		return "%.2fs" % t
	elif t < 3600:
		return "%dm%ds" % ( t/60, t%60 )
	else:
		return "%dh%dm%ds" % ( t/3600, (t%3600)/60, t%60 )

start_time = time.time()
main()
elapsed_time = time.time() - start_time
print shift_count, "shifts in", pretty_time( elapsed_time )
