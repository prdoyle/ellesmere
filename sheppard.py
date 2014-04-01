#! /usr/bin/python -O

import string, re, time
from itertools import islice
from sys import argv

object_counter = 0

class Object:

	def __init__( self, tag, **edges ):
		global object_counter
		assert( tag.isupper() )
		self._tag = tag
		self._elements = {}
		self._fields = sorted([ k for k in edges ]) # _fields can be adjusted if we want a particular field ordering
		self._id = object_counter
		object_counter = object_counter+1
		for ( name, value ) in edges.iteritems():
			setattr( self, name, value )

	# Allow map syntax for convenience

	def __getitem__( self, key ):
		try:
			return getattr( self, key )
		except AttributeError:
			return self._elements[ key ]

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
		try:
			return self._name
		except AttributeError:
			pass
		if self is null:
			return "null"
#		elif is_a( self, "ENVIRONMENT" ):
#			return "%s_%d{ %s }" % ( self._tag, self._id, repr( self.outer ) )
		else:
			return "%s_%d" % ( self._tag, self._id )

	def __str__( self ):
		if self is null:
			return "null"
		else:
			return repr( self ) + "{ " + string.join([ "%s:%s" % ( field, repr( self[field] ) ) for field in self._fields ], ', ') + " }"

	def description( self, already_described=None, indent=1 ):
		if already_described is None:
			already_described = set()
		if self is null:
			return "null"
		elif self in already_described:
			return repr( self )
		else:
			already_described.add( self )
			indent_str = " |" * indent
			return repr( self ) + "{" + string.join([ "\n%s%s:%s" % ( indent_str, field, self._description( self[ field ], already_described, indent+1 ) ) for field in self._fields ], ',' ) + " }"

	def _description( self, value, already_described, indent ):
		if isinstance( value, Object ):
			return value.description( already_described, indent )
		else:
			return str( value )

	# null and zero are false; all else are true

	def __nonzero__( self ): return self != 0 and self != null

class Null( Object ): # Just to make debugging messages more informative

	def __init__( self ):
		Object.__init__( self, "NULL" )

null = Null()

# Generally, these can't be methods, because some Python objects like symbols are
# represented by plain old Python objects like strings that have no such methods.
# Also, methods of Object can confuse __getattr__ and end up trying to call Sheppard objects.

def is_int( obj ):    return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_symbol( obj ): return isinstance( obj, str ) # Sheppard symbols are represented by Python strs

def tag( obj ):
	try:
		return obj._tag
	except AttributeError:
		if is_symbol( obj ):
			result = 'SYMBOL'
		elif is_int( obj ):
			result = "INT"
		else: # All other Sheppard objects are represented by instances of Object
			result = obj._tag
	assert( result.isupper() )
	return result

def is_a( obj, t ):
	assert( t.isupper() )
	return tag( obj ) == t # TODO: inheritance

def sharp( arg ):
	if isinstance( arg, str ): #if is_a( arg, 'SYMBOL' ):
		return arg + '#'
	else:
		return arg

def flat( arg ):
	if isinstance( arg, str ): #if is_a( arg, 'SYMBOL' ):
		assert( arg[-1] == '#' )
		return arg[:-1]
	else:
		return arg


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

eof = Object( 'EOF' )

def Nothing():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( 'NOTHING', environment=ENVIRONMENT(null) )
	endless_eof = LIST( eof, null )
	endless_eof.tail = endless_eof
	result.tokens = endless_eof
	result.resumption   = result
	return result

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
		debug( ">+  ACTION: ( %s ) %s %s", list_str( action.formal_args ), action.name, action )
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

def pop_list( base, field_symbol_sharp ):
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	#debug( "!!! pop_list( %s, %s ) = %s", base, field_symbol_sharp, result )
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
	# First we check of the object is itself a symbol that has an edge from this
	# state (ie. it's acting as a keyword).  Failing that, we check the object's
	# "tag edge symbol" to see if the object is of a type that has an edge.
	return next_state2( state, obj_sharp, take( state, obj_sharp ) )

debug_do = silence

def do_action_primitive( act, environment, action ):
	debug_do( "  Primitive bindings: %s", dict( environment.bindings ) )
	act.cursor = DIGRESSION( null, environment, act.cursor )
	action.function( act.thread, **dict( environment.bindings ) )
	finish_digression( act, act.cursor.tokens ) # Just in case the macro is totally empty

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
	#debug_shift = debug
	#debug_shift( 'shift' )
	#if act.operands != null and act.operands.head in action_words:
	#	error( Missed_Action_Word( act.operands.head ) )
	#debug_shift( "  cursor: %s", repr( act.cursor ) )
	raw_token_sharp = get_token( take( act.cursor.tokens, 'head#' ) )
	#debug_shift( "  token: %s", repr( flat( raw_token_sharp ) ) )
	#debug_shift( "    environment: %s", act.cursor.environment )
	token_sharp = bound( raw_token_sharp, act.cursor.environment )
	act.cursor.tokens = act.cursor.tokens.tail
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
def LIBRARY( name, dialect, environment ): return Object( 'LIBRARY', name=name, dialect=dialect, environment=environment )


#####################################
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

	#debug( "Global scope: %s", global_scope )
	#debug( "  bindings: %s", global_scope.bindings )
	#debug( "  dialect: %s", dialect.description() )

	return PROCEDURE( 'go_world',
		Start_script().
			go.world.
		End_script(),
		dialect, global_scope )

def parse_library( name, string, environment ):
	debug_parse = silence
	bindings = environment.bindings
	edge_symbols = set()
	dispatch_symbols = set()
	( name, args, script ) = ( None, None, [] )
	def done( bindings, name, args, script ):
		if name != None:
			args.append( null ) # Automatically add a don't-care arg for the macro name
			bindings[ 'ACTION_' + name ] = MACRO( name, List( script ), Stack( args ), environment )
			debug_parse( "PARSED MACRO[ ACTION_%s ]: %s", name, name )
	for word in re.findall( r'\([^)]*\)|\{[^}]*\}|\[[^]]*\]|\S+(?:/:?\w+#*)?', string.strip() ):
		debug_parse( "WORD: '%s'", word )
		if word[0] == '(': # Arguments to a new macro
			done( bindings, name, args, script )
			name = None
			script = []
			args = word[1:-1].split()
		elif word[0] == '{': # Long symbol/string
			script.append( word[1:-1] )
		elif word[0] == '[': # List of object types
			done( bindings, name, args, script )
			name = None
			script = []
			object_types = word[1:-1].split()
			debug_parse( "object_types: %s", object_types )
			edge_symbols.update( object_types )
		elif name == None: # Macro name
			name = word
			try:
				[ actual_name, dispatch_symbol ] = name.split( '/' )
				dispatch_symbols.add( dispatch_symbol )
				edge_symbols.add( actual_name )
			except ValueError: # No slash in the name
				pass
		else: # Append word to the script
			script.append( word )
	done( bindings, name, args, script )

	debug_parse( "    edge_symbols = %s", sorted( edge_symbols ) )
	debug_parse( "dispatch_symbols = %s", sorted( dispatch_symbols ) )

	automaton = polymorphic_postfix_automaton( environment, edge_symbols, dispatch_symbols )
	return LIBRARY( name, automaton, bindings )

def polymorphic_postfix_automaton( scope, edge_symbols, dispatch_symbols ):
	# A little silly parser generator algorithm to deal with postfix languages
	# with single-dispatch based on the topmost operand on the stack.
	# This will suffice until I write a proper parser generator.
	action_bindings = scope.bindings
	debug_ppa = silence
	default_state = Shift()
	debug_ppa( "default_state: %s", default_state )
	def shifty():
		while 1:
			yield Shift()
	dispatch_states = {}
	for symbol, state in zip( dispatch_symbols, shifty() ):
		dispatch_states[ symbol ] = state # dispatch_states[ x ] is the state we're in if the last item shifted was x
	debug_ppa( "dispatch_states: %s", dispatch_states )
	for state in dispatch_states.values() + [ default_state ]:
		# The default shift action
		for symbol in edge_symbols-dispatch_symbols:
			state[ symbol ] = default_state
		# Symbol-specific shift actions
		for symbol in dispatch_symbols:
			state[ symbol ] = dispatch_states[ symbol ]
		# Monomorphic reduce actions
		for ( symbol, action ) in action_bindings:
			if not '/' in action.name:
				name = action.name
				state[ name ] = Reduce0( symbol )
				debug_ppa( 'Monomorphic: %s[ %s ] = Reduce0( %s )', repr(state), name, symbol )
	# Polymorphic reduce actions
	for ( symbol, action ) in action_bindings:
		try:
			[ name, dispatch_symbol ] = action.name.split( '/' )
			dispatch_states[ dispatch_symbol ][ name ] = Reduce0( symbol )
			debug_ppa( 'Polymorphic: state[ %s ][ %s ] = Reduce0( %s ) # %s', dispatch_symbol, name, symbol, repr(dispatch_states[ dispatch_symbol ]) )
		except ValueError: # No slash in the name
			pass
	default_state[ ':EOF' ] = Accept()
	return default_state

# Sheppard builtins

def define_builtins( bindings, global_scope ):
	# TODO: parse_library ought to be able to define primitives itself

	debug_builtins = silence
	def digress( th, *values ):
		finish_digression( th.activation, th.activation.cursor.tokens )
		th.activation.cursor = DIGRESSION( List( values ), th.activation.cursor.environment, th.activation.cursor )

	def bind_with_name( func, name, *args ):
		bindings[ 'ACTION_' + name ] = PRIMITIVE( name, func, Stack( list( args ) ), global_scope )
		debug_builtins( "Binding primitive: %s %s", name, list(args) )

	# Cython doesn't support func_name
	#def bind( func, *args ):
	#	bind_with_name( func, func.func_name, *args )

	def current_environment( th ):
		digress( th, th.activation.cursor.environment.digressor )
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

	def get2( th, base, field1, field2 ):
		digress( th, base[ field1 ][ field2 ] )
	bind_with_name( get2, 'get2', 'base', 'field1', 'field2', null )

	def get3( th, base, field1, field2, field3 ):
		digress( th, base[ field1 ][ field2 ][ field3 ] )
	bind_with_name( get3, 'get3', 'base', 'field1', 'field2', 'field3', null )

	def _give( th, value, base, field ):
		give( value, base, field )
	bind_with_name( _give, 'give', 'value', 'base', 'field', null )

	def _take( th, base, field ):
		digress( th, take( base, field ) )
	bind_with_name( _take, 'take', 'base', 'field', null )

	def put( th, value, base, field ):
		base[ field ] = value
	bind_with_name( put, 'put', 'value', 'base', 'field', null )

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

	def _sharp( th, **args ):
		digress( th, sharp( **args ) )
	bind_with_name( _sharp, 'sharp', 'arg', null )

	def _flat( th, **args ):
		digress( th, flat( **args ) )
	bind_with_name( _flat, 'flat', 'arg', null )

	def buildin_exec( th, code ):
		exec code.strip() in globals(), th.activation.cursor.environment.digressor.bindings
	bind_with_name( buildin_exec, 'exec', 'code', null )

	def _eval( th, code ):
		digress( th, eval( code.strip(), globals(), th.activation.cursor.environment.digressor.bindings ) )
	bind_with_name( _eval, 'eval', 'code', null )


#####################################
#
# Meta-interpreter
#

meta_interpreter_text = """
( value symbol ) bind 
		value
		current_environment digressor bindings get2
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
		act cursor resumption get2
	act cursor put

( act remaining_tokens ) finish_digression/:LIST

( arg_value_sharp arg_bindings arg_symbol_sharp ) bind_arg/:SYMBOL
		arg_value_sharp
	arg_bindings arg_symbol_sharp give

( arg_value_sharp arg_bindings arg_symbol_sharp ) bind_arg/:NULL

( act arg_bindings formal_args ) bind_args/:NULL

( act arg_bindings formal_args ) bind_args/:LIST
		act history tail get2
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
			null
			environment
			act cursor get
		Digression
	act cursor put
	{ action.function( act.thread, **dict( environment.bindings ) ) } exec
		act
		act cursor tokens get2
	finish_digression

( act environment action ) do_action/:MACRO
			action script get
			environment
			act cursor get
		Digression
	act cursor put
		act
		act cursor tokens get2
	finish_digression

( act state ) perform/:ACCEPT
	false

( probe ) get_token/TAKE_FAILED
	eof

( probe ) get_token
	probe

( act state ) perform/:SHIFT
			act cursor tokens get2
			head#
		take get_token
	raw_token_sharp bind
			raw_token_sharp
			act cursor environment get2
		bound
	token_sharp bind
		act cursor tokens tail get3
	act cursor get tokens put
		act
		act cursor tokens get2
	finish_digression
			token_sharp
			act operands get
		cons
	act operands put
			act history head get2
			token_sharp
		next_state
	new_state bind
			new_state
			act history get
		cons
	act history put
	true

( act state ) perform/:REDUCE0
	{ print_stuff( act.thread ) } exec
			act history head get2 action# take
			act scope get
		bound
	action bind
		action environment get Environment
	reduce_env bind
		act cursor environment get2
	reduce_env digressor put
		act
		reduce_env bindings get
		action formal_args get
	bind_args
	{ print_reduce_stuff( act.thread, action, reduce_env ) } exec
		act
		reduce_env
		action
	do_action
	{ print_program( act.thread ) } exec
	true

( act probe ) execute2/:FALSE
	false

( act probe ) execute2/:TRUE
		act
			act
			act history head get2
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

[ :ACCEPT :ACTIVATION :BINDINGS :BOOLEAN :DIGRESSION
	:ENVIRONMENT :EOF :FALSE :LIST :MACRO :NOTHING :NULL
	:OBJECT :PRIMITIVE :PROCEDURE :PROGRAM :SHIFT :STATE
	:SYMBOL :THREAD :TRUE ]

"""

#
#
#####################################


sheppard_interpreter_library = None
def wrap_procedure( inner_procedure ):
	global global_scope, sheppard_interpreter_library, action_words
	if sheppard_interpreter_library is None:
		global_scope = ENVIRONMENT( null )
		define_builtins( global_scope.bindings, global_scope )
		sheppard_interpreter_library = parse_library( "sheppard_interpreter", meta_interpreter_text, global_scope )
	action_words = [ s[7:] for s in global_scope.bindings._fields ]
	nothing.environment = global_scope
	bindings = global_scope.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'nothing' ] = nothing
	outer_procedure = PROCEDURE( 'meta_' + inner_procedure.name, List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment, 'execute' ]), sheppard_interpreter_library.dialect, global_scope )
	return outer_procedure

def test( depth, plt ):
	global printing_level_threshold
	procedure = go_world()
	printing_level_threshold = plt
	for _ in range(depth):
		procedure = wrap_procedure( procedure )
	if printing_level_threshold < depth:
		debug( "procedure: %s", str( procedure ) )
		debug( " bindings: %s", str( procedure.environment.bindings ) )
		debug( "  dialect: %s", sheppard_interpreter_library.dialect.description() )
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
print shift_count, "shifts in", pretty_time( elapsed_time ), "with", object_counter, "objects"
