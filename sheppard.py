#! /usr/bin/python -O

import string, re, time
from itertools import islice
from sys import argv

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

debug_digressions = silence

debug_object = silence
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
			raise KeyError # When using dict syntax, we should raise the dict exception, so this can be used as bindings for python exec and eval
		except TypeError:
			debug_object( "getitem is checking %r._elements %s", self, self._elements )
			return self._elements[ key ] # If _elements were implemented as a list, we should catch IndexError and raise KeyError

	def __setitem__( self, key, value ):
		debug_object( "%r[ %r ] = %r", self, key, value )
		if isinstance( key, int ):
			debug_object( "setitem is setting %r._elements[%r] = %s", self, key, value )
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

	def __iter__( self ): # Range over all (index,value) pairs
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
			return repr( self ) + "{ " + string.join([ "%s=%r" % ( field, self[field] ) for field in self._fields ], ', ') + " }"

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
			return repr( self ) + "{" + string.join([ "\n%s%s=%s" % ( indent_str, key, self._description( value, already_described, indent+1 ) ) for ( key, value ) in self ], ',' ) + " }"

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
			result = 'INT'
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
	elif is_a( arg, 'INT' ):
		return Object( 'INT', value=arg )
	else:
		return arg

def flat( arg ):
	if isinstance( arg, str ): #if is_a( arg, 'SYMBOL' ):
		assert( arg[-1] == '#' )
		return arg[:-1]
	elif is_a( arg, 'INT' ):
		#debug( "flat( %s ) = %s", arg, arg.value )
		return arg.value
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

def INHERITANCE( **bindings ):
	return Object( 'INHERITANCE', **bindings )

def flat_inheritance( type_list ):
	result = Object( ':FLAT_INHERITANCE' )
	result[ ':FLAT_INHERITANCE' ] = ':INHERITANCE' # Ok, not quite flat
	result[ ':INHERITANCE' ]      = ':ANY'
	for t in type_list:
		result[ t ] = ':ANY'
	return result

def ENVIRONMENT( outer, **bindings ): return Object( 'ENVIRONMENT', outer=outer, bindings=Object('BINDINGS', **bindings) )

def DIGRESSION( tokens, environment, resumption ): return Object( 'DIGRESSION', tokens=tokens, environment=environment, resumption=resumption )

eof = Object( 'EOF' )

def Nothing():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( 'NOTHING', environment=ENVIRONMENT( null ) )
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
	return ':' + tag( obj ) + '#'

# These functions operate in sharp-land, so they're safe to call directly from
# a Sheppard program without interfering with the automaton

give_failed = 'GIVE_FAILED'

def give( obj, key_sharp, value_sharp ):
	#debug( "--GIVE-- %r %r %r", value_sharp, obj, key_sharp )
	if is_symbol( key_sharp ):
		obj[ flat(key_sharp) ] = flat( value_sharp )
	else:
		return give_failed

take_failed = 'TAKE_FAILED'

def take( obj, key_sharp ):
	#debug( "--TAKE--    %r.take( %r )", obj, flat( key_sharp ) )
	try:
		return sharp( obj[ flat( key_sharp ) ] )
	except KeyError:
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
		debug( "|   cursor: %s", act.cursor )
		debug( "| bindings: %s", act.cursor.environment.bindings )

def print_reduce_stuff( th, action, environment ):
	if meta_level( th ) >= printing_level_threshold:
		act = th.activation
		debug( ">+  ACTION: %r: %s  (%r)", action.name, list_str( action.formal_args ), action )
		#debug( ">+  ACTION: %s %s", action.name, action )
		debug( " |    with: %r %s", environment, environment.bindings )

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
#  top level, and must use is_a based on the arguments or compare the argument
#  against a specific symbol.  This represents sheppard automaton-based dispatch.
#  - Loops will be replaced with tail digression.  There's only one loop anyway
#  so that's no big deal.
#

def pop_list( base, field_symbol_sharp ):
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	#debug( "!!! pop_list( %s, %s ) = %s", base, field_symbol_sharp, result )
	give( base, field_symbol_sharp, take( current, 'tail#' ) )
	return result

def finish_digression( act, remaining_tokens ):
	if remaining_tokens is null: #is_a( remaining_tokens, 'NULL' ):
		debug_digressions( "  (finished %r %r %s)", act.cursor, act.cursor.environment, act.cursor.environment.bindings  )
		act.cursor = act.cursor.resumption

def bind_arg( arg_bindings, arg_symbol_sharp, arg_value_sharp ):
	debug_bind = silence
	if arg_symbol_sharp is null: # is_a( arg_symbol_sharp, 'NULL' )
		debug_bind( "    pop %r", flat( arg_value_sharp ) )
	else:
		assert( is_a( arg_symbol_sharp, 'SYMBOL' ) )
		give( arg_bindings, arg_symbol_sharp, arg_value_sharp )
		debug_bind( "    %s=%r", flat( arg_symbol_sharp ), flat( arg_value_sharp ) )

def bind_args( act, arg_bindings, formal_args ):
	if formal_args is null: #is_a( formal_args, 'NULL' ):
		pass
	else:
		act.history = act.history.tail
		bind_arg( arg_bindings, take( formal_args, 'head#' ), pop_list( act, 'operands#' ) )
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

def next_state3( state, obj_sharp, probe ):
	if probe is take_failed:
		try:
			return state[ ':ANY' ]
		except KeyError:
			raise Unexpected_token( state, obj_sharp )
	else:
		return probe

def next_state2( state, obj_sharp, possible_match ):
	if possible_match is take_failed: # Need to use TAKE_FAILED to get a short-circuit version of take.  If state[obj_sharp] exists and state[ tag_edge_symbol(obj_sharp) ] does not, we can't evaluate the latter
		return next_state3( state, obj_sharp, take( state, tag_edge_symbol(obj_sharp) ) )
	else:
		return possible_match

def next_state( state, obj_sharp ):
	# First we check of the object is itself a symbol that has an edge from this
	# state (ie. it's acting as a keyword).  Failing that, we check the object's
	# "tag edge symbol" to see if the object is of a type that has an edge.
	#debug( "-- looking up %r in: %s", obj_sharp, state )
	return next_state2( state, obj_sharp, take( state, obj_sharp ) )

debug_do = silence

def do_action_primitive( act, environment, action ):
	debug_do( "  Primitive bindings: %s", dict( environment.bindings ) )
	act.cursor = DIGRESSION( null, environment, act.cursor )
	debug_digressions( "    new primitive digression: %r", act.cursor )
	action.function( act.thread, **dict( environment.bindings ) )
	finish_digression( act, act.cursor.tokens ) # Just in case the macro is totally empty

def do_action_macro( act, environment, action ):
	act.cursor = DIGRESSION( action.script, environment, act.cursor )
	debug_digressions( "    new macro digression: %r", act.cursor )
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
	#debug_shift( "  state: %s", act.history.head )
	raw_token_sharp = get_token( take( act.cursor.tokens, 'head#' ) )
	#debug_shift( "  token: %r (%r)", flat( raw_token_sharp ), raw_token_sharp )
	#debug_shift( "    bindings: %s", act.cursor.environment.bindings )
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
		debug2_reduce = silence
	else:
		debug_reduce = silence
		debug2_reduce = silence
	print_stuff( act.thread )
	debug2_reduce( ">-- reduce0 %s --", act.history.head.action )
	action = bound( take( act.history.head, 'action#' ), act.scope ) # 'take' here just to get a sharp result
	debug2_reduce( "  action: %s", repr( action ) )
	#if is_a( action, 'MACRO' ):
	#	debug_reduce( "    %s", python_list( action.script ) )
	reduce_env = ENVIRONMENT( action.environment )
	reduce_env.digressor = act.cursor.environment  # Need this in order to make 'bind' a macro, or else I can't access the environment I'm trying to bind
	bind_args( act, reduce_env.bindings, action.formal_args )
	print_reduce_stuff( act.thread, action, reduce_env )
	debug2_reduce( "  environment: %s", reduce_env )
	debug2_reduce( "    based on: %s", act.cursor )
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
		#debug( "-__ execute2 __ %s", perform[ command ] )
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
		return "Unexpected token %r in state %s" % ( self._token, self._state )

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

def maybe_int( symbol ):
	try:
		return int( symbol, 0 )
	except( TypeError, ValueError ):
		return symbol

def parse_postfix_library( name, string, environment ):
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
				dispatch_symbols.add( maybe_int( dispatch_symbol ) )
				edge_symbols.add( maybe_int( actual_name ) )
			except ValueError: # No slash in the name
				pass
		else: # Append word to the script
			script.append( maybe_int( word ) )
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
		if True:
			state[ ':ANY' ] = default_state
		else:
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
			[ name, dispatch_symbol ] = [ maybe_int( name ), maybe_int( dispatch_symbol ) ]
			dispatch_states[ dispatch_symbol ][ name ] = Reduce0( symbol )
			debug_ppa( 'Polymorphic: state[ %s ][ %s ] = Reduce0( %s ) # %s', dispatch_symbol, name, symbol, repr(dispatch_states[ dispatch_symbol ]) )
		except ValueError: # No slash in the name
			pass
	default_state[ ':EOF' ] = Accept()
	return default_state

class Macro_factory: # Eventually split off the stuff that's acting like a production

	def __init__( self ):
		self._name = None

	def begin_args( self ):
		check = self._name
		if True:
			# Dummy "argument" representing the name token
			self._arg_names = [ null ]
			self._arg_types = [ self._name ]
		else:
			self._arg_names = []
			self._arg_types = []

	def begin_script( self ):
		check = self._arg_names
		self._script = []

	def process_word( self, word ):
		debug_parse = silence
		# Could be part of the script
		try:
			self._script.append( word )
			debug_parse( '  script %r', word )
			return
		except AttributeError:
			pass
		# Could be an argument
		try:
			if ':' in word:
				[ name, tp ] = word.split( ':' )
				tp = ':' + tp
			elif '@' in word:
				[ tp, name ] = word.split( '@' )
				tp = maybe_int( tp )
			else:
				name = word
				tp = ':ANY'
			self._arg_names.append( name )
			self._arg_types.append( tp )
			debug_parse( '  arg %s %r', name, tp )
			return
		except AttributeError:
			pass
		# Could be the name
		assert( self._name == None )
		self._name = word
		debug_parse( '  name %r', word )
		self.begin_args()

	def process_arg( self, name, tp ):
		debug_parse = silence
		try:
			x = self._script
			assert( False ) # Shouldn't see an argument after having started a script!
		except AttributeError:
			pass
		self._arg_names.append( name )
		self._arg_types.append( tp )
		debug_parse( '  arg %r %r', name, tp )

	def create_macro( self, environment ):
		return MACRO( self._name, List( self._script ), Stack( self._arg_names ), environment )

def parse_postfix_procedure( name, library_text, script ):
	env = ENVIRONMENT( null )
	define_builtins( env )
	lib = parse_postfix_library( name, library_text, env )
	define_predefined_bindings( env )
	result = PROCEDURE( name, List( script ), lib.dialect, env )
	return result

def parse_prefix_library( name, string, environment ):
	debug_parse = silence
	bindings = environment.bindings
	builtin_action_symbols = set([ k for (k,v) in bindings ])
	macro_symbols = set()
	factory = None
	factories_by_action_symbol = {}
	def done( factory, factories_by_action_symbol ):
		if factory and factory._name:
			name = factory._name
			macro = factory.create_macro( environment )
			action_symbol = 'ACTION_%d' % macro._id
			macro_symbols.add( action_symbol )
			factories_by_action_symbol[ action_symbol ] = factory
			bindings[ action_symbol ] = macro
			debug_parse( "PARSED MACRO %r[ %s ]: %s %s %s", bindings, action_symbol, name, zip( factory._arg_names, factory._arg_types), factory._script )
		else:
			debug_parse( "(no macro)" )
	for word in re.findall( r'\([^)]*\)|\{[^}]*\}|\[[^]]*\]|\S+(?:/:?\w+#*)?', string.strip() ):
		debug_parse( "WORD: '%s'", word )
		if word == "to":
			done( factory, factories_by_action_symbol )
			factory = Macro_factory()
		elif word == "do":
			factory.begin_script()
		elif word[0] == '[': # TODO
			done( factory, factories_by_action_symbol )
			factory = Macro_factory()
			object_types = word[1:-1].split()
			debug_parse( "object_types: %s", object_types )
		elif word[0] == '{': # Long symbol/string
			factory.process_word( word[1:-1] )
		else: # Ordinary word; what it means depends on the state
			factory.process_word( maybe_int( word ) )
	done( factory, factories_by_action_symbol )

	automaton = polymorphic_prefix_automaton( factories_by_action_symbol, builtin_action_symbols, macro_symbols, environment )
	return LIBRARY( name, automaton, bindings )

def polymorphic_prefix_automaton( factories_by_action_symbol, builtin_action_symbols, macro_symbols, environment ):
	# A little silly parser generator algorithm to deal with simple
	# multi-dispatch languages using prefix notation.
	# This will suffice until I write a proper parser generator.
	action_bindings = environment.bindings
	debug_ppa = silence
	default_state = Shift()
	shift_states = [ default_state ]
	debug_ppa( "default_state: %s", default_state )
	debug_ppa( "bindings: %s", environment.bindings )
	debug_ppa( "builtin_action_symbols: %s", builtin_action_symbols )
	debug_ppa( "actions: %s", [environment.bindings[a] for a in builtin_action_symbols] )
	names = set([ f._name for f in factories_by_action_symbol.values() ]) | set([ environment.bindings[a].name for a in builtin_action_symbols ])
	debug_ppa( "names: %s", names )
	name_states = {}

	# The parse graph here starts out as a forest: one tree for each name
	debug_ppa( "Building parse forest" )
	for action_symbol in builtin_action_symbols | macro_symbols:
		action = action_bindings[ action_symbol ]
		name = action.name
		if action_symbol in builtin_action_symbols:
			# Note that arg_names here is backward
			arg_names = python_list( action.formal_args )
			arg_types = [ name ] + [':ANY'] * ( len( arg_names ) -1 )
		else:
			f = factories_by_action_symbol[ action_symbol ]
			arg_names = None
			arg_types = f._arg_types
		cur_state = default_state
		debug_ppa( '  %r @ %r', name, cur_state )
		debug_ppa( '    action: %s', action )
		for tp in arg_types[:-1]:
			try:
				cur_state = cur_state[ tp ]
				debug_ppa( '    %r => %r', tp, cur_state )
			except KeyError:
				next_state = Shift()
				cur_state[ tp ] = next_state
				shift_states.append( next_state )
				cur_state = next_state
				debug_ppa( '    %r >> %r', tp, cur_state )

		reduce_state = Reduce0( action_symbol )
		cur_state[ arg_types[-1] ] = reduce_state
		debug_ppa( '    %r >> %r', arg_types[-1], reduce_state )
		name_states[ name ] = default_state[ name ]

	if 0:
		debug_ppa( "Adding macros to parse forest" )
		for f in factories:
			# Shift until you get to the last arg
			cur_state = default_state
			debug_ppa( '  %r', cur_state )
			for tp in f._arg_types[:-1]:
				try:
					cur_state = cur_state[ tp ]
					debug_ppa( '    %s => %s', tp, cur_state )
				except KeyError:
					next_state = Shift()
					cur_state[ tp ] = next_state
					shift_states.append( next_state )
					cur_state = next_state
					debug_ppa( '    %s >> %s', tp, cur_state )
			# Last arg causes a reduce
			reduce_state = Reduce0( 'ACTION_' + f._name )
			cur_state[ f._arg_types[-1] ] = reduce_state
			debug_ppa( '    %s >> %s', f._arg_types[-1], reduce_state )

	# Names are effectively keywords.  Any time we see one of those, whatever shift state we are in, we leap to that name_state
	debug_ppa( "Implementing keywords" )
	for name in names:
		debug_ppa( '  %s:', name )
		for s in shift_states:
			s[ name ] = name_states[ name ]
			debug_ppa( '    %r => %r', s, s[ name ] )

	# The accept state
	default_state[ ':EOF' ] = Accept()
	debug_ppa( 'EOF => %s', default_state[ ':EOF' ] )

	debug_ppa( 'Automaton:\n%s', default_state.description() )
	return default_state

def parse_prefix_procedure( name, library_text, script ):
	env = ENVIRONMENT( null )
	define_builtins( env, True )
	lib = parse_prefix_library( name, library_text, env )
	define_predefined_bindings( env )
	result = PROCEDURE( name, List( script ), lib.dialect, env )
	return result

# Sheppard builtins

def define_predefined_bindings( env ):
	# Note: You usually want to add these after calling
	# polymorphic_postfix_automaton, because that routine expects all bindings
	# to be for actions only.
	bindings = env.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'nothing' ] = nothing

def define_builtins( global_scope, prefix ):
	debug_builtins = silence
	def digress( th, *values ):
		finish_digression( th.activation, th.activation.cursor.tokens )
		th.activation.cursor = DIGRESSION( List( values ), th.activation.cursor.environment, th.activation.cursor )
		debug_digressions( "    new builtin digression: %r = %s", th.activation.cursor, list_str( th.activation.cursor.tokens ) )

	def bind_with_name( func, name, *args ):
		if prefix:
			arg_stack = Stack( [null] + list( args ) )
		else:
			arg_stack = LIST( null, Stack(list( args )) )
		global_scope.bindings[ 'ACTION_' + name ] = PRIMITIVE( name, func, arg_stack, global_scope )
		debug_builtins( "Binding primitive: %s %s", name, list(args) )

	# Cython doesn't support func_name
	#def bind( func, *args ):
	#	bind_with_name( func, func.func_name, *args )

	def builtin_exec( th, environment, code ):
		exec code.strip() in globals(), environment.bindings
	bind_with_name( builtin_exec, 'exec', 'environment', 'code' )

	def builtin_eval( th, environment, code ):
		result = eval( code.strip(), globals(), environment.bindings )
		# One day, we should sharp/flat all the args and results of eval I guess.  Good thing the meta-interpreter doesn't use ints yet.
		#result = sharp( result )
		digress( th, result )
	bind_with_name( builtin_eval, 'eval', 'environment', 'code' )

	def buildin_current_thread( th ):
		digress( th, th )
	bind_with_name( buildin_current_thread, 'current_thread' )

	def builtin_current_environment( th ):
		# I could probably implement this somehow using current_thread and exec,
		# but it's awkward as long as "bind" uses this, because I have no easy
		# way to bind "th" before calling this.
		digress( th, th.activation.cursor.environment.digressor )
	bind_with_name( builtin_current_environment, 'current_environment' )

	def builtin_nop( th ):
		# It's a pain defining this in every library just so I can use exec and eval
		pass
	bind_with_name( builtin_nop, 'nop' )

	# These are not really needed, but make a huge impact on performance.
	# I'd like to keep the level-2 meta-interpreter under one minute right now,
	# but one day I could eliminate these.

	def builtin_get( th, base, field ): # Saves 163 shifts
		digress( th, base[ field ] )
	bind_with_name( builtin_get, 'get', 'base', 'field' )

	def builtin_get2( th, base, field1, field2 ): # Saves 146 shifts
		digress( th, base[ field1 ][ field2 ] )
	bind_with_name( builtin_get2, 'get2', 'base', 'field1', 'field2' )

	def builtin_take( th, base, field ): # Saves 107 shifts
		digress( th, take( base, field ) )
	bind_with_name( builtin_take, 'take', 'base', 'field' )

	def builtin_put( th, value, base, field ): # Saves 147 shifts
		base[ field ] = value
	if prefix:
		bind_with_name( builtin_put, 'put', 'base', 'field', 'value' )
	else:
		bind_with_name( builtin_put, 'put', 'value', 'base', 'field' )

	def builtin_cons( th, **args ): # Saves 27 shifts
		digress( th, cons(**args) )
	bind_with_name( builtin_cons, 'cons', 'head_sharp', 'tail' )


#####################################
#
# Meta-interpreter
#

meta_interpreter_postfix_text = """
( name script dialect environment )       Procedure   current_environment { PROCEDURE(**dict( locals() )) }   eval
( tokens environment resumption )         Digression  current_environment { DIGRESSION(**dict( locals() )) }  eval
( cursor operands history scope caller )  Activation  current_environment { ACTIVATION(**dict( locals() )) }  eval
( activation meta_thread )                Thread      current_environment { THREAD(**dict( locals() )) }      eval
( outer )                                 Environment current_environment { ENVIRONMENT(**dict( locals() )) } eval

( obj )          tag_edge_symbol current_environment { ':' + tag( obj ) + '#' }     eval
( base f1 f2 f3 )           get3 current_environment { base[ f1 ][ f2 ][ f3 ] }     eval
( value base field )        give current_environment { give( base, field, value ) } exec

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
	arg_value_sharp arg_bindings arg_symbol_sharp give

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

( state obj_sharp probe ) next_state3/TAKE_FAILED
	state :ANY get

( state obj_sharp probe ) next_state3
	probe

( state obj_sharp probe ) next_state2/TAKE_FAILED
		state
		obj_sharp
			state
			obj_sharp tag_edge_symbol
		take
	next_state3

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
	current_environment { action.function( act.thread, **dict( environment.bindings ) ) } exec
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
	current_environment { print_stuff( act.thread ) } exec
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
	current_environment { print_reduce_stuff( act.thread, action, reduce_env ) } exec
		act
		reduce_env
		action
	do_action
	current_environment { print_program( act.thread ) } exec
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
	:ENVIRONMENT :EOF :FALSE :INT :LIST :MACRO :NOTHING :NULL
	:OBJECT :PRIMITIVE :PROCEDURE :PROGRAM :SHIFT :STATE
	:SYMBOL :THREAD :TRUE ]

[ :BINDINGS :ENVIRONMENT :HASH_TABLE :HASH_ENTRY :INT :PAIR :SYMBOL ]
"""
# Super cheesy: note above that we need to include all the types of things ever
# manipulated by programs executed by the meta-interpreter.  This makes a
# really strong case for some kind of default edge or inheritance mechanism.

# Implementing a primitive with eval/exec requires three additional shifts
# (including the nop) compared with a builtin.  In some cases, that's enough to
# make the interpreter much slower, so we just leave them as builtins for now.
#
not_used_because_they_are_too_slow = """
( base field )              take current_environment { take( base, field ) }        eval
( base field )               get current_environment { base[ field ] }              eval
( base f1 f2 )              get2 current_environment { base[ f1 ][ f2 ] }           eval
( value base field )         put current_environment { base[ field ] = value }      exec
( head_sharp tail )         cons current_environment { cons( head_sharp, tail ) }   eval
"""

meta_interpreter_prefix_text = """
to Procedure
	name script dialect environment
do eval current_environment
	{ PROCEDURE(**dict( locals() )) }

to Digression
	tokens environment resumption
do eval current_environment
	{ DIGRESSION(**dict( locals() )) }

to Activation
	cursor operands history scope caller
do eval current_environment
	{ ACTIVATION(**dict( locals() )) }

to Thread
	activation meta_thread
do eval current_environment
	{ THREAD(**dict( locals() )) }

to Environment
	outer
do eval current_environment
	{ ENVIRONMENT(**dict( locals() )) }


to tag_edge_symbol
	obj
do eval current_environment
	{ ':' + tag( obj ) + '#' }

to get3
	base f1 f2 f3
do eval current_environment
	{ base[ f1 ][ f2 ][ f3 ] }

to give
	base key_sharp value_sharp
do exec current_environment
	{ give( base, key_sharp, value_sharp ) }

to bind 
	symbol value
do put
	get2 current_environment digressor bindings
	symbol
	value


to pop_list 
	base field_symbol_sharp
do
	bind current
		take base field_symbol_sharp
	bind result
		take current head#
	give base field_symbol_sharp
		take current tail#
	result

to finish_digression 
	act remaining_tokens:NULL
do
	put act cursor
		get2 act cursor resumption

to finish_digression 
	act remaining_tokens
do


to bind_arg 
	arg_bindings arg_symbol_sharp:NULL arg_value_sharp
do

to bind_arg 
	arg_bindings arg_symbol_sharp:SYMBOL arg_value_sharp
do
	give arg_bindings arg_symbol_sharp arg_value_sharp


to bind_args 
	act arg_bindings formal_args:NULL
do

to bind_args 
	act arg_bindings formal_args:LIST
do
	put act history
		get2 act history tail
	bind_arg
		arg_bindings
		take formal_args head#
		pop_list act operands#
	bind_args
		act
		arg_bindings
		get formal_args tail


to bound2 
	obj_sharp environment TAKE_FAILED@probe
do
	bound
		obj_sharp
		get environment outer

to bound2 
	obj_sharp environment probe
do
	probe


to bound 
	obj_sharp environment:NULL
do
	obj_sharp

to bound 
	obj_sharp environment:ENVIRONMENT
do
	bound2
		obj_sharp
		environment
		take
			get environment bindings
			obj_sharp


to next_state3 
	state obj_sharp TAKE_FAILED@probe
do
	get state :ANY

to next_state3 
	state obj_sharp probe
do
	probe


to next_state2 
	state obj_sharp TAKE_FAILED@possible_match
do
	next_state3
		state
		obj_sharp
		take
			state
			tag_edge_symbol obj_sharp

to next_state2 
	state obj_sharp possible_match
do
	possible_match


to next_state 
	state obj_sharp
do
	next_state2
		state
		obj_sharp
		take state obj_sharp


to do_action 
	act environment action:PRIMITIVE
do
	put act cursor
		Digression
			null
			environment
			get act cursor
	exec current_environment
		{ action.function( act.thread, **dict( environment.bindings ) ) }
	finish_digression
		act
		get2 act cursor tokens

to do_action 
	act environment action:MACRO
do
	put act cursor
		Digression
			get action script
			environment
			get act cursor
	finish_digression
		act
		get2 act cursor tokens


to get_token 
	TAKE_FAILED@probe
do
	eof

to get_token 
	probe
do
	probe


to perform 
	act state:ACCEPT
do
	false

to perform 
	act state:SHIFT
do
	bind raw_token_sharp
		get_token take
			get2 act cursor tokens
			head#
	bind token_sharp
		bound
			raw_token_sharp
			get2 act cursor environment
	put
		get act cursor
		tokens
		get3 act cursor tokens tail
	finish_digression
		act
		get2 act cursor tokens
	put act operands
		cons
			token_sharp
			get act operands
	bind new_state
		next_state
			get2 act history head
			token_sharp
	put act history
		cons new_state get act history
	true

to perform 
	act state:REDUCE0
do
	exec current_environment
		{ print_stuff( act.thread ) }
	bind action
		bound
			take
				get2 act history head
				action#
			get act scope
	bind reduce_env
		Environment
			get action environment
	put reduce_env digressor
		get2 act cursor environment
	bind_args
		act
		get reduce_env bindings
		get action formal_args
	exec current_environment
		{ print_reduce_stuff( act.thread, action, reduce_env ) }
	do_action
		act
		reduce_env
		action
	exec current_environment
		{ print_program( act.thread ) }
	true

to execute2 
	act probe:FALSE
do

to execute2 
	act probe:TRUE
do
	execute2
		act
		perform
			act
			get2 act history head

to execute 
	procedure environment scope
do
	bind act
		Activation
			Digression
				get procedure script
				environment
				nothing
			null
			cons
				get procedure dialect
				null
			scope
			null
	put act thread
		Thread act current_thread
	execute2 act true

"""

#
#
#####################################

fib_text_with_compare = """
( a b ) +   { a+b } current_environment eval
( a b ) -   { a-b } current_environment eval
( a b ) <=  { a<=b and true or false } current_environment eval

( n ) print_result current_environment { print "*** RESULT IS", n, "***" } exec

( n is_base_case ) fib2/:TRUE
	1

( n is_base_case ) fib2/:FALSE
	n 1 - fib
	n 2 - fib
	+

( n ) fib
	n
	n 1 <=
	fib2

[ :INT :SYMBOL ]
"""

fib_text_with_dispatch = """
( a b ) +   { a+b } current_environment eval
( a b ) -   { a-b } current_environment eval

( n ) print_result current_environment { print "*** RESULT IS", n, "***" } exec

( n ) fib/0  1
( n ) fib/1  1

( n ) fib
	n 1 - fib
	n 2 - fib
	+

[ :INT :SYMBOL ]
"""

fib_text_prefix = """
to + a b   do eval current_environment { a+b }
to - a b   do eval current_environment { a-b }

to print_result n do exec current_environment { print "*** RESULT IS", n, "***" }

to fib 0@ do 1
to fib 1@ do 1

to fib n do
	+
	fib - n 1
	fib - n 2

"""

def fib_procedure():
	#return parse_postfix_procedure( "fib", fib_text_with_dispatch, [ 3, 'fib', 'print_result' ] )
	return parse_prefix_procedure( "fib", fib_text_prefix, [ 'print_result', 'fib', 3 ] )

# Note: the explicit int() calls in hash_test are just for error detection.  We
# don't want 'foo'*12345 buliding some immense string by accident.

hash_test_text = """
( a b ) +        { int(a) +  int(b) } current_environment eval
( a b ) -        { int(a) -  int(b) } current_environment eval
( a b ) *        { int(a) *  int(b) } current_environment eval
( a b ) ^        { int(a) ^  int(b) } current_environment eval
( a b ) =/:INT   { int(a) == int(b) and true or false } current_environment eval

( a b ) &&/:FALSE   false
( a b ) &&/:TRUE    a
( a b ) ||/:FALSE   a
( a b ) ||/:TRUE    true

( left right )           PAIR { Object( 'PAIR', **dict(locals()) ) } current_environment eval
()                 HASH_TABLE { Object( 'HASH_TABLE', **dict(locals()) ) } current_environment eval
( key value next ) HASH_ENTRY { Object( 'HASH_ENTRY', **dict(locals()) ) } current_environment eval

( value ) sharp { sharp( value ) } current_environment eval

( value symbol ) bind 
		value
		current_environment digressor bindings get2
	symbol put

( put_value table put_key hash_code candidate_entry key_matches ) hash_put3/:TRUE
		put_value
	candidate_entry value put

( put_value table put_key hash_code candidate_entry key_matches ) hash_put3/:FALSE
		put_value
		table
		put_key
		hash_code
		candidate_entry next get
	hash_put2

( put_value table put_key hash_code candidate_entry ) hash_put2/TAKE_FAILED
		put_key put_value null HASH_ENTRY
	table hash_code put

( put_value table put_key hash_code candidate_entry ) hash_put2/:NULL
		put_key put_value  table hash_code get  HASH_ENTRY
	table hash_code put

( put_value table put_key hash_code candidate_entry ) hash_put2/:HASH_ENTRY
		put_value table put_key hash_code candidate_entry
			candidate_entry key get
			put_key
		=
	hash_put3

( value table put_key ) hash_put
		put_key hash_code
	hc bind
		value
		table
		put_key
		hc
		table hc sharp take
	hash_put2

( candidate_entry key_matches ) hash_get3/:TRUE
	candidate_entry value get

( candidate_entry key_matches ) hash_get3/:FALSE
	candidate_entry next get hash_get2

( get_key candidate_entry ) hash_get2/:HASH_ENTRY
		candidate_entry
			candidate_entry key get
			get_key
		=
	hash_get3

( get_key candidate_entry ) hash_get2/TAKE_FAILED
	TAKE_FAILED

( get_key candidate_entry ) hash_get2/:NULL
	TAKE_FAILED

( table key ) hash_get
		key
			table
			key hash_code sharp
		take
	hash_get2

( value ) hash_code/:INT
	value

( pair ) hash_code/:PAIR
	pair left  get hash_code
	pair right get hash_code
	^

( a b ) =/:PAIR
			a left  get b left  get =
			a right get b right get =
		&&
			a left  get b right get =
			a right get b left  get =
		&&
	||

( table left right_limit ) add_pairs/0
( table left right_limit ) add_pairs
		left right_limit PAIR
	pair bind
		pair
		table
		pair
	hash_put
		table
		left
		right_limit 1 -
	add_pairs

( table left_limit ) populate_table/0
( table left_limit ) populate_table
	table left_limit 2 add_pairs
	table left_limit 1 - populate_table

() main
	HASH_TABLE table bind
	table 2 populate_table
	current_environment { print "*** Table:", table.description(), "***" } exec

[ :BINDINGS :ENVIRONMENT :HASH_TABLE :HASH_ENTRY :INT :PAIR :SYMBOL ]
"""
sheppard_interpreter_library = None
def wrap_procedure( inner_procedure ):
	global global_scope, sheppard_interpreter_library, action_words
	if sheppard_interpreter_library is None:
		global_scope = ENVIRONMENT( null )
		prefix = True
		define_builtins( global_scope, prefix )
		if prefix:
			sheppard_interpreter_library = parse_prefix_library( "sheppard_interpreter", meta_interpreter_prefix_text, global_scope )
			script = List([ 'execute', inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment ])
		else:
			sheppard_interpreter_library = parse_postfix_library( "sheppard_interpreter", meta_interpreter_postfix_text, global_scope )
			script = List([ inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment, 'execute' ])
		define_predefined_bindings( global_scope )
	action_words = [ s[7:] for s in global_scope.bindings._fields ]
	nothing.environment = global_scope
	bindings = global_scope.bindings
	outer_procedure = PROCEDURE( 'meta_' + inner_procedure.name, script, sheppard_interpreter_library.dialect, global_scope )
	return outer_procedure

def test( depth, plt ):
	global printing_level_threshold
	#procedure = go_world()
	procedure = fib_procedure()
	#procedure = parse_postfix_procedure( "hash_test", hash_test_text, ['main'] )
	#printing_level_threshold = plt
	for _ in range(depth):
		procedure = wrap_procedure( procedure )
	if printing_level_threshold < depth:
		debug( "procedure: %s", str( procedure ) )
		debug( "   script: %s", str( procedure.script ) )
		debug( " bindings: %s", str( procedure.environment.bindings ) )
		#debug( "  dialect: %s", procedure.dialect.description() )
	execute( procedure, ENVIRONMENT( procedure.environment ), procedure.environment )

def pretty_time( t ):
	if t < 1:
		return "%.2fms" % (t*1000)
	elif t < 60:
		return "%.2fs" % t
	elif t < 3600:
		return "%dm%ds" % ( t/60, t%60 )
	else:
		return "%dh%dm%ds" % ( t/3600, (t%3600)/60, t%60 )

def main():
	print "# SHEPPARD OUTPUT"
	try:
		depth = int( argv[1] )
	except IndexError:
		depth = 0
	global printing_level_threshold
	try:
		printing_level_threshold = int( argv[2] )
	except IndexError:
		printing_level_threshold = depth
	start_time = time.time()
	test( depth, printing_level_threshold )
	elapsed_time = time.time() - start_time
	print shift_count, "shifts in", pretty_time( elapsed_time ), "with", object_counter, "objects"

main()
