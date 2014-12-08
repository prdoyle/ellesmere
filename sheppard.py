#! /usr/bin/python -O

import string, re, time
from sys import argv

# I've adopted a convention that strings used as Sheppard symbols (or parts
# thereof) have single-quotes, and ordinary Python strings have double-quotes.
# Since Python sees no difference between these, there's no way to enforce this
# rule, so you may find some unintentional inconsistencies.

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

def reprs( xs ):
	return "[ " + string.join([ repr(x) for x in xs ], ", ") + " ]"

# Calls to these are often commented out below to improve performance.  "Enabling" these here may not be enough.
#debug_digressions = debug
#debug_object = debug

object_counter = 0

class Object:
	"""Warps Python's object model to make it more Sheppard-like"""

	def __init__( _self, _tag, **edges ):
		global object_counter
		assert( _tag.isupper() )
		_self._tag = _tag
		# The terminology is a little weird here, from a time when numbers were not considered
		# symbols, so I differentiated "elements" (indexed by ints) from "fields".
		_self._elements = {}
		_self._fields = sorted([ k for k in edges ]) # _fields can be adjusted if we want a particular field ordering
		_self._id = object_counter
		object_counter += 1
		for ( name, value ) in edges.iteritems():
			setattr( _self, name, value )

	# Allow map syntax for convenience

	def __getitem__( self, key ):
		try:
			return getattr( self, key )
		except AttributeError:
			raise KeyError # When using dict syntax, we should raise the dict exception, so this can be used as bindings for python exec and eval
		except TypeError:
			#debug_object( "getitem is checking %r._elements %s", self, self._elements )
			return self._elements[ key ] # If _elements were implemented as a list, we should catch IndexError and raise KeyError

	def __setitem__( self, key, value ):
		#debug_object( "setitem %r[ %r ] = %r", self, key, value )
		try:
			setattr( self, key, value )
			if not key in self._fields:
				self._fields.append( key )
		except TypeError:
			assert(isinstance( key, int ))
			#debug_object( "setitem is setting %r._elements[%r] = %s", self, key, value )
			self._elements[ key ] = value

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
		return "%s_%d" % ( self._tag, self._id )

	def __str__( self ):
		suffix = ""
		return repr( self ) + "{ " + string.join([ "%s=%s" % ( field, self._short_description( self[field] ) ) for field in ( self._fields + self._elements.keys() ) ], ', ') + " }"

	def description( self ):
		# TODO: This should move out of Object.  Any names in here render the object unable to have a field of the same name.
		work_queue = []
		already_described = set()
		result = self._description( work_queue, already_described )
		while work_queue:
			next_item = work_queue.pop(0)
			if next_item not in already_described:
				result = result + ";\n" + next_item._description( work_queue, already_described )
		return result

	def _description( self, work_queue, already_described, indent=1 ):
		debug_description = silence
		silence( "description( %r )", self )
		if already_described is None:
			already_described = set()
		if self is null:
			return "null"
		elif self in already_described:
			silence( "%r alredy described", self )
			return repr( self )
		else:
			already_described.add( self )
			silence( "   %r is being described", self )
			indent_str = " |" * indent
			if self._expand_children():
				children_strings = [ "\n%s%s=%s" % ( indent_str, key, self._long_description( value, already_described, work_queue, indent+1 ) ) for ( key, value ) in self ]
			else:
				keys = [ key for ( key, value ) in self ]
				def smart_order( key1, key2 ):
					global smart_splitter
					smart_splitter = re.compile(r"(.*[^0-9])(.*)$")
					try:
						split1 = smart_splitter.match( key1 ).groups()
						split2 = smart_splitter.match( key2 ).groups()
						result = cmp( split1, split2 )
						return result
					except TypeError:
						return 0
				keys.sort( smart_order )

				children = [ value for ( key, value ) in self if isinstance( value, Object ) ]
				silence( "Children of %r: %s", self, string.join([ "%r" % c for c in children ], ', ') )
				if len( children ) <= 6:
					children_strings = [ " %s=%s" % ( key, self._short_description( self[ key ] ) ) for key in keys ]
				else:
					children_strings = [ "\n%s%s=%s" % ( indent_str, key, self._short_description( self[ key ] ) ) for key in keys ]
				work_queue += children
			result = repr( self ) + "{" + string.join( children_strings, ',' ) + " }"
			return result

	def _expand_children( self ):
		return False

	def _long_description( self, value, already_described, work_queue, indent ):
		if isinstance( value, Object ):
			return value._description( work_queue, already_described, indent )
		else:
			return str( value )

	def _short_description( self, value ):
		try:
			return "<<%s>>" % value.__name__
		except AttributeError:
			pass
		if isinstance( value, Object ):
			return repr( value )
		else:
			return str( value )

	# null and zero are false; all else are true

	def __nonzero__( self ): return self != 0 and self != null

class Null( Object ): # Just to make debugging messages more informative

	def __init__( self ):
		Object.__init__( self, 'NULL' )

	def __repr__( self ):
		return "null"

	def __str__( self ):
		return "null"

null = Null()  # The very first object!  It gets _id=0

# These need to be disembodied functions.  They can't be methods, because some
# Python objects like symbols are represented by plain old Python objects like
# strings that have no such methods.
# Also, methods of Object can confuse __getattr__ and end up trying to call Sheppard objects.

def is_int( obj ):    return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_symbol( obj ): return isinstance( obj, str ) # Sheppard symbols are represented by Python strs

def all_fields( obj ):
	if isinstance( obj, Object ):
		str_fields = obj._fields
		int_fields = obj._elements.keys()
		str_fields.sort()
		int_fields.sort()
		fields = str_fields + int_fields
		result = Object( 'FIELD_ARRAY', length=len( fields ), subject=obj )
		for ( i, value ) in enumerate( fields ):
			result[ i+1 ] = value # Let's count from 1, just to be zany
		return result
	else:
		return Object( 'FIELD_ARRAY', length=0, subject=obj )

def tag( obj ):
	try:
		return obj._tag
	except AttributeError:
		if isinstance( obj, str ): #is_symbol( obj ):
			result = 'SYMBOL'
		elif isinstance( obj, int ): #is_int( obj ):
			result = 'INT'
		else: # All other Sheppard objects are represented by instances of Object
			result = obj._tag
	assert( result.isupper() )
	return result

def is_a( obj, t ):
	assert( t.isupper() )
	return tag( obj ) == t # TODO: inheritance

def sharp( arg ):
	"""
	This is a way of quoting a symbol to protect it from being interpreted.

	Invariant: flat(sharp( X )) is x
	"""
	if isinstance( arg, str ): #if is_a( arg, 'SYMBOL' ):
		# I like putting the sharp at the end, because it reads nicely, like
		# 'foo#' as "foo sharp".  However, the ordering becomes ambiguous when we
		# add other symbol prefixes like ':'.  A symbol like ':INT#' might mean
		# ":INT sharp" or it might mean "an object of type INT#".  If both were
		# prefixes, these would be '#:INT' and ':#INT' respectively.  Putting the
		# colon at the end would solve the problem too, but 'INT:' looks weird
		# when the variable was declared as "n:INT".  I've gotten away with this
		# ambiguity for now by considering colons to bind more tightly than
		# sharps, so ':INT#' is '#:INT' in prefix notation; seems we haven't had
		# a use for ':#INT' yet.  Maybe we never will?
		return arg + '#'
	elif is_a( arg, 'INT' ):
		return Object( 'INT', value=arg ) # Wrap arg (which may be a python int) in an Object of type INT
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

def Dial_tone():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( 'DIAL_TONE', environment=ENVIRONMENT( null ) )
	endless_eof = LIST( eof, null )
	endless_eof[ 'tail' ] = endless_eof # oooo, tricky
	result[ 'tokens' ] = endless_eof
	result[ 'resumption' ] = result # oooo, equally tricky
	return result

dial_tone = Dial_tone()
false = Object( 'FALSE' )
true  = Object( 'TRUE' )

def ACTIVATION( cursor, operands, history, scope, caller ): return Object( 'ACTIVATION', cursor=cursor, operands=operands, history=history, scope=scope, caller=caller )
def THREAD( activation, meta_thread ): return Object( 'THREAD', activation=activation, meta_thread=meta_thread )
def PROCEDURE( name, script, dialect, environment ): return Object( 'PROCEDURE', name=name, script=script, dialect=dialect, environment=environment )

def MACRO( name, script, formal_arg_names, formal_arg_types, environment ):
	return Object( 'MACRO', name=name, script=script, formal_arg_names=formal_arg_names, formal_arg_types=formal_arg_types, environment=environment )
def PRIMITIVE( name, function, formal_arg_names, formal_arg_types, environment ):
	return Object( 'PRIMITIVE', name=name, script=null, function=function, formal_arg_names=formal_arg_names, formal_arg_types=formal_arg_types, environment=environment )

def PRODUCTION( lhs, rhs, action ): return Object( 'PRODUCTION', lhs=lhs, rhs=rhs, action=action )

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

# A few functions to help with the Python / Sheppard impedance mismatch

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

def future_str( cursor ):
	if cursor is dial_tone:
		return ""
	else:
		return repr( cursor ) + " " + future_str( cursor.resumption )

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

def cons( head_sharp, tail ):
	return LIST( flat( head_sharp ), tail )

debug_ellision_limit=999
printing_level_threshold=1

def cursor_description( cursor ):
	if cursor == dial_tone:
		return ''
	else:
		return string.join( [ repr(x) for x in python_list( cursor.tokens ) ], "  " ) + " . " + cursor_description( cursor.resumption )

def print_program( th ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		debug( "# PROGRAM%d: %s ^ %s", meta_level(th), list_str( frame.operands, "  ", debug_ellision_limit ), cursor_description( frame.cursor ) )

def print_stuff( th ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		#debug( "stack: %s", zip( python_list( frame.history ), python_list( frame.operands ) ) )
		print_program( th )
		debug( "|  history: %s", list_str( frame.history, ':', debug_ellision_limit ) )
		debug( "|   future: %s", future_str( frame.cursor ) )
		#debug( "| bindings: %s", frame.cursor.environment.bindings )

def print_reduce_stuff( th, action, reduce_environment ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		debug( ">+  ACTION: %r %r with %s", action, action.name, reduce_environment.bindings )
		#debug( ">+  ACTION: %s %s", action.name, action )
		#debug( " |    with: %s %s", reduce_environment.bindings, reduce_environment )

def print_backtrace( th ):
	print_stuff( th )
	frame = th.activation
	debug( "| backtrace:" )
	history  = python_list( frame.history )
	operands = python_list( frame.operands )
	for ( state, obj ) in zip( history, operands ):
		debug( "|   |      %s", state )
		debug( "|   | %s",      obj )
	debug( "|   |      %s", history[-1] )

# Sheppard builtins

def define_predefined_bindings( env ):
	# Note: You usually want to add these after calling
	# polymorphic_automaton, because that routine expects all bindings
	# to be for actions only.
	bindings = env.bindings
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'dial_tone' ] = dial_tone

def define_builtins( global_scope ):
	debug_builtins = silence
	def digress( th, *values ):
		end_digression_if_finished( th.activation, th.activation.cursor.tokens )
		th.activation[ 'cursor' ] = DIGRESSION( List( values ), th.activation.cursor.environment, th.activation.cursor )
		#debug_digressions( "    new builtin digression: %r = %s", th.activation.cursor, list_str( th.activation.cursor.tokens ) )

	def bind_with_name( func, name, *args ):
		arg_name_stack = Stack( [null] + list( args ) )
		arg_type_stack = Stack( [name] + [':ANY'] * len( args ) )
		global_scope.bindings[ 'ACTION_' + name ] = PRIMITIVE( name, func, arg_name_stack, arg_type_stack, global_scope )
		debug_builtins( "Binding primitive: %s %s", name, list(args) )

	# We're down to just one builtin: current_thread
	def builtin_current_thread( th, args ):
		digress( th, th )
	bind_with_name( builtin_current_thread, 'current_thread' )

prologue_text = """
to take       base key_sharp               python_eval { take( base, key_sharp ) }
to give       base key_sharp value_sharp   python_exec { give( base, key_sharp, value_sharp ) }
to get        base key:SYMBOL              python_eval { base[ key ] }
to get2       base f1:SYMBOL f2:SYMBOL     python_eval { base[ f1 ][ f2 ] }
to put        base key:SYMBOL value        python_exec { base[ key ] = value }
to cons       head_sharp tail              python_eval { cons( head_sharp, tail ) }
to all_fields obj                          python_eval { all_fields( obj ) }

/* This could really just be a binding */
to current_environment              do current_environment2 current_thread
to current_environment2 th:THREAD   python_eval { th.activation.cursor.environment.digressor }

to set 
	symbol:SYMBOL value
do put
	get2 current_environment digressor bindings  /* Bindings at the point that 'set' was expanded */
	symbol
	value
"""


#####################################
#
# Parsing
#

debug_parse = silence

class ParseError( Exception ):
	pass

def maybe_int( symbol ):
	try:
		return int( symbol, 0 )
	except( TypeError, ValueError ):
		return symbol

def unpack_symbol( symbol ):
	if symbol[0] == '{':
		return maybe_int( symbol[1:-1] )
	else:
		return maybe_int( symbol )

def library_words( library_text ):
	raw_words = re.findall( r'/\*.*?\*/|\{[^}]*\}|\S+(?:/:?\w+#*)?', library_text )
	words = map( unpack_symbol, filter( lambda s: s[0:2] != '/*', raw_words ) )
	return List( words )

def FORMAL_ARG( name, symbol ): return Object( 'FORMAL_ARG', name=name, symbol=symbol )

def take_word( word_cursor ):
	return pop_list( word_cursor, 'current#' )

def parse_arg_list( word_cursor ):
	return parse_arg_list2( word_cursor.current.head, word_cursor )

def parse_arg_list2( lookahead, word_cursor ):
	if lookahead in [ 'do', 'python_eval', 'python_exec', take_failed ]:
		debug_parse( "Ending arg list on %r", lookahead )
		return null
	else:
		word_sharp = take_word( word_cursor )
		if '=' in word_sharp:
			[ name, symbol ] = flat( word_sharp ).split( '=' )
			symbol = maybe_int( symbol )
		elif ':' in word_sharp:
			[ name, symbol ] = flat( word_sharp ).split( ':' )
			symbol = ':' + symbol
		else:
			name = flat( word_sharp )
			symbol = ':ANY'
		debug_parse( '  arg %s %r', name, symbol )
		return LIST( FORMAL_ARG( name, symbol ), parse_arg_list( word_cursor ) )

def parse_script( word_cursor ):
	return parse_script2( take( word_cursor.current, 'head#' ), word_cursor )

def parse_script2( lookahead_sharp, word_cursor ):
	if lookahead_sharp in [ 'to#', take_failed ]:
		return null
	else:
		return cons( take_word( word_cursor ), parse_script( word_cursor ) )

def arg_stack( arg_list, field, tail=null ):
	if arg_list is null:
		return tail
	else:
		return arg_stack( arg_list.tail, field, LIST( arg_list.head[ field ], tail ) )

def parse_action( word_cursor, environment ):
	return parse_action2( take_word( word_cursor ), word_cursor, environment )

def parse_action2( start_word_sharp, word_cursor, environment ):
	if start_word_sharp is take_failed:
		return null
	else:
		if start_word_sharp != 'to#':
			raise ParseError( "Expected action declaration to begin with 'to'; found %r" % start_word_sharp )
		name_sharp = take_word( word_cursor )
		arg_list = LIST( FORMAL_ARG( null, flat( name_sharp ) ), parse_arg_list( word_cursor ) )
		arg_names = arg_stack( arg_list, 'name' )
		arg_types = arg_stack( arg_list, 'symbol' )
		result = parse_action3( name_sharp, take_word( word_cursor ), arg_names, arg_types, word_cursor, environment )
		debug_parse( "Parsed %s", result )
		return result

def parse_action3( name_sharp, kind_sharp, arg_names, arg_types, word_cursor, environment ):
	debug_parse( "Action keyword: %r", kind_sharp )
	if kind_sharp == 'do#':
		script = parse_script( word_cursor )
		debug_parse( "Parsed script: %r", python_list( script ) )
		result = MACRO( flat( name_sharp ), script, arg_names, arg_types, environment )
	elif kind_sharp == 'python_exec#':
		code = flat( take_word( word_cursor ) ).strip()
		debug_parse( "Code: %r", code )
		def builtin_exec( th, args ):
			#debug( "builtin_exec running: %r", code )
			exec code in globals(), args
		result = PRIMITIVE( flat( name_sharp ), builtin_exec, arg_names, arg_types, environment )
	elif kind_sharp == 'python_eval#':
		code = flat( take_word( word_cursor ) ).strip()
		debug_parse( "Code: %r", code )
		def builtin_eval( th, args ):
			#debug( "builtin_eval running: %r", code )
			result = eval( code, globals(), args )
			end_digression_if_finished( th.activation, th.activation.cursor.tokens )
			th.activation[ 'cursor' ] = DIGRESSION( List([ result ]), th.activation.cursor.environment, th.activation.cursor )
		result = PRIMITIVE( flat( name_sharp ), builtin_eval, arg_names, arg_types, environment )
	else:
		raise ParseError( "Expected 'do', 'python_eval', or 'python_exec'; found %r" % kind_sharp )
	return result

def parse_library( name, string, environment ):
	debug_parse( "parse_library( %r, %r, %r )", name, string, environment )
	bindings = environment.bindings
	word_cursor = Object( 'WORD_CURSOR', current = library_words( string ) )
	while bind_action( bindings, parse_action( word_cursor, environment ) ):
		pass
	automaton = polymorphic_automaton( bindings )
	return LIBRARY( name, automaton, bindings )

def bind_action( bindings, action ):
	if action is null:
		return False
	else:
		action_symbol = 'ACTION_%d' % action._id
		bindings[ action_symbol ] = action
		debug_parse( "Bound %r to %s", action_symbol, action )
		return True

def polymorphic_automaton( action_bindings ):
	# A little silly parser generator algorithm to deal with simple
	# multi-dispatch LR(0) languages using prefix notation and offering very
	# little error detection.
	# This will suffice until I write a proper parser generator.
	debug_ppa = silence
	debug_ppa( "Building automaton" )
	initial_state = Shift()
	shift_states = [ initial_state ]
	debug_ppa( "  initial_state: %s", initial_state )
	debug_ppa( "  action_bindings: %s", action_bindings )
	name_states = {}

	debug_ppa( "  Building parse tree" )
	for ( action_symbol, action ) in action_bindings:
		name = action.name
		arg_types = list( reversed( python_list( action.formal_arg_types ) ) )
		cur_state = initial_state
		#debug_ppa( '    %r @ %r', name, cur_state )
		debug_ppa( '    %r %r', action, arg_types )
		for tp in arg_types[:-1]:
			try:
				cur_state = cur_state[ tp ]
				debug_ppa( '      %r -> %r', tp, cur_state )
			except KeyError:
				next_state = Shift()
				cur_state[ tp ] = next_state
				shift_states.append( next_state )
				cur_state = next_state
				debug_ppa( '      %r => %r', tp, cur_state )

		reduce_state = Reduce0( action_symbol )
		cur_state[ arg_types[-1] ] = reduce_state
		debug_ppa( '      %r >> %r', arg_types[-1], reduce_state )
		name_states[ name ] = initial_state[ name ]

	# Names are effectively keywords.  Any time we see one of those, whatever shift state we are in, we leap to that name_state
	names = set([ action.name for ( symbol, action ) in action_bindings ])
	debug_ppa( "  Implementing keywords: %s", names )
	for name in names:
		target_state = name_states[ name ]
		debug_ppa( '    %s -> %r from %s', name, target_state, reprs( shift_states ) )
		for s in shift_states:
			s[ name ] = target_state

	# The accept state
	initial_state[ ':EOF' ] = Accept()
	debug_ppa( '  EOF => %s', initial_state[ ':EOF' ] )

	#debug_ppa( '  Automaton:\n%s\n', initial_state.description() )
	return initial_state

def parse_procedure( name, library_text, script ):
	env = ENVIRONMENT( null )
	define_builtins( env )
	lib = parse_library( name, library_text, env )
	define_predefined_bindings( env )
	result = PROCEDURE( name, List( script ), lib.dialect, env )
	return result


#####################################
#
# The interpreter.
#
#
# This is written in a slightly odd style to make it look as much as possible
# like the Sheppard meta-interpreter.  There's some inevitable dual-maintenance
# there, so we want them to look as similar as possible to make them easy to
# compare.
#
# Some rules to make it look more like Sheppard code:
#  - Procedures are only allowed one if statement sequence.  It must be at the
#    top level, and must use is_a based on the arguments or compare the
#    argument against a specific symbol.  This represents sheppard
#    automaton-based dispatch.  (For singletons like null and take_failed, we
#    can compare against object identity for performance reasons.)
#  - Loops will be replaced with tail digression.  There's only one loop anyway
#    so that's no big deal.
#
# Because no general if-statements are allowed (Sheppard has none yet), we
# adopt an unusual style in which all conditionals are turned into dispatches.
# A routine foo computes some value whose type determines what to do next, and
# passes that to a polymorphic routine foo2, using the dispatch mechanism to
# pick the right variant.
#

def pop_list( base, field_symbol_sharp ):
	"""Odd little routine that's just super handy for dealing with object fields that are stacks"""
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	#debug( "!!! pop_list( %s, %s ) = %s", base, field_symbol_sharp, result )
	give( base, field_symbol_sharp, take( current, 'tail#' ) )
	return result

def end_digression_if_finished( frame, remaining_tokens ):
	if remaining_tokens is null: #is_a( remaining_tokens, 'NULL' ):
		#debug_digressions( "  (finished %r %r %s)", frame.cursor, frame.cursor.environment, frame.cursor.environment.bindings  )
		frame[ 'cursor' ] = frame.cursor.resumption

def bind_arg( arg_bindings, arg_symbol_sharp, arg_value_sharp ):
	debug_bind = silence
	if arg_symbol_sharp is null: # is_a( arg_symbol_sharp, 'NULL' )
		#debug_bind( "    pop %r", flat( arg_value_sharp ) )
		pass
	else:
		assert( is_a( arg_symbol_sharp, 'SYMBOL' ) )
		give( arg_bindings, arg_symbol_sharp, arg_value_sharp )
		#debug_bind( "    %s=%r", flat( arg_symbol_sharp ), flat( arg_value_sharp ) )

def bind_args( frame, arg_bindings, formal_arg_names ):
	if formal_arg_names is null:
		pass
	else:
		frame[ 'history' ] = frame.history.tail
		bind_arg( arg_bindings, take( formal_arg_names, 'head#' ), pop_list( frame, 'operands#' ) )
		bind_args( frame, arg_bindings, formal_arg_names.tail )

def bound( obj_sharp, environment ):
	if environment is null:
		return obj_sharp
	else:
		return bound2( obj_sharp, environment, take( environment.bindings, obj_sharp ) )

def bound2( obj_sharp, environment, possible_match ):
	if possible_match is take_failed:
		return bound( obj_sharp, environment.outer )
	else:
		return possible_match

def next_state( state, obj_sharp ):
	# First we check if the object is itself a symbol that has an edge from this
	# state (ie. it's acting as a keyword).  Failing that, we check the object's
	# "tag edge symbol" to see if the object is of a type that has an edge.  If
	# that also fails, we default to the ":ANY" edge.  (This is where we should
	# be doing something smart for inheritance.)  These successive checks
	# necessitate not just one next_state2 routine, but another next_state3
	# routine too.
	return next_state2( state, obj_sharp, take( state, obj_sharp ) )

def next_state2( state, obj_sharp, possible_match ):
	if possible_match is take_failed:
		return next_state3( state, obj_sharp, take( state, tag_edge_symbol(obj_sharp) ) )
	else:
		return possible_match

def next_state3( state, obj_sharp, possible_match ):
	if possible_match is take_failed:
		try:
			return state[ ':ANY' ]
		except KeyError:
			# Raise a more descriptive Sheppard error
			raise Unexpected_token( state, obj_sharp )
	else:
		return possible_match


def call_function( frame, action ):
	if is_a( action, 'PRIMITIVE' ):
		action.function( frame.thread, dict( frame.cursor.environment.bindings ) )
	else:
		pass

def get_token( possible_token ):
	"""Transmute TAKE_FAILED into eof to make all token lists appear to end with an infinite stream of eofs"""
	if possible_token is take_failed:
		return sharp( eof )
	else:
		return possible_token

def perform_accept( frame, state ):
	debug( 'accept' )
	return false

shift_count = 0
def perform_shift( frame, state ):
	global shift_count
	shift_count += 1
	#debug_shift = silence
	#debug_shift( 'shift' )
	#debug_shift( "  cursor: %r", frame.cursor )
	#debug_shift( "  state: %s", state )
	token_sharp = bound( get_token( pop_list( frame.cursor, "tokens#" ) ), frame.cursor.environment )
	end_digression_if_finished( frame, frame.cursor.tokens )
	#debug_shift( "    value: %r", flat( token_sharp ) )
	frame[ 'operands' ] = cons( token_sharp, frame.operands )
	frame[ 'history' ] = cons( next_state( state, token_sharp ), frame.history )
	#debug_shift( "  new_state: %r", state )
	if list_length( frame.operands ) > 50:
		error( RuntimeError( "Operand stack overflow" ) )
	return true

def perform_reduce0( frame, state ):
	if meta_level( frame.thread ) >= printing_level_threshold:
		debug_reduce = debug
		debug2_reduce = silence
	else:
		debug_reduce = silence
		debug2_reduce = silence
	print_stuff( frame.thread )
	#debug2_reduce( ">-- reduce0 %s --", state.action )
	action = bound( take( state, 'action#' ), frame.scope ) # 'take' here just to get a sharp result
	#debug2_reduce( "  action: %r", action )
	#if is_a( action, 'MACRO' ):
	#	debug_reduce( "    %s", python_list( action.script ) )
	reduce_environment = ENVIRONMENT( action.environment )
	reduce_environment[ 'digressor' ] = frame.cursor.environment  # Need this in order to make 'set' a macro, or else I can't access the environment I'm trying to bind
	bind_args( frame, reduce_environment.bindings, action.formal_arg_names )
	print_reduce_stuff( frame.thread, action, reduce_environment )
	#debug2_reduce( "  environment: %s", reduce_environment )
	#debug2_reduce( "    based on: %s", frame.cursor )
	frame[ 'cursor' ] = DIGRESSION( action.script, reduce_environment, frame.cursor )
	call_function( frame, action )
	end_digression_if_finished( frame, frame.cursor.tokens )
	print_program( frame.thread )
	return true

perform = {
	'ACCEPT':  perform_accept,
	'SHIFT':   perform_shift,
	'REDUCE0': perform_reduce0,
	}

def execute( procedure, environment, scope ):
	frame = ACTIVATION( DIGRESSION( procedure.script, environment, dial_tone ), null, LIST( procedure.dialect, null ), scope, null )
	global current_thread # Allow us to print debug info without passing this all over the place
	current_thread = THREAD( frame, null )
	frame[ 'thread' ] = current_thread # I don't love this back link, but it's really handy and efficient
	debug( "starting thread: %r with digression:\n\t%s", current_thread, frame.cursor )
	print_program( current_thread )
	execute2( frame, true )

def execute2( frame, keep_going ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while keep_going == true: #is_a( keep_going, 'TRUE' ):
		#print_stuff( frame.thread )
		perform_func = perform[ tag( frame.history.head ) ]
		keep_going = perform_func( frame, frame.history.head )


#####################################
#
# Meta-interpreter
#

meta_interpreter_text = """
to Procedure
	name script dialect environment
python_eval
	{ PROCEDURE(**dict( locals() )) }

to Digression
	tokens environment resumption
python_eval
	{ DIGRESSION(**dict( locals() )) }

to Activation
	cursor operands history scope caller
python_eval
	{ ACTIVATION(**dict( locals() )) }

to Thread
	activation meta_thread
python_eval
	{ THREAD(**dict( locals() )) }

to Environment
	outer
python_eval
	{ ENVIRONMENT(**dict( locals() )) }

to tag_edge_symbol
	obj
python_eval
	{ ':' + tag( obj ) + '#' }

to pop_list 
	base field_symbol_sharp:SYMBOL
python_eval
	{ pop_list( base, field_symbol_sharp ) }


to end_digression_if_finished 
	frame remaining_tokens:NULL
do
	put frame cursor
		get2 frame cursor resumption

to end_digression_if_finished 
	frame remaining_tokens
do


to bind_arg
	arg_bindings arg_symbol_sharp:NULL arg_value_sharp
do

to bind_arg 
	arg_bindings arg_symbol_sharp:SYMBOL arg_value_sharp
do
	give arg_bindings arg_symbol_sharp arg_value_sharp


to bind_args 
	frame arg_bindings formal_arg_names:NULL
do

to bind_args 
	frame arg_bindings formal_arg_names:LIST
do
	put frame history
		get2 frame history tail
	bind_arg
		arg_bindings
		take formal_arg_names head#
		pop_list frame operands#
	bind_args
		frame
		arg_bindings
		get formal_arg_names tail


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


to bound2 
	obj_sharp environment possible_match
do
	possible_match

to bound2 
	obj_sharp environment x=TAKE_FAILED
do
	bound
		obj_sharp
		get environment outer


to next_state 
	state obj_sharp
do
	next_state2
		state
		obj_sharp
		take state obj_sharp


to next_state2 
	state obj_sharp possible_match
do
	possible_match

to next_state2 
	state obj_sharp x=TAKE_FAILED
do
	next_state3
		state
		obj_sharp
		take
			state
			tag_edge_symbol obj_sharp


to next_state3 
	state obj_sharp possible_match
do
	possible_match

to next_state3 
	state obj_sharp x=TAKE_FAILED
do
	get state :ANY


to call_function 
	frame action:PRIMITIVE
python_exec
	{ action.function( frame.thread, dict( frame.cursor.environment.bindings ) ) }

to call_function 
	frame action:MACRO
do
	/* Macros have no function.  They just expand into a digression, and that's it. */

/* Behave as though every token list ends with an infinite sequence of eofs */
to get_token possible_token do possible_token
to get_token x=TAKE_FAILED  do eof


to perform 
	frame state:ACCEPT
do
	false

to perform 
	frame state:SHIFT
do
	set token_sharp
		bound
			get_token
				pop_list
					get frame cursor
					tokens#
			get2 frame cursor environment
	end_digression_if_finished
		frame
		get2 frame cursor tokens
	put frame operands
		cons
			token_sharp
			get frame operands
	put frame history
		cons
			next_state state token_sharp
			get frame history
	true

to perform 
	frame state:REDUCE0
do
	print_stuff frame
	set action
		bound
			take state action#
			get frame scope  /* TODO: Does this make sense?  Should there be a specific action_bindings object? */
	set reduce_environment
		Environment
			get action environment
	put reduce_environment digressor
		get2 frame cursor environment
	bind_args
		frame
		get reduce_environment bindings
		get action formal_arg_names
	print_reduce_stuff frame action reduce_environment
	put frame cursor
		Digression
			get action script
			reduce_environment
			get frame cursor
	call_function
		frame
		action
	end_digression_if_finished
		frame
		get2 frame cursor tokens
	print_program frame
	true


to print_stuff        frame                             python_exec { print_stuff( frame.thread ) }
to print_reduce_stuff frame action reduce_environment   python_exec { print_reduce_stuff( frame.thread, action, reduce_environment ) }
to print_program      frame                             python_exec { print_program( frame.thread ) }
to print              obj                               python_exec { print str( obj ) }


to execute 
	procedure environment scope
do
	set frame
		Activation
			Digression
				get procedure script
				environment
				dial_tone
			null  /* Empty operand stack */
			cons
				get procedure dialect  /* Start state */
				null
			scope
			null  /* No caller */
	put frame thread  /* Putting a thread pointer in each frame seems wasteful, but it's handy */
		Thread frame current_thread
	print_program frame
	execute2 frame true


to execute2 
	frame keep_going:FALSE
do

to execute2 
	frame keep_going:TRUE
do
	execute2
		frame
		perform
			frame
			get2 frame history head

"""

# Implementing a primitive with eval/exec requires three additional shifts
# compared with a builtin.  In some cases, that's enough to make the
# interpreter much slower, so we just leave them as builtins for now.
#
not_used_because_they_are_too_slow = """
to pop_list 
	base field_symbol_sharp
do
	set current
		take base field_symbol_sharp
	set result
		take current head#
	give base field_symbol_sharp
		take current tail#
	result
"""


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

fib_text = """
to fib  n=0  do 1
to fib  n=1  do 1

to fib  n  do
	+
		fib - n 1
		fib - n 2

to + a b   python_eval { a+b }
to - a b   python_eval { a-b }

to print_result  n  python_exec { print "*** RESULT IS", n, "***" }
"""

def fib_procedure():
	return parse_procedure( "fib", fib_text, [ 'print_result', 'fib', 2 ] )

sheppard_interpreter_library = None
def wrap_procedure( inner_procedure ):
	global global_scope, sheppard_interpreter_library
	if sheppard_interpreter_library is None:
		global_scope = ENVIRONMENT( null )
		define_builtins( global_scope )
		sheppard_interpreter_library = parse_library( "sheppard_interpreter", prologue_text + meta_interpreter_text, global_scope )
		define_predefined_bindings( global_scope )
	bindings = global_scope.bindings
	script = List([ 'execute', inner_procedure, ENVIRONMENT( inner_procedure.environment ), inner_procedure.environment ])
	outer_procedure = PROCEDURE( 'meta_' + inner_procedure.name, script, sheppard_interpreter_library.dialect, global_scope )
	return outer_procedure

def test( depth, plt ):
	global printing_level_threshold
	#procedure = go_world()
	procedure = fib_procedure()
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

if False:
	import cProfile
	cProfile.run( "main()", None, "time" )
else:
	main()

