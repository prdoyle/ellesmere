#! /usr/bin/python -O
# vim: set fileencoding=utf-8 :

import string, re, time, ast
from sys import argv

# I've adopted a convention that strings used as Sheppard monikers (or parts
# thereof) have single-quotes, and ordinary Python strings have double-quotes.
# Since Python sees no difference between these, there's no way to enforce this
# rule, so you may find some unintentional inconsistencies.

debug_indent_level = 0
def debug( message, *args ):
	if args:
		message = message % args
	print debug_indent_level * "| " + message

def silence( message, *args ):
	pass

def debug_indent( delta=1 ):
	global debug_indent_level
	debug_indent_level += delta

def error( th, exception ):
	debug( "!! ERROR: %s", exception )
	print_backtrace( th )
	raise exception

test_shogun = False

def reprs( xs ):
	return "[ " + string.join([ repr(x) for x in xs ], ", ") + " ]"

def symbol_str( obj ):
	global identifier_re
	identifier_re = re.compile(r":*[A-Za-z]\w*#*")
	if isinstance( obj, int ):
		return str( obj )
	elif isinstance( obj, str ):
		if identifier_re.match( obj ):
			return obj
		else:
			return repr( obj )
	else:
		# Could be an ANON etc.
		return repr( obj )

def smart_order( key1, key2 ):
	# One day, this is supposed to grow into something that sorts the numeric
	# parts of a string numerically instead of alphabetically...  One day...
	global smart_splitter
	smart_splitter = re.compile(r"(.*[^0-9])(.*)$")
	try:
		split1 = smart_splitter.match( key1 ).groups()
		split2 = smart_splitter.match( key2 ).groups()
		result = cmp( split1, split2 )
		return result
	except TypeError:
		return 0

def memoized( func ):
	cache = {}
	def wrapper( arg ):
		try:
			return cache[ arg ]
		except KeyError:
			result = func( arg )
			cache[ arg ] = result
			return result
	return lambda x: wrapper( x )

# Calls to these are often commented out below to improve performance.  "Enabling" these here may not be enough.
#debug_digressions = debug
#debug_object = debug

object_counter = 0
tag_counters = None # {}

class Object( dict ):
	"""A Sheppard object is based on a Python dict with extra goodies"""

	def __init__( _self, _tag, **edges ):
		global object_counter, tag_counters
		assert( _tag.isupper() )
		_self._tag = _tag
		if tag_counters is not None:
			tag_counters[ _tag ] = 1 + tag_counters.get( _tag, 0 )
		_self._id = object_counter
		_self._fields = []
		object_counter += 1
		for ( name, value ) in edges.iteritems():
			_self[ name ] = value
		_self._fields = sorted( _self._fields ) # _fields can be adjusted if we want a particular field ordering

	# Note that this is not in sharp-land.  When we're interpreting be_python or
	# do_python, the code needs to act like Python or it will be too confusing.

	def __getattr__( self, key ):
		try:
			return self[ key ]
		except KeyError:
			# Why not check the built-in dict attributes for convenience.  Might as well.
			# This has the side-benefit of throwing the right exception if there's no such attribute.
			#
			# HMM, this doesn't actually work...
			#return dict.__getattr__( self, key )
			raise AttributeError( repr(key) )

	def __setattr__( self, key, value ):
		if key[0] == "_":
			dict.__setattr__( self, key, value )
		else:
			self[ key ] = value

	def __delattr__( self, key ):
		if key[0] == "_":
			dict.__delattr__( self, key )
		else:
			try:
				del self[ key ]
			except KeyError:
				# Let's not offer to delete the built-in attributes of dict
				raise AttributeError( repr(key) )

	def __setitem__( self, key, value ):
		assert( not isinstance( key, str ) or key[0] != "_" )
		if not dict.__contains__( self, key ):
			assert( key not in self._fields )
			self._fields.append( key )
		dict.__setitem__( self, key, value )

	#def __iter__( self ):
	#	raise TypeError( "Object %r does not implement iteration" % self )

	def __cmp__( self, other ):
		raise TypeError( "Object %r does not implement comparison" % self )

	_iterkeys    = dict.iterkeys
	_itervalues  = dict.itervalues
	_iteritems   = dict.iteritems

	def __repr__( self ):
		try:
			return self._name
		except AttributeError:
			pass
		if is_a( self, 'SHARPED_SYMBOL' ):
			try:
				return repr( self.value ) + "#"
			except AttributeError:
				# This can happen if we have debug output enabled during the construction of a SHARPED_SYMBOL
				pass
		return "%s_%d" % ( self._tag, self._id )

	def __str__( self ):
		suffix = ""
		return repr( self ) + "{ " + string.join([ "%s=%s" % ( field, self._short_description( self[field] ) ) for field in ( self._fields ) ], ', ') + " }"

	def _description( self, indent=0 ):
		work_queue = []
		already_described = set()
		result = self._description_impl( work_queue, already_described, indent )
		while work_queue:
			next_item = work_queue.pop(0)
			if next_item not in already_described:
				result = result + ";\n" + next_item._description_impl( work_queue, already_described, indent )
		return result

	def _description_impl( self, work_queue, already_described, indent ):
		debug_description = silence
		debug_description( "description( %r )", self )
		if self is null:
			return "null"
		elif self in already_described:
			debug_description( "%r alredy described", self )
			return repr( self )
		else:
			already_described.add( self )
			debug_description( "   %r is being described", self )
			indent_str = " |" * ( indent+1 )
			if self._expand_children():
				children_strings = [ "\n%s%s=%s" % ( indent_str, symbol_str( key ), self._long_description( value, already_described, work_queue, indent+1 ) ) for ( key, value ) in self ]
			else:
				keys = [ key for ( key, value ) in self._iteritems() ]
				debug_description( "  Keys of %r: %s", self, string.join([ "%r" % k for k in keys ], ', ') )
				debug_description( "  Values of %r: %s", self, string.join([ "%r" % self[k] for k in keys ], ', ') )
				keys.sort( smart_order )

				children = [ value for ( key, value ) in self._iteritems() if isinstance( value, Object ) ]
				debug_description( "  Children of %r: %s", self, string.join([ "%r" % c for c in children ], ', ') )
				if len( children ) <= 6:
					children_strings = [ " %s=%s" % ( symbol_str( key ), self._short_description( self[ key ] ) ) for key in keys ]
				else:
					children_strings = [ "\n%s%s=%s" % ( indent_str, symbol_str( key ), self._short_description( self[ key ] ) ) for key in keys ]
				work_queue += children
			result = " |" * indent + repr( self ) + "{" + string.join( children_strings, ',' ) + " }"
			return result

	def _expand_children( self ):
		return False

	def _long_description( self, value, already_described, work_queue, indent ):
		if isinstance( value, Object ):
			return value._description_impl( work_queue, already_described, indent )
		else:
			return repr( value )

	def _short_description( self, value ):
		try:
			return "<<%s>>" % value.__name__
		except AttributeError:
			pass
		if isinstance( value, Object ) or isinstance( value, str ):
			return repr( value )
		else:
			return str( value )

	# null and zero are false; all else are true

	def __nonzero__( self ): return self is not 0 and self is not null

	# Most Sheppard objects can't be dict keys (i.e. they are not symbols)

	def __hash__( self ):
		if True:
			return self._id # nfa2dfa needs sets of objects, so now we need to hash them
		# Prevent most Sheppard objects from being hashed.  We raise KeyError to
		# make getitem and setitem act like they don't have data for the given
		# (illegal) "key" object, which I think is usually how we want them to behave.
		raise KeyError( '%r is not a symbol' % self )

	def __eq__( self, other ):
		if True:
			return self is other
		raise TypeError( "Object %r does not implement comparison" % self )

	# Note: I'd also like to un-define __eq__ because I think having __eq__
	# default to identity comparison may be unwise.  However, it's so damn handy
	# in Python that un-defining __eq__ would cause undue hardship.

class Null( Object ): # Just to make debugging messages more informative

	def __init__( self ):
		Object.__init__( self, 'NULL' )

	def __repr__( self ):
		return "null"

	def __str__( self ):
		return "null"

	def __iter__( self ):
		if False:
			yield False

	def _links( self ):
		if False:
			yield False

null = Null()  # The very first object!  It gets _id=0

class Anonymous_symbol( Object ):

	def __init__( self, pseudonym=None ):
		Object.__init__( self, "ANON" )
		if pseudonym is not None:
			self.pseudonym = pseudonym
			self._name = "%s#%d" % ( pseudonym, self._id )

	def __hash__( self ): return id( self )

	def __eq__( self, other ): return self is other

def ANON( pseudonym=None ): return Anonymous_symbol( pseudonym )

# These need to be disembodied functions.  They can't be methods, because some
# Sheppard objects like monikers are represented by plain old Python objects like
# strings that have no such methods.

def is_int( obj ):     return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_moniker( obj ): return isinstance( obj, str ) # Sheppard monikers are represented by Python strs

def all_fields( obj ):
	if isinstance( obj, Object ):
		fields = obj._fields[:]
		fields.sort()
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
		if isinstance( obj, str ): #is_moniker( obj ):
			result = 'MONIKER'
		elif isinstance( obj, int ): #is_int( obj ):
			result = 'INT'
		else: # All other Sheppard objects are represented by instances of Object
			result = obj._tag
	assert( result.isupper() )
	return result

def take_tag( obj_sharp ):
	return tag( flat( obj_sharp ) )

def is_a( obj, t ):
	assert( t.isupper() )
	return tag( obj ) == t # TODO: inheritance

def sharp( arg ):
	"""
	This is a way of quoting a symbol to protect it from being interpreted.

	Invariant: flat(sharp( X )) is x
	"""
	if isinstance( arg, str ): #if is_a( arg, 'MONIKER' ):
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
		return arg + '#'  # Hmm, should this be considered a SHARPED_SYMBOL?  Right now it would just be another MONIKER.  I guess that's a job for is_a once it handles inheritance.
	elif is_a( arg, 'INT' ) or is_a( arg, 'ANON' ):
		return Sharped_symbol( arg )
	else:
		return arg

def flat( arg ):
	if isinstance( arg, str ): #if is_a( arg, 'MONIKER' ):
		assert( arg[-1] == '#' )
		return arg[:-1]
	elif is_a( arg, 'SHARPED_SYMBOL' ):
		#debug( "flat( %s ) = %s", arg, arg.value )
		return arg.value
	else:
		return arg

def Sharped_symbol( arg ):
	return Object( 'SHARPED_SYMBOL', value=arg )
Sharped_symbol = memoized( Sharped_symbol )

#####################################
#
# Serialization
#
# "Shogun" = Sheppard object graph UTF-8 notation
#

debug_shogun   = silence
debug_deshogun = silence
sanity_limit = 999

def get_index( obj, obj_list ):
	try:
		return obj_list.index( obj )
	except ValueError:
		obj_list.append( obj )
		return len( obj_list ) - 1

shogun_predefined_objects = None # Lazy init
def init_shogun_predefined_objects():
	global shogun_predefined_objects, shogun_predefined_names_by_object
	shogun_predefined_objects = dict()
	add_predefined_bindings( shogun_predefined_objects )
	shogun_predefined_names_by_object = dict([ (v,k) for (k,v) in shogun_predefined_objects.iteritems() ])

def shogun( obj ):
	"""Sheppard Object Graph Unicode Notation"""
	debug_shogun( "shogun( %r )", obj )
	init_shogun_predefined_objects()
	objects = [ obj ]
	result = "{\n"
	i = 0
	while i < len(objects):
		obj = objects[ i ]
		if isinstance( obj, int ) or isinstance( obj, str ):
			result += "\t%r : %r\n" % ( i, obj )
		elif tag( obj ) == 'LIST':
			try:
				result += "\t%r : [%s]\n" % ( i, ", ".join( shogun_list_elements( obj, objects, sanity_limit ) ) )
			except NotAListError:
				result += "\t%r : %s" % ( i, shogun_object_by_fields( obj, objects ) )
		else:
			result += "\t%r : %s" % ( i, shogun_object_by_fields( obj, objects ) )
		i += 1
	return result + "}[0]"

class ParseError( Exception ):
	pass

class NotAListError( ParseError ):
	pass

class NotASimpleValueError( ParseError ):
	pass

def shogun_list_elements( obj, objects, length_limit ):
	# length_limit is a lame way to avoid stack overflow (or even infinite recursion on a cyclic list)
	debug_shogun( "shogun_list_elements( %r )", obj )
	if length_limit < 0:
		raise NotAListError
	elif obj is null:
		return []
	elif tag( obj ) == 'LIST':
		try:
			# Can't do relocations in here yet, so we don't want "@" references, so pass None as the objects list
			debug_shogun( "  Checking for %r.head", obj )
			value_str = shogun_value( obj.head, None )
			debug_shogun( "  Recursing for %r.tail", obj )
			return [ value_str ] + shogun_list_elements( obj.tail, objects, length_limit-1 )
		except AttributeError:
			raise NotAListError
	else:
		raise NotAListError

def shogun_object_by_fields( obj, objects ):
	debug_shogun( "shogun_object_by_fields( %r )", obj )
	result = "%s {\n" % tag( obj ) # TODO: What if the tag is really weird?
	fields = [ f for (f,v) in obj._iteritems() ]
	fields.sort( smart_order )
	for field in fields:
		value = obj[ field ]
		result += "\t\t%r : %s\n" % ( field, shogun_value( value, objects ) )
	result += "\t}\n"
	return result

def shogun_value( value, objects ):
	debug_shogun( "shogun_value( %r )", value )
	if value in shogun_predefined_names_by_object:
		return shogun_predefined_names_by_object[ value ]
	elif isinstance( value, int ) or isinstance( value, str ):
		return repr( value )
	elif tag( value ) == 'LIST':
		try:
			return "[ %s ]" % " , ".join( shogun_list_elements( value, objects, sanity_limit ) )
		except NotAListError:
			pass
	# Default
	return "@%r" % get_index( value, objects )

def consume( expectation, words ):
	word = words.pop()
	if word != expectation:
		words = words[:]
		words.reverse()
		raise ParseError( "Encountered %r instead of %r before %r" % ( word, expectation, words ) )

def deshogun( text ):
	# Note that we prime the dictionary with the null object.  We don't want
	# just any instance of NULL: we want the same "mull" that the rest of the
	# program will be using.  Without special handling, we can easily end up
	# with multiple instances of NULL floating around.
	#
	# HOWEVER, this is not sufficient.  Other singletons (notably those from
	# add_predefined_bindings) might need the same treatment, and I'm not
	# sure what else.
	#
	init_shogun_predefined_objects()
	dictionary = { }

	relocations = []
	words = re.findall( r"'[^']*'|@\d+|\d+|[{}:]|\[|\]|'.*'|\".*\"|\S+", text )
	debug_deshogun( "Words:\n%r", words )
	words.reverse()
	consume( "{", words )
	while words[-1] != "}":
		key = deshogun_symbol( words.pop() )
		debug_deshogun( "key is %r", key )
		consume( ":", words )
		item = words.pop()
		debug_deshogun( "item is %r", item )
		debug_indent()
		try:
			dictionary[ key ] = deshogun_simple_value( item, words )
		except NotASimpleValueError, e:
			try:
				consume( "{", words )
			except ParseError:
				raise e # Re-raise the exception that brought us here
			obj = Object( item )
			dictionary[ key ] = obj
			debug_deshogun( "created [ %r ] = %r", key, obj )
			while words[-1] != "}":
				field = words.pop()
				if field[0] in [ "'", '"' ]:
					field = ast.literal_eval( field )
				else:
					field = deshogun_symbol( field )
				debug_deshogun( "field is %r", field )
				consume( ":", words )
				value = words.pop()
				debug_deshogun( "value is %r", value )
				try:
					obj[ field ] = deshogun_simple_value( value, words )
				except NotASimpleValueError, e:
					if value[0] == "@":
						relocations.append(( obj, field, int( value[1:] ) ))
					else:
						raise e
			consume( "}", words )
		debug_indent(-1)
	consume( "}", words )
	for ( obj, key, index ) in relocations:
		debug_deshogun( "Relocation %r[ %r ] = @%d %r", obj, key, index, dictionary[ index ] )
		obj[ key ] = dictionary[ index ]
	consume( "[", words )
	result = dictionary[ maybe_int( words.pop() ) ]
	consume( "]", words )
	return result

anon_symbol_name_regex = None
def deshogun_symbol( word ):
	debug_deshogun( "deshogun_symbol( %r )", word )
	global anon_symbol_name_regex
	if not anon_symbol_name_regex:
		anon_symbol_name_regex = re.compile(r"([A-Za-z_0-9]+)#\d+")
	match = anon_symbol_name_regex.match( word )
	if match:
		# NOTE: This doesn't actually work properly.  We need to get the right
		# identity for ANONs.  We probably need them to be references to the
		# top-level array, complete with relocations etc.
		debug_deshogun( "  ANON" )
		return ANON( match.group(1) )
	elif word in shogun_predefined_objects:
		# TODO: Assert that the predefined object is a symbol
		return shogun_predefined_objects[ word ]
	else:
		# TODO: I'm not thrilled about this behaviour.  A symbol like abc can
		# suddenly change meaning if abc becomes a predefined object.  Quotes
		# should be mandatory.
		debug_deshogun( "  normal" )
		return maybe_int( word )

def deshogun_simple_value( word, words ):
	debug_deshogun( "deshogun_simple_value( %r )", word )
	if word in shogun_predefined_objects:
		return shogun_predefined_objects[ word ]
	elif word[0].isdigit():
		return int( word )
	elif word[0] in [ "'", '"' ]:
		return ast.literal_eval( word )
	elif word == "@0": # SPECIAL CASE FOR null
		return null
	elif word == "[":
		list_words = pop_list_words( words )
		debug_deshogun( "  list_words: %r" % list_words )
		# This almost works, but not quite, because we have no way to do relocations for "@" references
		list_values = [ deshogun_simple_value( word, words ) for word in list_words if word != "," ]
		value = List( list_values )
		debug_deshogun( "  result: %r" % value )
		return value
	else:
		raise NotASimpleValueError( "Value word %r must start with digit, quote, apostrophe, or bracket" % word )

def pop_list_words( words, depth=1 ):
	result = []
	while words:
		word = words.pop()
		if word == "]":
			depth -= 1
			if depth <= 0:
				return result
		elif word == "[":
			depth += 1
		result.append( word )
	return result


# Object constructors

class List_object( Object ):
	"""Allows Python code to use list comprehensions on LIST objects"""

	def __iter__( self ):
		current = self
		while current is not null:
			yield current.head
			current = current.tail

	def _links( self ):
		current = self
		while current is not null:
			yield current
			current = current.tail

def LIST( head, tail ):
	#debug( "LIST( %r )", head )
	result = List_object( 'LIST', head=head, tail=tail )
	result._fields = ['head','tail'] # We want them in this order just for readability
	return result

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

def ENVIRONMENT( outer, bindings=None ): return Object( 'ENVIRONMENT', outer=outer, bindings=( bindings or Object('BINDINGS') ) )

def DIGRESSION( tokens, local_scope, resumption ): return Object( 'DIGRESSION', tokens=tokens, local_scope=local_scope, resumption=resumption )

eof = Object( 'EOF' )

def Dial_tone():
	# An endless stack of digressions each returning an endless stream of EOFs
	result = Object( 'DIAL_TONE', local_scope=ENVIRONMENT( null ) )
	endless_eof = LIST( eof, null )
	endless_eof[ 'tail' ] = endless_eof # oooo, tricky
	result[ 'tokens' ] = endless_eof
	result[ 'resumption' ] = result # oooo, equally tricky
	return result

dial_tone = Dial_tone()
false = Object( 'FALSE' )
true  = Object( 'TRUE' )

def ACTIVATION( cursor, operands, history, action_bindings, fallback_function, caller ): return Object( 'ACTIVATION', cursor=cursor, operands=operands, history=history, action_bindings=action_bindings, fallback_function=fallback_function, caller=caller )
def THREAD( activation, meta_thread ): return Object( 'THREAD', activation=activation, meta_thread=meta_thread )
def AUTOMATON( initial_state, fallback_function ): return Object( 'AUTOMATON', initial_state=initial_state, fallback_function=fallback_function )

# I've stuffed action_bindings in here for convenience.  Not sure that's wise.
def PROCEDURE( name, script, automaton, action_bindings, enclosing_scope ): return Object( 'PROCEDURE', name=name, script=script, automaton=automaton, action_bindings=action_bindings, enclosing_scope=enclosing_scope )

def FALLBACK_FUNCTION( **bindings ):
	result = Object( 'FALLBACK_FUNCTION', **bindings )
	add_predefined_fallbacks( result )
	return result

def MACRO( script, formal_arg_names, enclosing_scope ):
	return Object( 'MACRO', script=script, formal_arg_names=formal_arg_names, enclosing_scope=enclosing_scope )
def PYTHON_EVAL( expression, formal_arg_names, enclosing_scope ):
	return Object( 'PYTHON_EVAL', expression=expression, formal_arg_names=formal_arg_names, enclosing_scope=enclosing_scope )
def PYTHON_EXEC( statement, formal_arg_names, enclosing_scope ):
	return Object( 'PYTHON_EXEC', statement=statement, formal_arg_names=formal_arg_names, enclosing_scope=enclosing_scope )

def Reduce( action_symbol ): return Object( 'REDUCE', action_symbol=action_symbol )
def Accept(): return Object( 'ACCEPT' )
def Shift(): return Object( 'SHIFT' )
def Lookahead1(): return Object( 'LOOKAHEAD1' )

def LIBRARY( name, grammar, action_bindings ): return Object( 'LIBRARY', name=name, grammar=grammar, action_bindings=action_bindings )

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

# A few functions to help with the Python / Sheppard impedance mismatch

def python_list( sheppard_list, head='head', tail='tail' ):
	if sheppard_list:
		return [ sheppard_list[ head ] ] + python_list( sheppard_list[ tail ], head, tail )
	else:
		return []

def list_str( stack, sep=", " ):
	return string.join([ repr(s) for s in python_list( stack )], sep )

def stack_str( lst, sep=", ", ellision_limit=999 ):
	pl = python_list( lst )
	prefix = ''
	if len( pl ) > ellision_limit:
		pl = pl[ : ellision_limit-2 ]
		prefix = "... "
	return prefix + string.join([ repr(s) for s in reversed( pl )], sep )

# These functions operate in sharp-land, so they're safe to call directly from
# a Sheppard program without interfering with the execution of the automaton

def give( obj, key_sharp, value_sharp ):
	#debug( "--GIVE-- %d %r . %r = %r", id(obj), obj, flat(key_sharp), flat(value_sharp) )
	obj[ flat(key_sharp) ] = flat( value_sharp )

take_failed = 'TAKE_FAILED'  # TODO: Why is this uppercase if it's not a type?

def take( obj, key_sharp ):
	#debug( "--TAKE--    %r.take( %r )", obj, flat( key_sharp ) )
	try:
		return sharp( obj[ flat( key_sharp ) ] )
	except KeyError:
		return take_failed

def cons( head_sharp, tail ):
	return LIST( flat( head_sharp ), tail )

debug_ellision_limit=999

def cursor_description( cursor ):
	if cursor == dial_tone:
		return ''
	else:
		return string.join( [ repr(x) for x in python_list( cursor.tokens ) ], "  " ) + " . " + cursor_description( cursor.resumption )

printing_level_threshold=1
def print_program( th ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		debug( "# PROGRAM%d: %s ^ %s", meta_level(th), stack_str( frame.operands, "  ", debug_ellision_limit ), cursor_description( frame.cursor ) )

def print_stuff( th ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		#debug( "stack: %s", zip( python_list( frame.history ), python_list( frame.operands ) ) )
		print_program( th )
		debug( "|  history: %s", stack_str( frame.history, ':', debug_ellision_limit ) )
		debug( "|   future: %s", future_str( frame.cursor ) )
		#debug( "| bindings: %s", frame.cursor.local_scope.bindings )

def print_reduce_stuff( th, action, reduce_environment ):
	if meta_level( th ) >= printing_level_threshold:
		frame = th.activation
		debug( ">+  ACTION: %r %s with %s", action, getattr( action, 'keyword', "" ), reduce_environment.bindings )
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

def add_predefined_bindings( bindings ):
	bindings[ 'null' ] = null
	bindings[ 'eof' ] = eof
	bindings[ 'false' ] = false
	bindings[ 'true' ] = true
	bindings[ 'dial_tone' ] = dial_tone

	bindings[ 'epsilon' ] = epsilon
	bindings[ 'eta' ]     = eta

def add_predefined_fallbacks( fallback_function ):
	for x in [ 'MONIKER', 'INT', 'ANON', 'SHARPED_SYMBOL' ]:
		fallback_function[ x ] = 'SYMBOL'
	fallback_function[ 'DIAL_TONE' ] = 'DIGRESSION'

prologue_text = """
let take       base key_sharp               be_python << take( base, key_sharp ) >>
to  give       base key_sharp value_sharp   do_python << give( base, key_sharp, value_sharp ) >>
let get        base key:SYMBOL              be_python << base[ key ] >>
let get2       base f1 f2                   be_python << base[ f1 ][ f2 ] >>
to  put        base key:SYMBOL value        do_python << base[ key ] = value >>
let cons       head_sharp tail              be_python << cons( head_sharp, tail ) >>
let all_fields obj                          be_python << all_fields( obj ) >>
let sharp      symbol                       be_python << sharp( symbol ) >>
let take_tag   obj_sharp                    be_python << sharp( tag( flat( obj_sharp ) ) ) >>
let current_thread                          be_builtin_current_thread

/* This could really just be a binding */
let current_environment th:THREAD   be_python << th.activation.cursor.local_scope >>

to set        symbol:MONIKER value          do set2 symbol value current_thread
to set2       symbol:MONIKER value th       do_python << th.activation.cursor.local_scope.bindings[ symbol ] = value >>

/* Math */
let + a b   be_python << a+b >>
let - a b   be_python << a-b >>

"""

arithmetic_text = """
/* We cast all these to int in Python just to avoid really weird behaviour like 'some_symbol' * 100000 */

define :INT  a:INT + b:INT  as_python  << int(a) + int(b) >>  /* left_assoc(1) */
define :INT  a:INT - b:INT  as_python  << int(a) - int(b) >>  /* left_assoc(1) */
define :INT  a:INT * b:INT  as_python  << int(a) * int(b) >>  /* left_assoc(2) */
define :INT  a:INT / b:INT  as_python  << int(a) / int(b) >>  /* left_assoc(2) */
define :INT  a:INT % b:INT  as_python  << int(a) % int(b) >>  /* left_assoc(2) */
define :INT    ( a:INT )    as_python  << a >>
"""

prologue_text += arithmetic_text


#####################################
#
# Parsing
#

debug_parse = silence

def KEYWORD_PATTERN( formal_arg_types, action_symbol ): return Object( 'KEYWORD_PATTERN', formal_arg_types=formal_arg_types, action_symbol=action_symbol )
def KEYWORD_GRAMMAR( patterns ): return Object( 'KEYWORD_GRAMMAR', patterns=patterns )

def parse_library( name, string, enclosing_scope ):
	debug_parse( "parse_library( %r, %r, %r )", name, string, enclosing_scope )
	word_cursor = Object( 'WORD_CURSOR', current = library_words( string ) )
	action_bindings = Object( 'BINDINGS' )
	grammar = KEYWORD_GRAMMAR( null )
	definition = parse_definition( word_cursor, enclosing_scope, action_bindings )
	while definition:
		grammar.patterns = LIST( definition, grammar.patterns )
		definition = parse_definition( word_cursor, enclosing_scope, action_bindings )
	return LIBRARY( name, grammar, action_bindings )

def maybe_int( word ):
	try:
		return int( word, 0 )
	except( TypeError, ValueError ):
		return word

def unpack_word( word ):
	if word.startswith( "<<" ):
		return maybe_int( word[2:-2] )
	else:
		return maybe_int( word )

def library_words( library_text ):
	raw_words = re.findall( r'/\*.*?\*/|<<.*>>|\S+(?:/:?\w+#*)?', library_text )
	words = map( unpack_word, filter( lambda s: s[0:2] != '/*', raw_words ) )
	return List( words )

def FORMAL_ARG( name, symbol ): return Object( 'FORMAL_ARG', name=name, symbol=symbol )

def take_word( word_cursor ):
	if word_cursor.current is null:
		return take_failed
	result = pop_list( word_cursor, 'current#' )
	#debug( "take_word: %r", result )
	return result

def parse_arg_list( word_cursor, a_plain_word_is_a_name ):
	return parse_arg_list2( take( word_cursor.current, 'head#' ), word_cursor, a_plain_word_is_a_name )

def parse_arg_list2( lookahead_sharp, word_cursor, a_plain_word_is_a_name ):
	if lookahead_sharp in [ 'do#', 'be#', 'as#', 'do_python#', 'be_python#', 'as_python#', 'be_builtin_current_thread#' ]:
		debug_parse( "Ending arg list on %r", lookahead_sharp )
		return null
	else:
		word_sharp = take_word( word_cursor )
		if '=' in word_sharp:
			[ name, symbol ] = flat( word_sharp ).split( '=' )
			symbol = maybe_int( symbol )
		elif ':' in word_sharp:
			[ name, symbol ] = flat( word_sharp ).split( ':' )
			symbol = ':' + symbol
		elif a_plain_word_is_a_name:
			name = flat( word_sharp )
			symbol = ':ANY'
		else:
			name = null
			symbol = flat( word_sharp )
		debug_parse( '  arg %s %r', name, symbol )
		return LIST( FORMAL_ARG( name, symbol ), parse_arg_list( word_cursor, a_plain_word_is_a_name ) )

def parse_script( word_cursor ):
	return parse_script2( take( word_cursor.current, 'head#' ), word_cursor )

def parse_script2( lookahead_sharp, word_cursor ):
	if lookahead_sharp in [ 'to#', 'let#', take_failed ]:
		return null
	else:
		return cons( take_word( word_cursor ), parse_script( word_cursor ) )

def arg_stack( formal_args, field, tail=null ):
	if formal_args is null:
		return tail
	else:
		return arg_stack( formal_args.tail, field, LIST( formal_args.head[ field ], tail ) )

def arg_list( formal_args, field ):
	if formal_args is null:
		return null
	else:
		return LIST( formal_args.head[ field ], arg_list( formal_args.tail, field ) )

def parse_definition( word_cursor, enclosing_scope, action_bindings ):
	return parse_definition2( take_word( word_cursor ), word_cursor, enclosing_scope, action_bindings )

def parse_definition2( start_word_sharp, word_cursor, enclosing_scope, action_bindings ):
	if start_word_sharp is take_failed:
		return null
	else:
		if start_word_sharp == 'define#':
			lhs_sharp = take_word( word_cursor )
			formal_args = parse_arg_list( word_cursor, a_plain_word_is_a_name=False )
			rhs = arg_list ( formal_args, 'symbol' )
			arg_names = arg_stack( formal_args, 'name' )
			action = parse_action( take_word( word_cursor ), arg_names, word_cursor, enclosing_scope )
			action_symbol = bind_action( action_bindings, action )
			return PRODUCTION( flat( lhs_sharp ), rhs, action_symbol ) # HEY this gets rhs backward
		else:
			if start_word_sharp == 'let#':
				# How should we represent the return type?
				#type_name = take_word( word_cursor )
				pass
			elif start_word_sharp != 'to#':
				raise ParseError( "Expected action declaration to begin with 'to' or 'let'; found %r" % start_word_sharp )
			name_sharp = take_word( word_cursor )
			formal_args = LIST( FORMAL_ARG( null, flat( name_sharp ) ), parse_arg_list( word_cursor, a_plain_word_is_a_name=True ) )
			arg_names = arg_stack( formal_args, 'name' )
			arg_types = arg_stack( formal_args, 'symbol' )
			action = parse_action( take_word( word_cursor ), arg_names, word_cursor, enclosing_scope )
			action.keyword = flat( name_sharp )
			action_symbol = bind_action( action_bindings, action )
			return KEYWORD_PATTERN( arg_types, action_symbol )

def parse_action( kind_sharp, arg_names, word_cursor, enclosing_scope ):
	debug_parse( "Action keyword: %r", kind_sharp )
	if kind_sharp in [ 'do#', 'be#', 'as#' ]:
		script = parse_script( word_cursor )
		debug_parse( "Parsed script: %r", python_list( script ) )
		action = MACRO( script, arg_names, enclosing_scope )
	elif kind_sharp in [ 'be_python#', 'as_python#' ]:
		expression = flat( take_word( word_cursor ) ).strip()
		debug_parse( "Expression: %r", expression )
		action = PYTHON_EVAL( expression, arg_names, enclosing_scope )
	elif kind_sharp == 'do_python#':
		statement = flat( take_word( word_cursor ) ).strip()
		debug_parse( "Statement: %r", statement )
		action = PYTHON_EXEC( statement, arg_names, enclosing_scope )
	elif kind_sharp == 'be_builtin_current_thread#':
		action = Object( 'CURRENT_THREAD', formal_arg_names=arg_names, enclosing_scope=enclosing_scope )
	else:
		raise ParseError( "Expected 'do', 'be_python', 'do_python', or builtin; found %r" % kind_sharp )
	return action

def bind_action( bindings, action ):
	if action is null:
		return null
	else:
		if test_shogun:
			action_symbol = 'ACTION__%d' % action._id # TODO: hack because shogun can't yet handle ANON symbols as field names
		else:
			keyword_sharp = take( action, 'keyword#' )
			if keyword_sharp is take_failed:
				action_symbol = ANON()
			else:
				action_symbol = ANON( flat( keyword_sharp ) )
		bindings[ action_symbol ] = action
		debug_parse( "Bound %r to %s", action_symbol, action )
		return action_symbol

# build_keyword_based_automaton

debug_kba = silence

def build_keyword_based_automaton( grammar ):
	# A little silly parser generator algorithm to deal with simple
	# multi-dispatch LR(0) languages using prefix notation and offering very
	# little error detection.
	# This will suffice until I write a proper parser generator.
	if True:
		debug_kba( "Building automaton" )
		initial_state = Shift()
		all_shift_states = Object( 'SHIFT_STATE_COLLECTOR', first = cons( initial_state, null ) )
		debug_kba( "  initial_state: %s", initial_state )
		debug_kba( "  Building parse tree" )
		make_branches( initial_state, grammar.patterns, all_shift_states )
		# Names are effectively keywords.  Any time we see one of those, whatever shift state we are in, we leap to that name's state.
		debug_kba( "  Connecting keywords" )
		connect_keywords( initial_state, grammar.patterns, all_shift_states.first )
		# The accept state
		initial_state[ ':EOF' ] = Accept()
		debug_kba( '  EOF => %s', initial_state[ ':EOF' ] )
		debug_kba( ' Automaton:\n%s\n', shogun( initial_state ) )
		#debug( ' Automaton again:\n%s\n', shogun( deshogun( shogun( initial_state ) ) ) )
		return AUTOMATON( initial_state, FALLBACK_FUNCTION() )

def make_branches( initial_state, patterns, all_shift_states ):
	if patterns is null:
		pass
	else:
		pattern = patterns.head
		if is_a( pattern, 'PRODUCTION' ):
			debug_kba( "Productions not yet supported, but this one would have been:\n%s", shogun( pattern ) )
		else:
			tip = extend_branch( initial_state, pattern.formal_arg_types.tail, all_shift_states )
			reduce_state = Reduce( flat( take( pattern, 'action_symbol#' ) ) )
			reduce_word_sharp = take( pattern.formal_arg_types, 'head#' )
			give( tip, reduce_word_sharp, reduce_state )
			debug_kba( '      %r:%r >> %r', tip, flat( reduce_word_sharp ), reduce_state )
			# Tail digression
		make_branches( initial_state, patterns.tail, all_shift_states )

def extend_branch( state, arg_types, all_shift_states ):
	if arg_types is null:
		return state
	else:
		return extend_one_step( extend_branch( state, arg_types.tail, all_shift_states ), take( arg_types, 'head#' ), all_shift_states )

def extend_one_step( state, arg_type_sharp, all_shift_states ):
	return extend_one_step2( state, arg_type_sharp, take( state, arg_type_sharp ), all_shift_states )

def extend_one_step2( state, arg_type_sharp, possible_match, all_shift_states ):
	if possible_match is take_failed:
		result = Shift()
		all_shift_states.first = cons( result, all_shift_states.first )
		give( state, arg_type_sharp, result )
		debug_kba( '      %r:%r => %r', state, flat( arg_type_sharp ), result )
		return result
	else:
		debug_kba( '      %r:%r -> %r', state, flat( arg_type_sharp ), possible_match )
		return possible_match

def connect_keywords( initial_state, patterns, states ):
	#debug_kba( "      connect_keywords( %r, %r, %r )" % ( initial_state, patterns, states ) )
	if states is null:
		pass
	else:
		connect_keywords_to_state( initial_state, patterns, states.head )
		connect_keywords         ( initial_state, patterns, states.tail )

def connect_keywords_to_state( initial_state, patterns, shift_state ):
	#debug_kba( "      connect_keywords_to_state( %r, %r, %r )" % ( initial_state, patterns, shift_state ) )
	if patterns is null:
		pass
	else:
		if is_a( patterns.head, 'PRODUCTION' ):
			debug_kba( "Skip %s", patterns.head )
		else:
			keyword_sharp = last_head( patterns.head.formal_arg_types )
			give( shift_state, keyword_sharp, take( initial_state, keyword_sharp ) )
		connect_keywords_to_state( initial_state, patterns.tail, shift_state )

def last_head( items ):
	if items.tail is null:
		return take( items, 'head#' )
	else:
		return last_head( items.tail )

def parse_procedure( name, library_text, script ):
	global_scope = ENVIRONMENT( null )
	lib = parse_library( name, library_text, global_scope )
	automaton = build_keyword_based_automaton( lib.grammar )
	add_predefined_bindings( global_scope.bindings )
	result = PROCEDURE( name, List( script ), automaton, lib.action_bindings, global_scope )
	if False:
		debug( "Procedure:\n%s\n", result._description( 1 ) )
	if False:
		debug( "Procedure:\n%s\n", shogun( result ) )
	return result

def parse_grammar( grammar_text ):
	words = re.findall( r"\S+", grammar_text )
	words.reverse()
	productions = null
	goal_symbol = None
	while words:
		lhs = words.pop()
		if not goal_symbol:
			goal_symbol = lhs
		consume( "->", words )
		rhs_stack = null
		while words[-1] != ";":
			rhs_stack = LIST( words.pop(), rhs_stack )
		consume( ";", words )
		productions = LIST( PRODUCTION( lhs, rhs_stack, ANON( 'action' ) ), productions )
	return GRAMMAR( goal_symbol, productions )

#####################################
#
# NFA management with eta edges
#

debug_cetera = silence

def nfa_successors( obj, key ):
	for cetera in nfa_cetera_chain( obj ):
		try:
			yield cetera[ key ]
		except KeyError:
			pass # Next object in the eta chain might have this key

def nfa_add_successor( obj, key, value ):
	for cetera in nfa_cetera_chain( obj ):
		if key not in cetera:
			cetera[ key ] = value
			return
	# Need a new cetera object
	cetera = Object( 'CETERA' )
	if eta in obj:
		cetera[ eta ] = obj[ eta ]
	obj[ eta ] = cetera
	cetera[ key ] = value

def nfa_remove_successor( obj, key, value ):
	for cetera in nfa_cetera_chain( obj ):
		try:
			if cetera[ key ] == value:
				del cetera[ key ]
				return
		except KeyError:
			pass # Next object in the eta chain might have this key
	raise KeyError( "Successor %r not found at %r[ %r ]" % ( value, obj, key ) )

def nfa_cetera_chain( obj ):
	current = obj
	while True:
		debug_cetera( "(nfa_cetera_chain on %r yielding %r)", obj, current )
		yield current
		try:
			current = current[ eta ]
		except KeyError:
			break

#####################################
#
# Subset algorithm to convert NFA -> DFA
#

# The "eta" symbol η has the same meaning as "epsilon" ε when interpreting an NFA.
# It exists in order to support efficient representation of mutable NFAs as
# Sheppard object graphs, in which every state can have only one outgoing edge
# for each symbol.  The η-transitions are interpreted to point at states that
# are considered to have the same identity, so if you want to add a new transitions
# to some state S, you can add the transition to any state S' such that S[η*] = S'.
# This makes each state into a kind of linked list, where the "next" pointer is
# the η-transition.  I've been calling the second and subsequent states the "cetera"
# states (get it? eta-cetera), and the rule is that each "cetera" state has
# exactly one incoming η-transition, and normal states have none.
#
epsilon = ANON('epsilon')
eta     = ANON('eta')

class ConflictError( Exception ):
	pass

accepting_states    = frozenset([ 'ACCEPT', 'REDUCE' ])
superposable_states = frozenset([ 'SHIFT', 'SHIFT_RAW', 'LOOKAHEAD1', 'CETERA', 'STATION', 'ARRIVAL_GATE', 'DEPARTURE_GATE' ])
state_tags = accepting_states | superposable_states

priority_symbol = ANON( 'priority' )
def nfa2dfa( nfa_initial_state ):
	debug_n2d = silence
	dfa_states_by_superposition = {}
	def dfa( nfa_states ):
		superposition = frozenset( nfa_states )
		try:
			return dfa_states_by_superposition[ superposition ]
		except KeyError:
			states = list( superposition )
			if len( states ) == 1 and tag( states[0] ) in accepting_states:
				result = states[0] # Use the actual object since we're not going to change it anyway
			else:
				states.sort( by_priority_descending )
				primary_state = states[0]
				top_priority = primary_state.get( priority_symbol, 0 )
				result = Object( tag( primary_state ) )
				if tag( primary_state ) == 'REDUCE':
					result.action_symbol = primary_state.action_symbol
				for state in states:
					if state.get( priority_symbol, 0 ) == top_priority:
						if tag( state ) in superposable_states and tag( result ) in superposable_states:
							pass
						elif tag( state ) != tag( result ):
							raise ConflictError( "Conflict between %s and %s at %d %d" % ( result, state, state.get( priority_symbol, 0 ), top_priority ) )
						elif tag( result ) == 'REDUCE' and result.action_symbol != state.action_symbol:
							raise ConflictError( "Reduce conflict between %r and %r" % ( result.action_symbol, state.action_symbol ) )
			dfa_states_by_superposition[ superposition ] = result
			return result

	def closure( nfa_states ):
		nfa_states = filter( lambda s: tag(s) in state_tags, nfa_states )
		result = set( nfa_states )
		work_stack = list( nfa_states )[:]
		def visit( state, key ):
			try:
				next_state = state[ key ]
				if next_state not in result:
					work_stack.append( next_state )
					result.add( next_state )
			except KeyError:
				pass
		while work_stack:
			state = work_stack.pop()
			visit( state, epsilon )
			visit( state, eta )
		result = frozenset( result )
		if result == frozenset( nfa_states ):
			#debug( "Closure returning %r", result )
			return result
		else:
			return closure( result )

	def union( sets ):
		return reduce( lambda x,y: x|y, sets, frozenset() )

	debug_n2d( "nfa2dfa on the following DFA:\n%s", nfa_initial_state._description() )
	debug_indent()
	initial_superposition = closure([ nfa_initial_state ])
	debug_n2d( "initial_superposition: %r", initial_superposition )
	work_stack = [ initial_superposition ] # invariant: work_stack contains only closed superpositions
	symbols_not_in_dfa = frozenset([ epsilon, eta ])
	while work_stack:
		superposition = work_stack.pop()
		dfa_state = dfa( superposition )
		if tag( dfa_state ) in superposable_states:
			debug_n2d( "Visiting %r: %s", dfa_state, list( superposition ) )
			debug_indent()
			unique_keys = union([ frozenset( nfa_state._iterkeys() ) - symbols_not_in_dfa for nfa_state in superposition ])
			for key in unique_keys:
				debug_indent()
				next_superposition = closure([ nfa_state[ key ] for nfa_state in superposition if key in nfa_state ])
				debug_n2d( "%r[ %r ] contains %r", dfa_state, key, next_superposition )
				if next_superposition:
					if next_superposition not in dfa_states_by_superposition:
						work_stack.append( next_superposition )
					dfa_state[ key ] = dfa( next_superposition )
					debug_n2d( "%r[ %r ] = %r  <==> %r[ %r ] = %r", dfa_state, key, dfa_state[ key ], list( superposition ), key, list( next_superposition ) )
				debug_indent(-1)
		debug_indent(-1)
	debug_indent(-1)
	return dfa( initial_superposition )

def by_priority_descending( s1, s2 ):
	return -cmp( s1.get( priority_symbol, 0 ), s2.get( priority_symbol, 0 ) )

def the_element( x ):
	( element, ) = x
	return element

def test_nfa2dfa():
	nfa = deshogun("""
{
	1 : PROCEDURE {
		'action_bindings' : @2
		'automaton' : @3
		'enclosing_scope' : @4
		'name' : 'fib'
		'script' : [ 'print_result', 'fib', 2 ]
	}
	2 : BINDINGS {
		'ACTION__104' : @5
		'ACTION__119' : @6
		'ACTION__134' : @7
		'ACTION__145' : @8
		'ACTION__66' : @9
		'ACTION__80' : @10
	}
	3 : AUTOMATON {
		'fallback_function' : @11
		'initial_state' : @12
	}
	4 : ENVIRONMENT {
		'bindings' : @13
		'outer' : @0
	}
	5 : MACRO {
		'enclosing_scope' : @4
		'formal_arg_names' : [ 'n', @0 ]
		'script' : [ '+', 'fib', '-', 'n', 1, 'fib', '-', 'n', 2 ]
	}
	6 : PYTHON_EVAL {
		'enclosing_scope' : @4
		'expression' : 'a+b'
		'formal_arg_names' : [ 'b', 'a', @0 ]
	}
	7 : PYTHON_EVAL {
		'enclosing_scope' : @4
		'expression' : 'a-b'
		'formal_arg_names' : [ 'b', 'a', @0 ]
	}
	8 : PYTHON_EXEC {
		'enclosing_scope' : @4
		'formal_arg_names' : [ 'n', @0 ]
		'statement' : 'print "*** RESULT IS", n, "***"'
	}
	9 : MACRO {
		'enclosing_scope' : @4
		'formal_arg_names' : [ 'n', @0 ]
		'script' : [ 0 ]
	}
	10 : MACRO {
		'enclosing_scope' : @4
		'formal_arg_names' : [ 'n', @0 ]
		'script' : [ 1 ]
	}
	11 : FALLBACK_FUNCTION {
		'ANON' : 'SYMBOL'
		'DIAL_TONE' : 'DIGRESSION'
		'INT' : 'SYMBOL'
		'MONIKER' : 'SYMBOL'
		'SHARPED_SYMBOL' : 'SYMBOL'
	}
	12 : SHIFT {
		'+' : @14
		'-' : @15
		':EOF' : @16
		'fib' : @17
		'print_result' : @18
	}
	13 : BINDINGS {
		'dial_tone' : @19
		'eof' : @20
		'eta' : @22
		'false' : @23
		'null' : @0
		'true' : @24
	}
	14 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @25
		'fib' : @17
		'print_result' : @18
	}
	15 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @26
		'fib' : @17
		'print_result' : @18
	}
	16 : ACCEPT {
	}
	17 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @27
		'fib' : @17
		'print_result' : @18
		epsilon : @36
		0 : @28
		1 : @29
	}
	18 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @30
		'fib' : @17
		'print_result' : @18
	}
	19 : DIAL_TONE {
		'local_scope' : @31
		'resumption' : @19
		'tokens' : @32
	}
	20 : EOF {
	}
	21 : ANON {
	}
	22 : ANON {
	}
	23 : FALSE {
	}
	24 : TRUE {
	}
	25 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @33
		'fib' : @17
		'print_result' : @18
	}
	26 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @34
		'fib' : @17
		'print_result' : @18
	}
	27 : REDUCE {
		'action_symbol' : 'ACTION__104'
	}
	28 : REDUCE {
		'action_symbol' : 'ACTION__66'
	}
	29 : REDUCE {
		'action_symbol' : 'ACTION__80'
	}
	30 : REDUCE {
		'action_symbol' : 'ACTION__145'
	}
	31 : ENVIRONMENT {
		'bindings' : @35
		'outer' : @0
	}
	32 : LIST {
		'head' : @20
		'tail' : @32
	}
	33 : REDUCE {
		'action_symbol' : 'ACTION__119'
	}
	34 : REDUCE {
		'action_symbol' : 'ACTION__134'
	}
	35 : BINDINGS {
	}
	36 : SHIFT {
		'booger' : @26
	}
}[12]
	""")
	dfa = nfa2dfa( nfa )
	print " -- NFA --"
	print nfa._description()
	print " -- DFA --"
	print dfa._description()

def parenthesized_arithmetic_parsing_automaton( include_accept_state=False ):
	if False:
		return generate_parenthesized_arithmetic_parsing_automaton()
	else:
		initial_state = deshogun("""
		{
			0 : SHIFT { '(' : @1 }
			1 : SHIFT { '(' : @1 ':INT' : @2 }
			2 : SHIFT { ')' : @21 '+' : @3 '-' : @5 '*' : @11 '/' : @12 '%' : @13 }
			3 : SHIFT { '(' : @1 ':INT' : @4 }
			4 : LOOKAHEAD1 { '+' : @22 '-' : @22 ')' : @22 '*' : @2 '/' : @2 '%' : @2 }
			5 : SHIFT { '(' : @1 ':INT' : @6 }
			6 : LOOKAHEAD1 { '+' : @23 '-' : @23 ')' : @23 '*' : @2 '/' : @2 '%' : @2 }
			11 : SHIFT { '(' : @1 ':INT' : @24 }
			12 : SHIFT { '(' : @1 ':INT' : @25 }
			13 : SHIFT { '(' : @1 ':INT' : @26 }
			21 : REDUCE { 'action_symbol' : '()' }
			22 : REDUCE { 'action_symbol' : '+' }
			23 : REDUCE { 'action_symbol' : '-' }
			24 : REDUCE { 'action_symbol' : '*' }
			25 : REDUCE { 'action_symbol' : '/' }
			26 : REDUCE { 'action_symbol' : '%' }
		}[0]
		""")
		if include_accept_state:
			initial_state[ ':INT' ] = lookahead = Lookahead1()
			lookahead[ eof ] = Accept()
		return AUTOMATON( initial_state, FALLBACK_FUNCTION() )

arithmetic_library_text = """
/* Cheesy way to use build_keyword_based_automaton to get almost the right actions constructed */
/* Must use variable names "a" and "b" for the re-binding operation performed by test_parenthesized_arithmetic */
/* Also the first of each one will be hard-coded to "a" so might as well call it that */
let a + b   be_python << a+b >>
let a - b   be_python << a-b >>
let a * b   be_python << a*b >>
let a / b   be_python << a/b >>
let a % b   be_python << a%b >>
let a b ()  be_python << b >>
"""

def parenthesized_arithmetic_dummy_procedure( include_accept_state=False ):
	proc = parse_procedure( "Parenthesized_arithmetic", arithmetic_library_text, [] )
	# Swap in the automaton that knows about precedence
	proc.automaton = parenthesized_arithmetic_parsing_automaton( include_accept_state )
	# Cheese up the action bindings and formal arg names
	for action in proc.action_bindings.values():
		for link in action.formal_arg_names._links():
			formal_arg = link.head
			#debug( "Checking formal_arg %r", formal_arg )
			if formal_arg is null:
				link.head = 'a'
			elif formal_arg not in ['a','b']:
				#debug( "  Binding %r to %r", formal_arg, action )
				proc.action_bindings[ formal_arg ] = action
				link.head = null
	#debug( "test_parenthesized_arithmetic procedure:\n%s", shogun( proc ) )
	if False:
		print proc.automaton.initial_state._description()
		for x in iterate_states( proc.automaton.initial_state ):
			print repr(x)
		for x in iterate_state_transitions( proc.automaton.initial_state ):
			print repr(x)
	return proc

def test_parenthesized_arithmetic():
	proc = base_proc = parenthesized_arithmetic_dummy_procedure( True )
	try:
		depth = int( argv[1] )
	except IndexError:
		depth = 0
	for _ in range( depth ):
		proc = wrap_procedure( proc )
	tests = [ # The right answer here is always 6
		"6",
		"( 6 )",
		"( ( 6 ) )",
		"( 4 + 2 )",
		"( 8 - 2 )",
		"( 3 * 2 )",
		"( 12 / 2 )",
		"( 16 % 10 )",
		"( 4 + 3 - 1 )",
		"( 8 - 1 - 1 )",
		"( 2 + 2 * 2 )",
		"( 2 + 8 / 2 )",
		"( 1 + 15 % 10 )",
		"( 1 + ( 3 + 2 ) )",
		]
	for test in tests:
		base_proc.script = List( map( maybe_int, test.split() ) )
		final_stack = execute( proc, proc.action_bindings )
		if final_stack.head == 6:
			print "passed", test
		else:
			print "FAILED", test, python_list( final_stack )

def generate_parenthesized_arithmetic_parsing_automaton():
	# Doesn't work yet
	int_station = Object( 'STATION' )
	def dfa( symbols, action_symbol, priority, penultimate=None ):
		if penultimate is None:
			penultimate = []
		else:
			penultimate = [ penultimate ]
		shifts = [ Shift() for s in symbols ]
		states = shifts + penultimate + [ Reduce( action_symbol ) ]
		for ( i, state ) in enumerate( shifts ):
			state[ symbols[i] ] = states[ i+1 ]
			if symbols[i] == ':INT':
				state[ epsilon ] = int_station
		if penultimate:
			states[ -2 ][ epsilon ] = states[ -1 ]
		for state in states:
			tag_bonus = 0
			if tag( state ) == 'REDUCE':
				tag_bonus = 1 # Left-to-right evaluation prefers reduce over others
			state[ priority_symbol ] = priority + tag_bonus
		return states[0]
	add = dfa([ ':INT', '+', ':INT' ], '+', 10, Lookahead1() )
	sub = dfa([ ':INT', '-', ':INT' ], '-', 10, Lookahead1() )
	mul = dfa([ ':INT', '*', ':INT' ], '*', 20, Lookahead1() )
	div = dfa([ ':INT', '/', ':INT' ], '/', 20, Lookahead1() )
	rem = dfa([ ':INT', '%', ':INT' ], '%', 20, Lookahead1() )
	parens = dfa([ '(', ':INT', ')' ], '()', 0 )
	patterns = [ add, sub, mul, div, rem, parens ]
	ceteras = [ Object( 'CETERA' ) for p in patterns ]
	int_station[ eta ] = ceteras[0]
	for (i,state) in enumerate( ceteras[:-1] ):
		state[ eta ] = ceteras[ i+1 ]
	for (i,pattern) in enumerate( patterns ):
		ceteras[ i ][ epsilon ] = pattern
	return nfa2dfa( parens )

def iterate_states( initial_state, already_visited=None ):
	if already_visited is None:
		already_visited = set()
	yield initial_state
	for (k,v) in initial_state._iteritems():
		if tag(v) in state_tags and v not in already_visited:
				already_visited.add( v )
				for x in iterate_states( v, already_visited ):
					yield x

def iterate_state_transitions( initial_state, already_visited=None ):
	if already_visited is None:
		already_visited = set()
	for (k,v) in initial_state._iteritems():
		if tag(v) in state_tags:
			yield ( initial_state, k, v )
			if v not in already_visited:
				already_visited.add( v )
				for x in iterate_state_transitions( v, already_visited ):
					yield x

def duplicate_states( initial_state ):
	image_state_map = {}
	def image_state( obj ):
		try:
			return image_state_map[ obj ]
		except KeyError:
			obj_tag = tag( obj )
			if obj_tag in state_tags:
				result = Object( obj_tag )
				image_state_map[ obj ] = result
				for ( field, value ) in obj._iteritems():
					result[ field ] = image_state( value )
				return result
			else:
				image_state_map[ obj ] = obj
				return obj

	for ( source, symbol, target ) in iterate_state_transitions( initial_state ):
		image_state( source )[ symbol ] = image_state( target )
	return image_state( initial_state )

def augmented( initial_state ):
	nfa_initial_state = duplicate_states( initial_state )
	pa = parenthesized_arithmetic_dummy_procedure()
	pa_nfa_initial_state = duplicate_states( pa.automaton.initial_state )
	states_expecting_int = [ source for ( source, symbol, target ) in iterate_state_transitions( nfa_initial_state ) if symbol == ':INT' ]
	pa_states_expecting_int = [ source for ( source, symbol, target ) in iterate_state_transitions( pa_nfa_initial_state ) if symbol == ':INT' ]
	for s in states_expecting_int:
		s[ epsilon ] = pa_nfa_initial_state
	fib_state = nfa_initial_state[ 'fib' ]
	for s in pa_states_expecting_int:
		s[ eta ] = fib_state
	result = nfa2dfa( nfa_initial_state )
	debug( result._description() )
	return result

def graft( main_initial_state, branch_initial_state, branch_goal_symbol ):
	image_state_map = {}
	def image_state( obj ):
		try:
			return image_state_map[ obj ]
		except KeyError:
			obj_tag = tag( obj )
			if obj_tag in state_tags:
				result = Object( obj_tag )
				image_state_map[ obj ] = result
				for ( field, value ) in obj._iteritems():
					result[ field ] = image_state( value )
				return result
			else:
				image_state_map[ obj ] = obj
				return obj
	image_state = memoized( image_state )

	branch_initial_state = image_state( branch_initial_state )
	nfa_initial_state = image_state( main_initial_state )
	work_stack = [ nfa_initial_state ]
	already_done = set()
	while work_stack:
		state = work_stack.pop()
		already_done.add( state )
		for ( field, value ) in state._iteritems():
			if field == branch_goal_symbol:
				state[ epsilon ] = branch_initial_state
				break
			if tag( value ) in state_tags and value not in already_done:
				work_stack.append( value )
	return nfa2dfa( nfa_initial_state )

def augmented_with_parenthesized_arithmetic( procedure ):
	pa = parenthesized_arithmetic_dummy_procedure()
	grafted = graft( procedure.automaton.initial_state, pa.automaton.initial_state, ':INT' )
	#grafted = augmented( procedure.automaton.initial_state ) # Attempt to support infix + between fib calls
	augmented_automaton = AUTOMATON( grafted, procedure.automaton.fallback_function )
	action_bindings = Object( 'BINDINGS' )
	#debug( "Adding in %s", procedure.action_bindings )
	for ( symbol, action ) in procedure.action_bindings._iteritems():
		action_bindings[ symbol ] = action
	#debug( "Adding in %s", pa.action_bindings )
	for ( symbol, action ) in pa.action_bindings._iteritems():
		action_bindings[ symbol ] = action
	return PROCEDURE( procedure.name, procedure.script, augmented_automaton, action_bindings, procedure.enclosing_scope )

def quasiquote_dummy_procedure( include_accept_state=False ):
	result = deshogun(""" {
		1 : SHIFT { '{' : @21 ':QUASIQUOTER' : @2 }
		2 : SHIFT_RAW { ':SYMBOL' : @31 '}' : @23 '${' : @11 '$' : @15 }
		11 : SHIFT { ':ANY' : @12 }
		12 : SHIFT { '}$' : @33 }
		15 : SHIFT { ':ANY' : @32 }
		21 : REDUCE { 'action_symbol' : 'create_empty_quasiquoter' }
		23 : REDUCE { 'action_symbol' : 'close_quotation' }
		31 : REDUCE { 'action_symbol' : 'append_item' }
		32 : REDUCE { 'action_symbol' : 'append_item2' }
		33 : REDUCE { 'action_symbol' : 'append_item3' }
		101 : PROCEDURE {
			'action_bindings' : @104
			'automaton' : @150
			'enclosing_scope' : @102
			'name' : 'quasiquote'
			'script' : null
		}
		102 : ENVIRONMENT {
			'bindings' : @103
			'outer' : null
		}
		103 : BINDINGS {
			'dial_tone' : dial_tone
			'eof' : eof
			'eta' : eta
			'false' : false
			'null' : null
			'true' : true
		}
		104 : BINDINGS {
			'create_empty_quasiquoter' : @105
			'close_quotation' : @107
			'append_item' : @106
			'append_item2' : @109
			'append_item3' : @108
		}
		105 : PYTHON_EVAL {
			'enclosing_scope' : @102
			'formal_arg_names' : [ null ]
			'expression' : 'QUASIQUOTER()'
		}
		106 : PYTHON_EVAL {
			'enclosing_scope' : @102
			'formal_arg_names' : [ 'symbol' , 'qq' ]
			'expression' : 'append_item_to_quasiquoter( qq, symbol )'
		}
		107 : PYTHON_EVAL {
			'enclosing_scope' : @102
			'formal_arg_names' : [ null , 'qq' ]
			'expression' : 'qq.start_marker.tail'
		}
		108 : PYTHON_EVAL {
			'enclosing_scope' : @102
			'formal_arg_names' : [ null , 'symbol' , null , 'qq' ]
			'expression' : 'append_item_to_quasiquoter( qq, symbol )'
		}
		109 : PYTHON_EVAL {
			'enclosing_scope' : @102
			'formal_arg_names' : [ 'symbol' , null , 'qq' ]
			'expression' : 'append_item_to_quasiquoter( qq, symbol )'
		}
		150 : AUTOMATON {
			'initial_state' : @1
		}
	}[101] """)
	result.automaton.fallback_function = FALLBACK_FUNCTION()
	if include_accept_state:
		result.automaton.initial_state[ ':LIST' ] = lookahead = Lookahead1()
		lookahead[ eof ] = Accept()
	return result

def QUASIQUOTER():
	start_marker = end = LIST( null, null )
	return Object( 'QUASIQUOTER', start_marker=start_marker, end=end )

def append_item_to_quasiquoter( qq, symbol ):
	new_tail = LIST( symbol, null )
	qq.end.tail = new_tail
	qq.end = new_tail
	return qq

def test_quasiquote():
	p = quasiquote_dummy_procedure( True )
	p.script = List([ '{', 'hello', 'world', '}' ])
	final_stack = execute( p, p.action_bindings )
	check_result( python_list( final_stack.head ), [ 'hello', 'world' ] )
	p.script = List([ '{', 'null', '$', 'null', 'null', '}' ])
	final_stack = execute( p, p.action_bindings )
	check_result( python_list( final_stack.head ), [ 'null', null, 'null' ] )
	p.script = List([ '{', 'null', '${', 'null', '}$', 'null', '}' ])
	final_stack = execute( p, p.action_bindings )
	check_result( python_list( final_stack.head ), [ 'null', null, 'null' ] )

def check_result( ideal, actual ):
	print "Result:", actual
	try:
		if isinstance( ideal, list ):
			if len( ideal ) != len( actual ):
				raise Exception
			for ( i, x ) in enumerate( ideal ):
				y = actual[ i ]
				if isinstance( x, Object ):
					if x is not y:
						raise Exception
				else:
					if x != y:
						raise Exception
		elif ideal != actual:
			return
	except Exception:
		raise Exception( "Test failure.  Expected: %r  Actual: %r" % ( ideal, actual ) )

#####################################
#
# Parser generation using arrival and departure gates
#
# I haven't thought of a snappy name for the NFA constructed this way.  If you
# picture a nondeterministic infinite automaton that parses the language, this
# one is like a projection of that automaton into an NFA, so let me call it a
# projection graph for now.
#
# This code is making a lot of use of the hashability of Sheppard objects,
# which makes me a little uneasy, but I'm gradually accepting the notion that
# perhaps hashability is an important general notion.
#

def GRAMMAR( goal_symbol, productions ):   return Object( 'GRAMMAR', goal_symbol=goal_symbol, productions=productions )
def PRODUCTION( lhs, rhs_stack, action_symbol ):
	rhs = List( list( reversed( python_list( rhs_stack ) ) ) )
	return Object( 'PRODUCTION', lhs=lhs, rhs=rhs, rhs_stack=rhs_stack, action_symbol=action_symbol )

def PROJECTION():
	return Object( 'PROJECTION', arrival_gates=Object( 'GATE_MAP' ), departure_gates=Object( 'GATE_MAP' ), start=Object( 'ARRIVAL_GATE' ), end=Object( 'DEPARTURE_GATE' ), terminals=Object( 'SET_OF_SYMBOLS' ) )

def new_projection_graph( grammar ):
	result = PROJECTION()

	# Gates for nonterminals
	for p in grammar.productions:
		if p.lhs not in result.arrival_gates:
			result.arrival_gates  [ p.lhs ] = Object( 'ARRIVAL_GATE' )
			result.departure_gates[ p.lhs ] = Object( 'DEPARTURE_GATE' )

	# The path for each RHS
	deferred = []
	for p in grammar.productions:
		ag = result.arrival_gates[ p.lhs ]
		dg = result.departure_gates[ p.lhs ]
		# Build the path
		current_state = ag
		for symbol in p.rhs:
			new_state = Object( 'SHIFT' )
			current_state[ symbol ] = new_state
			if symbol in result.arrival_gates:
				nfa_add_successor( current_state, epsilon, result.arrival_gates[ symbol ] )
				nfa_add_successor( result.departure_gates[ symbol ], epsilon, new_state )
			else:
				result.terminals[ symbol ] = symbol
			current_state = new_state
		deferred.append( ( current_state, epsilon, dg ) )

	# The path for the goal_symbol
	s = Object( 'SHIFT' )
	result.start[ grammar.goal_symbol ] = s
	nfa_add_successor( result.start, epsilon, result.arrival_gates[ grammar.goal_symbol ] )
	nfa_add_successor( result.departure_gates[ grammar.goal_symbol ], epsilon, s )
	s[ eof ] = result.end
	result.terminals[ eof ] = eof

	# At this point, the projection graph is actually the characteristic graph
	# because we haven't hooked up the RHS paths to the departure gates.
	for ( source, symbol, target ) in deferred:
		nfa_add_successor( source, symbol, target )

	return result

def compute_transitive_closure_of_terminals( projection_graph ):
	# DeRemer and Penello Digraph algorithm
	debug_digraph = silence

	def traverse( x ):
		debug_digraph( "traverse( %r )", x )
		debug_indent()
		stack.append( x )
		d = len( stack )
		n[ x ] = d
		for y in nfa_successors( x, epsilon ):
			debug_digraph( " x:%r y:%r", x, y )
			if n[y] == 0:
				traverse( y )
			if x is not y:
				n[x] = min( n[x], n[y] )
				add_terminal_transitions( x, y )
		if n[x] == d:
			while True:
				top = stack.pop()
				n[top] = 1e9999
				if top == x:
					break
				else:
					add_terminal_transitions( top, x )
		debug_indent( -1 )

	def add_terminal_transitions( target, source ):
		debug_digraph( "add_terminal_transitions( %r, %r )", target, source )
		for ( k,v ) in source._iteritems():
			if k in projection_graph.terminals and k not in target:
				debug_digraph( "  %r", k )
				target[ k ] = v

	stack = []
	n = {}
	for state in iterate_states( projection_graph.start ):
		n[ state ] = 0
	for state in iterate_states( projection_graph.start ):
		debug_digraph( "iterating state %r", state )
		debug_indent()
		if n[ state ] == 0:
			traverse( state )
		else:
			debug_digraph( "already traversed %r", state )
		debug_indent( -1 )

def follow_set( projection_graph, nonterminal ):
	filter_out = set( [ eta, epsilon ] )
	return [ k for k in projection_graph.departure_gates[ nonterminal ]._iterkeys() if k not in filter_out ]

#####################################
#
# Traditional Dragon book parser generation
#

def or_changed( target, source ):
	if target >= source:
		return False
	else:
		target |= source
		return True

def nullable_symbols( grammar ):
	result = set()
	while add_nullable_symbols( result, grammar.productions ):
		pass
	return result

def add_nullable_symbols( result, productions ):
	something_changed = False
	while productions is not null:
		production = productions.head
		if production.lhs not in result and are_all_nullable( production.rhs, result ):
			result.add( productions.lhs )
			something_changed = True
		productions = productions.tail
	return something_changed

def are_all_nullable( symbols, nullable_symbols ):
	while symbols is not null:
		if symbols.head in nullable_symbols:
			symbols = symbols.tail
		else:
			return False
	return True

def first_sets( grammar, nullable_symbols ):
	first_by_symbol = {}
	def first( symbol ):
		try:
			return first_by_symbol[ symbol ]
		except KeyError:
			first_by_symbol[ symbol ] = set([ symbol ])
			return first( symbol )
	something_changed = True
	while something_changed:
		something_changed = False
		for production in grammar.productions:
			lhs = production.lhs
			for symbol in production.rhs:
				changed = or_changed( first(lhs), first(symbol) )
				if changed:
					debug( "included first( %r ) in first( %r ): %r", symbol, lhs, first(symbol) )
				something_changed |= changed
				if symbol not in nullable_symbols:
					break
	return first_by_symbol

def follow_sets( grammar, nullable_symbols, first_by_symbol ):
	debug( "follow_sets:" )
	follow_by_symbol = {}
	def follow( symbol ):
		try:
			return follow_by_symbol[ symbol ]
		except KeyError:
			follow_by_symbol[ symbol ] = set()
			return follow( symbol )
	something_changed = True
	while something_changed:
		something_changed = False
		for production in grammar.productions:
			lhs = production.lhs
			rhs_stack = production.rhs_stack
			debug( "  production %r -> %r  --  %r", lhs, list_str( production.rhs ), list_str( production.rhs_stack ) )
			if rhs_stack is not null:
				debug( "    propagate follow( %r ) from follow( %r )", rhs_stack.head, lhs )
				something_changed != or_changed( follow( rhs_stack.head ), follow( lhs ) )
				for link in rhs_stack._links():
					if link.tail is not null:
						symbol = link.head
						prev_symbol = link.tail.head
						debug( "      propagate follow( %r ) from first( %r )", prev_symbol, symbol )
						something_changed |= or_changed( follow( prev_symbol ), first_by_symbol[ symbol ] )
						if prev_symbol in nullable_symbols:
							debug( "      propagate follow( %r ) from follow( %r )", prev_symbol, symbol )
							something_changed |= or_changed( follow( prev_symbol ), follow( symbol ) )
	return follow_by_symbol

def test_parser_generator():
	grammar_text = """
		S -> E        ;
		E -> T + T    ;
		E -> T - T    ;
		T -> F * F    ;
		T -> F / F    ;
		F -> :INT     ;
		F -> ( E )    ;
		"""

	# Example 4.17 dragon p.189
	grammar_text = """
		E  -> T E'     ;
		E' -> + T E'   ;
		E' ->          ;
		T  -> F T'     ;
		T' -> * F T'   ;
		T' ->          ;
		F  -> :INT     ;
		F  -> ( E )    ;
		"""
	grammar = parse_grammar( grammar_text )

	if False:
		n = nullable_symbols( grammar )
		print "nullable_symbols: ", n
		first = first_sets( grammar, n )
		print "first: ", first
		follow = follow_sets( grammar, n, first )
		print "follow: ", follow

	if True:
		print "=== BEFORE ==="
		p = new_projection_graph( grammar )
		print p._description()
		print "=== AFTER ==="
		compute_transitive_closure_of_terminals( p )
		print p._description()
		print "=== FOLLOW ==="
		for nonterminal in p.departure_gates._iterkeys():
			print "\t%r\t%s" % ( nonterminal, follow_set( p, nonterminal ) )

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
#    top level, and must use is_a based on one argument, or compare the
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

def execute( procedure, action_bindings ): # One day we can get partial evaluation by having static and dynamic action_bindings
	frame = ACTIVATION( DIGRESSION( procedure.script, ENVIRONMENT( procedure.enclosing_scope ), dial_tone ), null, LIST( procedure.automaton.initial_state, null ), action_bindings, procedure.automaton.fallback_function, null )
	thread = THREAD( frame, null )
	frame[ 'thread' ] = thread # I don't love this back link, but it's really handy and efficient
	debug( "starting thread: %r with digression:\n\t%s", thread, frame.cursor )
	print_program( thread )
	execute2( frame, 'CONTINUE_INTERPRETING' )
	return frame.operands

def execute2( frame, x ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while x == 'CONTINUE_INTERPRETING':
		#print_stuff( frame.thread )
		x = leave( frame.history.head, frame )

def leave( state, frame ):
	return leave_dispatch_map[ tag(state) ]( state, frame )

def leave_accept( state, frame ):
	debug( 'accept' )
	return 'EXIT_INTERPRETER'

debug_shift = silence
shift_count = 0
def leave_shift( state, frame ):
	global shift_count
	shift_count += 1
	if debug_shift is not silence:
		print_stuff( frame.thread )
	if tag( state ) == 'SHIFT_RAW':
		token_sharp = get_token( pop_list( frame.cursor, "tokens#" ) )
	else:
		token_sharp = bound_value( get_token( pop_list( frame.cursor, "tokens#" ) ), frame.cursor.local_scope )
	end_digression_if_finished( frame.cursor.tokens, frame )
	frame[ 'operands' ] = cons( token_sharp, frame.operands )
	frame[ 'history' ] = cons( get_next_state( state, token_sharp, frame.fallback_function, token_sharp ), frame.history )
	if debug_shift is not silence:
		debug_shift( "    bound_value: %r", flat( token_sharp ) )
		debug_shift( "  new_state: %r", frame.history.head )
	if list_length( frame.operands ) > 50:
		error( frame.thread, RuntimeError( "Operand stack overflow" ) )
	return 'CONTINUE_INTERPRETING'

def leave_lookahead1( state, frame ):
	# Note that the token binding gets looked up twice in this case: once for
	# the lookup, and once for the next shift.
	if debug_shift is not silence:
		print_stuff( frame.thread )
		debug_shift( "Lookahead token: %r", get_token( take( frame.cursor.tokens, "head#" ) ) )
		debug_shift( "  bindings: %s", frame.cursor.local_scope.bindings._description() )
	token_sharp = bound_value( get_token( take( frame.cursor.tokens, "head#" ) ), frame.cursor.local_scope )
	#debug_shift( "  bound_value: %r", flat( token_sharp ) )
	return leave( get_next_state( state, token_sharp, frame.fallback_function, token_sharp ), frame )

def leave_reduce( state, frame ):
	if meta_level( frame.thread ) >= printing_level_threshold:
		debug_reduce = debug
		debug2_reduce = silence
	else:
		debug_reduce = silence
		debug2_reduce = silence
	print_stuff( frame.thread )
	#debug2_reduce( ">-- reduce %s --", state.action_symbol )
	action = take( frame.action_bindings, take( state, 'action_symbol#' ) )
	#debug2_reduce( "  action: %s", action )
	#if is_a( action, 'MACRO' ):
	#	debug_reduce( "    %s", python_list( action.script ) )
	reduce_environment = ENVIRONMENT( action.enclosing_scope )
	#debug2_reduce( "  environment1: %s with %s", reduce_environment, reduce_environment.bindings )
	bind_args( frame, reduce_environment.bindings, action.formal_arg_names )
	debug2_reduce( "  environment2: %s with %s", reduce_environment, reduce_environment.bindings )
	print_reduce_stuff( frame.thread, action, reduce_environment )
	#debug2_reduce( "  environment: %s with %s", reduce_environment, reduce_environment.bindings )
	#debug2_reduce( "    based on: %s", action.enclosing_scope )
	perform( action, frame, reduce_environment )
	end_digression_if_finished( frame.cursor.tokens, frame )
	print_program( frame.thread )
	return 'CONTINUE_INTERPRETING'

leave_dispatch_map = {
	'ACCEPT':      leave_accept,
	'SHIFT':       leave_shift,
	'SHIFT_RAW':   leave_shift,
	'LOOKAHEAD1':  leave_lookahead1,
	'REDUCE':      leave_reduce,
	}

def perform( action, frame, reduce_environment ):
	if is_a( action, 'PYTHON_EXEC' ):
		perform_python_exec( frame, action, reduce_environment )
	elif is_a( action, 'PYTHON_EVAL' ):
		perform_python_eval( frame, action, reduce_environment )
	elif is_a( action, 'CURRENT_THREAD' ):
		perform_current_thread( frame, action, reduce_environment )
	else:
		assert( is_a( action, 'MACRO' ) )
		frame[ 'cursor' ] = DIGRESSION( action.script, reduce_environment, frame.cursor )

def perform_python_exec( frame, action, reduce_environment ):
	try:
		exec action.statement in globals(), dict( reduce_environment.bindings._iteritems() )
	except:
		print "\nError in exec:", action.statement, "with", reduce_environment.bindings
		raise

def perform_python_eval( frame, action, reduce_environment ):
	try:
		result = eval( action.expression,  globals(), dict( reduce_environment.bindings._iteritems() ) )
	except:
		print "\nError in eval:", action.expression, "with", reduce_environment.bindings
		raise
	end_digression_if_finished( frame.cursor.tokens, frame )
	frame[ 'cursor' ] = DIGRESSION( List([ result ]), frame.cursor.local_scope, frame.cursor )

def perform_current_thread( frame, action, reduce_environment ):
	end_digression_if_finished( frame.cursor.tokens, frame )
	frame[ 'cursor' ] = DIGRESSION( List([ frame.thread ]), frame.cursor.local_scope, frame.cursor )

def end_digression_if_finished( remaining_tokens, frame ):
	if remaining_tokens is null: #is_a( remaining_tokens, 'NULL' ):
		#debug_digressions( "  (finished %r %r %s)", frame.cursor, frame.cursor.local_scope, frame.cursor.local_scope.bindings  )
		frame[ 'cursor' ] = frame.cursor.resumption

debug_get_next_state = silence
def get_next_state( state, obj_sharp, fallback_function, original_obj_for_error_reporting ):
	# Pseudocode:
	#
	# if take( state, obj_sharp ):
	#    return that
	# else:
	#    t = tag_sharp( obj_sharp )
	#    while t is not take_failed:
	#       if take( state, tag_edge_symbol( t ) )
	#          return that
	#       else:
	#          t = fallback_function[ t ]
	# raise Unexpected_token
	#
	debug_get_next_state( "get_next_state( %r, %r, fallback, %r )", state, obj_sharp, original_obj_for_error_reporting )
	return get_next_state2( state, obj_sharp, take( state, obj_sharp ), fallback_function, original_obj_for_error_reporting )

def get_next_state2( state, obj_sharp, possible_match, fallback_function, original_obj_for_error_reporting ):
	if possible_match is take_failed:
		tag_sharp = sharp( take_tag( obj_sharp ) )
		return get_next_state_by_tag( state, tag_sharp, fallback_function, original_obj_for_error_reporting )
	else:
		return possible_match

def get_next_state_by_tag( state, tag_sharp, fallback_function, original_obj_for_error_reporting ):
	debug_get_next_state( "get_next_state_by_tag( %r, %r, fallback, %r )", state, tag_sharp, original_obj_for_error_reporting )
	if tag_sharp is take_failed:
		raise Unexpected_token( state, original_obj_for_error_reporting )
	else:
		return get_next_state_by_tag2( state, tag_sharp, take( state, tag_edge_symbol( tag_sharp ) ), fallback_function, original_obj_for_error_reporting )

def get_next_state_by_tag2( state, tag_sharp, possible_next_state, fallback_function, original_obj_for_error_reporting ):
	if possible_next_state is take_failed:
		return get_next_state_by_tag( state, get_fallback( fallback_function, tag_sharp, take( fallback_function, tag_sharp ) ), fallback_function, original_obj_for_error_reporting )
	else:
		return possible_next_state

def get_fallback( fallback_function, obj_sharp, possible_result ):
	silence( "get_fallback( %s, %r, %r )", fallback_function, obj_sharp, possible_result )
	if possible_result is take_failed:
		if obj_sharp == "ANY#":
			return take_failed
		else:
			return "ANY#"
	else:
		return possible_result

def bind_arg( arg_bindings, arg_symbol_sharp, arg_value_sharp ):
	debug_bind = silence
	#debug_bind( "  bind_arg( %r, %r, %r )", arg_bindings, flat( arg_symbol_sharp ), flat( arg_value_sharp ) )
	if arg_symbol_sharp is null: # is_a( arg_symbol_sharp, 'NULL' )
		#debug_bind( "    pop %r", flat( arg_value_sharp ) )
		pass
	else:
		assert( is_a( arg_symbol_sharp, 'MONIKER' ) )
		give( arg_bindings, arg_symbol_sharp, arg_value_sharp )
		#debug_bind( "    %r.%s=%r", arg_bindings, flat( arg_symbol_sharp ), flat( arg_value_sharp ) )

def bind_args( frame, arg_bindings, formal_arg_names ):
	if formal_arg_names is null:
		pass
	else:
		frame[ 'history' ] = frame.history.tail
		bind_arg( arg_bindings, take( formal_arg_names, 'head#' ), pop_list( frame, 'operands#' ) )
		bind_args( frame, arg_bindings, formal_arg_names.tail )

def bound_value( obj_sharp, environment ):
	#debug( "bound_value( %r, %r )", obj_sharp, environment )
	if environment is null:
		#debug( "  returning original obj_sharp %r", obj_sharp )
		return obj_sharp
	else:
		return bound_value2( obj_sharp, environment, take( environment.bindings, obj_sharp ) )

def bound_value2( obj_sharp, environment, possible_match ):
	if possible_match is take_failed:
		return bound_value( obj_sharp, environment.outer )
	else:
		return possible_match

def tag_edge_symbol( tag_sharp ):
	return ':' + tag_sharp

def pop_list( base, field_symbol_sharp ):
	"""Odd little routine that's just super handy for dealing with object fields that are stacks"""
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	#debug( "!!! pop_list( %s, %s ) = %s", base, field_symbol_sharp, result )
	give( base, field_symbol_sharp, take( current, 'tail#' ) )
	return result

def get_token( possible_token ):
	"""Transmute TAKE_FAILED into eof to make all token lists appear to end with an infinite stream of eofs"""
	if possible_token is take_failed:
		return sharp( eof )
	else:
		return possible_token


#####################################
#
# Meta-interpreter
#

meta_interpreter_text = """
to execute 
	procedure action_bindings
do
	set frame
		Activation
			Digression
				get procedure script
				Environment
					get procedure enclosing_scope
				dial_tone
			null  /* Empty operand stack */
			cons
				get2 procedure automaton initial_state
				null
			action_bindings
			get2 procedure automaton fallback_function
			null  /* No caller */
	/* I haven't got this right yet, because it makes no sense to spawn a Thread just to execute a procedure */
	put frame thread  /* Putting a thread pointer in each frame seems wasteful, but it's handy */
		Thread frame current_thread
	print_program frame
	execute2 frame CONTINUE_INTERPRETING
	/* return get frame operands  /* the "return values" */

to execute2 
	frame x=CONTINUE_INTERPRETING
do
	execute2
		frame
		leave
			get2 frame history head
			frame

to execute2 
	frame x=EXIT_INTERPRETER
do
	/* Nothing left to do */


/* The routine "leave" performs one execution step of the program, thereby exiting the current state and moving into the next one */

let leave 
	state:ACCEPT frame
be
	EXIT_INTERPRETER

let leave 
	state:SHIFT frame
be
	set token_sharp
		bound_value
			get_token
				pop_list
					get frame cursor
					tokens#
			get2 frame cursor local_scope
	end_digression_if_finished
		get2 frame cursor tokens
		frame
	put frame operands
		cons
			token_sharp
			get frame operands
	put frame history
		cons
			get_next_state
				state
				token_sharp
				get frame fallback_function
				token_sharp
			get frame history
	CONTINUE_INTERPRETING

let leave 
	state:LOOKAHEAD1 frame
be
	set token_sharp
		bound_value
			get_token
				take
					get2 frame cursor tokens
					head#
			get2 frame cursor local_scope
	leave
		get_next_state
			state
			token_sharp
			get frame fallback_function
			token_sharp
		frame

let leave 
	state:REDUCE frame
be
	print_stuff frame
	set action
		take
			get frame action_bindings
			take state action_symbol#
	set reduce_environment
		Environment
			get action enclosing_scope
	bind_args
		get reduce_environment bindings
		get action formal_arg_names
		frame
	print_reduce_stuff frame action reduce_environment
	perform
		action
		frame
		reduce_environment
	end_digression_if_finished
		get2 frame cursor tokens
		frame
	print_program frame
	CONTINUE_INTERPRETING


to perform  action:PYTHON_EXEC    frame reduce_environment  do_python << perform_python_exec( frame, action, reduce_environment ) >>
to perform  action:PYTHON_EVAL    frame reduce_environment  do_python << perform_python_eval( frame, action, reduce_environment ) >>
to perform  action:CURRENT_THREAD frame reduce_environment  do_python << perform_current_thread( frame, action, reduce_environment ) >>
to perform
	action:MACRO frame reduce_environment
do
	put frame cursor
		Digression
			get action script
			reduce_environment
			get frame cursor

to end_digression_if_finished 
	remaining_tokens:NULL frame
do
	put frame cursor
		get2 frame cursor resumption

to end_digression_if_finished 
	remaining_tokens frame
do
	/* Not finished -- don't end the digression */


/* The routine "get_next_state" determines which automaton edge to follow */

let get_next_state 
	state obj_sharp fallback_function original_obj_for_error_reporting
be
	get_next_state2
		state
		obj_sharp
		take state obj_sharp
		fallback_function
		original_obj_for_error_reporting


let get_next_state2 
	state obj_sharp possible_match fallback_function original_obj_for_error_reporting
be
	possible_match

let get_next_state2 
	state obj_sharp x=TAKE_FAILED fallback_function original_obj_for_error_reporting
be
	get_next_state_by_tag
		state
		take_tag obj_sharp
		fallback_function
		original_obj_for_error_reporting


let get_next_state_by_tag
	state tag_sharp=TAKE_FAILED fallback_function original_obj_for_error_reporting
do_python
	<< raise Unexpected_token( state, original_obj_for_error_reporting ) >>

let get_next_state_by_tag
	state tag_sharp fallback_function original_obj_for_error_reporting
be
	get_next_state_by_tag2
		state
		tag_sharp
		take
			state
			tag_edge_symbol tag_sharp
		fallback_function
		original_obj_for_error_reporting

let get_next_state_by_tag2
	state tag_sharp possible_next_state fallback_function original_obj_for_error_reporting
be
	possible_next_state

let get_next_state_by_tag2
	state tag_sharp x=TAKE_FAILED fallback_function original_obj_for_error_reporting
be
	get_next_state_by_tag
		state
		get_fallback
			fallback_function
			tag_sharp
			take fallback_function tag_sharp
		fallback_function
		original_obj_for_error_reporting


let get_fallback  fallback_function obj_sharp      possible_result              be possible_result
let get_fallback  fallback_function obj_sharp      possible_result=TAKE_FAILED  be ANY#
let get_fallback  fallback_function obj_sharp=ANY# possible_result=TAKE_FAILED  be TAKE_FAILED  /* Holy double dispatch, Batman */


/* Binding logic to create new bindings, and to look up existing bindings */

to bind_args 
	arg_bindings formal_arg_names:LIST frame
do
	put frame history
		get2 frame history tail
	bind_arg
		arg_bindings
		take formal_arg_names head#
		pop_list frame operands#
	bind_args
		arg_bindings
		get formal_arg_names tail
		frame

to bind_args 
	arg_bindings formal_arg_names:NULL frame
do
	/* stop when there are no more formal_arg_names */


to bind_arg 
	arg_bindings arg_symbol_sharp:MONIKER arg_value_sharp
do
	give arg_bindings arg_symbol_sharp arg_value_sharp

to bind_arg
	arg_bindings arg_symbol_sharp:NULL arg_value_sharp
do
	/* no symbol -- no binding */


let bound_value 
	obj_sharp environment:ENVIRONMENT
be
	bound_value2
		obj_sharp
		environment
		take
			get environment bindings
			obj_sharp

let bound_value 
	obj_sharp environment:NULL
be
	/* without an environment, an object represents itself */
	obj_sharp


let bound_value2 
	obj_sharp environment possible_match
be
	possible_match

let bound_value2 
	obj_sharp environment x=TAKE_FAILED
be
	bound_value
		obj_sharp
		get environment outer

to return
	value
do
	/* TODO: Take off my operand stack and put on the caller's */

/* Constructors for data types used by the interpreter */

let Digression
	tokens local_scope resumption
be_python
	<< DIGRESSION(**dict( locals() )) >>

let Activation
	cursor operands history action_bindings fallback_function caller
be_python
	<< ACTIVATION(**dict( locals() )) >>

let Thread
	activation meta_thread
be_python
	<< THREAD( activation, meta_thread ) >>

let Environment
	outer
be_python
	<< ENVIRONMENT( outer ) >>


/* Some helpful utilities */

let tag_edge_symbol
	tag_sharp
be_python
	<< ':' + tag_sharp >>

let pop_list 
	base field_symbol_sharp:MONIKER
be_python
	<< pop_list( base, field_symbol_sharp ) >>


/* Behave as though every token list ends with an infinite sequence of eofs */

let get_token possible_token be possible_token
let get_token x=TAKE_FAILED  be eof


/* Diagnostics */

to print_stuff        frame                             do_python << print_stuff( frame.thread ) >>
to print_reduce_stuff frame action reduce_environment   do_python << print_reduce_stuff( frame.thread, action, reduce_environment ) >>
to print_program      frame                             do_python << print_program( frame.thread ) >>
to print              obj                               do_python << print str( obj ) >>
"""

not_used_because_they_are_too_slow = """
let pop_list 
	base field_symbol_sharp
be
	set current
		take base field_symbol_sharp
	set result
		take current head#
	give base field_symbol_sharp
		take current tail#
	result

to print_fields
	obj
do
	for each field in obj
		/* Hypothetical quasiquote syntax, with obj inserted into the list */
		do { print get ${ obj } field }
		/* Without unquoting, with the symbol "obj" inserted.  This may be equivalent depending on binding rules. */
		do { print get obj field }
	if x then { print hello }

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

	def __init__( self, state, token_sharp ):
		self._state = state
		self._token = token_sharp

	def __str__( self ):
		return "Unexpected token %r in state %s" % ( flat( self._token ), self._state )

fib_text = """
let fib  n=0  be  0
let fib  n=1  be  1

let fib  n:INT  be
	+
		fib ( n - 1 )
		fib ( n - 2 )

let + a:INT b:INT   be_python  << a+b >>
let - a:INT b:INT   be_python  << a-b >>

to print_result  n:INT  do_python << print "*** RESULT IS", n, "***" >>
"""

def fib_procedure():
	result = parse_procedure( "fib", fib_text, [ 'print_result', 'fib', 2 ] )
	if True:
		result = augmented_with_parenthesized_arithmetic( result )
		#print shogun( result )
	return result

sheppard_interpreter_library = None
def wrap_procedure( inner_procedure ):
	global global_scope, sheppard_interpreter_library, sheppard_interpreter_automaton
	if sheppard_interpreter_library is None:
		global_scope = ENVIRONMENT( null )
		sheppard_interpreter_library = parse_library( "sheppard_interpreter", prologue_text + meta_interpreter_text, global_scope )
		add_predefined_bindings( global_scope.bindings )
		sheppard_interpreter_automaton = build_keyword_based_automaton( sheppard_interpreter_library.grammar )
	bindings = global_scope.bindings
	script = List([ 'execute', inner_procedure, inner_procedure.action_bindings ])
	outer_procedure = PROCEDURE( 'meta_' + inner_procedure.name, script, sheppard_interpreter_automaton, sheppard_interpreter_library.action_bindings, global_scope )
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
		debug( " bindings: %s", str( procedure.enclosing_scope.bindings ) )
		#debug( "automaton: %s", procedure.automaton._description() )
	if test_shogun:
		s1 = shogun( procedure )
		file("s1.txt", "w").write(s1)
		s2 = shogun( deshogun( s1 ) )
		file("s2.txt", "w").write(s2)
		if s1 != s2:
			raise ParseError # Shogun test mismatch: check for differences between the files
	execute( procedure, procedure.action_bindings )

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
	if tag_counters:
		tc = tag_counters.items()
		tc.sort( lambda (k1,v1), (k2,v2): cmp((-v1,k1),(-v2,k2)) )
		for ( t, num ) in tc:
			print "%25s: %d" % ( t, num )

if False:
	import cProfile
	cProfile.run( "main()", None, "time" )
elif False:
	test_parenthesized_arithmetic()
else:
	#test_nfa2dfa()
	#test_parser_generator()
	#test_quasiquote()
	main()


# Directions to pursue:
#
# Cleanup:
# - Change get/set/put so the only way to get a flat symbol on the operand stack is by calling "flat"
# - Figure out a way to have Sheppard procedures return a value.  Currently, we only reach Accept on EOF, so the final crud pushed on the stack may not be parseable.
#
# Possible:
# - Write an SLR parser generator in python?
# - Handle quasiquoting in python?  Then I could use it in Sheppard programs, so I could have if statements and foreach and loops.
#
# Cleaning up get/put
# - get should sharp its result, and put should take a sharp value.  Difference from give/take would be that get/put take un-sharped field symbols.
# - getattr/setattr and getitem/setitem on Object probably should NOT sharp/flat things.  When we're in Python code, it should act like Python code, or it's too confusing.
# - probably remove the _sharp suffixes everywhere because that can be taken for granted.
#

