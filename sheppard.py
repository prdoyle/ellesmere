#! /usr/bin/python -O
# vim: set fileencoding=utf-8 :

import string, re, time, ast
from sys import argv

# I've adopted a convention that strings used as Sheppard monikers (or parts
# thereof) have single-quotes, and ordinary Python strings have double-quotes.
# Since Python sees no difference between these, there's no way to enforce this
# rule, so you may find some unintentional inconsistencies.

def debug( message, *args ):
	if args:
		message = message % args
	print message

def silence( message, *args ):
	pass

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

# Calls to these are often commented out below to improve performance.  "Enabling" these here may not be enough.
#debug_digressions = debug
#debug_object = debug

object_counter = 0
tag_counters = None # {}

class Object:
	"""Warps Python's object model to make it more Sheppard-like"""

	def __init__( _self, _tag, **edges ):
		global object_counter, tag_counters
		assert( _tag.isupper() )
		_self._tag = _tag
		if tag_counters is not None:
			tag_counters[ _tag ] = 1 + tag_counters.get( _tag, 0 )
		# The terminology is a little weird here, from a time when numbers were not considered
		# symbols, so I differentiated "elements" (indexed by ints) from "fields".
		_self._elements = {}
		_self._fields = sorted([ k for k in edges ]) # _fields can be adjusted if we want a particular field ordering
		_self._id = object_counter
		object_counter += 1
		for ( name, value ) in edges.iteritems():
			_self[ name ] = value

	# Allow map syntax for convenience.
	#
	# NOTE: getattr and setattr are not implemented yet.  Setattr, in particular,
	# won't yet behave the way it should: it won't add the new field to _fields.
	# They're a little awkward to implement, so I just haven't bothered yet.
	#
	# Also note that this is not in sharp-land.  When we're interpreting be_python or
	# do_python, the code needs to act like Python or it will be too confusing.

	def __getitem__( self, key ):
		#debug_object( "getitem %r[ %r ]", self, key )
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
			if key[0] == '_':
				raise KeyError
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

	def __iter__( self ):
		raise TypeError( "Object %r does not implement iteration" % self )

	def _iterkeys( self ):
		for key in self._fields:
			yield key
		for key in self._elements.iterkeys():
			yield key

	def _iteritems( self ):
		for key in self._iterkeys():
			yield ( key, self[ key ] )

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
		return repr( self ) + "{ " + string.join([ "%s=%s" % ( field, self._short_description( self[field] ) ) for field in ( self._fields + self._elements.keys() ) ], ', ') + " }"

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

	def __nonzero__( self ): return self != 0 and self != null

	# Most Sheppard objects can't be dict keys (i.e. they are not symbols)

	def __hash__( self ):
		if True:
			return self._id # nfa2dfa needs sets of objects, so now we need to hash them
		# Prevent most Sheppard objects from being hashed.  We raise KeyError to
		# make getitem and setitem act like they don't have data for the given
		# (illegal) "key" object, which I think is usually how we want them to behave.
		raise KeyError( '%r is not a symbol' % self )

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
# Also, methods of Object can confuse __getattr__ and end up trying to call Sheppard objects.

def is_int( obj ):     return isinstance( obj, int ) # Sheppard integers are represented by Python ints
def is_moniker( obj ): return isinstance( obj, str ) # Sheppard monikers are represented by Python strs
# TODO: What about the fact that I'm now considering ints to be symbols?

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
		if isinstance( obj, str ): #is_moniker( obj ):
			result = 'MONIKER'
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
		return Object( 'SHARPED_SYMBOL', value=arg )
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

#####################################
#
# Serialization
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
		try:
			dictionary[ key ] = deshogun_simple_value( item, words )
		except NotASimpleValueError, e:
			try:
				consume( "{", words )
			except ParseError:
				raise e # Re-raise the exception that brought us here
			obj = Object( item )
			dictionary[ key ] = obj
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
	consume( "}", words )
	for ( obj, key, index ) in relocations:
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

def LIST( head, tail ):
	result = Object( 'LIST', head=head, tail=tail )
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

# For nondeterministic automata
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

def Reduce0( action_symbol ): return Object( 'REDUCE0', action_symbol=action_symbol )
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

def tag_edge_symbol( tag_sharp ):
	return ':' + tag_sharp

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
let tag        obj_sharp                    be_python << sharp( tag( flat( obj_sharp ) ) ) >>
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
			return PRODUCTION( flat( lhs_sharp ), rhs, action_symbol )
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
			reduce_state = Reduce0( flat( take( pattern, 'action_symbol#' ) ) )
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


#####################################
#
# Subset algorithm to convert NFA -> DFA
#

def nfa2dfa( nfa_initial_state ):
	debug_n2d = debug
	dfa_states_by_superposition = {}
	def dfa( nfa_states ):
		superposition = frozenset( nfa_states )
		try:
			return dfa_states_by_superposition[ superposition ]
		except KeyError:
			if len( superposition ) == 1 and tag( the_element( superposition ) ) in [ 'ACCEPT', 'REDUCE0' ]:
				result = the_element( superposition ) # Use the actual REDUCE0 object since we're not going to change it anyway
			else:
				tags = list( frozenset([ tag(s) for s in superposition ]) )
				tags.sort()
				result = Object( '_'.join( tags ) )
			dfa_states_by_superposition[ superposition ] = result
			return result

	def closure( nfa_states ):
		nfa_states = filter( lambda s: tag(s) in ['SHIFT','REDUCE0','ACCEPT'], nfa_states )
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

	initial_superposition = closure([ nfa_initial_state ])
	work_stack = [ initial_superposition ] # invariant: work_stack contains only closed superpositions
	nfa_only = frozenset([ epsilon, eta ])
	while work_stack:
		superposition = work_stack.pop()
		dfa_state = dfa( superposition )
		debug_n2d( "Visiting %r: %s", dfa_state, list( superposition ) )
		unique_keys = union([ frozenset( nfa_state._iterkeys() ) - nfa_only for nfa_state in superposition ])
		for key in unique_keys:
			next_superposition = closure([ nfa_state[ key ] for nfa_state in superposition if key in nfa_state ])
			if next_superposition:
				if next_superposition not in dfa_states_by_superposition:
					work_stack.append( next_superposition )
				dfa_state[ key ] = dfa( next_superposition )
				debug_n2d( "  %r[ %r ] = %r  <==> %r[ %r ] = %r", dfa_state, key, dfa_state[ key ], list( superposition ), key, list( next_superposition ) )
					
	return dfa( initial_superposition )

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
		'epsilon' : @21
		'eta' : @22
		'false' : @23
		'null' : @0
		'true' : @24
	}
	14 : SHIFT {
		'+' : @14
		'-' : @15
		':ANY' : @25
		epsilon : @15
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
	27 : REDUCE0 {
		'action_symbol' : 'ACTION__104'
	}
	28 : REDUCE0 {
		'action_symbol' : 'ACTION__66'
	}
	29 : REDUCE0 {
		'action_symbol' : 'ACTION__80'
	}
	30 : REDUCE0 {
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
	33 : REDUCE0 {
		'action_symbol' : 'ACTION__119'
	}
	34 : REDUCE0 {
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


#####################################
#
# Proper parser generator.
#

def PRODUCTION( lhs, rhs, action_symbol ): return Object( 'PRODUCTION', lhs=lhs, rhs=rhs, action_symbol=action_symbol )
def GRAMMAR( goal_symbol, productions ):   return Object( 'GRAMMAR', goal_symbol=goal_symbol, productions=productions )

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
			first_by_symbol[ symbol ] = set()
			return first( symbol )
	something_changed = True
	while something_changed:
		something_changed = False
		for production in grammar.productions:
			lhs = production.lhs
			for symbol in production.rhs:
				something_changed |= or_changed( first(lhs), first(symbol) )
				if symbol in nullable_symbols:
					break
	return first_by_symbol

def follow_sets( grammar, nullable_symbols, first_by_symbol ):
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
			rhs = production.rhs
			if len( rhs ) >= 1:
				something_changed != or_changed( follow( rhs[-1] ), follow( lhs ) )
				for ( i, symbol ) in reversed(list(enumerate( production.rhs[:-1] ))):
					next_symbol = production.rhs[ i+1 ]
					something_changed |= or_changed( follow( symbol ), first( next_symbol ) )
					if next_symbol in nullable_symbols:
						something_changed |= or_changed( follow( symbol ), follow( next_symbol ) )
	return follow_by_symbol

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

def pop_list( base, field_symbol_sharp ):
	"""Odd little routine that's just super handy for dealing with object fields that are stacks"""
	current = take( base, field_symbol_sharp )
	result  = take( current, 'head#' )
	#debug( "!!! pop_list( %s, %s ) = %s", base, field_symbol_sharp, result )
	give( base, field_symbol_sharp, take( current, 'tail#' ) )
	return result

def end_digression_if_finished( frame, remaining_tokens ):
	if remaining_tokens is null: #is_a( remaining_tokens, 'NULL' ):
		#debug_digressions( "  (finished %r %r %s)", frame.cursor, frame.cursor.local_scope, frame.cursor.local_scope.bindings  )
		frame[ 'cursor' ] = frame.cursor.resumption

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

def bound( obj_sharp, environment ):
	#debug( "bound( %r, %r )", obj_sharp, environment )
	if environment is null:
		return obj_sharp
	else:
		return bound2( obj_sharp, environment, take( environment.bindings, obj_sharp ) )

def bound2( obj_sharp, environment, possible_match ):
	if possible_match is take_failed:
		return bound( obj_sharp, environment.outer )
	else:
		return possible_match

def next_state( state, obj_sharp, fallback_function, original_obj_for_error_reporting ):
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
	return next_state2( state, obj_sharp, take( state, obj_sharp ), fallback_function, original_obj_for_error_reporting )

def next_state2( state, obj_sharp, possible_match, fallback_function, original_obj_for_error_reporting ):
	if possible_match is take_failed:
		tag_sharp = sharp( tag( obj_sharp ) )
		return next_state_by_tag( state, tag_sharp, fallback_function, original_obj_for_error_reporting )
	else:
		return possible_match

def next_state_by_tag( state, tag_sharp, fallback_function, original_obj_for_error_reporting ):
	if tag_sharp is take_failed:
		raise Unexpected_token( state, original_obj_for_error_reporting )
	else:
		return next_state_by_tag2( state, tag_sharp, take( state, tag_edge_symbol( tag_sharp ) ), fallback_function, original_obj_for_error_reporting )

def next_state_by_tag2( state, tag_sharp, possible_next_state, fallback_function, original_obj_for_error_reporting ):
	if possible_next_state is take_failed:
		return next_state_by_tag( state, get_fallback( fallback_function, tag_sharp, take( fallback_function, tag_sharp ) ), fallback_function, original_obj_for_error_reporting )
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


def perform( frame, action, reduce_environment ):
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
	end_digression_if_finished( frame, frame.cursor.tokens )
	frame[ 'cursor' ] = DIGRESSION( List([ result ]), frame.cursor.local_scope, frame.cursor )

def perform_current_thread( frame, action, reduce_environment ):
	end_digression_if_finished( frame, frame.cursor.tokens )
	frame[ 'cursor' ] = DIGRESSION( List([ frame.thread ]), frame.cursor.local_scope, frame.cursor )

def get_token( possible_token ):
	"""Transmute TAKE_FAILED into eof to make all token lists appear to end with an infinite stream of eofs"""
	if possible_token is take_failed:
		return sharp( eof )
	else:
		return possible_token

def process_accept( frame, state ):
	debug( 'accept' )
	return false

shift_count = 0
def process_shift( frame, state ):
	global shift_count
	shift_count += 1
	#debug_shift = silence
	#debug_shift( 'shift' )
	#debug_shift( "  cursor: %r", frame.cursor )
	#debug_shift( "  state: %s", state )
	token_sharp = bound( get_token( pop_list( frame.cursor, "tokens#" ) ), frame.cursor.local_scope )
	end_digression_if_finished( frame, frame.cursor.tokens )
	#debug_shift( "    value: %r", flat( token_sharp ) )
	frame[ 'operands' ] = cons( token_sharp, frame.operands )
	frame[ 'history' ] = cons( next_state( state, token_sharp, frame.fallback_function, token_sharp ), frame.history )
	#debug_shift( "  new_state: %r", state )
	if list_length( frame.operands ) > 50:
		error( frame.thread, RuntimeError( "Operand stack overflow" ) )
	return true

def process_reduce0( frame, state ):
	if meta_level( frame.thread ) >= printing_level_threshold:
		debug_reduce = debug
		debug2_reduce = silence
	else:
		debug_reduce = silence
		debug2_reduce = silence
	print_stuff( frame.thread )
	#debug2_reduce( ">-- reduce0 %s --", state.action_symbol )
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
	perform( frame, action, reduce_environment )
	end_digression_if_finished( frame, frame.cursor.tokens )
	print_program( frame.thread )
	return true

process = {
	'ACCEPT':  process_accept,
	'SHIFT':   process_shift,
	'REDUCE0': process_reduce0,
	}

def execute( procedure, action_bindings ): # One day we can get partial evaluation by having static and dynamic action_bindings
	frame = ACTIVATION( DIGRESSION( procedure.script, ENVIRONMENT( procedure.enclosing_scope ), dial_tone ), null, LIST( procedure.automaton.initial_state, null ), action_bindings, procedure.automaton.fallback_function, null )
	thread = THREAD( frame, null )
	frame[ 'thread' ] = thread # I don't love this back link, but it's really handy and efficient
	debug( "starting thread: %r with digression:\n\t%s", thread, frame.cursor )
	print_program( thread )
	execute2( frame, true )

def execute2( frame, keep_going ):
	# Actual Sheppard would use tail recursion, but Python doesn't have that, so we have to loop
	while keep_going == true: #is_a( keep_going, 'TRUE' ):
		#print_stuff( frame.thread )
		process_func = process[ tag( frame.history.head ) ]
		keep_going = process_func( frame, frame.history.head )


#####################################
#
# Meta-interpreter
#

meta_interpreter_text = """
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

let tag_edge_symbol
	tag_sharp
be_python
	<< ':' + tag_sharp >>

let pop_list 
	base field_symbol_sharp:MONIKER
be_python
	<< pop_list( base, field_symbol_sharp ) >>


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
	arg_bindings arg_symbol_sharp:MONIKER arg_value_sharp
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


let bound 
	obj_sharp environment:NULL
be
	obj_sharp

let bound 
	obj_sharp environment:ENVIRONMENT
be
	bound2
		obj_sharp
		environment
		take
			get environment bindings
			obj_sharp


let bound2 
	obj_sharp environment possible_match
be
	possible_match

let bound2 
	obj_sharp environment x=TAKE_FAILED
be
	bound
		obj_sharp
		get environment outer


let next_state 
	state obj_sharp fallback_function original_obj_for_error_reporting
be
	next_state2
		state
		obj_sharp
		take state obj_sharp
		fallback_function
		original_obj_for_error_reporting


let next_state2 
	state obj_sharp possible_match fallback_function original_obj_for_error_reporting
be
	possible_match

let next_state2 
	state obj_sharp x=TAKE_FAILED fallback_function original_obj_for_error_reporting
be
	set tag_sharp
		tag obj_sharp
	next_state_by_tag
		state
		tag_sharp
		fallback_function
		original_obj_for_error_reporting


let next_state_by_tag
	state tag_sharp=TAKE_FAILED fallback_function original_obj_for_error_reporting
do_python
	<< raise Unexpected_token( state, original_obj_for_error_reporting ) >>

let next_state_by_tag
	state tag_sharp fallback_function original_obj_for_error_reporting
be
	next_state_by_tag2
		state
		tag_sharp
		take
			state
			tag_edge_symbol tag_sharp
		fallback_function
		original_obj_for_error_reporting

let next_state_by_tag2
	state tag_sharp possible_next_state fallback_function original_obj_for_error_reporting
be
	possible_next_state

let next_state_by_tag2
	state tag_sharp x=TAKE_FAILED fallback_function original_obj_for_error_reporting
be
	next_state_by_tag
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


to perform  frame action:PYTHON_EXEC    reduce_environment  do_python << perform_python_exec( frame, action, reduce_environment ) >>
to perform  frame action:PYTHON_EVAL    reduce_environment  do_python << perform_python_eval( frame, action, reduce_environment ) >>
to perform  frame action:CURRENT_THREAD reduce_environment  do_python << perform_current_thread( frame, action, reduce_environment ) >>

to perform
	frame action:MACRO reduce_environment
do
	put frame cursor
		Digression
			get action script
			reduce_environment
			get frame cursor


/* Behave as though every token list ends with an infinite sequence of eofs */
let get_token possible_token be possible_token
let get_token x=TAKE_FAILED  be eof


let process 
	frame state:ACCEPT
be
	false

let process 
	frame state:SHIFT
be
	set token_sharp
		bound
			get_token
				pop_list
					get frame cursor
					tokens#
			get2 frame cursor local_scope
	end_digression_if_finished
		frame
		get2 frame cursor tokens
	put frame operands
		cons
			token_sharp
			get frame operands
	put frame history
		cons
			next_state
				state
				token_sharp
				get frame fallback_function
				token_sharp
			get frame history
	true

let process 
	frame state:REDUCE0
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
		frame
		get reduce_environment bindings
		get action formal_arg_names
	print_reduce_stuff frame action reduce_environment
	perform
		frame
		action
		reduce_environment
	end_digression_if_finished
		frame
		get2 frame cursor tokens
	print_program frame
	true


to print_stuff        frame                             do_python << print_stuff( frame.thread ) >>
to print_reduce_stuff frame action reduce_environment   do_python << print_reduce_stuff( frame.thread, action, reduce_environment ) >>
to print_program      frame                             do_python << print_program( frame.thread ) >>
to print              obj                               do_python << print str( obj ) >>


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
		process
			frame
			get2 frame history head

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

let fib  n  be
	+
		fib - n 1
		fib - n 2

let + a b   be_python  << a+b >>
let - a b   be_python  << a-b >>

to print_result  n  do_python << print "*** RESULT IS", n, "***" >>
"""

def fib_procedure():
	return parse_procedure( "fib", fib_text, [ 'print_result', 'fib', 2 ] )

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
else:
	#test_nfa2dfa()
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

