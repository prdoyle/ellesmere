
import string

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
#			return "%s#%d{ %s }" % ( self._tag, self._id, repr( self.outer ) )
		else:
			return "%s#%d" % ( self._tag, self._id )

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

