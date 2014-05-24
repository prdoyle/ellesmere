#! /usr/bin/python -O

# Experiments with LALR parser generation

import re
import string

def debug( s, *v ):
	print s % v

def lstr( vs ):
	return "[%s]" % string.join([ str(v) for v in vs ], ', ')

def dstr( dictionary ):
	return "{%s}" % string.join([ "%s:%s"%(k,lstr(v)) for (k,v) in dictionary.items()], ', ')

def todo():
	raise NotImplementedError

class Grammar:

	def __init__( self, productions ):
		self.productions = []
		self.nonterminals = set()
		self.productions_by_lhs = {}
		for p in productions:
			self.append( p )

	def append( self, p ):
		self.productions.append( p )
		self.nonterminals.add( p.lhs )
		try:
			self.productions_by_lhs[ p.lhs ].append( p )
		except KeyError:
			self.productions_by_lhs[ p.lhs ] = [ p ]

	def __add__( self, p ):
		self.append( p ) # Note: modifies self!
		return self

	def __repr__( self ):
#		return "Grammar([ %s ])" % string.join([ repr(p) for p in self.productions ], ', ')
#
#	def __str__( self ):
		return string.join([ str(p) for p in self.productions ], ' + ')

	def complicated_nullable_nonterminals( self ):
		possibly_null_productions = set( p for p in self.productions if p.rhs_is_composed_of( self.nonterminals ) )
		productions_with_rhs_nonterminal = {}
		for s in set( p.lhs for p in possibly_null_productions ):
			productions_with_rhs_nonterminal[ s ] = set()
		for p in possibly_null_productions:
			for s in p.rhs:
				try:
					productions_with_rhs_nonterminal[ s ].add( p )
				except KeyError:
					pass # s is not the lhs of any possible null production, so ignore it
		debug( "productions_with_rhs_nonterminal:\n%s", dstr( productions_with_rhs_nonterminal ) )
		known_null_productions = set( p for p in possibly_null_productions if not p.rhs )
		todo = set( p.lhs for p in known_null_productions )
		debug( "initial todo: %s", lstr(todo) )
		result = set()
		while todo:
			nullable = todo.pop()
			debug( "pop: %s", str(nullable) )
			result.add( nullable )
			# Inefficient: rescans productions unnecessarily, and rescans each p.rhs repeatedly...
			todo.update( p.lhs for p in productions_with_rhs_nonterminal[ nullable ] if p.lhs not in result and p.rhs_is_composed_of( result ) )
		return result

	def nullable_nonterminals( self ):
		# I feel like there should be some way to compute this using a
		# variation on the digraph algorithm augmented to cope with
		# conjunctions (in addition to disjunctions, which it already
		# handles) by using DeMorgan's theorem to turn them into
		# disjunctions.  I still need to work out the details though.
		# For the time being, I'll just use the rather brute-force
		# algorithm above.
		return self.complicated_nullable_nonterminals()

	def augmented( self, goal_symbol ):
		result = Grammar( self.productions[:] )
		result.accept_production = ( accept_symbol <= [ goal_symbol, eof_symbol ] )
		result.append( result.accept_production )
		return result

	def is_augmented( self ):
		try:
			self.accept_production
			return True
		except AttributeError:
			return False

class Production:

	def __init__( self, lhs, rhs ):
		self.lhs = lhs
		self.rhs = rhs

	def __hash__( self ):
		return hash( self.lhs ) ^ hash(tuple( self.rhs ))

	def __eq__( self, other ):
		return self.lhs == other.lhs and self.rhs == other.rhs

	def rhs_is_composed_of( self, symbol_set ):
		return not [ t for t in self.rhs if t not in symbol_set ] # Unnecessarily scans the whole list

	def __repr__( self ):
#		return "Production( %s, [ %s ] )" % ( repr(self.lhs), string.join([ repr(s) for s in self.rhs ], ', ') )
#
#	def __str__( self ):
		return "%s <= ( %s )" % ( self.lhs, string.join([ str(s) for s in self.rhs ], ', ') )

	def make_grammar( self, other ):
		return Grammar([ self, other ])

	def __add__( self, other ): return self.make_grammar( other )

	def __hash__( self ): return hash( self.lhs ) ^ hash(tuple( self.rhs ))
	def __eq__( self, other ): return self.lhs == other.lhs and self.rhs == other.rhs

class Symbol:

	def __init__( self, name ):
		self.name = name

	def make_production( self, rhs ):
		return Production( self, rhs )

	def __le__( self, rhs ): return self.make_production( rhs )

	def __repr__( self ):
#		return "Symbol( %s )" % self.name
#
#	def __str__( self ):
		return self.name

	def __hash__( self ): return hash( self.name )
	def __eq__( self, other ): return self.name == other.name

accept_symbol = Symbol(" ACCEPT ")
eof_symbol    = Symbol(" EOF ")

symbols_by_name = {}
name_regex = re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*")
def define_symbols( x ):
	for name in name_regex.findall( x ):
		symbol = Symbol( name )
		symbols_by_name[ name ] = symbol
		globals()[ name ] = symbol

class LR0_Item:

	def __init__( self, production, dot ):
		self.production = production
		self.dot = dot

	def dot_symbol( self ):
		return self.production.rhs[ self.dot ]

	def is_rightmost( self ):
		return self.dot == len( self.production.rhs )

	def __hash__( self ): return hash( self.production ) ^ hash( self.dot )
	def __eq__( self, other ): return self.production == other.production and self.dot == other.dot

	def __repr__( self ):
		return "LR0_Item( %s, %s )" % ( repr( self.production ), repr( self.dot ) )

class Action:

	def name( self ):
		return "%s%d" % ( self.__class__.__name__, id(self) % 1000 )

	def __repr__( self ):
		return self.name()

class Shift( Action ):

	def __init__( self, target_state ):
		self.target_state = target_state

	def __repr__( self ):
		return "%s( %s )" % ( self.name(), self.target_state )

class Reduce( Action ):

	def __init__( self, production ):
		self.production = production

	def __repr__( self ):
		return "%s( %s )" % ( self.name(), self.production )

class Accept( Action ):

	def __init__( self ): pass

class ConflictError:

	def __init__( self, *actions ):
		self._actions = actions

	def __repr__( self ):
		return "ConflictError(%r)" % ( self._actions )

class State:

	def __init__( self ):
		self.actions = {}

	def __getitem__( self, symbol ):
		return self.actions[ symbol ]

	def __setitem__( self, symbol, action ):
		if symbol in self.actions:
			raise ConflictError
		else:
			self.actions[ symbol ] = action

class LR0_Automaton:

	def __init__( self, grammar ):
		"""Dragon 1st ed. p227 algorithm 4.8"""

		goal_symbol   = grammar.productions[0].lhs

		assert( grammar.is_augmented() )
		self.grammar = grammar

		epsilon = Symbol(" epsilon ")
		nullable_nonterminals = grammar.nullable_nonterminals()

		terminals = set()
		for p in grammar.productions:
			for s in p.rhs:
				if s not in grammar.nonterminals:
					terminals.add( s )
		debug( "terminals: %s", lstr( terminals ) )

		def FIRST_relation():
			"""
			My own linear-time algorithm using using DeRemer and Penello's "digraph" algorithm.
			"""
			debug( "-- FIRST_relation --" )
			immediate_FIRST = dict( ( n, set() ) for n in grammar.nonterminals )
			for t in terminals:
				immediate_FIRST[ t ] = set([ t ])
			for n in nullable_nonterminals:
				immediate_FIRST[ n ].add( epsilon )
			for p in grammar.productions:
				if p.rhs and p.rhs[0] not in grammar.nonterminals:
					immediate_FIRST[ p.lhs ].add( p.rhs[0] )
			debug( "immediate_FIRST: %s", dstr( immediate_FIRST ) )

			CAN_START_WITH = dict( ( n, set() ) for n in grammar.nonterminals | terminals )
			for p in grammar.productions:
				if not p.rhs:
					continue
				CAN_START_WITH[ p.lhs ].add( p.rhs[0] )
				for ( i, rhs_i ) in enumerate( p.rhs[:-1] ):
					if rhs_i in nullable_nonterminals:
						CAN_START_WITH[ p.lhs ].add( p.rhs[ i+1 ] )
					else:
						break # rhs_i is not nullable, so it breaks the CAN_START_WITH chain
			debug( "CAN_START_WITH: %s", dstr( CAN_START_WITH ) )

			result = digraph( grammar.nonterminals | terminals, CAN_START_WITH, immediate_FIRST )
			return result

		#debug( "FIRST: %s", dstr( FIRST_relation() ) )

		def FOLLOW_relation( FIRST ):
			"""
			My own linear-time algorithm using using DeRemer and Penello's "digraph" algorithm.
			"""
			debug( "-- FOLLOW_relation --" )
			immediate_FOLLOW = dict( ( n, set() ) for n in grammar.nonterminals )
			for p in grammar.productions:
				for ( i, s ) in enumerate( p.rhs[:-1] ):
					if s in grammar.nonterminals:
						immediate_FOLLOW[ s ] |= FIRST[ p.rhs[ i+1 ] ]

			CAN_END_UP_BEFORE = dict( ( n, set() ) for n in grammar.nonterminals )
			for p in grammar.productions:
				for ( i, s ) in enumerate( p.rhs[1:-1] ):
					neighbors = ( p.rhs[ i-1 ], p.rhs[ i+1 ] )
					if s in nullable_nonterminals and set( neighbors ) <= grammar.nonterminals:
						[ left, right ] = neighbors
						CAN_END_UP_BEFORE[ left ].add( right )
			result = digraph( grammar.nonterminals, CAN_END_UP_BEFORE, immediate_FOLLOW )
			return result

		#debug( "FOLLOW: %s", dstr( FOLLOW_relation( FIRST_relation() ) ) )

		def closure( items ):
			"""Dragon 1st ed. p223 fig 4.33"""
			result = set( items )
			length_before = 0
			while length_before != len( result ):
				length_before = len(result)
				for item in items:
					try:
						for production in grammar.productions_by_lhs[ item.dot_symbol() ]:
							result.add( LR0_Item( production, 0 ) )
					except IndexError:
						# item is a reduce item, so there's no dot_symbol.  Ignore it.
						pass
					except KeyError:
						# Dot symbol is a terminal.  Ignore it
						pass
			return frozenset( result )

		def goto( items, symbol ):
			"""Dragon 1st ed. p224"""
			result = set( LR0_Item( i.production, i.dot + 1 ) for i in items if not i.is_rightmost() and i.dot_symbol() == symbol )
			return closure( result )

		def all_item_sets():
			"""Dragon 1st ed. p224 fig 4.34"""
			root_item = LR0_Item( grammar.accept_production, 0 )
			result = set([ closure( frozenset([ root_item ]) ) ])
			length_before = 0
			while length_before != len( result ):
				length_before = len(result)
				for items in frozenset( result ):
					dot_symbols = frozenset( i.dot_symbol() for i in items if not i.is_rightmost() )
					for symbol in dot_symbols:
						result.add(frozenset( goto( items, symbol ) ))
			result.discard( frozenset() )
			return frozenset( result )

		def set_action( state, symbol, action ):
			try:
				if state[ symbol ] != action:
					raise ConflictError( state[ symbol ], action )
			except KeyError:
				state[ symbol ] == action

		FOLLOW = FOLLOW_relation( FIRST_relation() )
		item_sets = list( all_item_sets() )
		indexes_by_item_set = dict( ( item_set, index ) for ( index, item_set ) in enumerate( item_sets ) )
		states = []
		for ( index, items ) in enumerate( item_sets ):
			state = dict()
			states.append( state )
			for item in items:
				if item.is_rightmost():
					for a in FOLLOW[ item.production.lhs ]:
						if item.production.lhs is goal_symbol:
							# case 2c
							state[ a ] = Accept()
						else:
							# case 2b
							state[ a ] = Reduce( item.production )
				else:
					# case 2a
					state[ item.dot_symbol() ] = Shift( goto( items, item.dot_symbol() ) )
		self.states = states # Hmm, initial state??

	def __repr__( self ):
		return repr( self.__dict__ )

class LALR_Automaton( LR0_Automaton ):

	def __init__( self, grammar ):
		LR0_Automaton.__init__( self, grammar )

	def DR( self, transition ):
		todo()

	def reads_set( self, transition ):
		todo()

	def includes_set( self, transition ):
		todo()

	def lookback_set( self, state ):
		todo()

def digraph( Xs, R, F_prime ):
	"""
	DeRemer & Penello p.625
	R, F_prime are dicts.
	Values flow from Y to X for Y in R( X ); in other words, R needs to give a set of in edges for vertex X in the flow graph, which is counterintuitive.
	"""
	F = {}
	S = []
	N = dict( (X,0) for X in Xs )
	infinity = 1e9999
	def traverse( X ):
		S.append( X )
		d = len( S )
		N[X] = d
		F[X] = set( F_prime[ X ] )
		debug( "%d: F[%s] initialized to %s", d, X, F[X] )
		for Y in R[ X ]:
			if N[Y] == 0:
				traverse( Y )
			if X != Y:
				N[X] = min( N[X], N[Y] )
				F[X] = F[X] | F[Y]
				debug( "%d: F[%s] includes F[%s]", d, X, Y )
		if N[X] == d:
			while True:
				top = S.pop()
				N[ top ] = infinity
				if top == X:
					break
				else:
					F[ top ] = set( F[X] )
					debug( "%d: F[%s] becomes F[%s]", d, top, X )
	for X in Xs:
		if N[X] == 0:
			traverse( X )
	return F

def test_grammar():
	define_symbols("A,B,C,D,e,F,g,H")
	gr = (
		( A <= [ A, B, B ] )
		+ ( A <= [ B, g ] )
		+ ( A <= [] )
		+ ( B <= [ A ] )
		+ ( C <= [ A, B ] )
		+ ( D <= [ e ] )
		+ ( F <= [ A, g ] )
		+ ( H <= [ D, g ] )
		)
	return gr

def LR0_grammar():
	"""https://en.wikipedia.org/w/index.php?title=LR_parser&oldid=597146215#Grammar_for_the_Example_A.2A2_.2B_1"""
	define_symbols("Goal,Sums,Products,Value,plus,times,a,b")
	return (  ( Goal <= [ Sums, eof ] )
		+ ( Sums <= [ Sums, plus, Products ] )
		+ ( Sums <= [ Products ] )
		+ ( Products <= [ Products, times, Value ] )
		+ ( Products <= [ Value ] )
		+ ( Value <= [ a ] )
		+ ( Value <= [ b ] )
		)

def SLR_grammar():
	"""https://en.wikipedia.org/w/index.php?title=Simple_LR_parser&oldid=575142176 -- not LR(0)"""
	define_symbols("S,E,n")
	return (  ( S <= [ E ] )
		+ ( E <= [ E, n ] )
		+ ( E <= [ n ] )
		)

def LALR_grammar():
	"""Dragon p.229 -- not SLR"""
	define_symbols("S,L,R,eq,splat,i")
	return (  ( S <= [ L, eq, R ] )
		+ ( S <= [ R ] )
		+ ( L <= [ splat, R ] )
		+ ( L <= [ i ] )
		+ ( R <= [ L ] )
		)

def LR1_grammar():
	"""Dragon p.238 -- not LALR"""
	define_symbols("S,E,F,a,c,b,d,e")
	return (  ( S <= [ a, E, c ] )
		+ ( S <= [ a, F, d ] )
		+ ( S <= [ b, F, c ] )
		+ ( S <= [ b, E, d ] )
		+ ( E <= [ e ] )
		+ ( F <= [ e ] )
		)

def test_nullable():
	gr = test_grammar()
	print repr(gr)
	print gr
	nullable = gr.nullable_nonterminals()
	print "Nullable: ", nullable
	#print symbols_by_name

class DynaDict:

	def __init__( self, function ):
		self._function = function

	def __getattr__( self, key ):
		return self._function( key )

def test_digraph():
	def collatz( n ):
		if n % 2:
			return 3*n+1
		else:
			return n/2
	def loopy( n ):
		if n % 2:
			return n << n/2
		else:
			return n/2
	def down( n ):
		if n == 0:
			return 0
		else:
			return n-1
	items = digraph( xrange(0,21), DynaDict( lambda x: filter( lambda x: x<21, set([ loopy(x) ]) ) ), DynaDict( lambda x:set([x]) ) ).items()
	items.sort()
	for i in items:
		print i

def test_lr0():
	gr = test_grammar()
	gr = gr.augmented( gr.productions[0].lhs )
	print repr(gr)
	lr0 = LR0_Automaton( gr )
	print lr0

test_lr0()
