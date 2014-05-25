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
		result.accept_production = ( accept_symbol <= [ goal_symbol ] )
		result.append( result.accept_production )
		return result

	def is_augmented( self ):
		try:
			self.accept_production
			return True
		except AttributeError:
			return False

	def goal_symbol( self ):
		return self.accept_production.rhs[0]

	def accept_symbol( self ):
		return self.accept_symbol.lhs

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

accept_symbol = Symbol(" S' ")
eof_symbol    = Symbol(" $ ")

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
		before = self.production.rhs[ :self.dot ]
		after  = self.production.rhs[ self.dot: ]
		return "%s <= %s . %s" % ( self.production.lhs, string.join( map(str,before), " " ), string.join( map(str,after), " " ) )

class Action:

	def name( self ):
		return "%s%d" % ( self.__class__.__name__, id(self) % 1000 )

	def __repr__( self ):
		return self.name()

	def __eq__( self, other ):
		return self.__class__ is other.__class__ and self.components() == other.components()

	def __ne__( self, other ): return not self == other

	def components( self, other ):
		return None

class Shift( Action ):

	def __init__( self, target_state ):
		self.target_state = target_state

	def components( self ): return self.target_state

	def __repr__( self ):
		return "%s( %s )" % ( self.name(), self.target_state )

	def terse( self ): return "s%d" % self.target_state._number

class Reduce( Action ):

	def __init__( self, production, production_number ):
		self.production = production
		self.production_number = production_number

	def components( self ): return self.production

	def __repr__( self ):
		return "%s( %s )" % ( self.name(), self.production )

	def terse( self ):
		return "r%d" % ( self.production_number+1 ) # +1 to match 1-based indexing from Dragon book

class Accept( Action ):

	def __init__( self ): pass

	def terse( self ): return "acc"

class ConflictError( Exception ):

	def __init__( self, *actions ):
		self._actions = actions

	def __str__( self ):
		return str( self._actions )

class State:

	def __init__( self, number ):
		self._number = number
		self._actions = {}

	def __getitem__( self, symbol ):
		return self._actions[ symbol ]

	def __setitem__( self, symbol, action ):
		try:
			if action != self._actions[ symbol ]:
				raise ConflictError( self._actions[ symbol ], action )
		except KeyError:
			self._actions[ symbol ] = action

	def __repr__( self ):
		return "s%d" % self._number

def without_dupes( lst ):
	seen = set()
	return [ x for x in lst if ( not x in seen ) and ( not seen.add(x) ) ]

class LR0_Automaton:

	def __init__( self, grammar ):
		"""Dragon 1st ed. p227 algorithm 4.8"""

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
		self.terminals = terminals

		def FIRST_relation():
			"""
			My own linear-time algorithm using using DeRemer and Penello's "digraph" algorithm.
			See also Dragon p.189
			"""
			debug( "-- FIRST_relation --" )
			immediate_FIRST = dict( ( n, set() ) for n in grammar.nonterminals )
			for t in terminals:
				immediate_FIRST[ t ] = set([ t ]) # FIRST rule 1
			for n in nullable_nonterminals:
				immediate_FIRST[ n ].add( epsilon ) # FIRST rule 2
			for p in grammar.productions:
				if p.rhs and p.rhs[0] not in grammar.nonterminals:
					immediate_FIRST[ p.lhs ].add( p.rhs[0] ) # Part of FIRST rule 3: "everything in FIRST(Y1) is surely in FIRST(X)"
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

			# Am I actually getting any value out of digraph here?  Is it correct?  Is it nontrivial?
			result = digraph( grammar.nonterminals | terminals, CAN_START_WITH, immediate_FIRST )
			return result

		debug( "FIRST: %s", dstr( FIRST_relation() ) )

		def FOLLOW_relation( FIRST ):
			"""
			My own linear-time algorithm using using DeRemer and Penello's "digraph" algorithm.
			See also Dragon p.189
			"""
			debug( "-- FOLLOW_relation --" )
			immediate_FOLLOW = dict( ( n, set() ) for n in grammar.nonterminals )
			immediate_FOLLOW[ grammar.goal_symbol() ].add( eof_symbol ) # FOLLOW rule 1
			for p in grammar.productions:
				for ( i, s ) in enumerate( p.rhs[:-1] ):
					if s in grammar.nonterminals:
						# FOLLOW rule 2, combined with definition of FIRST(beta) minus epsilon
						for subsequent in p.rhs[ i+1: ]:
							immediate_FOLLOW[ s ] |= FIRST[ subsequent ]
							immediate_FOLLOW[ s ].discard( epsilon )
							if subsequent not in nullable_nonterminals:
								break

			# CAN_END captures FOLLOW rule 3
			CAN_END = dict( ( n, set() ) for n in grammar.nonterminals )
			for p in grammar.productions:
				if p.rhs and p.rhs[-1] in grammar.nonterminals:
					CAN_END[ p.rhs[-1] ].add( p.lhs )
				for ( i, s ) in enumerate( p.rhs[:-1] ):
					if s in grammar.nonterminals:
						epilogue = p.rhs[ i+1: ]
						debug( "Exploring %s epilogue %s because of production %s", s, epilogue, p )
						if set(epilogue) <= nullable_nonterminals:
							CAN_END[ s ].add( p.lhs )
			debug( "-- i_FOLLOW: %s", immediate_FOLLOW )
			debug( "--  CAN_END: %s", CAN_END )
			result = digraph( grammar.nonterminals, CAN_END, immediate_FOLLOW )
			return result

		#debug( "FOLLOW: %s", dstr( FOLLOW_relation( FIRST_relation() ) ) )

		def closure( items ):
			"""Dragon 1st ed. p223 fig 4.33"""
			# Note: we use lists here instead of sets to preserve ordering so we can
			# validate results against the Dragon book.  Sets would be more efficient.
			#result = set( items )
			result = list( items )
			length_before = 0
			while length_before != len( result ):
				length_before = len(result)
				for item in list(result):
					try:
						for production in grammar.productions_by_lhs[ item.dot_symbol() ]:
							item = LR0_Item( production, 0 )
							#result.add( item )
							if not item in result:
								result.append( item )
					except IndexError:
						# item is a reduce item, so there's no dot_symbol.  Ignore it.
						pass
					except KeyError:
						# Dot symbol is a terminal.  Ignore it
						pass
			#return frozenset( result )
			return result

		def goto_items( items, symbol ):
			"""Dragon 1st ed. p224"""
			result = set( LR0_Item( i.production, i.dot + 1 ) for i in items if not i.is_rightmost() and i.dot_symbol() == symbol )
			return closure( result )

		def goto_state( items, symbol ):
			return states[ indexes_by_item_set[ frozenset(goto_items(items,symbol)) ] ]

		#
		# SLR table construction, Dragon 1st eg. p227 Algorithm 4.8
		#

		def all_item_sets():
			"""Dragon 1st ed. p224 fig 4.34"""
			root_item = LR0_Item( grammar.accept_production, 0 )
			I0 = closure([ root_item ])
			debug( "I0: %s" % I0 )
			result = [ I0 ]
			already_added = set( frozenset( I0 ) )
			length_before = 0
			while length_before != len( result ):
				length_before = len(result)
				for items in result:
					#dot_symbols = frozenset( i.dot_symbol() for i in items if not i.is_rightmost() )
					dot_symbols = [ i.dot_symbol() for i in items if not i.is_rightmost() ]
					dot_symbols = without_dupes( dot_symbols )
					for symbol in dot_symbols:
						goto = goto_items( items, symbol )
						goto_set = frozenset( goto )
						if goto and goto_set not in already_added:
							already_added.add( goto_set )
							debug( "I%d: %s" % ( len(result), goto ) )
							result.append( goto )
			return result

		item_sets = all_item_sets()
		indexes_by_item_set = dict( ( frozenset(item_set), index ) for ( index, item_set ) in enumerate( item_sets ) )
		states = [ State(i) for i in range( 0, len(item_sets) ) ]

		FOLLOW = FOLLOW_relation( FIRST_relation() )
		debug( "FOLLOW: %s", FOLLOW )
		for ( index, items ) in enumerate( item_sets ):
			state = states[ index ]
			for item in items:
				if item.is_rightmost():
					if item.production is self.grammar.accept_production:
						# case 2c
						state[ eof_symbol ] = Accept()
					else:
						# case 2b
						for a in FOLLOW[ item.production.lhs ]:
							state[ a ] = Reduce( item.production, self.grammar.productions.index( item.production ) )
				else:
					# case 2a
					state[ item.dot_symbol() ] = Shift( goto_state( items, item.dot_symbol() ) )
		self.states = states # Hmm, initial state??

	def __repr__( self ):
		return repr( self.__dict__ )

	def print_table( self ):
		def entry( state, symbol ):
			try:
				return state[symbol].terse()
			except KeyError:
				return ""
		terminals = list( self.terminals )
		nonterminals = self.grammar.nonterminals
		print "STATE" + string.join( "\t%r" % t for t in terminals ) + string.join( "\t%r" % n for n in nonterminals )
		for state in self.states:
			print repr(state) + string.join( "\t%s" % entry( state,t ) for t in terminals ) + string.join( "\t%s" % entry( state,n ) for n in nonterminals )

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

def grammar_411():
	"""Dragon p.176"""
	define_symbols("E,Ep,T,Tp,F,plus,splat,i,l,r")
	return (
		( E <= [ T,Ep ] )
		+ ( Ep <= [ plus,T,Ep ] ) + ( Ep <= [] )
		+ ( T <= [ F,Tp ] )
		+ ( Tp <= [ splat,F,Tp] ) + ( Tp <= [] )
		+ ( F <= [ l,E,r ] ) + ( F <= [ i ] )
		)

def grammar_419():
	"""Dragon p.222"""
	# Item sets in Fig 4.35 p.225
	# Parsing table in Fig 4.31 p.219
	define_symbols("E,T,F,plus,splat,i,l,r")
	return (
		( E <= [ E,plus,T ] ) + ( E <= [ T ] )
		+ ( T <= [ T,splat,F ] ) + ( T <= [ F ] )
		+ ( F <= [ l, E, r ] ) + ( F <= [ i ] )
		)

def grammar_421():
	"""Dragon p.231"""
	define_symbols("S,C,c,d")
	return ( ( S <= [ C,C ] )
		+ ( C <= [ c,C ] )
		+ ( C <= [ d ] )
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

def test_SLR( gr ):
	gr = gr.augmented( gr.productions[0].lhs )
	print repr(gr)
	lr0 = LR0_Automaton( gr )
	# Should look like fig 4.31 p 219
	lr0.terminals = [ i, plus, splat, l, r, eof_symbol ]
	lr0.grammar.nonterminals = [ E, T, F ]
	lr0.print_table()

test_SLR( grammar_419() )
