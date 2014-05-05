#! /usr/bin/python -O

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
		return "Grammar([ %s ])" % string.join([ repr(p) for p in self.productions ], ', ')

	def __str__( self ): return string.join([ str(p) for p in self.productions ], ' + ')

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

	def nullable_nonterminals( self ): return self.complicated_nullable_nonterminals()

class Production:

	def __init__( self, lhs, rhs ):
		self.lhs = lhs
		self.rhs = rhs

	def __hash__( self ):
		return hash( self.lhs ) ^ hash(tuple( self.rhs ))

	def __eq__( self, other ):
		return self.lhs == other.lhs and self.rhs == other.rhs

	def rhs_is_composed_of( self, symbol_set ):
		return len([ t for t in self.rhs if t in symbol_set ]) == len( self.rhs )

	def __repr__( self ): return "Production( %s, [ %s ] )" % ( repr(self.lhs), string.join([ repr(s) for s in self.rhs ], ', ') )

	def __str__( self ): return "%s <= ( %s )" % ( self.lhs, string.join([ str(s) for s in self.rhs ], ', ') )

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

	def __repr__( self ): return "Symbol( %s )" % self.name

	def __str__( self ): return self.name

	def __hash__( self ): return hash( self.name )
	def __eq__( self, other ): return self.name == other.name

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

	def __repr__( self ): return "LR0_Item( %s, %s )" % ( repr( self.production ), repr( self.dot ) )
	def __str__( self ): return repr( self )

class Action: pass

class Shift( Action ):

	def __init__( self, target_state ):
		self.target_state = target_state

class Reduce0( Action ):

	def __init__( self, production ):
		self.production = production

class Accept( Action ):

	def __init__( self ): pass

class ConflictError: pass

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

		self.grammar = grammar

		epsilon = Symbol(" epsilon ")
		nullable_nonterminals = grammar.nullable_nonterminals()

		def FIRST_relation():
			"""My own algorithm"""
			terminals = set()
			for p in grammar.productions:
				for s in p.rhs:
					if s not in grammar.nonterminals:
						terminals.add( s )
			debug( "terminals: %s", lstr( terminals ) )

			EXPOSES = dict( ( n, set() ) for n in grammar.nonterminals | terminals)
			for p in grammar.productions:
				if not p.rhs:
					continue
				EXPOSES[ p.lhs ].add( p.rhs[0] )
				for ( i, rhs_i ) in enumerate( p.rhs[:-1] ):
					if rhs_i in nullable_nonterminals:
						EXPOSES[ p.lhs ].add( p.rhs[ i+1 ] )
					else:
						break # rhs_i is not nullable, so it breaks the exposure chain
			debug( "EXPOSES: %s", dstr( EXPOSES ) )

			immediate_FIRST = dict( ( n, set() ) for n in grammar.nonterminals )
			for t in terminals:
				immediate_FIRST[ t ] = set([ t ])
			for n in nullable_nonterminals:
				immediate_FIRST[ n ].add( epsilon )
			for p in grammar.productions:
				if p.rhs and p.rhs[0] not in grammar.nonterminals:
					immediate_FIRST[ p.lhs ].add( p.rhs[0] )
			debug( "immediate_FIRST: %s", dstr( immediate_FIRST ) )

			result = digraph( grammar.nonterminals | terminals, lambda x: EXPOSES[ x ], lambda x: immediate_FIRST[ x ] )
			return result

		debug( "FIRST: %s", dstr( FIRST_relation() ) )
		return

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
			return frozenset( result )

		def goto( items, symbol ):
			"""Dragon 1st ed. p224"""
			result = set( LR0_Item( i.production, i.dot + 1 ) for i in items if not i.is_rightmost() and i.dot_symbol() == symbol )
			return closure( result )

		def all_item_sets( goal_symbol ):
			"""Dragon 1st ed. p224 fig 4.34"""
			root_item = LR0_Item( ( Symbol(" accept ") <= [ goal_symbol ] ), 0 )
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

		item_sets = list( all_item_sets( grammar.productions[0].lhs ) )
		indexes_by_item_set = dict( ( item_set, index ) for ( index, item_set ) in enumerate( item_sets ) )
		states = []
		for ( index, items ) in enumerate( item_sets ):
			state = dict()
			states.append( state )
			for item in items:
				# TODO: Accept
				if item.is_rightmost():
					for a in follow( item.productions.lhs ):
						state[ a ] = Reduce( item.production )
				else:
					state[ item.dot_symbol() ] = Shift( goto( items, item.dot_symbol() ) )
		self.states = states # Hmm, initial state??

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
	R, F_prime are functions.
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
		F[X] = set( F_prime( X ) )
		Ys = R( X )
		debug( "%d: R( %s ) = %s", d, X, Ys )
		for Y in Ys:
			if N[Y] == 0:
				traverse( Y )
			N[X] = min( N[X], N[Y] )
			F[X] = F[X] | F[Y]
			debug( "%d: %s gets %s", d, X, Y )
		if N[X] == d:
			while True:
				top = S.pop()
				N[ top ] = infinity
				if top == X:
					break
				else:
					F[ top ] = set( F[X] )
					debug( "%d: %s becomes %s", d, top, X )
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

def test_nullable():
	gr = test_grammar()
	print repr(gr)
	print gr
	nullable = gr.nullable_nonterminals()
	print "Nullable: ", nullable
	#print symbols_by_name

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
	items = digraph( range(0,57), lambda x: set([ loopy(x) ]), lambda x:set([x]) ).items()
	items.sort()
	for i in items:
		print i

def test_lr0():
	gr = test_grammar()
	print repr(gr)
	lr0 = LR0_Automaton( gr )
	print lr0

test_lr0()
