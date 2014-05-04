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

class Symbol:

	def __init__( self, name ):
		self.name = name

	def make_production( self, rhs ):
		return Production( self, rhs )

	def __le__( self, rhs ): return self.make_production( rhs )

	def __repr__( self ): return "Symbol( %s )" % self.name

	def __str__( self ): return self.name

symbols_by_name = {}
name_regex = re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*")
def define_symbols( x ):
	for name in name_regex.findall( x ):
		symbol = Symbol( name )
		symbols_by_name[ name ] = symbol
		globals()[ name ] = symbol

class LR0_Automaton:

	def __init__():
		todo()

	def DR( transition ):
		todo()

	def reads( transition1, transition2 ):
		todo()

	def includes( transition1, transition2 ):
		todo()

	def lookback( state, production ):
		todo()

def digraph( Xs, R, F_prime ):
	"""
	DeRemer & Penello p.625
	R, F_prime are functions
	"""
	F = {}
	S = []
	N = dict( (X,0) for X in Xs )
	infinity = 1e9999
	def traverse( X ):
		S.append( X )
		d = len( S )
		N[X] = d
		F[X] = F_prime( X )
		for Y in Xs:
			if R( X,Y ):
				if N[Y] == 0:
					traverse( Y )
				N[X] = min( N[X], N[Y] )
				F[X] |= F[Y]
		if N[X] == d:
			while True:
				top = S.pop()
				N[ top ] = infinity
				if top == X:
					break
				else:
					F[ top ] = set( F[X] ) # Must make a copy
	for X in Xs:
		if N[X] == 0:
			traverse( X )
	return F

def test_nullable():
	define_symbols("A,B,C,D,E,F,G")
	gr = (
		( A <= [ A, B, B ] )
		+ ( A <= [] )
		+ ( B <= [ A ] )
		+ ( C <= [ A, B ] )
		+ ( D <= [ E ] )
		+ ( F <= [ A, G ] )
		)
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
	items = digraph( range(0,57), lambda x,y: y == loopy(x), lambda x:set([x]) ).items()
	items.sort()
	for i in items:
		print i

test_digraph()
