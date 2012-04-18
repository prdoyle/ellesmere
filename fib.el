#! ./ellesmere

# Conditional statements

def INT
	if TRUE then result:INT else TOKEN_BLOCK end
as { result }

def INT
	if FALSE then TOKEN_BLOCK else result:INT end
as { result }

def VOID
	if TRUE then result:VOID else TOKEN_BLOCK end
as { result }

def VOID
	if FALSE then TOKEN_BLOCK else result:VOID end
as { result }

# Infix stuff

def INT
	( value:INT )
as { value }

def BOOLEAN
	left:INT <= right:INT
as { left right le! }

def l2r INT
	left:INT + right:INT
as { left right add! }

def l2r INT
	left:INT - right:INT
as { left right sub! }

# Fib implementation

def INT
	n:INT fib!
as
	{
	# Perfornance-critical so we use postfix form
	if n 1 le! then
		{ 1 }
	else
		{
		n 1 sub! fib!
		n 2 sub! fib!
		add!
		}
	end
	}

def INT
	fib( n:INT )
as
	{ n fib! }

def VOID
	print( n:INT )
as
	{ n print! }

def VOID
	fibs( n:INT )
as
	{
	bindings nMinus1 n-1 setfield!
	if 1 <= n then
		{ fibs( n-1 ) }
#		{ fibs( nMinus1 ) } # I'd like to do this here, but recording mode doesn't apply bindings, so it can't parse nMinus1
	else
		{ }
	end
	print( n )
	print( fib(n) )
	}

optimize

#fibs( 2 )
#fibs( 12 )
#fibs( 24 )
#fib( 0 ) print! # To measure parser-gen performance
fib( 26 ) print!
print( time )
