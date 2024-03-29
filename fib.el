#! ./ellesmere

# Conditional statements

def INT
	if TRUE then result:TOKEN_BLOCK else TOKEN_BLOCK end
as { result eval! }

def INT
	if FALSE then TOKEN_BLOCK else result:TOKEN_BLOCK end
as { result eval! }

def VOID
	if TRUE then result:TOKEN_BLOCK else TOKEN_BLOCK end
as { result eval! }

def VOID
	if FALSE then TOKEN_BLOCK else result:TOKEN_BLOCK end
as { result eval! }

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
		{ 1 0 add! } # Partial evaluation opportunity!
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
	{ n println! }

def VOID
	fibs( n:INT )
as
	{
#	bindings nMinus1 n-1 setfield!
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
#fib( 0 ) println!  # To measure parser-gen performance
#fib( 12 ) println! # Confirm it's still working and get reasonable-sized log files
 fib( 26 ) println! # For performance measurements
print( time )
