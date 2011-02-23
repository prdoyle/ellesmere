
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

def TRUE
	left:INT <= right:INT
as { left right le! }

def FALSE
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
	fib( n:INT )
as
	{
	# Perfornance-critical so we use postfix form
	if n 1 le! then
		{ 1 }
	else
		{ fib( n 1 sub! ) fib( n 2 sub! ) add! }
	end
	}

def VOID
	fibs ( n:INT )
as
	{
	if 1 <= n then
		{ fibs( n-1 ) }
	else
		{ }
	end
	n print!
	fib(n) print!
	}

fibs( 24 )
