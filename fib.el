
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

def INT
	fib ( n:INT )
as
	{
	if n <= 1 then
		{ 1 }
	else
		{ fib(n-1) + fib(n-2) }
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
	print n
	print fib(n)
	}

fibs( 24 )
