
def :VOID
	if
		:TRUE
	then
		:STATEMENT_BLOCK
	else
		:TOKEN_BLOCK
	end
as { return :VOID }

def :VOID
	if
		:FALSE
	then
		:TOKEN_BLOCK
	else
		:STATEMENT_BLOCK
	end
as { return :VOID }

def :INT
	fib ( :INT@n )
as
	{
	if n <= 1 then
		{ return 1 }
	else
		{ return fib(n-1) + fib(n-2) }
	end
	}

def :VOID
	fibs ( :INT@n )
as
	{
	if 1 <= n then
		{ fibs( n-1 ) }
	else
		{ }
	end
	print n
	print fib(n)
	return :VOID
	}

fibs( 24 )
