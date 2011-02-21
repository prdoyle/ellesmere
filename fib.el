
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
	n:INT fib!
as
	{
	if n 1 le! then
		{ 1 }
	else
		{ n 1 sub! fib! n 2 sub! fib! add! }
	end
	}

def VOID
	fibs ( n:INT )
as
	{
	if 1 n le! then
		{ fibs( n 1 sub! ) }
	else
		{ }
	end
	n print!
	n fib! print!
	}

fibs( 24 )
