
def :INT
	fib ! ( ! :INT@n ) !
as
	{
	ifneg
		n - 2
	then
		{ return 1 }
	end
	ifneg
		1 - n
	then
		{ return fib( n - 1 ) + fib( n - 2 ) }
	end
	}

print fib( 0 )
print fib( 1 )
print fib( 2 )
print fib( 16 )
