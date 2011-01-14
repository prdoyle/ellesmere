
def :INT
	fib ! :INT@n
as
	{
	ifneg
		n - 2
	then
		{ 1 }
	end
	ifneg
		1 - n
	then
		{ fib n - 1 + fib n - 2 }
	end
	}

fib 3
