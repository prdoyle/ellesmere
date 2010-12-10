#! /usr/bin/python -O

def fib( n ):
	if n < 2:
		return 1
	else:
		return fib( n-1 ) + fib( n-2 )

for i in range(29):
	print i
	print fib( i )
