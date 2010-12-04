
| block # n -> fib(n)
	dup 2 recurse branchge
		pop
		1 return
	recurse
	# n
	dup 1 sub fib call
	# n fib(n-1)
	1 deep 2 sub fib call
	# n fib(n-1) fib(n-2)
	add
	# n fib(n)
	pop2
	return
| global fib set

| block # block n -> print fib(n) then re-run block with n+1
	dup fib call print
	1 add
	dup 30 quit branchge
	1 deep goto
	quit
| 0 1 deep goto
