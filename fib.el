
def fib 1 blockto
	|
	pop n
	pop _
	branchge recurse n 2
		1 return
	recurse
	add
		fib sub n 1
		fib sub n 2
	return
	|

blockto
	|
	pop n
	n print
	fib n print
	add n 1 pop n
	branchgt quit n 24
		n goto mainloop
	quit
	| pop mainloop

0 goto mainloop
