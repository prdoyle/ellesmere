"Hello, world!" print

# Three = 1+2
1 2 add
	dup print
	global Three set

# MyInt = new Integer
Integer new
	global MyInt set

# MyInt.Value = 123
123
	MyInt Value set

# print Three + MyInt.Value
MyInt Value get
	dup print
	Three add print

# if Three != 0 print "Not zero"
Three
| ifzero
	"Not zero" print
	hop
|

# if Three - 3 != 0 print "Not three"
Three -3 add
| ifzero
	"Not three" print
	hop
|

| block
	"Ran func1" print
	return
| global Func1 set

Func1 call
Func1 call

| block
	add print
	return
| global PrintSum set

1 2 PrintSum call
123 456 PrintSum call
