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
