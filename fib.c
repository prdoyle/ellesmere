
#include <stdio.h>

static int fib(int n)
	{
	if( n < 2 )
		return 1;
	else
		return fib(n-1) + fib(n-2);
	}

int main(int argc, char **argv)
	{
	int i;
	int limit = (argc >= 1)? atoi(argv[1]) : 28;
	for( i=0; i <= limit; i++ )
		{
		printf("%d\n", i);
		printf("%d\n", fib(i));
		}
	return 0;
	}

