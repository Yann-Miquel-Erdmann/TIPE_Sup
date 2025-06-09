#include <stdio.h>

int test(int n){
	int n;
	return n+1;
}

// hello
void main(void){
	// This is a comment line; it is ignored by the compiler
	printf("%s %d\n", "Hello, World!", test(1));
}
