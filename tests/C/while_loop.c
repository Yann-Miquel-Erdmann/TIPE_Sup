#include <stdio.h>

void factorial(void){
	//! define variables
	int nfact;
	int n;
	n = 1;
	nfact = 1;
	//! compute factorials   
	while (n <= 10){
		nfact = nfact * n;
		n = n + 1;
		printf("%i %s %i \n" ,n," ",nfact);
	}
}