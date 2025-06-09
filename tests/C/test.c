#include <stdio.h>
float func(float arg1, float arg2)
{
	float func;
	float arg1;
	float arg2;
	func = arg1 + arg2 * 3;
}
// addNumbers
void main(void)
{
	//! ceci est un commentaire
	float a, b, res;
	int n;
	float res;

	char s[3] = "sad", d[3] = "tet";
	n = 7;
	a = 12.0;
	b = 15.0;
	if (a == 12.0)
	{
		printf("%s \n", "'a = 12'");
	}
	else
	{
		printf("%s \n", "'a!=12'");
	}
	res = func(a, b);
	printf("%s %f \n", "'The result is '", res);
	printf("%s %d \n", "'The result is '", n);
}
