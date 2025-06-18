#include <stdio.h>
int fibonacci_rec(int n){


    int retval;

    if (n < 0){
        printf("%s", "Erreur: n ne doit pas etre negatif");
        retval =  -1;
        return retval;

    }
    if (n == 0){
        retval = 0;
        return retval;

    }    else if (n == 1){
        retval = 1;
        return retval;

    }    else {
        retval = retval;
    }

    retval = fibonacci_rec(n - 1) + fibonacci_rec(n - 2);


    return retval;
}
// fibonacci
void main(void){
    int n, result_rec;
    n = 10;
    // Appel de la fonction recursive
    result_rec = fibonacci_rec(n);
    printf("%s%d%s%d", "Fibonacci (recursif) de ", n, " est ", result_rec);



}