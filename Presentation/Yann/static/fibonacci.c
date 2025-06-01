#include <stdio.h>

int fibonacci_rec(int n) {
    int retval;

    if (n < 0) {
        printf("%s\n", "Erreur: n ne doit pas être négatif");
        retval = -1;
        return retval;
    }
    if (n == 0) {
        retval = 0;
        return retval;
    } else if (n == 1) {
        retval = 1;
        return retval;
    }

    retval = fibonacci_rec(n - 1) 
        + fibonacci_rec(n - 2);
    return retval;
}

// Fibonacci
void main(void) {
    int n, result_rec;
    n = 10;
    // Appel de la fonction récursive
    result_rec = fibonacci_rec(n);
    print("%s %d %s %d\n", "Fibonacci (récursif) de ", 
    n, " est ", result_rec);
}
