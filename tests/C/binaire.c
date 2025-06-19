#include <stdio.h>
int affiche_binaire(int n){


    int res;
    int dividende, mod_2;
    int i;

    for (i = 0; i < 15; i=i + 1) {
        dividende = n / 2;
        mod_2 = n - 2 * dividende;
        printf("%d", mod_2);
        n = dividende;
    }    res = 0;



    return res;
}
// binaire
void main(void){
    int a = 16330;

    a = affiche_binaire(a);




}