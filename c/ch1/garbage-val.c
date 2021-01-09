#include <stdio.h>
#include <limits.h>
#include <math.h>

int main()
{
    printf("%d\n", CHAR_BIT);
    printf("%d\n", WORD_BIT);
    printf("%d\n", LONG_BIT);

    int n = 8;
    printf("%f\n", sqrt(n));
}
