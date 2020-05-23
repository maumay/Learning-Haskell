#include <stdio.h>

int main()
{
    printf("To terminate enter: %d\n", EOF);
    for(int input = getchar(); input != EOF; input = getchar()) {
        putchar(input);
    }
}
