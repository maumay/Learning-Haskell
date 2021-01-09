#include <stdio.h>

int fun1(unsigned word);
int fun2(unsigned word);

int main()
{
    unsigned input[4] = {0x00000076, 0x87654321, 0x000000c9, 0xedcba987};

    printf("%-11s   %-11s   %-11s\n", "w", "fun1(w)", "fun2(w)");
    for (int i = 0; i < 4; i++) 
        printf(" 0x%08x 0x%08x 0x%08x\n", input[i], fun1(input[i]), fun2(input[i]));
}

int fun1(unsigned word)
{
    return (int) ((word << 24) >> 24);
}

int fun2(unsigned word)
{
    return ((int) word << 24) >> 24;
}
