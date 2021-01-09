#include <stdio.h>

main()
{
    unsigned short x = 65535; // == 2^16 - 1
    printf("%d\n", (short) x); 
    // -1

    x = 25;
    printf("%d\n", (short) x);
    // 25

    short y = -10;
    printf("%d\n", (unsigned short) y);
    // 65526 == -10 + 2^16

    y = 1243;
    printf("%d\n", (unsigned short) y);
    // 1243
}
