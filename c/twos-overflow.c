#include <stdio.h>

int tadd_ok_bad(int x, int y);

int main()
{
   int x = 0xfff00000;
   int y = 0xfab12000;
   printf("x + y = %d\n", x + y);
   printf("Overflows: %d\n", 1 - tadd_ok_bad(x, y));
}

int tadd_ok_bad(int x, int y)
{
    int sum = x + y;
    return (sum - x == y) && (sum - y == x);
}
