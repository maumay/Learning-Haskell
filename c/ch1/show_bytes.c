#include <stdio.h>

typedef unsigned char *byte_pointer;

void show_bytes(byte_pointer p, int len);

int main()
{
    int x = 312342;
    long int y = 312342;
    char s[4] = "abcd";
    show_bytes((byte_pointer) &x, sizeof(x));
}

void show_bytes(byte_pointer p, int len)
{
    for (int i = 0; i < len; i++) {
        printf(" %x", p[i]);
    }
    printf("\n");
}
