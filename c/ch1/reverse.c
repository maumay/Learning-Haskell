#include <stdio.h>
#define LIMIT 10

int getline2(char dest[]);
void reverse(char s[]);

main()
{
    char line[LIMIT + 1];
    int len = getline2(line);
    printf("Before %s\n", line);
    reverse(line);
    printf("After %s\n", line);
    return 0;
}

void reverse(char s[])
{
    int len;
    for (len = 0; s[len] != '\0'; len++);
    for (int i = 0; i < len / 2; i++) {
        char tmp = s[i];
        s[i] = s[len - 1 - i];
        s[len - 1 - i] = tmp;
    }
}

int getline2(char dest[]) 
{
    int i, c;
    for (i = 0, c = getchar(); i < LIMIT && c != EOF; i++, c = getchar()) {
        dest[i] = c;
    }
    dest[i] = '\0';
    while (c != EOF) {
        i++;
        c = getchar();
    }
    return i;
}
