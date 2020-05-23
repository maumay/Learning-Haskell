#include <stdio.h>
#define LOWER 0
#define UPPER 300
#define STEP 10

int main()
{
    printf("-- %4s --", "F2C");
    for(float fahr = LOWER; fahr < UPPER; fahr += STEP) {
        float celsius = (5.0 / 9.0) * (fahr - 32);
        printf("%3.0f %6.2f\n", fahr, celsius);
    }
}
