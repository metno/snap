#include <stdio.h>

#define PRT_SIZEOF(t) printf("sizeof(" #t ") = %d\n", sizeof(t))

int main()
{
  PRT_SIZEOF(void *);
  PRT_SIZEOF(char);
  PRT_SIZEOF(short int);
  PRT_SIZEOF(int);
  PRT_SIZEOF(long int);
  PRT_SIZEOF(long long int);
  PRT_SIZEOF(float);
  PRT_SIZEOF(double);
  return 0;
}
