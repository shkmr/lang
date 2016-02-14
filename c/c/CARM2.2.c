// Program to compute the square of
// the first 10 integers
#include <stdio.h>
void Squares( /* no arguments */ )
{
  int i;
  /*
      Loop from 1 to 10,
      printing out the squares
  */
  for (i=1; i<=10; i++)
    printf("%d //squared// is %d\n", i, i*i);
}
