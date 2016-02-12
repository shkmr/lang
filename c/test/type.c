#include <stdio.h>

/* duplicate typedef gives warning not error */

typedef signed char int8_a;
typedef signed char int8_a;

/* ``Normal'' type */

signed a_signed;
signed int signed_int;
signed long int signed_long_int;

/* These are invalid according to CARM, but clang accepts  */

long signed            long_signed;
long int signed        long_int_signed;
int long signed        int_long_signed;
long int long          long_int_long;

long double long_double;

/* print size of types */

main()
{
  printf("signed=%d\n",          sizeof a_signed);
  printf("signed int=%d\n",      sizeof signed_int);
  printf("signed long int=%d\n", sizeof signed_long_int);
  printf("long signed=%d\n",     sizeof long_signed);
  printf("long int signed=%d\n", sizeof long_int_signed);
  printf("int long signed=%d\n", sizeof long_int_signed);
  printf("long int long=%d\n",   sizeof long_int_long);
}
