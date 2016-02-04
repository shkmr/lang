#include <signal.h> /* we somewhat need this to successfully parse stdint.h */
#include <stdint.h>

typedef signed char int8_a;
typedef unsigned char int8_b, int8_x;
typedef unsigned char int8_c, *int8_y;
typedef char *int8_d;
typedef char **int8_d;
typedef char ***int8_d;
typedef char ***int8_e;
typedef char (int8_e)[];
typedef char int8_f[];
typedef char (int8_g)[];
typedef char (int8_g)[10];
typedef char (*int8_g)[10];
typedef char *(*int8_g)[10];

static unsigned char a, *b, c[10], (*d)[10];
static unsigned char a='a', *b, c[10], (*d)[10];

struct X {
  int a;
  char *p;
  struct X *next;
};

struct X y[10];

enum dow {
  Mon, Tue, Wed,
};

enum bool {
  false = 0,
  true
} coo;

static int char_num(int c)
{
  /* Assuming ASCII family */

  switch (c) {
  case '0':  case '1':  case '2':  case '3':  case '4':
  case '5':  case '6':  case '7':  case '8':  case '9':
    return c - '0';
  case 'A':  case 'B':  case 'C':
  case 'D':  case 'E':  case 'F':
    return c - 'A' + 10;
  case 'a':  case 'b':  case 'c':
  case 'd':  case 'e':  case 'f':
    return c - 'a' + 10;
  default:
    return 0;
  }
}


