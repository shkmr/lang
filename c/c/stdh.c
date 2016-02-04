/*
 */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/*
 *     If you include both of these, va_list will be redefined,
 *     which results parse error (This is a bug).
 */
#if 0
#include <stdio.h>
#else
#include <stdarg.h>
#endif

/* EOF */
