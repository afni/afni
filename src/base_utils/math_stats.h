#ifndef MATH_STATS_INCLUDED
#define MATH_STATS_INCLUDED

#include <math.h>

// -----------------------------------------------------------------------

/* Global: maximum r for r-to-Z (Fisher) transform

   Mathematically, the output Z is finite only for r \in (-1, 1). So
   we need to pick a value of r just inside these boundaries at which
   to freeze calculated Z values. Obviously, this MAX_R is the correct
   choice. Note that atanh(MAX_R) = 16.923; hence, we freeze the
   values at 17.0 */
#define MAX_R (0.999999999999996)

/* Quick function for common r-to-Z (Fisher) transformation.

   Input x should be in range (-1, 1); if it isn't, we don't output an
   error, just the frozen boundary value.
   
   Parameters:
   x      :(double) input Pearson r; should be in range (-1, 1)
   Returns:
   out    :(double) output Z-transformed version of r
*/
static inline double fast_r2z(double x)
{
   if (x < -MAX_R)
      return -17.0;
   if (x > MAX_R)
      return 17.0;

   return atanh(x);
}
// -----------------------------------------------------------------------



#endif
