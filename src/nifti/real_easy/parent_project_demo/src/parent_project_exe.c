/* A simple program to test linkage against the nifticdf package */
#include "nifticdf.h"
int main()
{
  double input= 7.0;
  const double output = alnrel(&input);

  return (output > 0.0) ? EXIT_SUCCESS: EXIT_FAILURE ;
}
