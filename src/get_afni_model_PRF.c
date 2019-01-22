/* a wrapper for quick parameter and setup testing
 *
 * This should have an option for setting nt, but that
 * would probably take multiple minutes of work.
 *
 * R Reynolds   2014
 */

#if defined(LINUX)
# include <malloc.h>
#endif

#include "mrilib.h"

#include "model_conv_PRF.c"

int main(int argc, char * argv[])
{
   float * result, parms[4];
   int   nt = 144;

   if( argc <= 2 ) {
      fprintf(stderr, "** usage: %s A x y sigma\n", argv[0]);
      return 0;  /* gentle help-style exit */
   } else if( argc != 5 ) {
      fprintf(stderr, "** usage: %s A x y sigma\n", argv[0]);
      return 1;
   }

   parms[0] = atof(argv[1]);
   parms[1] = atof(argv[2]);
   parms[2] = atof(argv[3]);
   parms[3] = atof(argv[4]);

   result = (float *)malloc(nt*sizeof(float));
   conv_model(parms, nt, NULL, result);

   inputs_to_coords(g_saset, parms[1], parms[2], parms[3]);
   disp_floats("result: ", result, nt);

   return 0;
}

