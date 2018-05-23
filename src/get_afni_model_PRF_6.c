
/* a wrapper for quick parameter and setup testing
 *
 * This should have an option for setting nt.
 *
 * R Reynolds   2014
 */

#include "mrilib.h"

#include "model_conv_PRF_6.c"

int main(int argc, char * argv[])
{
   float * result, parms[6];
   int     ind, nt = 144;

   if( argc != 7 ) {
      fprintf(stderr, "** usage: %s A x y sigma sigrat theta\n", argv[0]);
      return 1;
   }

   /* fill parameter array */
   for( ind = 0; ind < 6; ind++ ) parms[ind] = atof(argv[ind+1]);

   result = (float *)malloc(nt*sizeof(float));
   conv_model(parms, nt, NULL, result);

   inputs_to_coords(g_saset, parms[1], parms[2], parms[3], parms[4], parms[5]);
   disp_floats("result: ", result, nt);

   return 0;
}

