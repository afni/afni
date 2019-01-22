
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
      fprintf(stderr,
"\n"
"   consider something like:\n"
"\n"
"       setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
"       setenv AFNI_MODEL_PRF_STIM_DSET stim.144.LIA.bmask.resam+orig\n"
"       setenv AFNI_MODEL_PRF_ON_GRID NO\n"
"\n"
"       setenv AFNI_MODEL_PRF_GAUSS_FILE gauss_dset\n"
"\n"
"       # A=2, x=0.2, y=0.5,  sigma=0.05  sigrat=5  theta=PI/8=0.3927\n"
"       get_afni_model_PRF_6 2 .2 .5 .05 5 0.3927\n"
"\n");

      return 0;
   }

   /* fill parameter array */
   for( ind = 0; ind < 6; ind++ ) parms[ind] = atof(argv[ind+1]);

   result = (float *)malloc(nt*sizeof(float));
   conv_model(parms, nt, NULL, result);

   inputs_to_coords(g_saset, parms[1], parms[2], parms[3], parms[4], parms[5]);
   disp_floats("result: ", result, nt);

   return 0;
}

