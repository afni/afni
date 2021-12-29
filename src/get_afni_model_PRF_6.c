
/* a wrapper for quick parameter and setup testing
 *
 * 2020.07.21  added initial parameter for NT
 *
 * R Reynolds   2014
 */

#include "mrilib.h"

#include "model_conv_PRF_6.c"

int main(int argc, char * argv[])
{
   float * result, parms[6];
   int     ind, nt = 0;

   if( argc != 8 ) {
      printf("** usage: %s NT A x y sigma sigrat theta\n", argv[0]);
      printf(
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
         "       set nt = `3dinfo -nt $AFNI_MODEL_PRF_STIM_DSET`\n"
         "       get_afni_model_PRF_6 $nt 2 .2 .5 .05 5 0.3927\n"
         "\n");

      return 0;
   }

   /* set NT, now passed as an argument */
   nt = atoi(argv[1]);

   if ( nt < 10 ) {
      fprintf(stderr,"** invalid NT = %d (< 10), failing\n", nt);
      return 1;
   }

   /* fill parameter array, from argv offset by 2 */
   for( ind = 0; ind < 6; ind++ ) parms[ind] = atof(argv[ind+2]);
   printf("-- have nt = %d\n", nt);

   result = (float *)malloc(nt*sizeof(float));
   conv_model(parms, nt, NULL, result);

   inputs_to_coords(g_saset, parms[1], parms[2], parms[3], parms[4], parms[5]);
   disp_floats("result: ", result, nt);

   return 0;
}

