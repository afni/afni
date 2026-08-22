
/* a wrapper for quick parameter and setup testing
 *
 * 2025.04.12  initial program
 *
 * R Reynolds   2025
 */

#include "mrilib.h"

#include "model_conv_PRF_DN.c"

int main(int argc, char * argv[])
{
   float * result, parms[8];
   int     nparam=8, ind, nt = 0;

   if( argc != (nparam+2) ) {
      printf("** usage: %s NT  A0 x y Sig0 B0   A1 Sig1 B1\n", argv[0]);
      printf(
      "\n"
      "   This is mean to compute a model function for Conv_PRF_DN, given\n"
      "   NT and a set of parameter inputs.\n"
      "\n"
      "   consider something like:\n"
      "\n"
      "       setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
      "       setenv AFNI_MODEL_PRF_STIM_DSET stim.144.LIA.bmask.resam+orig\n"
      "       setenv AFNI_MODEL_PRF_ON_GRID NO\n"
      "\n"
      "       setenv AFNI_MODEL_PRF_GAUSS_FILE gauss_dset\n"
      "\n"
      "       # params: A0 X Y Sig0 B0  A1 Sig1 B1\n"
      "       set nt = `3dinfo -nt $AFNI_MODEL_PRF_STIM_DSET`\n"
      "       get_afni_model_PRF_DN $nt 5.9 -0.6 0.1  0.3 4.9  2.3 0.5 -1.4\n"
      "\n"
      );
      return 0;
   }

   /* set NT, now passed as an argument */
   nt = atoi(argv[1]);

   if ( nt < 10 ) {
      fprintf(stderr,"** invalid NT = %d (< 10), failing\n", nt);
      return 1;
   }

   /* fill parameter array, from argv offset by 2 */
   for( ind = 0; ind < nparam; ind++ ) parms[ind] = atof(argv[ind+2]);
   printf("-- have nt = %d\n", nt);

   result = (float *)malloc(nt*sizeof(float));
   conv_model(parms, nt, NULL, result);

   inputs_to_coords(g_saset, parms[1], parms[2], parms[3]);
   disp_floats("result: ", result, nt);

   return 0;
}

