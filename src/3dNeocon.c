#include "mrilib.h"

static void NCO_help(void) ;  /* prototype */


int main( int argc , char *argv[] )
{

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){ NCO_help(); exit(0); }

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif
   PRINT_VERSION("3dNeocon") ; AUTHOR("RW Cox");
   mainENTRY("3dNeocon main") ; machdep() ;
   AFNI_logger("3dNeocon",argc,argv) ;

}

/*-------------------------------------------------------------------------------*/

static void NCO_help(void)
{
   printf("\n"
    "This program fits a mixed linear/nonlinear deconvolution model to FMRI\n"
    "time series.  At present, it is experimental, so be careful out there.\n"
    "\n"
    "Usage: 3dNeocon ...\n"
    "\n"
    "Data Arguments\n"
    "--------------\n"
    "These arguments specify the input data and things about it.\n"
    "\n"
    "  -input dname      = read 3D+time dataset 'dname'\n"
    " [-mask mname]      = read dataset 'mname' as a mask\n"
    " [-automask]        = compute a mask from the 3D+time dataset\n"
    " [-TR tt]           = use 'tt' as the TR (in seconds)\n"
    " [-CENSORTR clist]  = like in 3dDeconvolve\n"
    " [-concat rname]    = like in 3dDeconvolve\n"
    " [-tpattern ppp]    = set the slice timing pattern to 'ppp', as in\n"
    "                      3dTshift, to override whatever slice timing\n"
    "                      information is in the '-input' dataset header;\n"
    "                      in particular, use 'zero' or 'equal' to specify\n"
    "                      that all slices are to be treated as acquired\n"
    "                      simultaneously (e.g., 3D imaging or 2D imaging\n"
    "                      with slice timing correction already performed)\n"
    "\n"
    "Baseline Model Arguments\n"
    "------------------------\n"
    "These arguments set up the baseline model, which is linear and estimated\n"
    "separately in each voxel (much as in 3dDeconvolve).\n"
    "\n"
    " [-polort pnum]     = like in 3dDeconvolve [default is 'pnum' == 'A']\n"
    "\n"
    " [-baseline bb]     = read 'bb' (a 1D file) for the baseline model;\n"
    "                      this file can have 1 or more columns:\n"
    "                      * if 1 column, then it is used in all slices\n"
    "                      * if more than 1 column, then 'bb' must have\n"
    "                        the same number of columns that the '-input'\n"
    "                        dataset has slices, and each column in 'bb'\n"
    "                        becomes part of the baseline model for only\n"
    "                        the corresponding slice\n"
    "\n"
    "Response Model Arguments\n"
    "------------------------\n"
    "These arguments specify the response model.  The Hemodynamic Response\n"
    "Function (HRF) model is nonlinear.  Given the HRF, the response to each\n"
    "stimulus is modeled additively.  Important notes:\n"
    " * Each slice might have a different time offset, in which case the HRF\n"
    "    will be applied slightly differently in each slice.\n"
    " * At least one '-stimtime' option must be given (or what would the\n"
    "    program be doing?).\n"
    " * The same HRF applies to all voxels -- this is one distinction\n"
    "    between 3dNeocon and 3dDeconvolve, where the HRF is different\n"
    "    in each voxel.  Only the amplitudes of the HRF fit vary between\n"
    "    voxels.\n"
    " * The HRF itself has amplitude 1 (maximum absolute value).  It is\n"
    "    the fit coefficients in each voxel that make the response model\n"
    "    vary in magnitude.\n"
    "\n"
    "  -stimtime tname label HRFmodel\n"
    "\n"
    "   'tname' is the same format as 3dDeconvolve's '-stim_times' option\n"
    "   'label' is a character string to identify the output coefficients\n"
    "   'HRFmodel' specifies the form of the nonlinear response model; the\n"
    "    possibilities are\n"
    "    * 'GAMVAR' == the HRF has two parameters: 'tpeak' and 'fwhm';\n"
    "        HRF(t) = (t/bc)^b exp(b-t/c) * Heaviside(t)\n"
    "        b      = (2.3*tpeak/fwhm)^2\n"
    "        c      = tpeak/b\n"
    "        tpeak is allowed to range from 5 to 8 s;\n"
    "        fwhm is allowed to range from 4 to 8 s.\n"
    "        This HRF choice is appropriate for brief (under 2 s)\n"
    "        activations in response to each stimulus.\n"
    "    * 'IGAMVAR(d)' == same as 'GAMVAR' but integrated over a duration\n"
    "        of 'd' seconds.  This HRF choice is designed for block-design\n"
    "        FMRI experiments.\n"
    "    * 'CSPLIN(b,t,n)' == same as in 3dDeconvolve, with the caveat that\n"
    "        the maximum amplitude of the HRF will be one.\n"
    "\n"
    " [-threshtype hhh]\n"
    "\n"
    "   'hhh' specifies the thresholding model used in the HRF analysis.\n"
    "   Given a set of HRF parameters, linear regression is used to fit\n"
    "   the baseline model and the baseline+response model in all voxels.\n"
    "   Those voxels that pass the threshold model (i.e., their response\n"
    "   fit is 'significant' in some sense) are used to judge the quality\n"
    "   of the overall fit.  The choices for 'hhh' are\n"
    "   * 'RCLU(p,c)' == the nominal R^2-statistic (assuming white noise)\n"
    "      is computed in each voxel, and those voxels with a p-value at\n"
    "      or below 'p' are kept, if they are in a cluster of at least 'c'\n"
    "      such contiguous voxels.  Subsequent to this thresholding, voxels\n"
    "      that fit better (have a larger R^2) are weighted more highly\n"
    "      in the HRF fitting objective function.\n"
    "   * At this time, there is no other thresholding model available.\n"
    "   * The default is currently 'RCLU(0.01,5)' which was just picked out\n"
    "      of thin air with no justification.  This default is subject to\n"
    "      change!\n"
    "\n"
    "Output Arguments\n"
    "----------------\n"
    " [-fitts fprefix]   = Output a fitted time series model dataset.\n"
    " [-errts eprefix]   = Output the residuals into a dataset.\n"
    " [-cbucket cprefix] = Output the fit coefficients into a dataset.\n"
   ) ;

   printf("\nAUTHOR = Zhark the Incurable, October 2007\n\n" ) ;
   return ;
}
