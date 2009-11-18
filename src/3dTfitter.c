/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See http://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

static void vstep_print(void) ; /* prototype */
static float lhs_legendre( float x, float bot, float top, float n ) ;

#define IC_POLORT 66666

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk , nx,ny,nz,nvox , vstep=0 ;
   THD_3dim_dataset *rhset=NULL ; char *rhsnam="?" ; int rhs1D=0 ;
   THD_3dim_dataset *lset ; MRI_IMAGE *lim ; int nlset=0 , nlhs=0 ;
   THD_3dim_dataset *fset=NULL ;
   XtPointer_array *dsar ;
   int ntime , nvar=0 , polort=-1,npol=0 ;
   char *prefix="Tfitter" ;
   int meth=2 , nbad=0,ngood=0,nskip=0 ;
   intvec *convec=NULL , *kvec=NULL ;
   byte *mask=NULL ; int mnx=0,mny=0,mnz=0 ;
   floatvec *bfit ;
   float *dvec , **rvec=NULL , *cvec=NULL , *evec ;
   char **lab=NULL ; int nlab=0 ;
   int verb=1 ;

   THD_3dim_dataset *fal_set=NULL ; MRI_IMAGE *fal_im=NULL ;
   char *fal_pre=NULL ; int fal_pencod=1, fal_klen=0 , fal_dcon=0 ;
   float *fal_kern=NULL , fal_penfac=0.0f ;
   THD_3dim_dataset *defal_set=NULL ;
   int nvoff=0 ;

   char *fitts_prefix=NULL; THD_3dim_dataset *fitts_set=NULL;
   char *ersum_prefix=NULL; THD_3dim_dataset *ersum_set=NULL; /* 23 Jul 2009 */
   int do_fitts=0 ;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dTfitter [options]\n"
      "\n"
      "* At each voxel, assembles and solves a set of linear equations.\n"
      "\n"
      "* Output is a bucket dataset with the beta parameters at each voxel.\n"
      "\n"
      "* Can also get output of fitted time series at each voxel, and\n"
      "  the error sum of squares (e.g., for generating statistics).\n"
      "\n"
      "* Can also deconvolve with a known kernel function (e.g., HRF model),\n"
      "  in which case the output dataset is a new time series dataset,\n"
      "  with the estimate of the source function that, when convolved\n"
      "  with the kernel function, fits the data (in each voxel).\n"
      "\n"
      "* The basic idea is to compute the beta_i so that the following\n"
      "  is approximately true:\n"
      "\n"
      "         RHS(t) = sum { beta_i * LHS_i(t) }\n"
      "                   i>=1\n"
      "\n"
      "  With the '-FALTUNG' option, the model expands to be\n"
      "\n"
      "         RHS(t) = sum { K(j)*S(t-j) } + sum { beta_i * LHS_i(t) }\n"
      "                   j>=0                  i>=1\n"
      "\n"
      "  where K() is the user-supplied causal kernel function, and S() is\n"
      "  the source time series to be estimated along with the betas.\n"
      "\n"
      "* The model basis functions LHS_i(t) and the kernel function K(t)\n"
      "  can be .1D files (fixed for all voxels) or 3D+time datasets\n"
      "  (different for each voxel).\n"
      "\n"
      "* The fitting approximation can be done in the L2 (least squares)\n"
      "  or L1 (least sum of absolute errors) senses.  Which one is better?\n"
      "  The answer to that question depends strongly on what you are\n"
      "  going to use the results for!  And on the quality of the data.\n"
      "\n"
      "-------------------------------------\n"
      "Lots of Options (some are mandatory):\n"
      "-------------------------------------\n"
      "  -RHS rset = Specifies the right-hand-side 3D+time dataset.\n"
      "                ('rset' can also be a 1D file with 1 column)\n"
      "             * Exactly one '-RHS' option must be given to 3dTfitter.\n"
      "\n"
      "  -LHS lset = Specifies a column (or columns) of the left-hand-side matrix.\n"
      "             * More than one 'lset' can follow the '-LHS' option, but each\n"
      "               input filename must NOT start with the '-' character!\n"
      "             * Or you can use multiple '-LHS' options, if you prefer.\n"
      "             * Each 'lset' can be a 3D+time dataset, or a 1D file\n"
      "               with 1 or more columns.\n"
      "             * A 3D+time dataset defines one column in the LHS matrix.\n"
      "              ++ If 'rset' is a 1D file, then you cannot input a 3D+time\n"
      "                 dataset with '-LHS'.\n"
      "              ++ If 'rset' is a 3D+time dataset, then the 3D+time dataset(s)\n"
      "                 input with '-LHS' must have the same voxel grid as 'rset'.\n"
      "             * A 1D file defines as many columns in the LHS matrix as\n"
      "               are in the file.\n"
      "              ++ For example, you could input the LHS matrix from the\n"
      "                 .xmat.1D file output by 3dDeconvolve, if you wanted\n"
      "                 to repeat the same linear regression using 3dTfitter,\n"
      "                 for some bizarre unfathomable twisted psychotic reason.\n"
      "            ** If some LHS vector is very small (less than a factor of 0.000333)\n"
      "               compared to the largest LHS vector, then it will be ignored\n"
      "               in the fitting.  This feature allows the case where some LHS\n"
      "               dataset voxels are all zero.  [Per Rasmus Birn et al.]\n"
      "           *** Columns are assembled in the order given on the command line,\n"
      "               which means that LHS parameters will be output in that order!\n"
      "           *** If all LHS inputs are 1D vectors AND you are using least\n"
      "               squares fitting without constraints, then 3dDeconvolve would\n"
      "               be more efficient, since each voxel would have the same set\n"
      "               of equations -- a fact that 3dDeconvolve exploits for speed.\n"
      "              ++ But who cares about CPU time?  Burn, baby, burn!\n"
      "\n"
      "  -polort p = Add 'p+1' Legendre polynomial columns to the LHS matrix.\n"
      "             * These columns are added to the LHS matrix AFTER all other\n"
      "               columns specified by the '-LHS' option, even if the '-polort'\n"
      "               option appears before '-LHS' on the command line.\n"
      "             * By default, NO polynomial columns will be used.\n"
      "\n"
      "  -label lb = Specifies a sub-brick label for the output LHS parameter dataset.\n"
      "             * More than one 'lb' can follow the '-label' option;\n"
      "               however, each label must NOT start with the '-' character!\n"
      "             * Labels are applied in the order given.\n"
      "             * Normally, you would provide exactly as many labels as\n"
      "               LHS columns.  If not, the program invents some labels.\n"
      "\n"
      "............................................................................\n"
      "  -FALTUNG fset fpre pen fac\n"
      "            = Specifies a convolution (German: Faltung) model to be\n"
      "              added to the LHS matrix.  Four arguments follow the option:\n"
      "\n"
      "         -->** 'fset' is a 3D+time dataset or a 1D file that specifies\n"
      "               the known kernel of the convolution.\n"
      "             * fset's time point [0] is the 0-lag point in the kernel,\n"
      "               [1] is the 1-lag into the past point, etc.\n"
      "              ++ Call the data z(t), the unknown signal s(t), and the\n"
      "                 known kernel h(t).  The equations being solved for\n"
      "                 the set of all s(t) values are of the form\n"
      "                   z(t) = h(0)s(t) + h(1)s(t-1) + ... + h(L)s(t-L) + noise\n"
      "                 where L is the last value in the kernel function.\n"
      "            ++++ N.B.: The TR of 'fset' and the TR of the RHS dataset\n"
      "                       MUST be the same, or the deconvolution results\n"
      "                       will be meaningless drivel!\n"
      "\n"
      "         -->** 'fpre' is the prefix for the output time series to\n"
      "               be created -- it will have the same length as the\n"
      "               input 'rset' time series.\n"
      "              ++ If you don't want this time series (why?), set 'fpre'\n"
      "                 to be the string 'NULL'.\n"
      "\n"
      "         -->** 'pen' selects the type of penalty function to be\n"
      "               applied to constrain the deconvolved time series:\n"
      "              ++ The following penalty functions are available:\n"
      "                   P0[s] = f^q * sum{ |s(t)|^q }\n"
      "                   P1[s] = f^q * sum{ |s(t)-s(t-1)|^q }\n"
      "                   P2[s] = f^q * sum{ |s(t)-0.5*s(t-1)-0.5*s(t+1)|^q }\n"
      "                 where s(t) is the deconvolved time series;\n"
      "                 where q=1 for L1 fitting, q=2 for L2 fitting;\n"
      "                 where f is the value of 'fac' (defined below).\n"
      "                   P0 tries to keep s(t) itself small\n"
      "                   P1 tries to keep point-to-point fluctuations\n"
      "                      in s(t) small (1st derivative)\n"
      "                   P2 tries to keep 3 point fluctuations\n"
      "                      in s(t) small (2nd derivative)\n"
      "              ++ In L2 regression, these penalties are like Wiener\n"
      "                 deconvolution with noise spectra proportional to\n"
      "                   P0 ==> f^2 (constant in frequency)\n"
      "                   P1 ==> f^2 * freq^2\n"
      "                   P2 ==> f^2 * freq^4\n"
      "                 However, 3dTfitter does deconvolution in the time\n"
      "                 domain, not the frequency domain, and you can choose\n"
      "                 to use L2 or L1 regression.\n"
      "              ++ The value of 'pen' is one of the following 7 cases:\n"
      "                     0 = use P0 only\n"
      "                     1 = use P1 only\n"
      "                     2 = use P2 only\n"
      "                    01 = use P0+P1 (the sum of these two functions)\n"
      "                    02 = use P0+P2\n"
      "                    12 = use P1+P2\n"
      "                   012 = use P0+P1+P2 (sum of three penalty functions)\n"
      "                 If 'pen' does not contain any of the digits 0, 1, or 2,\n"
      "                 then '01' will be used.\n"
      "\n"
      "         -->** 'fac' is the positive weight 'f' for the penalty function:\n"
      "              ++ if fac < 0, then the program chooses a penalty factor\n"
      "                 for each voxel separately and then scales that by -fac.\n"
      "              ++ use fac = -1 to get this voxel-dependent factor unscaled.\n"
      "              ++ fac = 0 is a special case: the program chooses a range\n"
      "                 of penalty factors, does the deconvolution regression\n"
      "                 for each one, and then chooses the fit it likes best\n"
      "                 (as a tradeoff between fit error and solution size).\n"
      "              ++ fac = 0 will be MUCH slower since it solves about 20\n"
      "                 problems for each voxel and then chooses what it likes.\n"
      "                 setenv AFNI_TFITTER_VERBOSE YES to get some progress\n"
      "                 reports, if you want to see what it is doing.\n"
      "              ++ SOME penalty has to be applied, since otherwise the\n"
      "                 set of linear equations for s(t) is under-determined\n"
      "                 and/or ill-conditioned!\n"
      "\n"
      "            ** If '-LHS' is used with '-FALTUNG', those basis vectors can\n"
      "               be thought of as a baseline to be regressed out at the\n"
      "               same time the convolution model is fitted.\n"
      "              ++ When '-LHS' supplies a baseline, it is important\n"
      "                 that penalty type 'pen' include '0', so that the\n"
      "                 collinearity between convolution with a constant s(t)\n"
      "                 and a constant baseline can be resolved!\n"
      "              ++ Instead of using a baseline here, you could project the\n"
      "                 baseline out of a dataset or 1D file using 3dDetrend,\n"
      "                 before using 3dTfitter.\n"
      "\n"
      "           *** At most one '-FALTUNG' option can be used!!!\n"
      "\n"
      "           *** Consider the time series model\n"
      "                 Z(t) = K(t)*S(t) + baseline + noise,\n"
      "               where Z(t) = data time series (in each voxel)\n"
      "                     K(t) = kernel (e.g., hemodynamic response function)\n"
      "                     S(t) = stimulus time series\n"
      "                 baseline = constant, drift, etc.\n"
      "                    and * = convolution in time\n"
      "               Then 3dDeconvolve solves for K(t) given S(t), and 3dTfitter\n"
      "               solves for S(t) given K(t).  The difference between the two\n"
      "               cases is that K(t) is presumed to be causal and have limited\n"
      "               support, whereas S(t) is a full-length time series.\n"
      "\n"
      "        ****** Deconvolution is a tricky business, so be careful out there!\n"
      "              ++ e.g., Experiment with the different parameters to make\n"
      "                 sure the results in your type of problems make sense.\n"
      "              ++ There is no guarantee that the automatic selection of\n"
      "                 of the penalty factor will give usable results for\n"
      "                 your problem!\n"
      "              ++ You should probably use a mask dataset with -FALTUNG,\n"
      "                 since deconvolution can often fail on pure noise\n"
      "                 time series.\n"
      "              ++ Unconstrained (no '-cons' options) least squares ('-lsqfit')\n"
      "                 is normally the fastest solution method for deconvolution.\n"
      "                 This, however, may only matter if you have a very long input\n"
      "                 time series dataset (e.g., more than 1000 time points).\n"
      "              ++ For unconstrained least squares deconvolution, a special\n"
      "                 sparse matrix algorithm is used for speed.  If you wish to\n"
      "                 disable this for some reason, set environment variable\n"
      "                 AFNI_FITTER_RCMAT to NO before running the program.\n"
      "              ++ Nevertheless, a problem with more than 1000 time points\n"
      "                 will probably take a LONG time to run, especially if\n"
      "                 'fac' is chosen to be 0.\n"
      "............................................................................\n"
      "\n"
      "  -lsqfit   = Solve equations via least squares [the default method].\n"
      "             * This is sometimes called L2 regression by mathematicians.\n"
      "             * '-l2fit' and '-L2' are synonyms for this option.\n"
      "\n"
      "  -l1fit    = Solve equations via least sum of absolute residuals.\n"
      "             * This is sometimes called L1 regression by mathematicians.\n"
      "             * '-L1' is a synonym for this option.\n"
      "             * L1 fitting is usually slower than L2 fitting, but\n"
      "               is perhaps less sensitive to outliers in the data.\n"
      "              ++ L1 deconvolution might give nicer looking results\n"
      "                 when you expect the deconvolved signal s(t) to\n"
      "                 have large-ish sections where s(t) = 0.\n"
      "             * L2 fitting is statistically more efficient when the\n"
      "               noise is KNOWN to be normally (Gaussian) distributed\n"
      "               (and a bunch of other assumptions are also made).\n"
      "\n"
      "  -consign  = Follow this option with a list of LHS parameter indexes\n"
      "              to indicate that the sign of some output LHS parameters\n"
      "              should be constrained in the solution; for example:\n"
      "                 -consign +1 -3\n"
      "              which indicates that LHS parameter #1 (from the first -LHS)\n"
      "              must be non-negative, and that parameter #3 must be\n"
      "              non-positive.  Parameter #2 is unconstrained (e.g., the\n"
      "              output can be positive or negative).\n"
      "             * Parameter counting starts with 1, and corresponds to\n"
      "               the order in which the LHS columns are specified.\n"
      "             * Unlike '-LHS or '-label', only one '-consign' option\n"
      "               can be used.\n"
      "             * Do NOT give the same index more than once after\n"
      "               '-consign' -- you can't specify that an coefficient\n"
      "               is both non-negative and non-positive, for example!\n"
      "           *** Constraints can be used with '-l1fit' AND with '-l2fit'.\n"
      "           *** '-consign' constraints only apply to the '-LHS'\n"
      "               fit parameters.  To constrain the '-FALTUNG' output,\n"
      "               use the option below.\n"
      "             * If '-consign' is not used, the signs of the fitted\n"
      "               LHS parameters are not constrained.\n"
      "\n"
      "  -consFAL c= Constrain the deconvolution time series from '-FALTUNG'\n"
      "              to be positive if 'c' is '+' or to be negative if\n"
      "              'c' is '-'.\n"
      "             * There is no way at present to constrain the deconvolved\n"
      "               time series s(t) to be positive in some regions and\n"
      "               negative in others.\n"
      "             * If '-consFAL' is not used, the sign of the deconvolved\n"
      "               time series is not constrained.\n"
      "\n"
      "  -prefix p = Prefix for the output dataset (LHS parameters) filename.\n"
      "             * Output datasets from 3dTfitter are always in float format.\n"
      "             * If you don't give this option, 'Tfitter' is the prefix.\n"
      "             * If you don't want this dataset, use 'NULL' as the prefix.\n"
      "             * If you are doing deconvolution and do not also give any\n"
      "               '-LHS' option, then this file will not be output, since\n"
      "               it comprises the fit parameters for the '-LHS' vectors.\n"
      "           *** If the input '-RHS' file is a .1D file, normally the\n"
      "               output files are written in the AFNI .3D ASCII format,\n"
      "               where each row contains the time series data for one\n"
      "               voxel.  If you want to have these files written in the\n"
      "               .1D format, with time represented down the column\n"
      "               direction, be sure to put '.1D' on the end of the prefix,\n"
      "               as in '-prefix Elvis.1D'.  If you use '-' or 'stdout' as\n"
      "               the prefix, the resulting 1D file will be written to the\n"
      "               terminal.\n"
      "\n"
      "  -fitts ff = Prefix filename for the output fitted time series dataset.\n"
      "             * Which is always in float format.\n"
      "             * Which will not be written if this option isn't given!\n"
      "           *** If you want the residuals, subtract this time series\n"
      "               from the '-RHS' input using 3dcalc (or 1deval).\n"
      "\n"
      "  -errsum e = Prefix filename for the error sums dataset, which\n"
      "              is calculated from the difference between the input\n"
      "              time series and the fitted time series (in each voxel):\n"
      "             * Sub-brick #0 is the sum of squares of differences (L2 sum)\n"
      "             * Sub-brick #1 is the sum of absolute differences (L1 sum)\n"
      "             * The L2 sum value, in particular, can be used to produce\n"
      "               a statistic to measure the significance of a fit model;\n"
      "               cf. the 'Correlation Coefficient Example' far below.\n"
      "\n"
      "  -mask ms  = Read in dataset 'ms' as a mask; only voxels with nonzero\n"
      "              values in the mask will be processed.  Voxels falling\n"
      "              outside the mask will be set to all zeros in the output.\n"
      "             * Voxels whose time series are all zeros will not be\n"
      "               processed, even if they are inside the mask!\n"
      "\n"
      "  -quiet    = Don't print the fun fun fun progress report messages.\n"
      "             * Why would you want to hide these delightful missives?\n"
      "\n"
      "----------------------\n"
      "ENVIRONMENT VARIABLES:\n"
      "----------------------\n"
      " AFNI_TFITTER_VERBOSE  =  YES means to print out information during\n"
      "                          the fitting calculations.\n"
      "                         ++ Automatically turned on for 1 voxel -RHS inputs.\n"
      " AFNI_TFITTER_P1SCALE  =  number > 0 will scale the P1 penalty by\n"
      "                          this value (e.g., to count it more)\n"
      " AFNI_TFITTER_P2SCALE  =  number > 0 will scale the P2 penalty by\n"
      "                          this value\n"
      " You could set these values on the command line using the AFNI standard\n"
      " '-Dvariablename=value' command line option.\n"
      "\n"
      "------------\n"
      "NON-Options:\n"
      "------------\n"
      "* There is no option to produce statistical estimates of the\n"
      "  significance of the parameter estimates.\n"
      "  ++ 3dTcorrelate might be useful, to compute the correlation\n"
      "     between the '-fitts' time series and the '-RHS' input data.\n"
      "  ++ You can use the '-errsum' option to get around this limitation.\n"
      "* There are no options for censoring or baseline generation.\n"
      "  ++ You could generate some baseline 1D files using 1deval, perhaps.\n"
      "* There is no option to constrain the range of the output parameters,\n"
      "  except the semi-infinite ranges provided by '-consign' and/or '-consFAL'.\n"
      "* The '-jobs N' option, to use multiple CPUs as in 3dDeconvolve,\n"
      "  is not available at the present chronosynclastic infundibulum.\n"
      "\n"
      "------------------\n"
      "Contrived Example:\n"
      "------------------\n"
      "The dataset 'atm' and 'btm' are assumed to have 99 time points each.\n"
      "We use 3dcalc to create a synthetic combination of these plus a constant\n"
      "plus Gaussian noise, then use 3dTfitter to fit the weights of these\n"
      "3 functions to each voxel, using 4 different methods.  Note the use of\n"
      "the input 1D time series '1D: 99@1' to provide the constant term.\n"
      "\n"
      " 3dcalc -a atm+orig -b btm+orig -expr '-2*a+b+gran(100,20)' -prefix 21 -float\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2u -l2fit\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1u -l1fit\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1c -l1fit \\\n"
      "           -consign -1 +3\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2c -l2fit \\\n"
      "           -consign -1 +3\n"
      "\n"
      "In the absence of noise and error, the output datasets should be\n"
      "  #0 sub-brick = -2.0 in all voxels\n"
      "  #1 sub-brick = +1.0 in all voxels\n"
      "  #2 sub-brick = +100.0 in all voxels\n"
      "\n"
      "---------------------\n"
      "Yet More Contrivance:\n"
      "---------------------\n"
      "You can input a 1D file for the RHS dataset, as in the example below,\n"
      "to fit a single time series to a weighted sum of other time series:\n"
      "\n"
      " 1deval -num 30 -expr 'cos(t)' > Fcos.1D\n"
      " 1deval -num 30 -expr 'sin(t)' > Fsin.1D\n"
      " 1deval -num 30 -expr 'cos(t)*exp(-t/20)' > Fexp.1D\n"
      " 3dTfitter -quiet -RHS Fexp.1D -LHS Fcos.1D Fsin.1D -prefix -\n"
      "\n"
      "* Note the use of the '-' as a prefix to write the results\n"
      "  (just 2 numbers) to stdout, and the use of '-quiet' to hide\n"
      "  the divertingly funny and informative progress messages.\n"
      "* For the Jedi AFNI Masters out there, the above example can be carried\n"
      "  out on using single complicated command line:\n"
      "\n"
      " 3dTfitter -quiet -RHS `1deval -1D: -num 30 -expr 'cos(t)*exp(-t/20)'` \\\n"
      "                  -LHS `1deval -1D: -num 30 -expr 'cos(t)'`            \\\n"
      "                       `1deval -1D: -num 30 -expr 'sin(t)'`            \\\n"
      "                  -prefix - \n"
      "\n"
      "  resulting in the single output line below:\n"
      "\n"
      " 0.535479 0.000236338\n"
      "\n"
      "  which are respectively the fit coefficients of 'cos(t)' and 'sin(t)'.\n"
      "\n"
      "--------------------------------\n"
      "Contrived Deconvolution Example:\n"
      "--------------------------------\n"
      "(1) Create a 101 point 1D file that is a block of 'activation'\n"
      "    between points 40..50, convolved with a triangle wave kernel\n"
      "    (the '-iresp' input below):\n"
      "       3dConvolve -input1D -polort -1 -num_stimts 1     \\\n"
      "                  -stim_file 1 '1D: 40@0 10@1 950@0'    \\\n"
      "                  -stim_minlag 1 0 -stim_maxlag 1 5     \\\n"
      "                  -iresp 1 '1D: 0 1 2 3 2 1' -nlast 100 \\\n"
      "            | grep -v Result | grep -v '^$' > F101.1D\n"
      "\n"
      "(2) Create a 3D+time dataset with this time series in each\n"
      "    voxel, plus noise that increases with voxel 'i' index:\n"
      "       3dUndump -prefix Fjunk -dimen 100 100 1\n"
      "       3dcalc -a Fjunk+orig -b F101.1D     \\\n"
      "              -expr 'b+gran(0,0.04*(i+1))' \\\n"
      "              -float -prefix F101d\n"
      "       /bin/rm -f Fjunk+orig.*\n"
      "\n"
      "(3) Deconvolve, then look what you get by running AFNI:\n"
      "       3dTfitter -RHS F101d+orig -l1fit \\\n"
      "                 -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal1 012 0.0\n"
      "       3dTfitter -RHS F101d+orig -l2fit \\\n"
      "                 -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal2 012 0.0\n"
      "\n"
      "(4) View F101d_fal1+orig, F101d_fal2+orig, and F101d+orig in AFNI,\n"
      "    (in Axial image and graph viewers) and see how the fit quality\n"
      "    varies with the noise level and the regression type -- L1 or\n"
      "    L2 regression.  Note that the default 'fac' level of 0.0 was\n"
      "    selected in the commands above, which means the program selects\n"
      "    the penalty factor for each voxel, based on the size of the\n"
      "    data time series fluctuations.\n"
      "\n"
      "(5) Add logistic noise to the noise-free 1D time series, then deconvolve\n"
      "    and plot the results directly to the screen, with L1 and L2 fitting:\n"
      "      1deval -a F101.1D -expr 'a+lran(.5)' > F101n.1D\n"
      "      3dTfitter -RHS F101n.1D -l1fit \\\n"
      "                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 0.0 | 1dplot -stdin &\n"
      "      3dTfitter -RHS F101n.1D -l2fit \\\n"
      "                -FALTUNG '1D: 0 1 2 3 2 1' stdout 01 0.0 | 1dplot -stdin &\n"
      "  **N.B.: you can only use 'stdout' as an output filename\n"
      "          when the output will be written as a 1D file!\n"
      "\n"
      "--------------------------------\n"
      "Correlation Coefficient Example:\n"
      "--------------------------------\n"
      "Suppose your initials are HJJ and you want to compute the partial\n"
      "correlation coefficient of time series Seed.1D with every voxel in\n"
      "a dataset Rest+orig once a spatially dependent 'artifact' time series\n"
      "Art+orig has been projected out.  You can do this with TWO 3dTfitter\n"
      "runs, plus 3dcalc:\n"
      "\n"
      "(1) Run 3dTfitter with ONLY the artifact time series and get the\n"
      "    error sum dataset\n"
      "       3dTfitter -RHS Rest+orig -LHS Art+orig -polort 2 -errsum Ebase\n"
      "\n"
      "(2) Run 3dTfitter again with the artifact PLUS the seed time series\n"
      "    and get the error sum dataset and also the beta coefficents\n"
      "       3dTfitter -RHS Rest+orig -LHS Seed.1D Art+orig -polort 2 \\\n"
      "                 -errsum Eseed -prefix Bseed\n"
      "\n"
      "(3) Compute the correlation coefficient from the amount of variance\n"
      "    reduction between cases 1 and 2, times the sign of the beta\n"
      "       3dcalc -a Eseed+orig'[0]' -b Ebase+orig'[0]' -c Bseed+orig'[0]' \\\n"
      "              -prefix CorrSeed -expr '(2*step(c)-1)*sqrt(1-a/b)'\n"
      "       3drefit -fbuc -sublabel 0 'SeedCorrelation' CorrSeed+orig\n"
      "\n"
      "More cleverness could be used to compute t- or F-statistics in a\n"
      "similar fashion, using the error sum of squares between 2 different fits.\n"
      "(Of course, these are assuming you use the default '-lsqfit' method.)\n"
      "\n"
      "************************************************************************\n"
      "** RWCox - Feb 2008.                                                  **\n"
      "** Created for the imperial purposes of John A Butman, MD PhD.        **\n"
      "** But may be useful for some other well-meaning souls out there.     **\n"
      "************************************************************************\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTfitter"); machdep();
   PRINT_VERSION("3dTfitter"); AUTHOR("RWCox") ;
   AFNI_logger("3dTfitter",argc,argv);

   /*------- read command line args -------*/

   iarg = 1 ; INIT_XTARR(dsar) ;
   while( iarg < argc ){

     if( strcasecmp(argv[iarg],"-polort") == 0 ){  /* 20 Mar 2008 */
       if( polort >= 0 )
         WARNING_message("you have more than 1 -polort option!") ;
       if( iarg+1 >= argc )
         ERROR_exit("Need an argument after '%s'",argv[iarg]);
       polort = (int)strtod(argv[++iarg],NULL) ;
       if( polort < 0 )
         WARNING_message("-polort value is negative ==> ignoring") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-faltung") == 0 ){
       int p0,p1,p2 ;
       if( fal_set != NULL || fal_im != NULL )
         ERROR_exit("Can't have two -FALTUNG arguments") ;
       if( iarg+4 >= argc )
         ERROR_exit("Need 4 arguments after '%s'",argv[iarg]);
       iarg++ ;
       if( strstr(argv[iarg],"1D") != NULL ){
         fal_im = mri_read_1D(argv[iarg]) ;
         if( fal_im == NULL )
           ERROR_exit("Can't read -FALTUNG time series from '%s'",argv[iarg]) ;
         fal_klen = fal_im->nx ;
         if( fal_klen < 2 )
           ERROR_exit("-FALTUNG 1D file '%s' has only 1 time point!",argv[iarg]);
         if( fal_im->ny > 1 )
           WARNING_message("Only using first column of -FALTUNG 1D file") ;
         fal_kern = MRI_FLOAT_PTR(fal_im) ;
         for( jj=0 ; jj < fal_klen && fal_kern[jj] == 0.0f ; jj++ ) ; /*nada*/
         for( jj=fal_klen-1 ; jj >= 0 && fal_kern[jj] == 0.0f ; jj-- ) ; /*nada*/
         if( jj < 0 )
           ERROR_exit("-FALTUNG 1D file '%s' is all zeros!",argv[iarg]) ;
#if 0
         if( jj < fal_klen-1 ){  /* truncate final zeros */
           WARNING_message("-FALTUNG 1D file '%s' has %d zeros at its end",
                           argv[iag],fal_klen-1-jj) ;
           fal_klen = jj+1 ;
         }
#endif
       } else {
         fal_set = THD_open_dataset(argv[iarg]) ;
         CHECK_OPEN_ERROR(fal_set,argv[iarg]) ;
         fal_klen = DSET_NVALS(fal_set) ;
         if( fal_klen < 2 )
           ERROR_exit("-FALTUNG dataset '%s' has only 1 sub-brick!",argv[iarg]);
       }
       fal_pre = strdup(argv[++iarg]) ;
       if( !THD_filename_ok(fal_pre) )
         ERROR_exit("Illegal filename prefix '%s'",argv[iarg]) ;
       iarg++ ;
       p0 = (strchr(argv[iarg],'0') != NULL) ;
       p1 = (strchr(argv[iarg],'1') != NULL) ;
       p2 = (strchr(argv[iarg],'2') != NULL) ;
       if( p0==0 && p1==0 && p2==0 ){
         WARNING_message(
           "-FALTUNG 'pen' value '%s' illegal: defaulting to '01'",argv[iarg]);
         p0 = p1 = 1 ;
       }
       fal_pencod = p0 + 2*p1 + 4*p2 ; /* encode in bits */
       fal_penfac = (float)strtod(argv[++iarg],NULL) ;
       if( fal_penfac == 0.0f ) fal_penfac = -666.0f ;  /* autopen */
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-consFAL",7) == 0 ){
            if( argv[iarg][8] == '+' ) fal_dcon =  1 ;
       else if( argv[iarg][8] == '-' ) fal_dcon = -1 ;
       else {
         if( ++iarg >= argc )
           ERROR_exit("Need argument after '%s'",argv[iarg-1]);
         fal_dcon = (argv[iarg][0] == '+') ?  1
                   :(argv[iarg][0] == '-') ? -1 : 0 ;
         if( fal_dcon == 0 )
           WARNING_message("value after '-consFAL' is not '+' or '-' -- ignoring!");
       }
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( mask != NULL )
         ERROR_exit("Can't have two -mask arguments!") ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       mset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mnx = DSET_NX(mset); mny = DSET_NY(mset); mnz = DSET_NZ(mset);
       mask = THD_makemask( mset, 0, 1.0f,0.0f ); DSET_delete(mset);
       if( mask == NULL ) ERROR_exit("Can't make mask") ;
       ii = THD_countmask( mnx*mny*mnz , mask ) ;
       INFO_message("%d voxels in the mask",ii) ;
       if( ii < 1 ) ERROR_exit("mask is empty?!") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-rhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( rhset != NULL )
         ERROR_exit("Can't have two '%s' options",argv[iarg-1]);
       rhsnam = malloc(sizeof(char)*(strlen(argv[iarg])+4)) ;
       strcpy(rhsnam,argv[iarg]) ;
       if( STRING_HAS_SUFFIX_CASE(rhsnam,"1D") ||
           strncmp(rhsnam,"1D:",3) == 0          ) strcat(rhsnam,"'");
       rhset = THD_open_dataset( rhsnam ) ;
       if( rhset == NULL )
         ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       rhs1D = DSET_IS_1D(rhset) ;  /* 05 Mar 2008 */
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-lhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( argv[iarg][0] == '-' )
         ERROR_exit("Illegal argument after '%s'",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         if( strstr(argv[iarg],"1D") != NULL ){
           lim = mri_read_1D(argv[iarg]) ;
           if( lim == NULL )
             ERROR_exit("Can't read 1D file '%s'",argv[iarg]) ;
           if( lim->ny > 1 )
             INFO_message("1D file '%s' has %d columns",argv[iarg],lim->ny);
           ADDTO_XTARR(dsar,lim) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_FLIM ;
           nvar += lim->ny ;
         } else {
           lset = THD_open_dataset(argv[iarg]) ;
           if( lset == NULL )
             ERROR_exit("Can't read dataset '%s'",argv[iarg]) ;
           ADDTO_XTARR(dsar,lset) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_DSET ;
           nvar++ ; nlset++ ;
         }
       }
       continue ;
     }

     if( strncasecmp(argv[iarg],"-label",4) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( argv[iarg][0] == '-' )
         ERROR_exit("Illegal argument after '%s'",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         lab = (char **)realloc((void *)lab,sizeof(char *)*(nlab+1)) ;
         lab[nlab++] = strdup(argv[iarg]) ;
       }
       continue ;
     }

     if( strncasecmp(argv[iarg],"-lsqfit",4) == 0 ||
         strncasecmp(argv[iarg],"-l2fit",4)  == 0 ||
         strcmp     (argv[iarg],"-L2")       == 0   ){
       meth = 2 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-l1fit") == 0 || strcmp(argv[iarg],"-L1") == 0 ){
       meth = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal string after -prefix: '%s'",prefix) ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-fitts",5) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       fitts_prefix = argv[iarg] ;
       if( !THD_filename_ok(fitts_prefix) )
         ERROR_exit("Illegal string after -fitts: '%s'",fitts_prefix) ;
       do_fitts = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-errsum",5) == 0 ){ /* 23 Jul 2009 */
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       ersum_prefix = argv[iarg] ;
       if( !THD_filename_ok(ersum_prefix) )
         ERROR_exit("Illegal string after -ersum: '%s'",ersum_prefix) ;
       do_fitts = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-consign",7) == 0 ){
       char *cpt , nvec ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( convec != NULL )
         ERROR_exit("Can't have 2 -consign options!") ;
       MAKE_intvec(convec,1) ;
       for( nvec=0 ; iarg < argc ; iarg++ ){
         ii = (int)strtod(argv[iarg],&cpt) ;
         if( ii == 0 || *cpt != '\0' ) break ;  /* bad */
         for( jj=0 ; jj < nvec ; jj++ ){
           if( abs(convec->ar[jj]) == abs(ii) )
             ERROR_exit("Duplicate indexes in -consign!") ;
         }
         RESIZE_intvec(convec,nvec+1) ;
         convec->ar[nvec++] = ii ;
       }
       if( nvec < 1 )
         ERROR_exit("No legal values after -consign?!") ;
       continue ;
     }

     if( strncasecmp(argv[iarg],"-quiet",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown argument on command line: '%s'",argv[iarg]) ;
   }

   /*------- check options for completeness and consistency -----*/

   if( rhset == NULL )
     ERROR_exit("No RHS dataset input!?") ;
   ntime = DSET_NVALS(rhset) ;
   if( ntime < 2 )
     ERROR_exit("RHS dataset '%s' has only 1 value per voxel?!",rhsnam) ;

   nx = DSET_NX(rhset); ny = DSET_NY(rhset); nz = DSET_NZ(rhset);
   nvox = nx*ny*nz;

   if( my_getenv("AFNI_TFITTER_VERBOSE") == NULL )  /* 31 Dec 2008 */
     AFNI_setenv("AFNI_TFITTER_VERBOSE=YES") ;

   if( mask != NULL && (mnx != nx || mny != ny || mnz != nz) )
     ERROR_exit("mask and RHS datasets don't match in grid size") ;

   if( nvar >= ntime )
     ERROR_exit("too many (%d) LHS time series for %d time points",nvar,ntime) ;

   npol = (polort < 0) ? 0 : polort+1 ;  /* number of polynomial columns */
   if( npol > 0 ){
     ADDTO_XTARR(dsar,NULL);
     ii = XTARR_NUM(dsar)-1; XTARR_IC(dsar,ii) = IC_POLORT; nvar += npol;
   }
   nlhs = dsar->num ;                    /* number of -LHS inputs */

   if( fal_klen > 0 ){
     if( fal_set != NULL ){
       if( DSET_NX(fal_set) != nx ||
           DSET_NY(fal_set) != ny || DSET_NZ(fal_set) != nz )
        ERROR_exit("-FALTUNG dataset and RHS dataset don't match in grid size");
     }
     if( fal_klen >= ntime )
       ERROR_exit("-FALTUNG kernel size %d must be shorter than dataset %d",
                       fal_klen , ntime ) ;
     if( fal_klen >= ntime/2 )
       WARNING_message("-FALTUNG kernel size %d longer than 1/2 dataset %d",
                       fal_klen , ntime/2 ) ;
     if( fal_set != NULL )
       fal_kern = (float *)malloc(sizeof(float)*fal_klen) ;
     nvoff = ntime ;
   } else if( nlhs == 0 ){
     ERROR_exit("no -LHS or -polort option given?!") ;
   }

   dvec = (float * )malloc(sizeof(float)*ntime) ;  /* RHS vector */
   evec = (float * )malloc(sizeof(float)*ntime) ;  /* RHS vector */
   if( nvar > 0 )
     rvec = (float **)malloc(sizeof(float *)*nvar ) ;  /* LHS vectors */

   /*--- check LHS inputs and assign ref vectors ---*/

   if( nlhs > 0 ){
     MAKE_intvec(kvec,nlhs) ;
     for( kk=ii=0 ; ii < nlhs ; ii++ ){
       if( XTARR_IC(dsar,ii) == IC_FLIM ){  /* 1D image: ref points to data */
         float *lar ; int mm ;
         lim = (MRI_IMAGE *)XTARR_XT(dsar,ii) ;
         jj = lim->nx ; lar = MRI_FLOAT_PTR(lim) ;
         if( jj < ntime )
           ERROR_exit("LHS 1D file is too short along time axis: %d < %d",jj,ntime) ;
         if( jj > ntime )
           WARNING_message(
             "LHS 1D file too long along time axis: %d > %d: ignoring extra",jj,ntime);
         for( mm=0 ; mm < lim->ny ; mm++ ) rvec[kk++] = lar + mm*lim->nx ;
         kvec->ar[ii] = -1 ;
       } else if( XTARR_IC(dsar,ii) == IC_POLORT ){ /* polort columns */
         int mm ;
         for( mm=0 ; mm < npol ; mm++,kk++ ){
           rvec[kk] = (float *)malloc(sizeof(float)*ntime) ;
           for( jj=0 ; jj < ntime ; jj++ )
             rvec[kk][jj] = lhs_legendre( (float)jj, 0.0f, ntime-1.0f, mm ) ;
         }
         kvec->ar[ii] = -1 ;
       } else if( XTARR_IC(dsar,ii) == IC_DSET ){ /* dset: create ref vector */
         lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
         if( DSET_NX(lset) != nx || DSET_NY(lset) != ny || DSET_NZ(lset) != nz )
           ERROR_exit("LHS dataset '%s' doesn't match RHS dataset grid size",
                      DSET_HEADNAME(lset) ) ;
         jj = DSET_NVALS(lset) ;
         if( jj < ntime )
           ERROR_exit("LHS dataset is too short along time axis: %d < %d",jj,ntime) ;
         if( jj > ntime )
           WARNING_message(
             "LHS dataset too long along time axis: %d > %d: ignoring extra",jj,ntime);
         kvec->ar[ii] = kk ;  /* index of vector from this dataset */
         rvec[kk++] = (float *)malloc(sizeof(float)*(jj+1)) ;
       } else {
         ERROR_exit("This message should never be seen by mortal eyes!") ;
       }
     }
   }

#if 0
   if( verb && nlset == 0 && meth == 2 && convec == NULL )
     INFO_message("LHS datasets all 1D files ==> you could use 3dDeconvolve");
#endif

   if( nlab < nvar ){
     char lll[32] ;
     if( verb && strcmp(prefix,"NULL") != 0 )
       INFO_message("Making up %d LHS labels (out of %d parameters)",nvar-nlab,nvar);
     lab = (char **)realloc((void *)lab,sizeof(char *)*nvar) ;
     for( ii=nlab ; ii < nvar ; ii++ ){
       sprintf(lll,"Param#%d",ii+1) ; lab[ii] = strdup(lll) ;
     }
   } else if( nlab > nvar && strcmp(prefix,"NULL") != 0 ){
     WARNING_message("Too many (%d) -label strings for %d parameters",nlab,nvar) ;
   }

   /*--- create constraint vector ---*/

   if( convec != NULL ){
     if( nvar > 0 ){
       cvec = (float *)calloc(sizeof(float),nvar) ;
       for( ii=0 ; ii < convec->nar ; ii++ ){
         kk = convec->ar[ii] ; jj = abs(kk) ;
         if( jj > nvar ) ERROR_exit("Index %d in -consign is too large",jj) ;
         cvec[jj-1] = (kk < 0) ? -1.0f : 1.0f ;
       }
     } else {
       WARNING_message("-consign option ignored: no -LHS given!") ;
     }
   }

   /*----- load input datasets -----*/

   if( verb && !rhs1D ) INFO_message("loading input datasets into memory") ;

   DSET_load(rhset) ; CHECK_LOAD_ERROR(rhset) ;
   for( ii=0 ; ii < nlhs ; ii++ ){
     if( XTARR_IC(dsar,ii) == IC_DSET ){
       lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
       DSET_load(lset) ; CHECK_LOAD_ERROR(lset) ;
     }
   }

   /*------ create output datasets ------*/

   if( nvar > 0 && strcmp(prefix,"NULL") != 0 ){  /** coefficients **/
     fset = EDIT_empty_copy(rhset) ;
     EDIT_dset_items( fset ,
                        ADN_nvals     , nvar           ,
                        ADN_ntt       , 0              ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_datum_all , MRI_float      ,
                        ADN_brick_fac , NULL           ,
                        ADN_prefix    , prefix         ,
                      ADN_none ) ;
     tross_Copy_History( rhset , fset ) ;
     tross_Make_History( "3dTfitter" , argc,argv , fset ) ;

     for( jj=0 ; jj < nvar ; jj++ ){ /* create empty bricks to be filled below */
       EDIT_substitute_brick( fset , jj , MRI_float , NULL ) ;
       EDIT_BRICK_LABEL( fset , jj , lab[jj] ) ;
     }
   }

   if( fitts_prefix != NULL && strcmp(fitts_prefix,"NULL") != 0 ){ /** fitts */
     fitts_set = EDIT_empty_copy(rhset) ;
     EDIT_dset_items( fitts_set ,
                        ADN_datum_all , MRI_float      ,
                        ADN_brick_fac , NULL           ,
                        ADN_prefix    , fitts_prefix   ,
                      ADN_none ) ;
     tross_Copy_History( rhset , fitts_set ) ;
     tross_Make_History( "3dTfitter" , argc,argv , fitts_set ) ;

     for( jj=0 ; jj < ntime ; jj++ ) /* create empty bricks to be filled below */
       EDIT_substitute_brick( fitts_set , jj , MRI_float , NULL ) ;
   }

   if( ersum_prefix != NULL && strcmp(ersum_prefix,"NULL") != 0 ){ /** ersum */
     ersum_set = EDIT_empty_copy(rhset) ;                     /* 23 Jul 2009 */
     EDIT_dset_items( ersum_set ,
                        ADN_datum_all , MRI_float      ,
                        ADN_brick_fac , NULL           ,
                        ADN_nvals     , 2              ,
                        ADN_ntt       , 2              ,
                        ADN_prefix    , ersum_prefix   ,
                      ADN_none ) ;
     tross_Copy_History( rhset , ersum_set ) ;
     tross_Make_History( "3dTfitter" , argc,argv , ersum_set ) ;

     EDIT_substitute_brick( ersum_set , 0 , MRI_float , NULL ) ;
     EDIT_substitute_brick( ersum_set , 1 , MRI_float , NULL ) ;
     EDIT_BRICK_LABEL     ( ersum_set , 0 , "errsum_L2"      ) ;
     EDIT_BRICK_LABEL     ( ersum_set , 1 , "errsum_L1"      ) ;
   }

   if( fal_klen > 0 && strcmp(fal_pre,"NULL") != 0 ){  /** deconvolution **/
     defal_set = EDIT_empty_copy(rhset) ;
     EDIT_dset_items( defal_set ,
                        ADN_datum_all , MRI_float ,
                        ADN_brick_fac , NULL      ,
                        ADN_prefix    , fal_pre   ,
                      ADN_none ) ;
     tross_Copy_History( rhset , defal_set ) ;
     tross_Make_History( "3dTfitter" , argc,argv , defal_set ) ;

     for( jj=0 ; jj < ntime ; jj++ ) /* create empty bricks to be filled below */
       EDIT_substitute_brick( defal_set , jj , MRI_float , NULL ) ;
   }

   /*------- loop over voxels and process them one at a time ---------*/

   if( verb && nvox > 499 ) vstep = nvox / 50 ;
   if( vstep > 0 ) fprintf(stderr,"++ voxel loop: ") ;

   THD_fitter_do_fitts( do_fitts ) ;  /* 05 Mar 2008 */

   for( ii=0 ; ii < nvox ; ii++ ){

     if( vstep > 0 && ii%vstep==vstep-1 ) vstep_print() ;

     if( mask != NULL && mask[ii] == 0 ) continue ; /* skip */

     THD_extract_array( ii , rhset , 0 , dvec ) ;   /* get RHS data vector */

     for( jj=0 ; jj < ntime && dvec[jj]==0.0f ; jj++ ) ; /*nada*/
     if( jj == ntime ){ nskip++; continue; }   /*** skip all zero vector ***/

     for( jj=0 ; jj < ntime ; jj++ ) evec[jj] = dvec[jj] ;  /* copy vector */

     THD_fitter_voxid(ii) ;             /* 10 Sep 2008: for error messages */

     for( jj=0 ; jj < nlhs ; jj++ ){               /* get LHS data vectors */
       if( XTARR_IC(dsar,jj) == IC_DSET ){         /* out of LHS datasets  */
         lset = (THD_3dim_dataset *)XTARR_XT(dsar,jj) ;
         kk = kvec->ar[jj] ;
         THD_extract_array( ii , lset , 0 , rvec[kk] ) ;
       }
     }

     /***** do the fitting work *****/

     if( fal_klen > 0 ){      /*-- deconvolution --*/

       if( fal_set != NULL )  /* get decon kernel if from a 3D+time dataset */
         THD_extract_array( ii , fal_set , 0 , fal_kern ) ;

       bfit = THD_deconvolve( ntime , dvec ,
                              0 , fal_klen-1 , fal_kern ,
                              nvar , rvec , meth , cvec , fal_dcon ,
                              fal_pencod , fal_penfac               ) ;

     } else {                 /*-- simple fitting --*/

       bfit = THD_fitter( ntime , dvec , nvar , rvec , meth , cvec ) ;

     }

     if( bfit == NULL ){ nbad++; continue; } /*** bad voxel ***/

     /** store the results **/

     if( nvar > 0 && fset != NULL )
       THD_insert_series( ii , fset , nvar , MRI_float , bfit->ar+nvoff , 1 ) ;

     if( fal_klen > 0 && defal_set != NULL )
       THD_insert_series( ii , defal_set , ntime , MRI_float , bfit->ar , 1 ) ;

     if( do_fitts ){
       floatvec *fv = THD_retrieve_fitts() ; float *fvar = fv->ar ;
       if( fitts_set != NULL )
         THD_insert_series( ii , fitts_set , ntime , MRI_float , fvar , 1 ) ;
       if( ersum_set != NULL ){                                /* 23 Jul 2009 */
         float qsum=0.0f , asum=0.0f , val , ee[2] ;
         for( jj=0 ; jj < ntime ; jj++ ){
           val = fabsf(fvar[jj]-evec[jj]) ; qsum += val*val ; asum += val ;
         }
         ee[0] = qsum ; ee[1] = asum ;
         THD_insert_series( ii , ersum_set , 2 , MRI_float , ee , 1 ) ;
       }
     }

     KILL_floatvec(bfit) ; ngood++ ;

   } /* end of loop over voxels */

   if( vstep > 0 ) fprintf(stderr," Done!\n") ;
   if( nskip > 0 ) WARNING_message("Skipped %d voxels for being all zero",nskip) ;

   /*----- clean up and go away -----*/

   if( nbad > 0 )
     WARNING_message("Fit worked in %d voxels; failed in %d",ngood,nbad) ;
   else if( verb )
     INFO_message("Fit worked on all %d voxels attempted",ngood) ;

   if( fset != NULL ){
     if( verb ) ININFO_message("Writing parameter dataset: %s",prefix) ;
     DSET_write(fset); DSET_unload(fset);
   }

   if( defal_set != NULL ){
     if( verb ) ININFO_message("Writing FALTUNG dataset: %s",fal_pre) ;
     DSET_write(defal_set); DSET_unload(defal_set);
   }

   if( fitts_set != NULL ){
     if( verb ) ININFO_message("Writing fitts dataset: %s",fitts_prefix) ;
     DSET_write(fitts_set); DSET_unload(fitts_set);
   }

   if( ersum_set != NULL ){
     if( verb ) ININFO_message("Writing errsum dataset: %s",ersum_prefix) ;
     DSET_write(ersum_set); DSET_unload(ersum_set);
   }

   if( verb ) INFO_message("Total CPU time = %.1f s",COX_cpu_time()) ;
   exit(0);
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}

/*---------------------------------------------------------------------------*/

static float lhs_legendre( float x, float bot, float top, float n )
{
   double xx ;
   xx = 2.0*(x-bot)/(top-bot) - 1.0 ;  /* now in range -1..1 */
   return (float)Plegendre(xx,n) ;
}

