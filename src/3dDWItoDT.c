/*********************** 3dDWItoDT.c **********************************************/
/* Author: Daniel Glen, 17 Nov 2004 */
/* compute 6 principle direction tensors from multiple gradient vectors*/
/* and corresponding image data */
/* This version includes a more complex algorithm that takes into account*/
/* noise.*/


/* Gregory Baxter (gbaxter@ucsd.edu), 2 Nov 2012                             */
/* UCSD Center for Scientific Computation in Imaging (csci.ucsd.edu)         */
/*                                                                           */
/* Added modifications to allow for reading a b-matrix file directly rather  */
/* constructing it from gradient directions.                                 */
/*                                                                           */
/* When -bmatrix is specified on the command line, the gradient file is      */
/* read as a bmatrix file instead and expected to have 6 columns (rather     */
/* than 3), with elements ordered Bxx, Byy, Bzz, Bxy, Bxz, Byz.              */
/*                                                                           */
/* For calculations with the -bmatrix option, the diffusion weighted dataset */
/* is assumed to have a single b0 image first, followed by all the diffusion */
/* weighted images. The bmatrix file should have the same number of rows as  */
/* the total number of diffusion weighted images (including the b0 image).   */


// Dec, 2013 (PT):
// + (probable) bug fix and new matrix format allowed for input (no
//   zeros at top).  search for '$$' to see the new lines for allowing
//   bmatrix input with no zeros in top row, and for '@@' for potential
//   bug fix for original `-bmatrix' options.

// Apr, 2016 (PT):
// + allow for non-unit gradient magnitudes, so that gradients can be
//   weighted by b-values- affects Form_R_Matrix() and computebmatrix(). 
//   search for 'apt,2016' to see changes.

// May, 2016 (PT):
// + extra badness condition for when fits go really wrong, for example if
//   data is missing.  By default, will censor when values are >100*(MD_0), 
//   where MD_0 is the default 'CSF value' based on the b_max.

// (end of) May, 2016 (PT):
// + allow user to scaled DT/MD/L? values by 1000 internally. If
//   bweighted matrix/gradient values have been input, this may be
//   nicer than having 0.0007 (mm^2/s), etc., values, changing them to
//   ~0.7 (x10^{-3} mm^2/s), etc., which is more the style propounded in
//   reported literature.

// Sep, 2016 (PT):
// + introduce user opt BMAX_REF into program, so we can identify if 
//   bvalue is really > 0, which can happen

// Oct, 2016 (PT):
// + now calcs/outputs RD as part of '-eigs'

// May, 2017 (PT):
// + can now calc goodness of fit chi-squared values (signal-based
//   ones, at the moment), if user wants; now happens when
//   '-debug_briks' is used.

// Aug, 2017 (PT): 
// + can now have cumulative weights dumped into a file, as well as
//   shown in terminal -> CORRECTLY now...


#include "thd_shear3d.h"
/*#ifndef FLOATIZE*/
# include "matrix.h"
/*#endif*/
#include "afni.h"

#define SIG_EPS 1E-8
#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4
#define HUGENUMBER 1E38
#define MAX_CONVERGE_STEPS 10           /* default maximum steps */
#define MAX_RWCONVERGE_STEPS 5

static char prefix[THD_MAX_PREFIX] = "DT";
static int datum = MRI_float;
static matrix Rtmat;
static double *Rvector;		/* residuals at each gradient */
static double *tempRvector;     /* copy of residuals at each gradient */
static int *B0list = NULL;
static matrix Fmatrix;
static matrix Dmatrix;
static matrix OldD;
static matrix Hplusmatrix, Hminusmatrix;
static vector Dvector;
static matrix tempFmatrix[2];
static matrix tempDmatrix[2];
static matrix tempHplusmatrix[2], tempHminusmatrix[2];
/* static vector tempDvector; */

static byte *maskptr = NULL;
static double eigs[12];
static double deltatau;
static double *wtfactor;	     /* weight factors for time points at
                                   each voxel */
static double *bmatrix;		     /* b matrix = GiGj for gradient
                                   intensities */
static double *cumulativewt;    /* overall wt. factor for each
                                   gradient */
static long rewtvoxels;         /* how many voxels were reweighted */ 
static double sigma;		        /* std.deviation */
static double ED;		           /* error for each iteration - cost
                                   function result */
static int automask = 0;        /* automasking flag - user option */
static int reweight_flag = 0;   /* reweight computation flag - user
                                   option */
static int method = -1;         /* linear or non-linear method - user
                                   option */
static int max_iter = -2;       /* maximum #convergence iteration
                                   steps - user option */
static int max_iter_rw = -2;    /* maximum #convergence iteration
                                   steps - user option */
static int eigs_flag = 0;       /* eigenvalue calculation in output -
                                   user option */
static int cumulative_flag = 0; /* calculate, display cumulative wts
                                   for gradients - user option */ 
static int debug_briks = 0;     /* put Ed, Ed0 and Converge step
                                   sub-briks in output - user
                                   option */
static int verbose = 0;         /* print out info every verbose number
                                   of voxels - user option */
static int afnitalk_flag = 0;   /* show convergence in AFNI graph -
                                   user option */
static int bmatrix_given = 0;   /* user input file is b matrix
                                   (instead of gradient direction
                                   matrix) with 6 columns: Bxx, Byy,
                                   Bzz, Bxy, Bxz, Byz */
static int BMAT_NZ = 0;         /* non-zero bmatrix supplied */
static int use_mean_b0 = 0;     /* compute initial linear estimate
                                   with first b=0 volume rather than
                                   mean of all b=0 volumes */
static int opt_method = 2;      /* use gradient descent instead of
                                   Powell's new optimize method*/
static int voxel_opt_method = 0;/* hybridize optimization between
                                    Powell and gradient descent */
static int Powell_npts = 1;     /* number of points in input dataset
                                   for Powell optimization function */
static float *Powell_ts;        /* pointer to time-wise voxel data for
                                   Powell optimization function */
static double Powell_J;
static double backoff_factor = 0.2; /* minimum allowable factor for
                                       lambda2,3 relative to lambda1
                                       eigenvalues*/
static float csf_val = 1.0;     /* a default value for diffusivity for
                                   CSF at 37C; apr,2016: now set to
                                   '1' */
static float rd_val = 1.0;     /* a default value for diffusivity for
                                   CSF at 37C; apr,2016: now set to
                                   '1' */

static float csf_fa = 0.012345678; /* default FA value where there is
                                      CSF */
static float MAX_BVAL = -1.;     /* use in scaling csf_val for MD and
                                   such, *if* user doesn't enter
                                   one */
int USER_CSF_VAL = 0;           // flag so we know if the user entered
                                // his/her own scaling
static float MIN_BAD_SCALE_MD = 100.; // May,2016: mult this number
                                      // times csf_val to detect some
                                      // baaadness
float SCALE_VAL_OUT = -1.0 ;         // allow users to scaled physical
                                     // values by 1000 easily; will be
                                     // set to 1 if negative after
                                     // reading in inputs
float BMAX_REF = 0.01;    // for identifying reference bvalues

// [PT: May, 2017]
static float *I0_ptr;           /* I0 matrix */
static int nvols = -1;
// used in copying DT info: NOTE THE ORDER!
static int sublist[7]={6,0,1,2,3,4,5}; 

static NI_stream_type * DWIstreamid = 0;     /* NIML stream ID */


int ChiSq_GOF( THD_3dim_dataset *DWmeas, 
               THD_3dim_dataset *DWfit, 
               int *b0list,
               float **ccc,
               byte *maskp);

// *Shamelessly* copied+pasted from 3dDTtoDWI.c for use here, because
// *it was a pain to navigate around static variables and linking.
static void DTtoDWI_tsfunc (double tzero, double tdelta, int npts, float ts[],
                            double ts_mean, double ts_slope, void *ud, 
                            int nbriks, float *val);
static void Form_R_Matrix (MRI_IMAGE * grad1Dptr);
static void DWItoDT_tsfunc ( double tzero, double tdelta, int npts, 
                             float ts[], double ts_mean, double ts_slope, 
                             void *ud, int nbriks, float *val);
static void EIG_func (void);
static float Calc_FA(float *val);
static float Calc_MD(float *val);
static float Calc_RD(float *val); // pt,Oct,2016: for RD
static void ComputeD0 (void);
static double ComputeJ (float ts[], int npts);
static void ComputeDeltaTau (void);
static void Computebmatrix (MRI_IMAGE * grad1Dptr, int NO_ZERO_ROW1); 
static void InitGlobals (int npts);
static void FreeGlobals (void);
static void Store_Computations (int i, int npts, int converge_step);
static void Restore_Computations (int i, int npts, int converge_step);
static void InitWtfactors (int npts);
static void ComputeWtfactors (int npts);
static void ComputeHpHm (double deltatau);
static void ComputeNewD (void);
static int TestConvergence (matrix NewD, matrix OldD);
static void udmatrix_to_vector (matrix m, vector * v);
static void udmatrix_copy (double *udptr, matrix * m);
static double matrix_sumabs (matrix m);
static double *InvertSym3 (double a, double b, double c, double e, double f,
                           double i);
static void matrix_copy (matrix a, matrix * b);
static int DWI_Open_NIML_stream(void);
static int DWI_NIML_create_graph(void);
static void DWI_AFNI_update_graph(double *Edgraph, double *dtau, int npts);
static void vals_to_NIFTI(float *val);
static void Save_Sep_DTdata(THD_3dim_dataset *, char *, int);
static void Copy_dset_array(THD_3dim_dataset *, int,int,char *, int);
static int ComputeDwithPowell(float *ts, float *val, int npts, int nbriks);
static int bad_DWI_data(int npts, float *ts);
static int bad_DWI_data_2_MD(float v1, float v2, float v3); // May,2016: new badness
                                        // check, based on MD val
static double compute_mean_B0(int npts, float *ts, char B0flag);
static void Assign_CSF_values(float *val);
static int all_dwi_zero(int npts, float *ts);

int
main (int argc, char *argv[])
{
   THD_3dim_dataset *old_dset=NULL, *new_dset=NULL;	 // input and output datasets 
   THD_3dim_dataset *fit_dset=NULL; // intermed dset: DWIs on tensor  
   THD_3dim_dataset *new_dsetDT=NULL, *old_dset0=NULL;
   int nopt, nbriks;
   int i, eigs_brik;
   MRI_IMAGE *grad1Dptr = NULL;
   MRI_IMAGE *anat_im = NULL;
#if 0
   int nvox;
   short *sar = NULL;
   short *tempsptr = NULL;
   byte *tempbptr = NULL;
   short tempval;
#endif

   double *cumulativewtptr=NULL;
   int mmvox=0 ;
   int nxyz;
   int sep_dsets = 0;

   // [PT: May, 2017]
   MRI_IMAGE *data_im = NULL;
   double fac;
   int n;

   // [PT: Aug, 2017]
   float tmp_cwvalue=0.0;    // for outputting cumulative_wt values
   int ncwts=0; 
   FILE *fout1=NULL;


   /*----- Read command line -----*/
   if (argc < 2 || strcmp (argv[1], "-help") == 0)
      {
         printf (
"\n"
" Usage: 3dDWItoDT [options] gradient-file dataset\n"
"\n"
" Computes 6 principle direction tensors from multiple gradient vectors\n"
" and corresponding DTI image volumes.\n"
" The program takes two parameters as input :  \n"
"    a 1D file of the gradient vectors with lines of ASCII floats:\n"
"            Gxi, Gyi, Gzi.\n"
"    Only the non-zero gradient vectors are included in this file (no G0 \n"
"    line). \n"
"    ** Now, a '1D' file of b-matrix elements can alternatively be input,\n"
"        and *all* the gradient values are included!**\n"
"    A 3D bucket dataset with Np+1 sub-briks where the first sub-brik is the\n"
"    volume acquired with no diffusion weighting.\n"
"\n"
" OUTPUTS: \n"
"     + you can output all 6 of the independent tensor values (Dxx, Dyy, \n"
"       etc.), as well as all three eigenvalues (L1, L2, L3) and \n"
"       eigenvectors (V1, V2, V3), and useful DTI parameters FA, MD and\n"
"       RD.\n"
"     + 'Debugging bricks' can also be output, see below.\n"
"\n"
" Options:\n"
"   -prefix pname = Use 'pname' for the output dataset prefix name.\n"
"                   [default='DT']\n\n"
"   -automask = mask dataset so that the tensors are computed only for\n"
"               high-intensity (presumably brain) voxels.  The intensity \n"
"               level is determined the same way that 3dClipLevel works.\n\n"
"   -mask dset = use dset as mask to include/exclude voxels\n\n"
" -bmatrix_NZ FF = switch to note that the input dataset is b-matrix, \n"
"               not gradient directions, and there is *no* row of zeros \n"
"               at the top of the file, similar to the format for the grad\n"
"               input: N-1 rows in this file for N vols in matched data set.\n"
"               There must be 6 columns of data, representing either elements\n"
"               of G_{ij} = g_i*g_j (i.e., dyad of gradients, without b-value\n"
"               included) or of the DW scaled version, B_{ij} = b*g_i*g_j.\n"
"               The order of components is: G_xx G_yy G_zz G_xy G_xz G_yz.\n"
" -bmatrix_Z FF = similar to '-bmatrix_NZ' above, but assumes that first\n"
"               row of the file is all zeros (or whatever the b-value for\n"
"               the reference volume was!), i.e. there are N rows to the\n"
"               text file and N volumes in the matched data set.\n\n"
" -bmatrix_FULL FF = exact same as '-bmatrix_Z FF' above (i.e. there are N\n"
"               rows to the text file and N volumes in the matched data set)\n"
"               with just a lot more commonsensical name.  Definitely would\n"
"               be preferred way to go, for ease of usage!\n"
"\n"
"   -scale_out_1000 = increase output parameters that have physical units\n"
"               (DT, MD, RD, L1, L2 and L3) by multiplying them by 1000. This\n"
"               might be convenient, as the input bmatrix/gradient values \n"
"               can have their physical magnitudes of ~1000 s/mm^2, for\n"
"               which typical adult WM has diffusion values of MD~0.0007\n"
"               (in physical units of mm^2/s), and people might not like so\n"
"               many decimal points output; using this option rescales the\n"
"               input b-values and would lead to having a typical MD~0.7\n"
"               (now in units of x10^{-3} mm^2/s).  If you are not using\n"
"               bmatrix/gradient values that have their physical scalings,\n"
"               then using this switch probably wouldn't make much sense.\n"
"               FA, V1, V2 and V3 are unchanged.\n\n" 
" -bmax_ref THR = if the 'reference' bvalue is actually >0, you can flag\n"
"                 that here.  Otherwise, it is assumed to be zero.\n"
"                 At present, this is probably only useful/meaningful if\n"
"                 using the '-bmatrix_Z ...' or '-bmatrix_FULL ...' \n"
"                 option, where the reference bvalue must be found and \n"
"                 identified from the input info alone.\n"
"\n"
"   -nonlinear = compute iterative solution to avoid negative eigenvalues.\n"
"                This is the default method.\n\n"
"   -linear = compute simple linear solution.\n\n"
"   -reweight = recompute weight factors at end of iterations and restart\n\n"
"   -max_iter n = maximum number of iterations for convergence (Default=10).\n"
"                 Values can range from -1 to any positive integer less than\n"
"                 101. A value of -1 is equivalent to the linear solution.\n"
"                 A value of 0 results in only the initial estimate of the\n"
"                 diffusion tensor solution adjusted to avoid negative\n"
"                 eigenvalues.\n\n"
"   -max_iter_rw n = max number of iterations after reweighting (Default=5)\n"
"                    values can range from 1 to any positive integer less\n"
"                    than 101.\n\n"
"   -eigs = compute eigenvalues, eigenvectors, fractional anisotropy and mean\n"
"           diffusivity in sub-briks 6-19. Computed as in 3dDTeig\n\n"
"   -debug_briks = add sub-briks with Ed (error functional), Ed0 (orig.\n"
"                  error), number of steps to convergence and I0 (modeled B0\n"
"                  volume).\n"
"                  [May, 2017] This also now calculates two goodness-of-fit\n"
"                  measures and outputs a new PREFIX_CHI* dset that has two\n"
"                  briks:\n"
"                     brik [0]: chi^2_p,\n"
"                     brik [1]: chi^2_c.\n"
"                  These values are essentially calculated according to\n"
"                  Papadakis et al. (2003, JMRI), Eqs. 4 and 3,\n"
"                  respectively (in chi^2_c, the sigma value is the\n"
"                  variance of measured DWIs *per voxel*). Note for both\n"
"                  chi* values, only DWI signal values are used in the\n"
"                  calculation (i.e., where b>THR; by default,\n"
"                  THR=0.01, which can be changed using '-bmax_ref ...').\n"
"                  In general, chi^2_p values seem to be <<1, consistent\n"
"                  with Papadakis et al.'s Fig. 4; the chi^2_c values are\n"
"                  are also pretty consistent with the same fig and seem to\n"
"                  be best viewed with the upper limit being roughly =Ndwi\n"
"                  or =Ndwi-7 (with the latter being the given degrees\n"
"                  of freedom value by Papadakis et al.)\n"
"   -cumulative_wts = show overall weight factors for each gradient level\n"
"                     May be useful as a quality control\n\n"
"   -verbose nnnnn = print convergence steps every nnnnn voxels that survive\n"
"                    to convergence loops (can be quite lengthy).\n\n"
"   -drive_afni nnnnn = show convergence graphs every nnnnn voxels that\n"
"                       survive to convergence loops. AFNI must have NIML\n"
"                       communications on (afni -niml)\n\n"
"   -sep_dsets = save tensor, eigenvalues, vectors, FA, MD in separate\n"
"                datasets\n\n"
"   -csf_val n.nnn = assign diffusivity value to DWI data where the mean\n"
"                    values for b=0 volumes is less than the mean of the\n"
"                    remaining volumes at each voxel. The default value is\n"
"                    '1.0 divided by the max bvalue in the grads/bmatrices'.\n"
"                    The assumption is that there are flow artifacts in CSF\n"
"                    and blood vessels that give rise to lower b=0 voxels.\n"
"                    NB: MD, RD L1, L2, L3, Dxx, Dyy, etc. values are all\n"
"                    scaled in the same way.\n\n"
"   -min_bad_md N  = change the min MD value used as a 'badness check' for\n"
"                    tensor fits that have veeery (-> unreasonably) large MD\n"
"                    values. Voxels where MD > N*(csf_val) will be treated\n"
"                    like CSF and turned into spheres with radius csf_val \n"
"                    (default N=100).\n"
"   -csf_fa n.nnn  = assign a specific FA value to those voxels described\n"
"                    above The default is 0.012345678 for use in tractography\n"
"                    programs that may make special use of these voxels\n\n"
"   -opt mname =  if mname is 'powell', use Powell's 2004 method for \n"
"                 optimization. If mname is 'gradient' use gradient descent\n"
"                 method. If mname is 'hybrid', use combination of methods.\n"
"                 MJD Powell, \"The NEWUOA software for unconstrained \n"
"                 optimization without derivatives\", Technical report DAMTP\n"
"                 2004/NA08, Cambridge University Numerical Analysis Group --\n"
"                 http://www.damtp.cam.ac.uk/user/na/reports.html\n\n"
"   -mean_b0 = use mean of all b=0 volumes for linear computation and initial\n"
"              linear for nonlinear method\n\n"
" Example:\n"
"  3dDWItoDT -prefix rw01 -automask -reweight -max_iter 10 \\\n"
"            -max_iter_rw 10 tensor25.1D grad02+orig.\n\n"
" The output is a 6 sub-brick bucket dataset containing \n"
"     Dxx, Dxy, Dyy, Dxz, Dyz, Dzz\n"
" (the lower triangular, row-wise elements of the tensor in symmetric matrix\n"
" form). Additional sub-briks may be appended with the -eigs and -debug_briks\n"
" options.  These results are appropriate as the input to 3dDTeig.\n"
"\n");
         printf ("\n" MASTER_SHORTHELP_STRING);
         exit (0);
      }

   mainENTRY ("3dDWItoDT main");
   machdep ();
   AFNI_logger ("3dDWItoDT", argc, argv);
   PRINT_VERSION("3dDWItoDT") ; AUTHOR("Daniel Glen") ;
   THD_check_AFNI_version("3dDWItoDT") ;

   nopt = 1;
   nbriks = 6;		/* output contains 6 sub-briks by default */
   method = -1;
   reweight_flag = 0;

   datum = MRI_float;
   while (nopt < argc && argv[nopt][0] == '-')
      {

         /*-- prefix --*/

         if (strcmp (argv[nopt], "-prefix") == 0)
            {
               if (++nopt >= argc)
                  {
                     ERROR_exit("Error - prefix needs an argument!");
                  }
               /* change name from default prefix */
               MCW_strncpy (prefix, argv[nopt], THD_MAX_PREFIX);	
               /* check file name to be sure not to overwrite - mod
                  drg 12/9/2004 */
               if (!THD_filename_ok (prefix))
                  {
                     ERROR_exit("Error - %s is not a valid prefix!", prefix);
                  }
               nopt++;
               continue;
            }

         /*-- datum --*/

         if (strcmp (argv[nopt], "-datum") == 0)
            {
               if (++nopt >= argc)
                  {
                     ERROR_exit("Error - datum needs an argument!");
                  }
               if (strcmp (argv[nopt], "short") == 0)
                  {
                     datum = MRI_short;
                  }
               else if (strcmp (argv[nopt], "float") == 0)
                  {
                     datum = MRI_float;
                  }
               else if (strcmp (argv[nopt], "byte") == 0)
                  {
                     datum = MRI_byte;
                  }
               else
                  {
                     ERROR_exit("-datum of type '%s' is not supported!",
                                argv[nopt]);
                  }
               nopt++;
               continue;
            }
         if (strcmp (argv[nopt], "-automask") == 0)
            {
               if(maskptr != NULL){
                  ERROR_exit("ERROR: can't use -mask with -automask!");
               }
               automask = 1;
               nopt++;
               continue;
            }

         if( strcmp(argv[nopt],"-mask") == 0 ){
            THD_3dim_dataset * mask_dset ;
            if( automask ){
               ERROR_exit("ERROR: can't use -mask with -automask!");
            }
            mask_dset = THD_open_dataset(argv[++nopt]) ;
            CHECK_OPEN_ERROR(mask_dset,argv[nopt]) ;
            if( maskptr != NULL ){
               ERROR_exit("** ERROR: can't have 2 -mask options!");
            }
            maskptr = THD_makemask( mask_dset , 0 , 1.0,-1.0 ) ;
            mmvox = DSET_NVOX( mask_dset ) ;

            DSET_delete(mask_dset) ; nopt++ ; continue ;
         }
         
         // input bmatrix with initial B=0 line
         if (strcmp(argv[nopt], "-bmatrix_Z") == 0) {  
            bmatrix_given = 1;
            nopt++;
            continue;
         }
         // duplicate behavior of above, but with a LOT easier name
         if (strcmp(argv[nopt], "-bmatrix_FULL") == 0) {  
            bmatrix_given = 1;
            nopt++;
            continue;
         }

         // input bmatrix without first B=0 line
         if (strcmp(argv[nopt], "-bmatrix_NZ") == 0) { 
            BMAT_NZ = 1;
            nopt++;
            continue;
         }

         //  compute mean across b=0 vols for init. linear 
         if (strcmp(argv[nopt], "-mean_b0") == 0) {   
            use_mean_b0 = 1;
            nopt++;
            continue;
         }
         
         //  can mult output diff values by 1000 
         if (strcmp(argv[nopt], "-scale_out_1000") == 0) {   
            SCALE_VAL_OUT = 1000.;
            nopt++;
            continue;
         }
                  
         if (strcmp (argv[nopt], "-bmax_ref") == 0){
            if(++nopt >=argc )
               ERROR_exit("Error: need an argument after -bmax_ref!");
            BMAX_REF = (float) strtod(argv[nopt], NULL); 
            nopt++;
            continue;
         }

         // May,2016: essentially, turn off badness criterion of
         // superlarge MD by setting the value SOOO high
         if (strcmp (argv[nopt], "-min_bad_md") == 0){
            if(++nopt >=argc )
               ERROR_exit("Error - need an argument after -min_bad_md!");
            MIN_BAD_SCALE_MD = (float) strtod(argv[nopt], NULL); 
            if(MIN_BAD_SCALE_MD < 0.00001)
               ERROR_exit("Bad MD scale! Needs to be >0.00001-- "
                          " and probably much more so!");
            nopt++;
            continue;
         }


         if (strcmp (argv[nopt], "-linear") == 0)
            {
               if(method==1)
                  {
                     ERROR_exit("Error - cannot select both linear and "
                                "non-linear methods at the same time");
                  }
               method = 0;
               nopt++;
               continue;
            }

         if ((strcmp (argv[nopt], "-nonlinear") == 0) || 
             (strcmp (argv[nopt], "-non-linear") == 0))
            {
               if(method==0)
                  {
                     ERROR_exit("Error - cannot select both linear "
                                "and non-linear methods at the same time");
                     exit(1);
                  }
               method = 1;
               nopt++;
               continue;
            }
         if (strcmp (argv[nopt], "-reweight") == 0) {
            reweight_flag = 1;
            nopt++;
            continue;
         }

         if (strcmp (argv[nopt], "-max_iter") == 0)
            {
               if(++nopt >=argc ){
                  ERROR_exit("Error - need an argument after -max_iter!");
               }
               max_iter = strtol(argv[nopt], NULL, 10);
               if ((max_iter <-1)||(max_iter>100)) {
                  ERROR_exit("Error - max_iter must be between -1 and 100");
               }
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-max_iter_rw") == 0)
            {
               if(++nopt >=argc ){
                  ERROR_exit("Error - need an argument after -max_iter_rw!");
               }
               max_iter_rw = strtol(argv[nopt], NULL, 10);
               if ((max_iter_rw <=0)||(max_iter_rw>100)) {
                  ERROR_exit("Error - max_iter_rw must be between 1 and 100");
               }
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-eigs") == 0)
            {
               eigs_flag = 1;
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-cumulative_wts") == 0)
            {
               cumulative_flag = 1;
               nopt++;
               continue;
            }

         if ((strcmp (argv[nopt], "-debug_briks") == 0) ||
             (strcmp (argv[nopt], "-debug_bricks") == 0))
            {
               debug_briks = 1;
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-verbose") == 0)
            {
               if(++nopt >=argc ){
                  ERROR_exit("*** Error - need an argument after -verbose!");
               }
               verbose = strtol(argv[nopt], NULL, 10);
               if (verbose<=0) {
                  ERROR_exit("Error- verbose steps must be a positive number!");
               }
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-drive_afni") == 0)
            {
               if(++nopt >=argc ){
                  ERROR_exit("Error - need an argument after -drive_afni!");
               }
               afnitalk_flag = strtol(argv[nopt], NULL, 10);
               if (afnitalk_flag<=0) {
                  ERROR_exit("Error - drive_afni steps must be "
                             "a positive number!");
               }
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-sep_dsets") == 0)
            {
               sep_dsets = 1;  /* save data in separate datasets */
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-opt") == 0)
            {
               if (++nopt >= argc)
                  {
                     ERROR_exit("Error - opt should be followed by "
                                "gradient or powell!");
                  }
               if (strcmp(argv[nopt], "gradient") == 0)
                  {
                     opt_method = 0;
                  }
               else if (strcmp(argv[nopt], "powell") == 0)
                  {
                     opt_method = 1; /* use Powell's new optimize
                                        method instead of gradient
                                        descent*/
                  }
               else if (strcmp(argv[nopt], "hybrid") == 0)
                  {
                     opt_method = 2; /* use combination of Powell and
                                        gradient descent*/
                  }
               else
                  {
                     ERROR_exit("-opt method '%s' is not supported!",
                                argv[nopt]);
                  }
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-backoff") == 0)
            {
               backoff_factor = strtod(argv[++nopt],NULL);
               if(backoff_factor<=0.0 || backoff_factor>1.0)
                  ERROR_exit("-backoff factor must be > 0 and <= 1");
               nopt++;
               continue;
            }

         if (strcmp (argv[nopt], "-csf_val") == 0){
            if(++nopt >=argc )
               ERROR_exit("Error - need an argument after -csf_val!");
            csf_val = (float) strtod(argv[nopt], NULL);
            rd_val = csf_val; // pt,Oct,2016: for RD
            USER_CSF_VAL = 1;
            nopt++;
            continue;
         }

         if (strcmp (argv[nopt], "-csf_fa") == 0){
            if(++nopt >=argc )
               ERROR_exit("Error - need an argument after -csf_fa!");
            csf_fa = (float) strtod(argv[nopt], NULL);
            nopt++;
            continue;
         }

         ERROR_exit("Error - unknown option %s", argv[nopt]);
      }

   if(method==-1)
      method = 1;    /* if not selected, choose non-linear method for now */

   if(max_iter>=-1){
      if(method==0)
         WARNING_message("Warning - max_iter will be ignored "
                         "for linear methods");
   }
   else
      max_iter = MAX_CONVERGE_STEPS;

   if(max_iter_rw>=0) {
      if(method==0)
         WARNING_message("Warning - max_iter_rw will be ignored "
                         "for linear methods");
      if(reweight_flag==0)
         WARNING_message("Warning - max_iter_rw will be ignored "
                         "when not reweighting");
   }
   else
      max_iter_rw = MAX_RWCONVERGE_STEPS;

   if((method==0)&&(reweight_flag==1)) {
      WARNING_message("Warning - can not reweight voxels for linear method");
      reweight_flag = 0;
   }

   if(cumulative_flag==1) {
      if(method==0) {
         WARNING_message("Warning - can not compute cumulative weights "
                         "for linear method");
         cumulative_flag = 0;
      }
      if(reweight_flag == 0) {
         WARNING_message("Warning - can not compute cumulative weights "
                         "if not reweighting");
         cumulative_flag = 0;
      }
   }

   if((method==0)&&(debug_briks==1)) {
      WARNING_message("Warning - can not compute debug sub-briks "
                      "for linear method");
      debug_briks = 0;
   }

   if((method==0)&&(afnitalk_flag>0)) {
      WARNING_message("Warning - can not graph convergence in AFNI "
                      "for linear method");
      afnitalk_flag = 0;
   }

   if(eigs_flag)
      nbriks += 15; // pt,Oct,2016: for RD

   if(debug_briks)
      nbriks += 4;


   /*----- read input datasets -----*/

   if (nopt >= argc)
      {
         ERROR_exit("Error - No input dataset!?");
      }

   /* first input dataset - should be gradient vector file of ascii
      floats Gx,Gy,Gz */

   /* read gradient vector 1D file */
   grad1Dptr = mri_read_1D (argv[nopt]);
   if (grad1Dptr == NULL)
      {
         ERROR_exit("Error reading gradient vector file");
      }

   // if ((grad1Dptr->ny != 3 && !bmatrix_given) || (grad1Dptr->ny !=
   // 6 && bmatrix_given)) // $$old
   if ((grad1Dptr->ny != 3 && !(bmatrix_given || BMAT_NZ) ) 
       || (grad1Dptr->ny != 6 && (bmatrix_given || BMAT_NZ ) )) // BMAT_NZ opts
      {
         ERROR_message("Error - Only 3 columns of gradient vectors "
                       "(or 6 columns for b matrices) "
                       "allowed: %d columns found", grad1Dptr->ny);
         mri_free (grad1Dptr);
         exit (1);
      }

   if (grad1Dptr->nx < 6)
      {
         mri_free (grad1Dptr);
         ERROR_message("Error - Must have at least 6 gradient vectors");
         ERROR_exit("%d columns found", grad1Dptr->nx);
      }
   
   // -----------------------------------------------------------------
   // apply 1/SCALE_VAL_OUT to the bvalue as a way to scale;
   // shamelessly pillaging format of mri_histog.c; later might allow
   // users to choose other values
   if( SCALE_VAL_OUT < 0 )
      SCALE_VAL_OUT = 1.;
   else
      INFO_message("Implementing user-selected scaling of diffusion values by %.0f",
                   SCALE_VAL_OUT);
   
   if( grad1Dptr->kind != MRI_float )
      ERROR_exit("How are the gradients not floats?");

   int npix;
   register float *flar=mri_data_pointer(grad1Dptr);
   npix = grad1Dptr->nvox; 
   for( i=0 ; i<npix ; i++ ){
      flar[i]/= SCALE_VAL_OUT;
   }
   BMAX_REF/= SCALE_VAL_OUT;

   //fprintf(stderr,"\n\n nyx=%d; npix=%d\n",grad1Dptr->nxy, npix);
   //for( i=0 ; i<npix ; i++)
   //   fprintf(stderr,"\n %f", flar[i]);
   // -----------------------------------------------------------------

   Form_R_Matrix (grad1Dptr);	/* use grad1Dptr to compute R matrix */

   nopt++;

   /* Now read in all the MRI volumes for each gradient vector */
   /* assumes first one is no gradient */
   old_dset = THD_open_dataset (argv[nopt]);
   CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;

   /* expect at least 7 values per voxel - 7 sub-briks as input dataset */
   if (!(!bmatrix_given && DSET_NVALS (old_dset) == (grad1Dptr->nx + 1))      
       && !((bmatrix_given && DSET_NVALS (old_dset) == (grad1Dptr->nx)))
       && !((BMAT_NZ && DSET_NVALS (old_dset) == (grad1Dptr->nx))) ) 
      {
         mri_free (grad1Dptr);
         ERROR_message("Error - Dataset must have number of sub-briks "
                       "equal to one more than number");
         ERROR_exit("  of gradient vectors (B0+Bi)!");
      }
   nxyz = DSET_NVOX(old_dset) ;
   if( maskptr != NULL && mmvox != nxyz ){
      ERROR_exit("Mask and input datasets not the same size!") ;
   }

   nvols = DSET_NVALS (old_dset);

   if (bmatrix_given) {
      InitGlobals (grad1Dptr->nx);  /* initialize all the matrs and vecs */
   }
   else { // $$same: can leave same, because BMAT_NZ will have `nx+1'
      InitGlobals (grad1Dptr->nx + 1);	/* initialize all the matrs and vecs */
   }

   Computebmatrix (grad1Dptr, BMAT_NZ);	/* compute bij=GiGj */
   // after this, the MAX_BVAL is known, as well; apply it to the CSF
   // val *if* the user didn't set his/her own scale.  apr,2016

   if(MAX_BVAL<=0)
      ERROR_exit("Whooa! How did the max bval get to look like %f??", MAX_BVAL);

   if (!bmatrix_given) 
      INFO_message("The maximum magnitude of the gradients "
                   "appears to be: %.5f", MAX_BVAL);
   else
      INFO_message("The maximum magnitude of the bmatrix "
                   "appears to be: %.2f", MAX_BVAL);
   if(!USER_CSF_VAL) {
      csf_val/= MAX_BVAL;
      rd_val/= MAX_BVAL;
      INFO_message("-> and, by scale, the 'CSF value' (e.g., MD)"
                   " for any unfit voxels will be: %.8f", csf_val);
   }
   INFO_message("Voxels with MD>%f will be deemed unfit for "
                "fitting and given 'CSF' status",
                MIN_BAD_SCALE_MD*csf_val);

   if (automask)
      {
         DSET_mallocize (old_dset);
         DSET_load (old_dset);	/* get B0 (anatomical image) from
                                    dataset */
         /*anat_im = THD_extract_float_brick( 0, old_dset ); */
         anat_im = DSET_BRICK (old_dset, 0);	/* set pointer to the 0th
                                                sub-brik of the
                                                dataset */
         maskptr = mri_automask_image (anat_im);	/* maskptr is a
                                                      byte pointer for
                                                      volume */
      }

   /* temporarily set artificial timing to 1 second interval */
   EDIT_dset_items (old_dset,
                    ADN_ntt, DSET_NVALS (old_dset),
                    ADN_ttorg, 0.0,
                    ADN_ttdel, 1.0, ADN_tunits, 
                    UNITS_SEC_TYPE, NULL);

   if(afnitalk_flag) {
      if(DWI_Open_NIML_stream()!=0) {   /* Open NIML stream */
         afnitalk_flag = 0;
         WARNING_message("Could not open NIML communications with AFNI");
      }
      else
         if(DWI_NIML_create_graph()!=0) {
            afnitalk_flag = 0;
            WARNING_message("Could not create graph within AFNI");
            /* Close NIML stream */
            NI_stream_close(DWIstreamid);
            DWIstreamid = 0;
         }
   }

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc (old_dset,	/* input dataset */
                                      prefix,	/* output prefix */
                                      datum,	/* output datum  */
                                      0,	/* ignore count  */
                                      0,	/* can't detrend in maker
                                             function KRH 12/02 */
                                      nbriks,	/* number of briks */
                                      DWItoDT_tsfunc,	/* timeseries processor */
                                      NULL,	/* data for tsfunc */
                                      NULL,  /* mask */
                                      0   /* Allow auto scaling of output */
                                      );

   if(afnitalk_flag && (DWIstreamid!=0)) {
      /* Close NIML stream */
      NI_stream_close(DWIstreamid);
   }
   
   if(cumulative_flag && reweight_flag) {
      
      // [PT: Aug, 2017] Functionality to output cumulative weights to
      // text file
      char cwprefix[THD_MAX_PREFIX], cprefix[THD_MAX_PREFIX];
      char *ext, nullch; 
      sprintf(cprefix,"%s", prefix);
      if(has_known_non_afni_extension(prefix)){   /* for NIFTI, 3D, Niml,
                                                     Analyze,...*/
      ext = find_filename_extension(prefix);
      cprefix[strlen(prefix) - strlen(ext)] = '\0';  /* remove
                                                        non-afni-extension
                                                        for now*/
      }
      else {
         nullch = '\0';
         ext = &nullch;
      }
      sprintf(cwprefix,"%s_cwts.1D", cprefix);

      if( (fout1 = fopen(cwprefix, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",cwprefix);
         exit(11);
      }

      cumulativewtptr = cumulativewt;
      INFO_message("Cumulative Wt. factors (and output to %s:", cwprefix);
      // need to get the number based on whether full mat was input
      if(bmatrix_given) 
         ncwts = grad1Dptr->nx;
      else
         ncwts = grad1Dptr->nx + 1;
      for(i=0; i<ncwts; i++){
         *cumulativewtptr = *cumulativewtptr / rewtvoxels;
         tmp_cwvalue = *cumulativewtptr++;
         INFO_message("%5.3f ", tmp_cwvalue);
         fprintf(fout1,"%5.3f\n",tmp_cwvalue);
      }      
      fclose(fout1);
      
      /* printf("\n");*/
   }
   
   
   if (new_dset != NULL)
      {
         // copy the tensor over.  Ugh.
         if( debug_briks) {
            new_dsetDT = THD_copy_dset_subs(new_dset, sublist);
            old_dset0 = THD_copy_one_sub( new_dset, nbriks-1); 
         }

         tross_Copy_History (old_dset, new_dset);
         // Warning for future generations: carefully note the order
         // of elements here for the DT!!!
         EDIT_dset_items (new_dset, ADN_brick_label_one + 0, "Dxx", ADN_none);
         EDIT_dset_items (new_dset, ADN_brick_label_one + 1, "Dxy", ADN_none);
         EDIT_dset_items (new_dset, ADN_brick_label_one + 2, "Dyy", ADN_none);
         EDIT_dset_items (new_dset, ADN_brick_label_one + 3, "Dxz", ADN_none);
         EDIT_dset_items (new_dset, ADN_brick_label_one + 4, "Dyz", ADN_none);
         EDIT_dset_items (new_dset, ADN_brick_label_one + 5, "Dzz", ADN_none);
         if(eigs_flag) {
            eigs_brik = ADN_brick_label_one + 6;   /* 1st eigenvalue brik */
            EDIT_dset_items(new_dset, eigs_brik+0, "lambda_1", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+1, "lambda_2", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+2, "lambda_3", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+3, "eigvec_1[1]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+4, "eigvec_1[2]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+5, "eigvec_1[3]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+6, "eigvec_2[1]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+7, "eigvec_2[2]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+8, "eigvec_2[3]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+9, "eigvec_3[1]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+10,"eigvec_3[2]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+11,"eigvec_3[3]", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+12,"FA", ADN_none);
            EDIT_dset_items(new_dset, eigs_brik+13,"MD", ADN_none);
            // pt,Oct,2016: for RD
            EDIT_dset_items(new_dset, eigs_brik+14,"RD", ADN_none); 
         }

         if(debug_briks) {
            EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 4, 
                             "Converge Step", ADN_none);
            EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 3, 
                             "ED", ADN_none);
            EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 2, 
                             "EDorig", ADN_none);
            EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 1, 
                             "I0", ADN_none);
         }

         tross_Make_History ("3dDWItoDT", argc, argv, new_dset);
         if(sep_dsets)
            Save_Sep_DTdata(new_dset, prefix, datum);
         else {
            DSET_write (new_dset);
            INFO_message("--- Output dataset %s", DSET_BRIKNAME(new_dset));
         } 
      }
   else
      {
         ERROR_exit("*** Error - Unable to compute output dataset!");
      }
   
   INFO_message("Finished main tensor+parameter calcs.");
   
   if ( debug_briks && (new_dset != NULL) ) {
      // pointer new_dset should just be to the AFNI-style DT, so this
      // should be fine...
      
      INFO_message("Start creating stats dset");

      char ffprefix[THD_MAX_PREFIX], fitprefix[THD_MAX_PREFIX];
      char *ext=NULL, nullch; 

      THD_3dim_dataset *CHIS=NULL;
      float **ccc=NULL;

      // ------------------ allocate ----------------------------
      // make output arrays
      ccc = calloc( nxyz, sizeof(ccc));   // major output set
      for(i=0 ; i<2 ; i++) 
         ccc[i] = calloc( nxyz, sizeof(float)); 
      
      if( (ccc == NULL) ) { 
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(4);
      } 

      // -------------------- calc ---------------------------
            
      EDIT_dset_items ( new_dsetDT,
                        ADN_func_type, old_dset->func_type,
                        ADN_type     , old_dset->type,
                        ADN_ntt      , DSET_NVALS (new_dsetDT),
                        ADN_ttorg    , 0.0,
                        ADN_ttdel    , 1.0, 
                        ADN_ttdur    , old_dset->taxis->ttdur,
                        ADN_nsl      , old_dset->taxis->nsl,
                        ADN_zorg_sl  , old_dset->taxis->zorg_sl,
                        ADN_toff_sl  , old_dset->taxis->toff_sl,
                        ADN_tunits   , UNITS_SEC_TYPE ,
                        ADN_none);

      // make the reference volume for the modeled signals be the I0
      // from the Powell_J value
      data_im = DSET_BRICK (old_dset0, 0);
      if( data_im == NULL ) {
         ERROR_exit("Null pointer when trying to make data_im set.");
      }

      fac = DSET_BRICK_FACTOR(old_dset0, 0); // get scale factor for
                                             // each sub-brik
      if(fac==0.0) fac=1.0;
      if((data_im->kind != MRI_float)) {
         fprintf (stderr, "*** Error - Can only open float datasets. "
                  "Use 3dcalc to convert.\n");
         mri_free (grad1Dptr);
         mri_free (data_im);
         exit (1);
      }
      
      I0_ptr = mri_data_pointer(data_im) ; /* pointer to I0 data */

      if( I0_ptr == NULL ) {
         ERROR_exit("Null pointer when trying to make I0 set.");
      }

      sprintf(ffprefix, "%s", prefix);
      if(has_known_non_afni_extension(prefix)){   // for NIFTI, 3D, Niml,
                                                  //   Analyze,...
         ext = find_filename_extension(prefix);
         // remove non-afni-extension for now
         ffprefix[strlen(prefix) - strlen(ext)] = '\0';  
      }
      else {
         nullch = '\0';
         ext = &nullch;
      }
      sprintf(fitprefix,"%s_CHI%s", ffprefix, ext);
      
      if ( new_dset == NULL) 
         ERROR_exit("New dset got reset to null?");

      // calculates FIT dset: !!! prob not use fitprefix *here*
      fit_dset = MAKER_4D_to_typed_fbuc (new_dsetDT, /* input dataset */
                                         fitprefix,  /* output prefix */
                                         datum,	     /* output datum  */
                                         0,	        /* ignore count  */
                                         0,      	  /* can't detrend in maker
                                                      function KRH 12/02 */
                                         nvols,	     /* number of briks */
                                         DTtoDWI_tsfunc,	/* timeseries processor */
                                         NULL,       /* data for tsfunc */
                                         NULL,       /* mask */
                                         0           /* Allow auto scaling of output */
                                         );

      if ( fit_dset == NULL) 
         ERROR_message("'fit' dset was somehow null!");

      // calc the goodness of fit!
      i = ChiSq_GOF( old_dset,
                     fit_dset,
                     B0list,
                     ccc,
                     maskptr);
      
      CHIS = EDIT_empty_copy( fit_dset );

      EDIT_dset_items( CHIS,
                       ADN_datum_all , MRI_float, 
                       ADN_ntt       , 2, 
                       ADN_nvals     , 2,
                       ADN_prefix    , fitprefix,
                       ADN_none );

      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(CHIS)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(CHIS));

      for( n=0; n<2 ; n++) {
         EDIT_substitute_brick(CHIS, n, MRI_float, ccc[n]);
         ccc[n]=NULL;
      }

      EDIT_BRICK_LABEL(CHIS, 0, "chi_p");      
      EDIT_BRICK_LABEL(CHIS, 1, "chi_c");      

      THD_load_statistics(CHIS);
      tross_Make_History("3dDWItoDT", argc, argv, CHIS);
      THD_write_3dim_dataset(NULL, NULL, CHIS, True);
      INFO_message("--- Output dataset %s", DSET_BRIKNAME(CHIS));

      // Count number of non-b0s
      n = 0;
      for(i=0 ; i<nvols ; i++) 
         if( !B0list[i] ) 
            n++;
      INFO_message("There were %d non-reference dsets (b>%.4f), "
                   "and %d reference ones.",
                   n, BMAX_REF, nvols-n);

      if(ccc) {
         for(i=0 ; i<2 ; i++) 
            free(ccc[i]);
         free(ccc);
      }
      if (CHIS) {
         DSET_delete(CHIS);
         free(CHIS);
      }

      /*
      tross_Copy_History (old_dset, fit_dset);
      tross_Make_History ("3dDWItoDT", argc, argv, fit_dset);
      //for(i=0 ; i<nvols ; i++)
      //  EDIT_dset_items (fit_dset, ADN_brick_label_one + i, "aa", ADN_none);

      DSET_write (fit_dset);
      INFO_message("--- Output dataset %s", DSET_BRIKNAME(fit_dset));
      */
   }
   INFO_message("Finished with all calcs: cleanup.");



   // ---------------- older cleanup ---------------------- 

   if (maskptr)
      {
         free (maskptr);
         if(anat_im)
            mri_free (anat_im);
#if 0
         DSET_unload_one (old_dset, 0);
         sar = NULL;
#endif
      }


   // !!!!! ADD MORE !!!!
   if (fit_dset) {
      DSET_delete(fit_dset);
      free(fit_dset);
   }
   if (old_dset0) {
      DSET_delete(old_dset0);
      free(old_dset0);
   }
   if (new_dsetDT) {
      DSET_delete(new_dsetDT);
      free(new_dsetDT);
   }

   FreeGlobals ();
   mri_free (grad1Dptr);
   matrix_destroy (&Rtmat); 

   exit (0);
}

/*! save separate datasets for each kind of output */
static void
Save_Sep_DTdata(whole_dset, prefix, output_datum)
     THD_3dim_dataset *whole_dset; /* whole dataset */
     char *prefix;
     int output_datum;
{
   /* takes base prefix and appends to it for DT, eigvalues,
      eigvectors, FA, MD, debug bricks */
   int nbriks;
   char nprefix[THD_MAX_PREFIX], tprefix[THD_MAX_PREFIX];
   char *ext, nullch; 

   ENTRY("Save_Sep_DTdata");
   sprintf(tprefix,"%s",prefix);
   if(has_known_non_afni_extension(prefix)){   /* for NIFTI, 3D, Niml,
                                                  Analyze,...*/
      ext = find_filename_extension(prefix);
      tprefix[strlen(prefix) - strlen(ext)] = '\0';  /* remove
                                                        non-afni-extension
                                                        for now*/
   }
   else {
      nullch = '\0';
      ext = &nullch;
   }

   sprintf(nprefix,"%s_DT%s", tprefix,ext);
   Copy_dset_array(whole_dset,0,6, nprefix, output_datum);
   if(eigs_flag) {
      sprintf(nprefix,"%s_L1%s", tprefix,ext);
      Copy_dset_array(whole_dset,6,1, nprefix, output_datum);
      sprintf(nprefix,"%s_L2%s", tprefix,ext);
      Copy_dset_array(whole_dset,7,1, nprefix, output_datum);
      sprintf(nprefix,"%s_L3%s", tprefix,ext);
      Copy_dset_array(whole_dset,8,1, nprefix, output_datum);
      sprintf(nprefix,"%s_V1%s", tprefix,ext);
      Copy_dset_array(whole_dset,9,3, nprefix, output_datum);
      sprintf(nprefix,"%s_V2%s", tprefix,ext);
      Copy_dset_array(whole_dset,12,3, nprefix, output_datum);
      sprintf(nprefix,"%s_V3%s", tprefix,ext);
      Copy_dset_array(whole_dset,15,3, nprefix, output_datum);
      sprintf(nprefix,"%s_FA%s", tprefix,ext);
      Copy_dset_array(whole_dset,18,1, nprefix, output_datum);
      sprintf(nprefix,"%s_MD%s", tprefix,ext);
      Copy_dset_array(whole_dset,19,1, nprefix, output_datum);
      sprintf(nprefix,"%s_RD%s", tprefix,ext);  // pt,Oct,2016: for RD
      Copy_dset_array(whole_dset,20,1, nprefix, output_datum);
   }  
   if(debug_briks) {
      sprintf(nprefix,"%s_debugbriks%s", tprefix,ext);
      nbriks =   whole_dset->dblk->nvals;
      Copy_dset_array(whole_dset,nbriks-4,4, nprefix, output_datum);
   }

   EXRETURN;
}

/*! create new dataset from part of existing dataset in memory */
static void
Copy_dset_array(whole_dset,startbrick,nbriks,prefix,output_datum)
     THD_3dim_dataset *whole_dset;
     int startbrick, nbriks;
     char *prefix;
     int output_datum;
{
   THD_3dim_dataset *out_dset;

   int i, ierror;
   MRI_IMAGE *fim;
   void *dataptr;
   float *fbuf;

   ENTRY("Copy_dset_array");

   out_dset = EDIT_empty_copy(whole_dset) ;
   fbuf = (float *)  malloc (sizeof(float)   * nbriks);

   tross_Copy_History (whole_dset, out_dset);
   ierror = EDIT_dset_items( out_dset ,
            ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
            ADN_prefix , prefix ,
            ADN_datum_all, output_datum,
            ADN_nvals, nbriks,
            ADN_ntt, 0,
            ADN_type        , ISHEAD(whole_dset)       /* dataset type */
            ? HEAD_FUNC_TYPE
            : GEN_FUNC_TYPE ,
            ADN_func_type   , FUNC_BUCK_TYPE ,        /* function type */
            ADN_none ) ;

   if(ierror>0) 
      ERROR_exit("*** Error - Unable to edit dataset!");

   THD_init_datablock_keywords( out_dset->dblk ) ;
   THD_init_datablock_stataux( out_dset->dblk ) ; /* for some reason,
                                                     need to do this
                                                     for single brick
                                                     NIFTI files */

   /* attach brick, factors and labels to new dataset using existing
      brick pointers */
   for(i=0;i<nbriks;i++) {
      fim = DSET_BRICK(whole_dset,startbrick+i);
      dataptr = mri_data_pointer(fim);
      fbuf[i] = whole_dset->dblk->brick_fac[startbrick+i];
      /* copy labels here too.....*/  
      EDIT_dset_items (out_dset, ADN_brick_label_one + i, 
                       whole_dset->dblk->brick_lab[startbrick+i], ADN_none);
      /*----- attach mri_image pointer to to be sub-brick #i -----*/
      EDIT_substitute_brick(out_dset, i, output_datum, dataptr);
   }

   (void) EDIT_dset_items( out_dset , ADN_brick_fac , fbuf , ADN_none ) ;
   DSET_write (out_dset);
   INFO_message("--- Output dataset %s", DSET_BRIKNAME(out_dset));
   /*----- deallocate memory -----*/
   THD_delete_3dim_dataset (out_dset, False);   out_dset = NULL ;
   free (fbuf);   fbuf = NULL;
   EXRETURN;
}


/* Form R matrix as matrix of [bxx 2bxy 2bxz byy 2byz bzz] for Np rows */
static void
Form_R_Matrix (MRI_IMAGE * grad1Dptr)
{
   matrix Rmat;
   register double sf = 1.0;	/* scale factor = 1.0 for now until we
                                 know DELTA, delta (gamma = 267.5
                                 rad/ms-mT) */
   register double sf2;		/* just scale factor * 2 */
   int i, nrows, noff; // @@new: offset counter, 'noff'= number for offset
   register float *imptr, *Gxptr, *Gyptr, *Gzptr;
   register float *Bxxptr, *Byyptr, *Bzzptr, *Bxyptr, *Bxzptr, *Byzptr;
   matrix *nullptr = NULL;
   register double Gx, Gy, Gz, Bxx, Byy, Bzz, Bxy, Bxz, Byz;
   double gscale;

   ENTRY ("Form_R_Matrix");
   sf2 = sf + sf;		/* 2 * scale factor for minor speed improvement */

   if (grad1Dptr->ny == 6) {
      /* just read from b-matrix and scale as necessary */
      noff = grad1Dptr->nx;         // offset is always whole dim of array
      if(BMAT_NZ)                   //  for non-zero first row of BMATs
         nrows = grad1Dptr->nx;
      else
         nrows = grad1Dptr->nx - 1; //$$old:  same for 'bmatrix_given' option
      matrix_initialize (&Rmat);
      matrix_create (nrows, 6, &Rmat);	/* Rmat = Np x 6 matrix */
      if (Rmat.elts == NULL)
         {				/* memory allocation error */
            ERROR_message("Could not allocate memory for Rmat");
            EXRETURN;
         }

      /* use simple floating point pointers to get values */
      Bxxptr = imptr = MRI_FLOAT_PTR (grad1Dptr);
      if( !BMAT_NZ ) {// @@new switch: start at the nonzero grad
         Bxxptr+= 1;
         imptr+= 1;
      }

      Byyptr = imptr + noff; // @@new:  needed to iterate by dim of array
      Bzzptr = Byyptr + noff;
      Bxyptr = Bzzptr + noff;
      Bxzptr = Bxyptr + noff;
      Byzptr = Bxzptr + noff;

      for (i = 0; i < nrows; i++)
         {
            Bxx = *Bxxptr++;
            Byy = *Byyptr++;
            Bzz = *Bzzptr++;
            Bxy = *Bxyptr++;
            Bxz = *Bxzptr++;
            Byz = *Byzptr++;
            Rmat.elts[i][0] = sf * Bxx;	/* bxx = Gx*Gx*scalefactor */
            Rmat.elts[i][1] = sf2 * Bxy;	/* 2bxy = 2GxGy*scalefactor */
            Rmat.elts[i][2] = sf2 * Bxz;	/* 2bxz = 2GxGz*scalefactor */
            Rmat.elts[i][3] = sf * Byy;	/* byy = Gy*Gy*scalefactor */
            Rmat.elts[i][4] = sf2 * Byz;	/* 2byz = 2GyGz*scalefactor */
            Rmat.elts[i][5] = sf * Bzz;	/* bzz = Gz*Gz*scalefactor */
         }
   }
   else {
      nrows = grad1Dptr->nx;
      matrix_initialize (&Rmat);
      matrix_create (nrows, 6, &Rmat);	/* Rmat = Np x 6 matrix */
      if (Rmat.elts == NULL)
         {				/* memory allocation error */
            ERROR_message("Could not allocate memory for Rmat");
            EXRETURN;
         }

      /* use simple floating point pointers to get values */
      Gxptr = imptr = MRI_FLOAT_PTR (grad1Dptr);	
      Gyptr = imptr + nrows;
      Gzptr = Gyptr + nrows;

      for (i = 0; i < nrows; i++)
         {
            Gx = *Gxptr++;
            Gy = *Gyptr++;
            Gz = *Gzptr++;
            gscale = sqrt(Gx*Gx + Gy*Gy + Gz*Gz); // apr,2016: scale by this
            if ( gscale < TINYNUMBER) // for b=0
               gscale = 1.;
            /* bxx = Gx*Gx*scalefactor */
            Rmat.elts[i][0] = sf * Gx * Gx / gscale;	
            /* 2bxy = 2GxGy*scalefactor */
            Rmat.elts[i][1] = sf2 * Gx * Gy / gscale;	
            /* 2bxz = 2GxGz*scalefactor */
            Rmat.elts[i][2] = sf2 * Gx * Gz / gscale;	
            /* byy = Gy*Gy*scalefactor */
            Rmat.elts[i][3] = sf * Gy * Gy / gscale;	
            /* 2byz = 2GyGz*scalefactor */
            Rmat.elts[i][4] = sf2 * Gy * Gz / gscale;	
            /* bzz = Gz*Gz*scalefactor */
            Rmat.elts[i][5] = sf * Gz * Gz / gscale;	
         }
   }

   /*
   fprintf(stderr,"\n\n TESTING: Bmatrix values...\n");
   for (i = 0; i < nrows; i++)
      INFO_message("%f  %f  %f  %f  %f  %f", 
                   Rmat.elts[i][0],
                   Rmat.elts[i][1],
                   Rmat.elts[i][2],
                   Rmat.elts[i][3],
                   Rmat.elts[i][4],
                   Rmat.elts[i][5]);
   */

   matrix_initialize (&Rtmat);
   /* compute pseudo-inverse of Rmat=Rtmat */
   matrix_psinv (Rmat, nullptr, &Rtmat);
   matrix_destroy (&Rmat);	/*  from the other two matrices */
   EXRETURN;
}





/*
  [PT: May, 2017]
  !! NB !! This has been copy/pasted from 3dDTtoDWI.c, so if changes
           happen there/here, likely make them here/there as well.
           This inglorious route was chosen rather than deal with
           compile-time issues with static variables.  I'm sure I'll
           adjust this more properly later.  Definitely.
*/
static void
DTtoDWI_tsfunc (double tzero, double tdelta,
                int npts, float ts[],
                double ts_mean, double ts_slope,
                void *ud, int nbriks, float *val)
{
   int i;
   static int nvox2, ncall2;
   double I0, bq_d;
   double *bptr;
   float *tempptr;
 
   ENTRY ("DTtoDWI_tsfunc");
   /* ts is input vector data of 6 floating point numbers.*/
   /* ts should come from data sub-briks in form of Dxx, Dxy, Dyy, Dxz, Dyz, Dzz */
   /* if automask is turned on, ts has 7 floating point numbers */
   /* val is output vector of form DWI0, DWI1, DWIn sub-briks */
   /* where n = number of gradients = nbriks-1 */

   /** is this a "notification"? **/
   if (val == NULL) {
      if (npts > 0) {			/* the "start notification" */
         nvox2 = npts;		/* keep track of   */
         ncall2 = 0;		/* number of calls */
      }
      else {			/* the "end notification" */
         
         /* nothing to do here */
      }
      EXRETURN;
   }
   
   ncall2++;
   
   if (automask)
      npts = npts - 1;

   tempptr = I0_ptr+ncall2-1;
   I0 = *tempptr;

   val[0] = I0; /* the first sub-brik is the I0 sub-brik */
   bptr = bmatrix+6;   /* start at the first gradient */

   // NOTE THE ORDER HERE! Need to match bmatrix and DT orderings,
   // which are different.  Yay.
   for(i=1;i<nbriks;i++) {
      bptr = bmatrix+(6*i);   /* start at the first gradient */
      bq_d = *bptr++ * ts[0];           /* GxGxDxx  */
      bq_d += *bptr++ * ts[1] * 2;      /* 2GxGyDxy */
      bq_d += *bptr++ * ts[3] * 2;      /* 2GxGzDxz */
      bq_d += *bptr++ * ts[2];          /* GyGyDyy  */
      bq_d += *bptr++ * ts[4] * 2;      /* 2GyGzDyz */
      bq_d += *bptr++ * ts[5];          /* GzGzDzz  */

      // for each gradient,q, Iq = J e -(bq.D) 
      val[i] = I0 * exp(-bq_d);
      // where bq.D is the large dot product of bq and D

   }

   EXRETURN;
}


/**********************************************************************
     Function that does the real work
***********************************************************************/

static void
DWItoDT_tsfunc (double tzero, double tdelta,
                int npts, float ts[],
                double ts_mean, double ts_slope,
                void *ud, int nbriks, float *val)
{
   int i, converge_step, converge, trialstep, ntrial, adjuststep, recordflag;
   double orig_deltatau, best_deltatau, EDold, J;
   static int ncall, noisecall;
   register double i0;
   register double dv, dv0;
   vector lnvector;
   int wtflag;          /* wtflag for recomputing wtfactor*/
   int max_converge_step, graphpoint;
   double dtau[50], Edgraph[50];
   int graphflag;
   int IS_BAD_MD = 0;

   ENTRY ("DWItoDT_tsfunc");
   /* ts is input vector data of Np+1 floating point numbers.
      For each point in volume brik convert vector data to
      symmetric matrix */
   /* ts should come from data sub-briks in form of I0,I1,...Ip */
   /* val is output vector of form 
      Dxx Dxy Dxz Dyy Dyz Dzz 
      for each voxel in 6 sub-briks */
   /* the Dij vector is computed as the product of Rt times ln(I0/Ip)
      where Rt is the pseudo-inverse of the [bxx 2bxy 2bxz byy 2byz
      bzz] for each gradient vector b */
   /** is this a "notification"? **/
   if (val == NULL){

      if (npts > 0)
         {			/* the "start notification" */
            /*nvox = npts;*/		/* keep track of   */
            ncall = 0;		/* number of calls */
            noisecall = 0;
         }
      else
         {			/* the "end notification" */

            /* nothing to do here */
         }
      EXRETURN;
   }

   ncall++;
   /* if there is any mask (automask or user mask), use corresponding
      voxel as a flag */
   if((maskptr && (maskptr[ncall-1]==0)) ||
      all_dwi_zero(npts, ts)){
      /* don't include this voxel for mask or if all zeros*/
      for (i = 0; i < nbriks; i++)	/* faster to copy preset vector */
         val[i] = 0.0;	/* return 0 for all Dxx,Dxy,... */
      if(debug_briks)  /* use -3 as flag for number of converge steps
                          to mean exited for masked voxels */
         val[nbriks-4] = -3.0;
      EXRETURN;
   }

   /* check for valid data at this series */
   if(bad_DWI_data(npts, ts)) {
      for (i = 0; i < nbriks; i++)	/* faster to copy preset vector */
         val[i] = 0.0;	/* return 0 for all Dxx,Dxy,... */
      Assign_CSF_values(val);
      EXRETURN;
   } 


   /* load the symmetric matrix vector from the "timeseries" subbrik
      vector values */
   vector_initialize (&lnvector);
   vector_create_noinit (npts - 1, &lnvector);
   if(use_mean_b0)
      dv0 = compute_mean_B0(npts, ts, 1);
   else
      dv0 = ts[0];


   if (dv0 > 0.0)
      i0 = log (dv0);
   else
      i0 = 0.0;
   for (i = 0; i < (npts - 1); i++)
      {
         dv = ts[i + 1];
         if ((dv > 0.0) && (dv0 > 0.0))
            lnvector.elts[i] = i0 - log (dv); /* ln I0/Ip = ln I0 - ln Ip */
         else
            lnvector.elts[i] = 0.0;
      }

   vector_multiply (Rtmat, lnvector, &Dvector);	/* D = Rt * ln(I0/Ip),
                                                   allocated Dvector
                                                   here */

   vector_destroy (&lnvector);	    /* free vector elements allocated */

   if((method==0)||(max_iter==-1)) {     /* for linear method,stop here
                                            and return D values */
      vector_to_array(Dvector, val);

      if(debug_briks) {
         InitWtfactors (npts);	  /* initialize all weight factors to 1
                                      for all gradient intensities */
         J = ComputeJ (ts, npts);  /* Ed (error) computed here */
         val[nbriks-4] = -1.0;     /* use -1 as flag for number of
                                      converge steps to mean exited
                                      for */
                                   /* or initial insignificant deltatau
                                      value */
         val[nbriks-3] = ED;
         val[nbriks-2] = ED;       /* store original error */
         val[nbriks-1] = J;
      }

      if(eigs_flag) {             /* if user wants eigenvalues in
                                     output dataset */
         EIG_func();              /* calculate eigenvalues,
                                     eigenvectors here */
         for(i=0;i<12;i++) 
            val[i+6] = eigs[i];
         /* calc FA */
         val[18] = Calc_FA(val+6); /* calculate fractional anisotropy */
         val[19] = Calc_MD(val+6); /* calculate mean diffusivity */
      }

      vals_to_NIFTI(val);

      EXRETURN;
   }


   /* now more complex part that takes into account noise */

   /* calculate initial estimate of D using standard linear model */
   EIG_func ();			   /* compute eigenvalues, eigenvectors standard
                              way */


   InitWtfactors (npts);	/* initialize all weight factors to 1 for all
                              gradient intensities */

   ComputeD0 ();			/* recalculate Dmatrix based on limits on
                           eigenvalues */


   if(matrix_sumabs(Dmatrix)<=TINYNUMBER) {
      for(i=0;i<nbriks;i++)
         val[i] = 0.0;
      if(debug_briks) {
         val[nbriks-4] = -2.0; /* use -2 as flag for number of converge
                                  steps to mean exited for insignificant
                                  D values*/
         val[nbriks-3] = 0;
         val[nbriks-2] = 0;    /* store original error */
         val[nbriks-1] = 0;
      }
      vals_to_NIFTI(val);      /* swap D tensor values for NIFTI
                                  standard */
      EXRETURN;
   }

   /* show verbose messages every verbose=n voxels */
   if(verbose&&(!(noisecall%verbose))) 
      recordflag = 1;
   else
      recordflag = 0;
   if(ncall==202293)
      recordflag = 1;

   /* graph in AFNI convergence steps every afnitalk_flag=n voxels */
   if(afnitalk_flag&&(!(noisecall%afnitalk_flag))) { 
      graphflag = 1;
      graphpoint = 0;
   }
   else
      graphflag = 0;

   noisecall++;

   /* allow up to max_iter=MAX_CONVERGE_STEPS (10) deltatau steps */
   converge_step = 0; 
   /* 1st time through set limit of converge steps to user option */
   max_converge_step = max_iter;   

   /* need to use Powell optimize method instead */
   if( (opt_method==1) || ((opt_method==2) && (voxel_opt_method==1))) { 
      if(recordflag)
         printf("using  powell method\n");
      converge_step = ComputeDwithPowell(ts, val, npts, nbriks); /*compute D */
      Dmatrix.elts[0][0] = val[0];
      Dmatrix.elts[0][1] = val[1];
      Dmatrix.elts[0][2] = val[2];
      Dmatrix.elts[1][1] = val[3];
      Dmatrix.elts[1][2] = val[4];
      Dmatrix.elts[2][2] = val[5];

      goto Other_Bricks;    /* compute the other bricks for eigenvalues
                               and debugging */
   }

   converge = 0;
   wtflag = reweight_flag;

   /* trial step */
   J = ComputeJ (ts, npts); 	           /* Ed (error) computed here */
   Store_Computations (0, npts, wtflag); /* Store 1st adjusted
                                            computations */
   matrix_copy (Dmatrix, &OldD);         /* store first Dmatrix in OldD
                                            too */

   if(debug_briks)
      val[nbriks-2] = ED;                /* store original error */

   EDold = ED;
   ComputeDeltaTau ();
   if(deltatau<=TINYNUMBER) {            /* deltatau too small, exit */
      for(i=0;i<nbriks;i++)
         val[i] = 0.0;
      if(debug_briks) {
         val[nbriks-4] = -1.0;           /* use -1 as flag for number
                                            of converge steps to mean
                                            exited for insignificant
                                            deltatau value */
         val[nbriks-1] = J;
      }
      vals_to_NIFTI(val);                /* swap D tensor values for
                                            NIFTI standard */
      EXRETURN;
   }

   ntrial = 0;

   while ( (converge_step < max_converge_step) && 
           (converge!=1) && (ntrial < 10) )
      {
         /* find trial step */
         /* first do trial step to find acceptable delta tau */
         /* first trial step is same size as previous time step */
         /* Then take half of previous tau step to see if less error */
         /* if there is less error, then delta tau is acceptable */
         /* Stop at first time step giving less error */
         /* try halving up to 10 times, if it does not work, */
         /* use first D from initial estimate (previous iteration) */
         trialstep = 1;
         ntrial = 0;
         orig_deltatau = best_deltatau = deltatau;
         while ((trialstep) && (ntrial < 10))
            {			/* allow up to 10 iterations to find trial step */
               ComputeHpHm (deltatau);
               ComputeNewD ();
               J = ComputeJ (ts, npts);	/* Ed (error) computed here */
               if (ED < EDold)            /* is error less than error
                                             of trial step or previous
                                             step? */
                  {
                     /* found acceptable step size of DeltaTau */
                     if(recordflag==1)
                        INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f, ntrial %d in find dtau", 
                                     ncall, converge_step, deltatau, ED, ntrial);
                     if(graphflag==1) {
                        dtau[graphpoint] = deltatau;
                        Edgraph[graphpoint] = ED;
                        graphpoint++;
                     }

                     EDold = ED;
                     best_deltatau = deltatau;
                     trialstep = 0;    /* leave trial stepping */
                     Store_Computations (1, npts, wtflag);	/* Store
                                                               current
                                                               computations */
                  }
               else {
                  Restore_Computations (0, npts, wtflag);	/* Restore
                                                               trial 0
                                                               computations */
                  deltatau = deltatau / 2;    /* find first DeltaTau
                                                 step with less error
                                                 than 1st guess */
                  /* by trying smaller step sizes */
                  ntrial++;
               }
            }

         deltatau = best_deltatau;

         /* end of finding trial step size */
         /* in trial step stage, already have result of deltatau step
            and may have already tried deltatau*2 if halved
            (ntrial>=1) */
         if(ntrial <10) {
            orig_deltatau = best_deltatau = deltatau;
            adjuststep = 1;

            for(i=0;i<2;i++) {
               if(i==0)
                  deltatau = orig_deltatau*2.0;
               else
                  deltatau = orig_deltatau/2.0;

               if((adjuststep==1) && ((i!=0) || (ntrial<2))) {   
                  /* if didn't shrink in initial deltatau step above */

                  Restore_Computations (1, npts, wtflag);	/* Restore
                                                               previous
                                                               Tau
                                                               step
                                                               computations */
                  ComputeHpHm (deltatau);
                  ComputeNewD ();
                  J = ComputeJ (ts, npts);	/* computes Intensity
                                                without noise,*/
                  /*   Ed, Residuals */
                  if(ED<EDold){
                     best_deltatau = deltatau;
                     adjuststep = 0;
                     Store_Computations(0, npts, wtflag); /* Store
                                                             Tau+dtau
                                                             step
                                                             computations */
                     EDold = ED;

                     if(recordflag==1) {
                        if(i==0)
                           INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f dt*2 best",
                                        ncall, converge_step, deltatau, ED);
                        else
                           INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f dt/2 best",
                                        ncall, converge_step, deltatau, ED);
                     }
                     if(graphflag==1) {
                        dtau[graphpoint] = deltatau;
                        Edgraph[graphpoint] = ED;
                        graphpoint++;
                     }
                  }
               }
            }

            deltatau = best_deltatau;

            if(adjuststep!=0){            /* best choice was first
                                             Delta Tau */
               ED = EDold;
               Restore_Computations (1,  npts, wtflag);	/* restore
                                                            old
                                                            computed
                                                            matrices*/
               /*   D,Hp,Hm,F,R */
               Store_Computations(0, npts, wtflag);	/* Store
                                                         Tau+dtau step
                                                         computations */
               if(recordflag==1)
                  INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f dt best", 
                               ncall, converge_step, deltatau, ED);
            }

            if(graphflag==1) {
               dtau[graphpoint] = deltatau;
               Edgraph[graphpoint] = ED;
               graphpoint++;
            }

            if (converge_step != 0) {	/* first time through
                                          recalculate*/
               /* now see if converged yet */
               converge = TestConvergence(Dmatrix, OldD);
            }

            matrix_copy (Dmatrix, &OldD);

            if(graphflag==1) {
               dtau[graphpoint] = deltatau;
               Edgraph[graphpoint] = ED;
               graphpoint++;
            }

            converge_step++;
         }
         else
            {
               if(recordflag==1)
                  INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f Exiting time evolution", 
                               ncall, converge_step, deltatau, ED);
               Restore_Computations(0, npts, wtflag);       /* Exit
                                                               with
                                                               original
                                                               step
                                                               calculation */
               ED = EDold;
            }


         if(((converge) || (converge_step==max_iter)) && wtflag && reweight_flag) {  
            /* if at end of iterations the first time*/

            converge = 0;                  /* through whole group of iterations */
            ComputeWtfactors (npts);       /* compute new weight factors */
            converge_step = 1;             /* start over now with computed weight factors */
            max_converge_step = max_iter_rw+1;   /* reset limit of converge steps to user option */
            wtflag = 0;                    /* only do it once - turn off next reweighting */
            J=ComputeJ(ts, npts);            /* compute new Ed value */
            EDold = ED;                    /* this avoids having to go through converge loop for two loops */
         }
      }  /* end while converge loop */

   ED = EDold;     

   val[0] = Dmatrix.elts[0][0];
   val[1] = Dmatrix.elts[0][1];
   val[2] = Dmatrix.elts[0][2];
   val[3] = Dmatrix.elts[1][1];
   val[4] = Dmatrix.elts[1][2];
   val[5] = Dmatrix.elts[2][2];





 Other_Bricks:

   // May,2016: final tensor has been estimated-- check on badness 2!
   if(bad_DWI_data_2_MD(val[0], val[3], val[5])) {
      for (i = 0; i < nbriks; i++)	/* faster to copy preset vector */
         val[i] = 0.0;	/* return 0 for all Dxx,Dxy,... */
      Assign_CSF_values(val);
      EXRETURN;
   } 

   if(eigs_flag) {                            /* if user wants eigenvalues in output dataset */
      double length, dl;
      udmatrix_to_vector(Dmatrix, &Dvector);
      EIG_func();                              /* calculate eigenvalues, eigenvectors here */
      for(i=0;i<3;i++) {
         if(fabs(eigs[i])<SMALLNUMBER)
            eigs[i] = 0.0;
      }
      for(i=0;i<12;i++)
         val[i+6] = eigs[i];
      /* calc FA */
      val[18] = Calc_FA(val+6);                /* calculate fractional anisotropy */
      val[19] = Calc_MD(val+6);               /* calculate average (mean) diffusivity */
      val[20] = Calc_RD(val+6);               // calculate radial diffusivity; pt,Oct,2016
      length = sqrt(eigs[3]*eigs[3]+eigs[4]*eigs[4]+eigs[5]*eigs[5]);
      dl = fabs(1.0 - length);
      if (dl>SMALLNUMBER) {
         if(recordflag) {
            printf("V1 not a unit vector, length = %f\n", length);
            printf("  V1 eigs %f %f %f\n", eigs[3], eigs[4], eigs[5]);
            printf("D tensor %f %f %f %f %f %f\n",
                   val[0],val[1],val[2],val[3],val[4],val[5]);
            printf("Time series:\n");
            for(i=0;i<npts;i++) printf("%f ", ts[i]);
            printf("\n");
         }
      }
   }

   /* testing information only */
   if(recordflag)
      INFO_message("ncall= %d, converge_step=%d, deltatau=%f, ED=%f", 
                   ncall, converge_step, deltatau, ED);
   if(debug_briks && ((opt_method==0) || ((opt_method==2) && (voxel_opt_method==0)) )){
      val[nbriks-4] = converge_step;
      val[nbriks-3] = ED;
      val[nbriks-1] = ComputeJ(ts, npts);            /* compute J value */;
   }
   if(graphflag==1) {
      DWI_AFNI_update_graph(Edgraph, dtau, graphpoint);
   }

   vals_to_NIFTI(val);   /* swap D tensor values for NIFTI standard */

   EXRETURN;
}

/* some simple checks for bad DWI data */
static int
bad_DWI_data(int npts, float *ts)
{
   double m0, m1;

   ENTRY("bad_DWI_data");
   /* check if mean of B0 is less than mean of rest of data */
   m0 = compute_mean_B0(npts, ts, 1);
   m1 = compute_mean_B0(npts, ts, 0);
   if(m0<=m1) RETURN(1); /* bad data */
   RETURN(0);
}

// May,2016-- added; extra badness check
static int
bad_DWI_data_2_MD(float v1, float v2, float v3)
{
   double m0;
   float v[3] = {0., 0., 0.};

   ENTRY("bad_DWI_data_2_MD");
   
   v[0] = v1;
   v[1] = v2;
   v[2] = v3;

   m0 = Calc_MD(v);
   if(m0 > MIN_BAD_SCALE_MD*csf_val) {
      //WARNING_message("Whoa! MD is reeeeally big: %f", m0);
      RETURN(1); /* bad data */
   }

   RETURN(0);
}

static int
all_dwi_zero(int npts, float *ts)
{
   int i;

   ENTRY("all_DWI_zero");
   for(i=0;i<npts;i++) {
      if(ts[i]!=0.0) RETURN(0);
   }
   RETURN(1);
}

static double
compute_mean_B0(int npts, float *ts, char B0flag)
{
   int i, nb=0;
   double m0=0.0, sum=0.0;

   ENTRY("compute_mean_B0");
   for(i=0;i<npts;i++) {
      if(B0list[i] == B0flag) {
         sum += (double) ts[i];
         nb++;
      }
   }
   m0 = sum / nb;
   RETURN(m0);
}

static void
Assign_CSF_values(float *val)
{
   /* assign default CSF values to tensor */
   // apr,2016: lots of '1.0' values changed to 'csf_val' for
   // consistency, and to not cause visualization issues anywhere.
   // Hopefully.  Maybe.  It still doesn't match technically with the
   // csf_fa, but at least that is *almost* zero.
   val[0] = val[3] = val[5] = csf_val;
   val[1] = val[2] = val[4] = 0.0;
   if(eigs_flag) {                    /* if user wants eigenvalues in
                                         output dataset */
      val[6] = csf_val;
      val[7] = csf_val;
      val[8] = csf_val;
      val[9] =  1.0;  val[10] = 0.0;  val[11] = 0.0; // eigenvectors 
      val[12] = 0.0;  val[13] = 1.0;  val[14] = 0.0;
      val[15] = 0.0;  val[16] = 0.0;  val[17] = 1.0;
      val[18] = csf_fa;               /* calculate fractional
                                         anisotropy */
      val[19] = csf_val;              /* calculate average (mean)
                                         diffusivity */
      val[20] = rd_val;              /* calculate average (mean)
                                         diffusivity */
   }
}

/* taken from 3dDTeig.c */
/*! given Dij tensor data for principal directions */
/* calculate 3 principal sets of eigenvalues and eigenvectors */
static void
EIG_func ()
{
   /*  THD_dmat33 inmat;
       THD_dvecmat eigvmat; */
   int i, j;
   int maxindex, minindex, midindex;
   float temp, minvalue, maxvalue;
   int sortvector[3];
   double a[9], e[3];
   int astart, vstart;


   ENTRY ("EIG_func");
   /* Dvector is vector data of 6 floating point numbers.
      For each point in volume brik convert vector data to
      symmetric matrix */
   /* Dvector should come in form of Dxx,Dxy,Dxz,Dyy,Dyz,Dzz */
   /* convert to matrix of form 
      [ Dxx Dxy Dxz]
      [ Dxy Dyy Dyz]
      [ Dxz Dyz Dzz]  */


   /* load the symmetric matrix vector from the "timeseries" subbrik
      vector values */

   a[0] = Dvector.elts[0];
   a[1] = Dvector.elts[1];
   a[2] = Dvector.elts[2];
   a[3] = Dvector.elts[1];
   a[4] = Dvector.elts[3];
   a[5] = Dvector.elts[4];
   a[6] = Dvector.elts[2];
   a[7] = Dvector.elts[4];
   a[8] = Dvector.elts[5];

   symeig_double (3, a, e);	/* compute eigenvalues in e,
                                 eigenvectors in a */

   maxindex = 2;			/* find the lowest, middle and highest
                           eigenvalue */
   maxvalue = e[2];
   minindex = 0;
   minvalue = e[0];
   midindex = 1;

   for (i = 0; i < 3; i++)
      {
         temp = e[i];
         if (temp > maxvalue)
            {			/* find the maximum */
               maxindex = i;
               maxvalue = temp;
            }
         if (temp < minvalue)
            {			/* find the minimum */
               minindex = i;
               minvalue = temp;
            }
      }

   for (i = 0; i < 3; i++)
      {				/* find the middle */
         if ((i != maxindex) && (i != minindex))
            {
               midindex = i;
               break;
            }
      }

   sortvector[0] = maxindex;
   sortvector[1] = midindex;
   sortvector[2] = minindex;

   /* put the eigenvalues at the beginning of the matrix */
   for (i = 0; i < 3; i++)
      {
         eigs[i] = e[sortvector[i]]; /* copy sorted eigenvalues */
         if(fabs (eigs[i]) < TINYNUMBER)
            eigs[i] = 0.0;
         /* start filling in eigenvector values */
         astart = sortvector[i] * 3; /* start index of double
                                        eigenvector */
         vstart = (i + 1) * 3;     	 /* start index of float val
                                        vector to copy eigenvector */

         for (j = 0; j < 3; j++)
            {
               eigs[vstart + j] = a[astart + j];
            }
      }

   EXRETURN;
}

/*! calculate Fractional Anisotropy */
/* passed float pointer to start of eigenvalues */
static float
Calc_FA(float *val)
{
   float FA;
   double ssq, dv0, dv1, dv2, dsq;

   ENTRY("Calc_FA");

   /* calculate the Fractional Anisotropy, FA */
   /*   reference, Pierpaoli C, Basser PJ. Microstructural and
        physiological features of tissues elucidated by
        quantitative-diffusion tensor MRI,J Magn Reson B 1996;
        111:209-19 */
   if((val[0]<=0.0)||(val[1]<0.0)||(val[2]<0.0)) {

      /* any negative eigenvalues-- should not see any for non-linear
         method. Set FA to 0*/
      RETURN(0.0);                                
   }

   /* sum of squares of eigenvalues */
   ssq = (val[0]*val[0])+(val[1]*val[1])+(val[2]*val[2]);        
   /* dsq = pow((val[0]-val[1]),2.0) + pow((val[1]-val[2]),2.0) +
      pow((val[2]-val[0]),2.0);*/ /* sum of differences squared */

   dv0 = val[0]-val[1];
   dv0 *= dv0;
   dv1 = val[1]-val[2];
   dv1 *= dv1;
   dv2 = val[2]-val[0];
   dv2 *= dv2;
   dsq = dv0+dv1+dv2;                 /* sum of differences squared */

   if(ssq!=0.0)
      FA = sqrt(dsq/(2.0*ssq));   /* FA calculated here */
   else
      FA = 0.0;

   RETURN(FA);
}

/*! calculate Mean Diffusivity */
/* passed float pointer to start of eigenvalues */
static float
Calc_MD(float *val)
{
   float MD;

   ENTRY("Calc_MD");

   /* calculate the Fractional Anisotropy, FA */
   /* reference, Pierpaoli C, Basser PJ. Microstructural and
      physiological features of tissues elucidated by
      quantitative-diffusion tensor MRI,J Magn Reson B 1996;
      111:209-19 */
   if((val[0]<=0.0)||(val[1]<0.0)||(val[2]<0.0)) {   
      /* any negative eigenvalues-- should not see any for non-linear
         method. Set FA to 0*/
      RETURN(0.0);                            
   }
   MD = (val[0] + val[1] + val[2]) / 3;

   RETURN(MD);
}

//! calculate radial Diffusivity;  pt,Oct,2016
/* passed float pointer to start of eigenvalues */
static float
Calc_RD(float *val)
{
   float RD;

   ENTRY("Calc_RD");

   /* calculate the Fractional Anisotropy, RD */
   if((val[0]<=0.0)||(val[1]<0.0)||(val[2]<0.0)) {   
      /* any negative eigenvalues-- should not see any for non-linear
         method. Set FA to 0*/
      RETURN(0.0);                            
   }
   RD = (val[1] + val[2]) / 2.;

   RETURN(RD);
}






/*! compute initial estimate of D0 */
/* D = estimated diffusion tensor matrix 
   [Dxx Dxy Dxz, Dxy Dyy Dyz, Dxz Dyz Dzz] */
/* updates Dvector and Dmatrix */
static void
ComputeD0 ()
{
   int i, j;
   /*   matrix ULmatrix, Ematrix; */
   double mu, alpha, sum;
   double e10, e11, e12, e20, e21, e22, e30, e31, e32;
   double l1, l2, l3;
   double t1, t3, t5, t8, t10, t12, t14, t18, t19, t21, t23, t32, t33, t35,
      t37;

   ENTRY ("ComputeD0");
   /* create and initialize D0 */

   if (eigs[0] < 0)
      {	/* if all eigenvalues are negative - may never happen */
         /* D0 = diag(a,a,a) where a=1/3 Sum(Abs(Lambda_i)) */
         sum = 0.0;
         for (i = 0; i < 3; i++)
            sum += fabs (eigs[i]);
         alpha = sum / 3;
         for (i = 0; i < 3; i++)
            {
               for (j = 0; j < 3; j++)
                  {
                     if (i == j)
                        Dmatrix.elts[i][j] = alpha;
                     else
                        Dmatrix.elts[i][j] = 0.0;
                  }
            }
         
         /* convert to vector format for D also */
         udmatrix_to_vector (Dmatrix, &Dvector);
         EXRETURN;
      }

   mu = backoff_factor * eigs[0];
   voxel_opt_method = 0;
   /*  mu = SMALLNUMBER;*/
   if (eigs[1] < mu) {	      /* set limit of eigenvalues to prevent */
      eigs[1] = mu;		      /* too much anisotropy */
      voxel_opt_method = 1;   /* switch to Powell optimization for
                                 this voxel */
   }
   if (eigs[2] < mu) {
      eigs[2] = mu;
      voxel_opt_method = 1;
   }
   /* D0 = U L UT */
   /*
     [e10 l1    e20 l2    e30 l3]
     [                          ]
     UL := [e11 l1    e21 l2    e31 l3]
     [                          ]
     [e12 l1    e22 l2    e32 l3]
   */


   /* assign variables to match Maple code */
   l1 = eigs[0];
   l2 = eigs[1];
   l3 = eigs[2];

   e10 = eigs[3];
   e11 = eigs[4];
   e12 = eigs[5];

   e20 = eigs[6];
   e21 = eigs[7];
   e22 = eigs[8];

   e30 = eigs[9];
   e31 = eigs[10];
   e32 = eigs[11];


#ifdef lkjsaklfj
   matrix_initialize (&Ematrix);
   matrix_create (3, 3, &Ematrix);
   /* fill Ematrix with Eigenvectors */

   matrix_initialize (&ULmatrix);
   matrix_create (3, 3, &ULmatrix);
   if (ULmatrix.elts == NULL)
      {				/* memory allocation error */
         ERROR_message("Could not allocate memory for Rmat");
         EXRETURN;
      }

   for (i = 0; i < 3; i++)
      {
         for (j = 0; j < 3; j++)
            {
               ULmatrix.elts[i][j] = Ematrix.elts[i][j] * eigs[i];
            }
      }

   /* new D is based on modified lambdas */
   matrix_multiply (ULmatrix, Ematrix, &Dmatrix);
#endif



   t1 = e10 * e10;
   t3 = e20 * e20;
   t5 = e30 * e30;
   t8 = e10 * l1;
   t10 = e20 * l2;
   t12 = e30 * l3;
   t14 = t8 * e11 + t10 * e21 + t12 * e31;
   t18 = t8 * e12 + t10 * e22 + t12 * e32;
   t19 = e11 * e11;
   t21 = e21 * e21;
   t23 = e31 * e31;
   t32 = e11 * l1 * e12 + e21 * l2 * e22 + e31 * l3 * e32;
   t33 = e12 * e12;
   t35 = e22 * e22;
   t37 = e32 * e32;
   Dmatrix.elts[0][0] = t1 * l1 + t3 * l2 + t5 * l3;
   Dmatrix.elts[0][1] = t14;
   Dmatrix.elts[0][2] = t18;
   Dmatrix.elts[1][0] = t14;
   Dmatrix.elts[1][1] = t19 * l1 + t21 * l2 + t23 * l3;
   Dmatrix.elts[1][2] = t32;
   Dmatrix.elts[2][0] = t18;
   Dmatrix.elts[2][1] = t32;
   Dmatrix.elts[2][2] = t33 * l1 + t35 * l2 + t37 * l3;

   /* convert to vector format for D */
   udmatrix_to_vector (Dmatrix, &Dvector);

   EXRETURN;
}

/*! compute the diffusion weighting matrix bmatrix for q number of gradients */
/* only need to calculate once */
/* bq = diffusion weighting matrix of qth gradient */
/*      GxGx GxGy GxGz
        GxGy GyGy GyGz
        GxGz GyGz GzGz */
/* b0 is 0 for all 9 elements */
/* bmatrix is really stored as 6 x npts array */
// -----> !! if you change anything here, probably should copy/paste
// -----> !! the function into 3dDTtoDWI, for consistency across
// -----> !! functions (as of apr,2016)!
static void
Computebmatrix (MRI_IMAGE * grad1Dptr, int NO_ZERO_ROW1) 
// flag to differentiate bmatrix cases that include a zero row or not
{
   int i, n;
   register double *bptr;
   register float *Gxptr, *Gyptr, *Gzptr;
   register float *Bxxptr, *Byyptr, *Bzzptr, *Bxyptr, *Bxzptr, *Byzptr;
   double Gx, Gy, Gz, Bxx, Byy, Bzz, Bxy, Bxz, Byz;
   double gscale;

   ENTRY ("Computebmatrix");
   n = grad1Dptr->nx;		/* number of gradients other than I0 */

   if ( (grad1Dptr->ny == 6)  && !NO_ZERO_ROW1 ) { // extra switch to
                                                   // keep OLD,
                                                   // zero-row version
      /* just read in b matrix */
      Bxxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point
                                             pointers to get values */
      Byyptr = Bxxptr + n;
      Bzzptr = Byyptr + n;
      Bxyptr = Bzzptr + n;
      Bxzptr = Bxyptr + n;
      Byzptr = Bxzptr + n;

      bptr = bmatrix;

      /*    B0list[0]= 1;*/  /* keep a record of which volumes have no
                                gradient: first one always assumed */

      for (i = 0; i < n; i++){
         Bxx = *Bxxptr++;
         Byy = *Byyptr++;
         Bzz = *Bzzptr++;
         Bxy = *Bxyptr++;
         Bxz = *Bxzptr++;
         Byz = *Byzptr++;
         *bptr++ = Bxx;
         *bptr++ = Bxy;
         *bptr++ = Bxz;
         *bptr++ = Byy;
         *bptr++ = Byz;
         *bptr++ = Bzz;

         // if(Bxx==0.0 && Byy==0.0 && Bzz==0.0)  

         // is this a zero gradient volume also? -> user can input a
         // larger value, if necessary.
         if( (Bxx+Byy+Bzz)<BMAX_REF )
            B0list[i] = 1;
         else{
            B0list[i] = 0;
            // apr,2016: need the MAX_BVAL for scaling
            gscale = Bxx + Byy + Bzz;
            if(gscale > MAX_BVAL)
               MAX_BVAL = gscale;
         }

      }
   } 
   else if( (grad1Dptr->ny == 6)  && NO_ZERO_ROW1 ) { //  very similar to old bmatrix option
      /* just read in b matrix */
      Bxxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
      Byyptr = Bxxptr + n;
      Bzzptr = Byyptr + n;
      Bxyptr = Bzzptr + n;
      Bxzptr = Bxyptr + n;
      Byzptr = Bxzptr + n;

      bptr = bmatrix;

      // do as grads below
      for (i = 0; i < 6; i++)
         *bptr++ = 0.0;		/* initialize first 6 elements to 0.0 for the I0 gradient */
      B0list[0]= 1;      /* keep a record of which volumes have no gradient */


      for (i = 0; i < n; i++){ 
         Bxx = *Bxxptr++;
         Byy = *Byyptr++;
         Bzz = *Bzzptr++;
         Bxy = *Bxyptr++;
         Bxz = *Bxzptr++;
         Byz = *Byzptr++;
         *bptr++ = Bxx;
         *bptr++ = Bxy;
         *bptr++ = Bxz;
         *bptr++ = Byy;
         *bptr++ = Byz;
         *bptr++ = Bzz;

         // is this a zero gradient volume also? 
         //if(Bxx==0.0 && Byy==0.0 && Bzz==0.0)  
         if( (Bxx+Byy+Bzz)<BMAX_REF )
            B0list[i+1] = 1; 
         else{
            B0list[i+1] = 0; 
            // apr,2016: need the MAX_BVAL for scaling
            gscale = Bxx + Byy + Bzz;
            if(gscale > MAX_BVAL)
               MAX_BVAL = gscale;
         }

      }
   }
   else {
      Gxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
      Gyptr = Gxptr + n;
      Gzptr = Gyptr + n;

      bptr = bmatrix;
      for (i = 0; i < 6; i++)
         *bptr++ = 0.0;		/* initialize first 6 elements to 0.0 for the I0 gradient */

      B0list[0]= 1;      /* keep a record of which volumes have no gradient */

      for (i = 0; i < n; i++)
         {
            gscale = 1.;  // apr,2016: allow for non-unit gradient magnitudes
            Gx = *Gxptr++;
            Gy = *Gyptr++;
            Gz = *Gzptr++;

            //if((Gx==0.0) && (Gy==0.0) && (Gz==0.0))
            gscale = sqrt(Gx*Gx + Gy*Gy + Gz*Gz);
            if( gscale<BMAX_REF )
               B0list[i+1] = 1;   /* no gradient applied*/
            else{
               B0list[i+1] = 0;
               if(gscale > MAX_BVAL)
                  MAX_BVAL = gscale; // apr,2016
            }

            *bptr++ = Gx * Gx / gscale; // apr,2016: allow for non-unit gradient magnitudes
            *bptr++ = Gx * Gy / gscale;
            *bptr++ = Gx * Gz / gscale;
            *bptr++ = Gy * Gy / gscale;
            *bptr++ = Gy * Gz / gscale;
            *bptr++ = Gz * Gz / gscale;
         }
   }
   EXRETURN;
}


/*! compute non-gradient intensity, J, based on current calculated values of 
  diffusion tensor matrix, D */
static double
ComputeJ (float ts[], int npts)
{
   /* J = Sum(wq Iq exp(-bq D)) / Sum (wq exp(-2bq D)) */
   /*     estimate of b0 intensity without noise and applied gradient */
   /* Iq = voxel value for qth gradient */
   /* bq = diffusion weighting matrix of qth gradient */
   /* wq = weighting factor for qth gradient at Iq voxel */
   /* D = estimated diffusion tensor matrix 
      [Dxx Dxy Dxz, Dxy Dyy Dyz, Dxz Dyz Dzz] */
   /* ts = Iq is time series voxel data from original data of intensities */

   register int i, j;
   double sum0, sum1, b1D1, b2D2, b4D4, wtexpbD, J, tempcalc, sumbD, Fscalar;
   double *expbD, *expbDptr, *wtfactorptr, *Ftempmatrix;
   register double *Fptr, *Rptr, *bptr;
   double D0,D1,D2,D3,D4,D5;

   ENTRY ("ComputeJ");
   sum0 = sum1 = 0.0;
   expbD = malloc (npts * sizeof (double));	/* allocate calculations for speed */
   expbDptr = expbD;		/* temporary pointers for indexing */
   wtfactorptr = wtfactor;
   bptr = bmatrix;		/* npts of b vectors (nx6) */

   D0 = Dmatrix.elts[0][0];
   D1 = Dmatrix.elts[0][1];
   D2 = Dmatrix.elts[0][2];
   D3 = Dmatrix.elts[1][1];
   D4 = Dmatrix.elts[1][2];
   D5 = Dmatrix.elts[2][2];


   for (i = 0; i < npts; i++)
      {
         /* compute bq.D */
         /* bq.D is large dot product of b and D at qth gradient */
         /* large dot product for Hilbert algebra */
         /* regular dot product is for Hilbert space (vectors only)- who knew? */
         /* calculate explicitly rather than loop to save time */
         b1D1 = *(bptr + 1) * D1;
         b1D1 += b1D1;
         b2D2 = *(bptr + 2) * D2;
         b2D2 += b2D2;
         b4D4 = *(bptr + 4) * D4;
         b4D4 += b4D4;

         sumbD = *bptr * D0 + b1D1 + b2D2 +	/* bxxDxx + 2bxyDxy +  2bxzDxz + */
            (*(bptr + 3) * D3) +	/* byyDyy + */
            b4D4 +			/* 2byzDyz + */
            (*(bptr + 5) * D5);	/* bzzDzz */

         /*  exp (-bq.D) */
         *expbDptr = exp (-sumbD);
         wtexpbD = *(wtfactor + i) * *expbDptr;
         sum0 += wtexpbD * ts[i];
         sum1 += wtexpbD * *expbDptr;
         expbDptr++;
         wtfactorptr++;
         bptr += 6;		/* increment to next vector of bmatrix */
      }

   J = sum0 / sum1;
   /* Now compute error functional,E(D,J) and gradient of E with respect to D ,Ed or F in notes */
   /* E(D,J)= 1/2 Sum[wq (J exp(-bq.D) - Iq)^2] */
   /* F = Ed =  - Sum[wq (J exp(-bq.D) - Iq) bq] *//* Ed is a symmetric matrix */
   sum0 = 0.0;
   sigma = 0.0;			/* standard deviation of noise for weight factors */
   expbDptr = expbD;
   wtfactorptr = wtfactor;
   /* initialize F matrix */
   Ftempmatrix = malloc(6*sizeof(double));
   Fptr = Ftempmatrix;
   for(i=0;i<6;i++)
      *Fptr++ = 0.0;
   Fptr = Ftempmatrix;
   Rptr = Rvector;		/* residuals calculated here - used in Wt.factor calculations */
   bptr = bmatrix;		/* npts of b vectors (nx6) */
   for (i = 0; i < npts; i++)
      {
         *Rptr = tempcalc = (J * *expbDptr) - ts[i];
         Fscalar = -*wtfactorptr * tempcalc;
         tempcalc = tempcalc * tempcalc;

         for (j = 0; j < 6; j++)
            {			/* for each entry of Fij (Fxx, Fxy,...) */
               /* F = - Sum[wq (J exp(-bq.D) - Iq) bq] = Sum[-wq (J exp(-bq.D) - Iq) bq] */
               *(Fptr+j) += Fscalar * (*bptr++);	/*  Fij = Fij + (Fscalar * bij)  */
            }

         sum0 += *wtfactorptr * tempcalc;	/* E(D,J) = Sum (wq temp^2) */
         sigma += tempcalc;	/* standard deviation of noise for weight factors */
         expbDptr++;
         wtfactorptr++;
         Rptr++;
      }

   udmatrix_copy (Ftempmatrix, &Fmatrix);	/* copy upper diagonal vector data into full matrix */

   ED = sum0 / 2;		/* this is the error for this iteration */

   free (Ftempmatrix);
   free (expbD);
   RETURN (J);
}

/*! compute initial step size for gradient descent */
static void
ComputeDeltaTau ()
{
   double sum0, sum1;
   matrix Dsqmatrix, FDsqmatrix, DsqFmatrix, Gmatrix;
   /* compute estimate of gradient, dD/dtau */
   /*G = [F] [D]^2 + [D]^2 [F] - ask Bob about ^2 and negative for this part to be sure */

   ENTRY ("ComputeDeltaTau");
   matrix_initialize (&Dsqmatrix);
   matrix_initialize (&FDsqmatrix);
   matrix_initialize (&DsqFmatrix);
   matrix_initialize (&Gmatrix);

   matrix_multiply (Dmatrix, Dmatrix, &Dsqmatrix);	/* compute D^2 */
   matrix_multiply (Fmatrix, Dsqmatrix, &FDsqmatrix);	/* FD^2 */
   matrix_multiply (Dsqmatrix, Fmatrix, &DsqFmatrix);	/* D^2F */
   matrix_add (FDsqmatrix, DsqFmatrix, &Gmatrix);	/* G= FD^2 +D^2F */


   /* deltatau = 0.01 * Sum(|Dij|) / Sum (|Gij|) */
   sum0 = matrix_sumabs (Dmatrix);
   sum1 = matrix_sumabs (Gmatrix);
   if (sum1 != 0.0)
      deltatau = 0.01 * sum0 / sum1;
   else
      deltatau = 0.0;
   matrix_destroy (&Dsqmatrix);
   matrix_destroy (&FDsqmatrix);
   matrix_destroy (&DsqFmatrix);
   matrix_destroy (&Gmatrix);
   EXRETURN;
}

/*! allocate all the global matrices and arrays once */
static void
InitGlobals (int npts)
{
   int i;
   double *cumulativewtptr;

   ENTRY ("InitGlobals");
   matrix_initialize (&Fmatrix);
   matrix_create (3, 3, &Fmatrix);
   matrix_initialize (&Dmatrix);
   matrix_create (3, 3, &Dmatrix);
   matrix_initialize (&Hplusmatrix);
   matrix_create (3, 3, &Hplusmatrix);
   matrix_initialize (&Hminusmatrix);
   matrix_create (3, 3, &Hminusmatrix);
   matrix_initialize (&OldD);
   matrix_create (3, 3, &OldD);
   for(i=0;i<2;i++){
      matrix_initialize (&tempFmatrix[i]);
      matrix_create (3, 3, &tempFmatrix[i]);
      matrix_initialize (&tempDmatrix[i]);
      matrix_create (3, 3, &tempDmatrix[i]);
      matrix_initialize (&tempHplusmatrix[i]);
      matrix_create (3, 3, &tempHplusmatrix[i]);
      matrix_initialize (&tempHminusmatrix[i]);
      matrix_create (3, 3, &tempHminusmatrix[i]);
   }
   Rvector = malloc (npts * sizeof (double));
   tempRvector = malloc (npts * sizeof(double));
   wtfactor = malloc (npts * sizeof (double));
   B0list = malloc (npts * sizeof (int));

   if(cumulative_flag && reweight_flag) {
      cumulativewt = malloc (npts * sizeof (double));
      cumulativewtptr = cumulativewt;
      for(i=0;i<npts;i++)
         *cumulativewtptr++ = 0.0;
      rewtvoxels = 0;
   }

   bmatrix = malloc (npts * 6 * sizeof (double));

   vector_initialize (&Dvector);	/* need to initialize vectors before 1st use-mod drg 12/20/2004 */
   /*  vector_initialize (&tempDvector);  vector_create(npts, &tempDvector);*/
   EXRETURN;
}

/*! free up all the matrices and arrays */
static void
FreeGlobals ()
{
   int i;

   ENTRY ("FreeGlobals");
   matrix_destroy (&Fmatrix);
   matrix_destroy (&Dmatrix);
   matrix_destroy (&Hplusmatrix);
   matrix_destroy (&Hminusmatrix);
   matrix_destroy (&OldD);
   for(i=0;i<2;i++){
      matrix_destroy (&tempFmatrix[i]);
      matrix_destroy (&tempDmatrix[i]);
      matrix_destroy (&tempHplusmatrix[i]);
      matrix_destroy (&tempHminusmatrix[i]);
   }


   free (wtfactor);
   wtfactor = NULL;
   free (bmatrix);
   bmatrix = NULL;
   free (Rvector);
   Rvector = NULL;
   free (tempRvector);
   tempRvector = NULL;
   free(B0list);
   B0list = NULL;

   vector_destroy (&Dvector);	/* need to free elements of Dvector - mod-drg 12/20/2004 */
   /*  vector_destroy (&tempDvector);*/
   if(cumulative_flag && reweight_flag) {
      free (cumulativewt);
      cumulativewt = NULL;
   }
   EXRETURN;
}

/*! store current computed matrices D,Hp,Hm, R */
static void
Store_Computations (int i, int npts, int wtflag)
{
   ENTRY ("Store_Computations");

   matrix_copy (Fmatrix, &tempFmatrix[i]);
   matrix_copy (Dmatrix, &tempDmatrix[i]);
   matrix_copy (Hplusmatrix, &tempHplusmatrix[i]);
   matrix_copy (Hminusmatrix, &tempHminusmatrix[i]);
   if(wtflag==1) 
      memcpy(tempRvector, Rvector, npts*sizeof(double));
   EXRETURN;
}

/*! restore old computed matrices D,Hp,Hm, R */
static void
Restore_Computations (int i, int npts, int wtflag)
{
   ENTRY ("Restore_Computations");

   matrix_copy (tempFmatrix[i], &Fmatrix);
   matrix_copy (tempDmatrix[i], &Dmatrix);
   matrix_copy (tempHplusmatrix[i], &Hplusmatrix);
   matrix_copy (tempHminusmatrix[i], &Hminusmatrix);
   if(wtflag==1)
      memcpy(Rvector, tempRvector, npts*sizeof(double));
   EXRETURN;
}

/*! set all wt factors for all gradient levels to be 1.0 the first time through */
static void
InitWtfactors (int npts)
{
   double *wtfactorptr;
   int i;

   ENTRY ("InitWtfactors");
   wtfactorptr = wtfactor;
   for (i = 0; i < npts; i++)
      *wtfactorptr++ = 1.0;
   EXRETURN;
}

/*! compute wt factors for each gradient level */
static void
ComputeWtfactors (int npts)
{
   /* Residuals, rq, computed above in ComputeJ, stored in Rmatrix */
   /* unnormalized standard deviation, sigma, computed there too */
   /*  wq = 1 / sqrt(1 + (rq/sigma)^2)
       where sigma = sqrt[1/Nq Sum(rq^2)] */
   /*  and rq = J exp(-bq.D) - Iq */

   int i;
   double *wtfactorptr, *Rptr;
   double *cumulativewtptr;
   double tempcalc, sum;

   ENTRY ("ComputeWtfactors");
   sigma = sigma / npts;
   sigma = sqrt (sigma);		/* sigma = std.dev. */

   wtfactorptr = wtfactor;
   Rptr = Rvector;

   sum = 0.0;
   for (i = 0; i < npts; i++)
      {
         tempcalc = *Rptr++ / sigma;
         tempcalc = tempcalc * tempcalc;
         tempcalc = 1.0 / (sqrt (1 + tempcalc));
         *wtfactorptr++ = tempcalc;
         sum += tempcalc;
      }
   /* now renormalize to avoid changing the relative value of E(D) */
   tempcalc = npts / sum;     /* normalization factor */

   wtfactorptr = wtfactor;
   for (i=0; i<npts; i++) {
      *wtfactorptr = *wtfactorptr * tempcalc;
      wtfactorptr++;
   }

   if(cumulative_flag) {
      wtfactorptr = wtfactor;
      cumulativewtptr = cumulativewt;
      /*  printf("Wt.factors: ");*/
      ++rewtvoxels;
      for (i=0; i<npts; i++){
         *cumulativewtptr++ += *wtfactorptr++;   /* calculate cumulative wt.factor across all voxels*/
      }
   }

   EXRETURN;
}

/*! compute Hplus and Hminus as a function of delta tau */
/* H+- = I +/- 1/2 deltatau F D */
static void
ComputeHpHm (double deltatau)
{
   matrix FDmatrix;
   double dtau;
   int i, j;

   ENTRY ("ComputeHpHm");
   dtau = 0.5 * deltatau;

   matrix_initialize (&FDmatrix);
   matrix_multiply (Fmatrix, Dmatrix, &FDmatrix);
   for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)
         FDmatrix.elts[i][j] = dtau * FDmatrix.elts[i][j];

   for (i = 0; i < 3; i++)
      {
         for (j = 0; j < 3; j++)
            {
               if (i == j)
                  {
                     Hplusmatrix.elts[i][j] = 1 + FDmatrix.elts[i][j];	/* I + dt/2 * FD */
                     Hminusmatrix.elts[i][j] = 1 - FDmatrix.elts[i][j];	/* I - dt/2 * FD */
                  }
               else
                  {
                     Hplusmatrix.elts[i][j] = Hminusmatrix.elts[i][j] =
                        FDmatrix.elts[i][j];
                  }
            }
      }

   matrix_destroy (&FDmatrix);
   EXRETURN;
}

/*! compute new D matrix */
/* D(tau+deltatau) = H-  H+^-1  D(tau)  H+^-1 H- */
/*                 = A          D(tau)  A^T */
/* where A = H- H+^-1 */
static void
ComputeNewD ()
{
   double *Hpinv;
   matrix Hpinvmatrix, Amatrix, ATmatrix, ADmatrix;

   ENTRY ("ComputeNewD");

   Hpinv =
      InvertSym3 (Hplusmatrix.elts[0][0], Hplusmatrix.elts[0][1],
                  Hplusmatrix.elts[0][2], Hplusmatrix.elts[1][1],
                  Hplusmatrix.elts[1][2], Hplusmatrix.elts[2][2]);
   matrix_initialize (&Hpinvmatrix);
   matrix_initialize (&Amatrix);
   matrix_initialize (&ATmatrix);
   matrix_initialize (&ADmatrix);

   matrix_create (3, 3, &Hpinvmatrix);
   udmatrix_copy (Hpinv, &Hpinvmatrix);	/* copy values from Hpinv vector to Hpinvmatrix */

   matrix_multiply (Hminusmatrix, Hpinvmatrix, &Amatrix);
   matrix_multiply (Amatrix, Dmatrix, &ADmatrix);
   matrix_transpose (Amatrix, &ATmatrix);
   matrix_multiply (ADmatrix, ATmatrix, &Dmatrix);

   matrix_destroy (&ADmatrix);
   matrix_destroy (&ATmatrix);
   matrix_destroy (&Amatrix);
   matrix_destroy (&Hpinvmatrix);

   free (Hpinv);
   EXRETURN;
}

/*! test convergence of calculation of D */
/* if sum of differences hasn't changed by more than 1E-4 */
/*  then the calculations have converged */
/* return 1 for convergence, 0 if not converged */
static int
TestConvergence(matrix NewD, matrix OldD)
{ 
   int converge;
   double convergence;

   ENTRY ("TestConvergence");
   /* convergence test */
   convergence = fabs (NewD.elts[0][0] - OldD.elts[0][0]) +	/* Dxx */
      fabs (NewD.elts[0][1] - OldD.elts[0][1]) +	/* Dxy */
      fabs (NewD.elts[0][2] - OldD.elts[0][2]) +	/* Dxz */
      fabs (NewD.elts[1][1] - OldD.elts[1][1]) +	/* Dyy */
      fabs (NewD.elts[1][2] - OldD.elts[1][2]) +	/* Dyz */
      fabs (NewD.elts[2][2] - OldD.elts[2][2]);	/* Dzz */

   if (convergence < SMALLNUMBER)
      converge = 1;
   else
      converge = 0;

   RETURN (converge);
}

/*! copy an upper diagonal matrix (6 point vector really) into a standard double
  array type matrix for n timepoints */
/* ud0 ud1 ud2         m0 m1 m2
   ud3 ud4   -->   m3 m4 m5
   ud5         m6 m7 m8 */
static void
udmatrix_copy (double *udptr, matrix * m)
{
   ENTRY ("udmatrix_copy");

   m->elts[0][0] = *udptr;
   m->elts[0][1] = *(udptr + 1);
   m->elts[0][2] = *(udptr + 2);
   m->elts[1][0] = *(udptr + 1);
   m->elts[1][1] = *(udptr + 3);
   m->elts[1][2] = *(udptr + 4);
   m->elts[2][0] = *(udptr + 2);
   m->elts[2][1] = *(udptr + 4);
   m->elts[2][2] = *(udptr + 5);
   EXRETURN;
}

/*! copy upper part of 3x3 matrix elements to 6-element vector elements */
/* m1 m2 m3
   m4 m5   ->  v = [m1 m2 m3 m4 m5 m6]
   m6
*/
static void
udmatrix_to_vector (matrix m, vector * v)
{
   ENTRY ("udmatrix_to_vector");
   v->elts[0] = m.elts[0][0];
   v->elts[1] = m.elts[0][1];
   v->elts[2] = m.elts[0][2];
   v->elts[3] = m.elts[1][1];
   v->elts[4] = m.elts[1][2];
   v->elts[5] = m.elts[2][2];
   EXRETURN;
}

/*! sum the absolute value of all  elements of a matrix */
static double
matrix_sumabs (matrix m)
{
   register int i, j;
   register double sum;

   ENTRY ("matrix_sumabs");
   sum = 0.0;
   for (i = 0; i < 3; i++)
      {
         for (j = 0; j < 3; j++)
            sum += fabs (m.elts[i][j]);
      }
   RETURN (sum);
}

/*! calculate inverse of a symmetric 3x3 matrix */
/* returns pointer to 9 element vector corresponding to 3x3 matrix */
/*  a b c */
/*  b e f */
/*  c f i */
/* Maple generated code */
static double *
InvertSym3 (double a, double b, double c, double e, double f, double i)
{
   double *symmat, *symmatptr;	/* invert matrix - actually 6 values in a vector form */
   double t2, t4, t7, t9, t12, t15, t20, t24, t30;

   ENTRY ("InvertSym3");
   symmat = malloc (6 * sizeof (double));
   symmatptr = symmat;
   t2 = f * f;
   t4 = a * e;
   t7 = b * b;
   t9 = c * b;
   t12 = c * c;
   t15 = 1 / (t4 * i - a * t2 - t7 * i + 2.0 * t9 * f - t12 * e);
   t20 = (b * i - c * f) * t15;
   t24 = (b * f - c * e) * t15;
   t30 = (a * f - t9) * t15;

   *symmatptr++ = (e * i - t2) * t15;	/*B[0][0] */
   *symmatptr++ = -t20;		/*B[0][1] */
   *symmatptr++ = t24;		/*B[0][2] */
   /* B[1][0] = -t20; */
   *symmatptr++ = (a * i - t12) * t15;	/* B[1][1] */
   *symmatptr++ = -t30;		/* B [1][2] */
   /* B[2][0] = t24; */
   /* B[2][1] = -t30; */
   *symmatptr = (t4 - t7) * t15;	/* B[2][2] */

   RETURN (symmat);
}

/*! copy elements from matrix a to matrix b */
/*  matrix_equate already exists but creates and initializes new matrix */
/*  steps we don't need to do here */
/* assumes both a and b already exist with equal dimensions */
static void
matrix_copy (matrix a, matrix * b)
{
   register int i;
   register int rows, cols;

   ENTRY ("matrix_copy");

   rows = a.rows;
   cols = a.cols;

   for (i = 0; i < rows; i++)
      {
#if 0
         register int j;
         for (j = 0; j < cols; j++)
            b->elts[i][j] = a.elts[i][j];
#else
         if (cols > 0)
            memcpy (b->elts[i], a.elts[i], sizeof (double) * cols);	/* RWCox */
#endif
      }
   EXRETURN;
}


#define DWI_WriteCheckWaitMax 2000
#define DWI_WriteCheckWait 400
/*-----------------------------------------------------*/
/* Stuff for an extra NIML port for non-SUMA programs. */

/* ZSS June 2011
   #ifndef NIML_TCP_FIRST_PORT
   #define NIML_TCP_FIRST_PORT 53212
   #endif
   Replace with: 
   get_port_named("AFNI_DEFAULT_LISTEN_NIML");
*/

/*! open NIML stream */
static int DWI_Open_NIML_stream()
{
   int nn, Wait_tot, tempport;
   char streamname[256];

   ENTRY("DWI_Open_NIML_stream");

   /* contact afni */
   tempport = get_port_named("AFNI_DEFAULT_LISTEN_NIML");
   sprintf(streamname, "tcp:localhost:%d",tempport);
   INFO_message("Contacting AFNI");

   DWIstreamid =  NI_stream_open( streamname, "w" ) ;
   if (DWIstreamid==0) {
      WARNING_message("Warning - NI_stream_open failed");
      DWIstreamid = NULL;
      RETURN(1);
   }

   INFO_message("Trying shared memory...");
   if (!NI_stream_reopen( DWIstreamid, "shm:DWIDT1M:1M" ))
      INFO_message("Warning: Shared memory communcation failed.");
   else
      INFO_message("Shared memory connection OK.");
   Wait_tot = 0;
   while(Wait_tot < DWI_WriteCheckWaitMax){
      nn = NI_stream_writecheck( DWIstreamid , DWI_WriteCheckWait) ;
      if( nn == 1 ){ 
         fprintf(stderr,"\n") ; 
         RETURN(0) ; 
      }
      if( nn <  0 ){ 
         WARNING_message("Bad connection to AFNI"); 
         DWIstreamid = NULL;
         RETURN(1);
      }
      Wait_tot += DWI_WriteCheckWait;
      fprintf(stderr,".") ;
   }

   WARNING_message("WriteCheck timed out (> %d ms).",DWI_WriteCheckWaitMax);
   RETURN(1);
}

/*! create the initial graph in AFNI - no points yet*/
static int DWI_NIML_create_graph()
{
   NI_element *nel;

   ENTRY("DWI_NIML_create_graph");
   nel = NI_new_data_element("ni_do", 0);
   NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
   NI_set_attribute ( nel, "ni_object", 
                      "OPEN_GRAPH_1D DWIConvEd 'DWI Convergence' "
                      "25 1 'converge step' 1 0 300000 Ed");
   if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
      WARNING_message("Failed to send data to AFNI");
      NI_free_element(nel) ; nel = NULL;
      RETURN(1);
   }
   NI_free_element(nel) ; 
   nel = NULL;
   RETURN(0);
}

/*! create new graph with left and right y axes scaled from 0 to max1, max2*/
static int DWI_NIML_create_newgraph(npts, max1, max2)
     int npts;
     double max1, max2;
{
   NI_element *nel;
   char stmp[256];
   static int nx = -1;

   ENTRY("DWI_NIML_create_newgraph");
   nel = NI_new_data_element("ni_do", 0);
   NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
   if((nx==-1) || (nx<npts))                 /* 1st time through close any existing graph by that name*/
      NI_set_attribute ( nel, "ni_object","CLOSE_GRAPH_1D DWIConvEd\n"); /* have to close graph to change axes */
   else
      NI_set_attribute ( nel, "ni_object","CLEAR_GRAPH_1D DWIConvEd\n");

   if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
      WARNING_message("Failed to send data to AFNI");
      NI_free_element(nel) ; nel = NULL;
      RETURN(1);
   }

   if((nx==-1) || (nx<npts)) {             /* update the graph only
                                              first time or if x-axis
                                              not big enough */
      nx = max_iter * 4  + 10;
      if(reweight_flag==1)
         nx += max_iter_rw * 4 + 10;
      if(nx<npts)                          /* fix graph to include
                                              largest number of
                                              steps */
         nx = npts;
      sprintf(stmp,"OPEN_GRAPH_1D DWIConvEd 'DWI Convergence' %d 1 'converge step' 2 0 100 %%Maximum Ed \\Delta\\tau\n",nx  );
      NI_set_attribute ( nel, "ni_object", stmp);
      if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
         WARNING_message("Failed to send data to AFNI");
         NI_free_element(nel) ; nel = NULL;
         RETURN(1);
      }
      NI_set_attribute ( nel, "ni_object", 
                         "SET_GRAPH_GEOM DWIConvEd geom=700x400+100+400\n");
      if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
         WARNING_message("Failed to send data to AFNI");
         NI_free_element(nel) ; nel = NULL;
         RETURN(1);
      }
   }
   NI_free_element(nel) ; 
   nel = NULL;
   RETURN(0);
}

/*! tell AFNI to graph data for convergence steps */
static void DWI_AFNI_update_graph(double *Edgraph, double *dtau, int npts)
{
   NI_element *nel;
   char stmp[256];
   int i;
   double Edmax, dtaumax;
   double *Edptr, *dtauptr;
   double tempEd, temptau;

   ENTRY("DWI_AFNI_update_graph");

   Edmax = 0.0; dtaumax = 0.0;
   Edptr = Edgraph;
   dtauptr = dtau;
   for(i=0;i<npts;i++) {
      if(*Edptr>Edmax)
         Edmax = *Edptr;
      if(*dtauptr>dtaumax)
         dtaumax = *dtauptr;
      ++Edptr; ++dtauptr;
   }

   NI_write_procins(DWIstreamid, "keep reading");
   DWI_NIML_create_newgraph(npts, Edmax, dtaumax);
   /* NI_sleep(250);*/

   nel = NI_new_data_element("ni_do", 0);
   NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
   NI_set_attribute ( nel, "ni_object", "CLEAR_GRAPH_1D DWIConvEd\n");
   /*      NI_sleep(25);*/
   if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
      WARNING_message("Failed to send data to AFNI");
   }


   for(i=0;i<npts;i++){
      if(Edmax!=0.0)
         tempEd = 100*Edgraph[i] / Edmax;
      else
         tempEd = 0.0;
      if(dtaumax!=0.0)
         temptau = 100*dtau[i] / dtaumax;
      else
         temptau = 0.0;

      /* put rel.error, Ed, and deltatau for all the convergence steps */
      sprintf(stmp,"ADDTO_GRAPH_1D DWIConvEd %4.2f %4.2f\n", tempEd, temptau);
      NI_set_attribute ( nel, "ni_object", stmp);  /* put command and data in stmp */
      NI_sleep(25);    /* for dramatic effect */
      if (NI_write_element( DWIstreamid, nel, NI_BINARY_MODE ) < 0) {
         WARNING_message("Failed to send data to AFNI");
      }
   }
   NI_free_element(nel) ; nel = NULL;
   EXRETURN;
}

/* need to put D tensor in NIFTI format standard */
static void vals_to_NIFTI(float *val)
{
   float temp;

   /* D tensor as lower triangular for NIFTI standard */
   temp = val[2];              
   val[2] = val[3];
   val[3] = temp;
}

/* function called at each optimization step */
double DT_Powell_optimize_fun(int n, double *x)
{
   /* n should always be 6 here */
   /* returns J*exp(-b/a) - data values */
   register int i;
   double sum0, sum1, b1D1, b2D2, b4D4, wtexpbD, tempcalc, sumbD;
   double *expbD, *expbDptr, *wtfactorptr;
   register double *Rptr,*bptr;
   double D0,D1,D2,D3,D4,D5;

   sum0 = sum1 = 0.0;
   expbD = malloc (Powell_npts * sizeof (double));
   expbDptr = expbD;		/* temporary pointers for indexing */
   wtfactorptr = wtfactor;
   bptr = bmatrix;		/* npts of b vectors (nx6) */

   D0 = x[0]*x[0];   /* D is used as upper triangular */
   D1 = x[0]*x[1];
   D2 = x[0]*x[3];
   D3 = (x[1]*x[1])+(x[2]*x[2]);
   D4 = (x[1]*x[3])+(x[2]*x[4]);
   D5 = (x[3]*x[3]) + (x[4]*x[4]) + (x[5]*x[5]);


   for (i = 0; i < Powell_npts; i++)
      {
         /* compute bq.D */
         /* bq.D is large dot product of b and D at qth gradient */
         /* large dot product for Hilbert algebra */
         /* regular dot product is for Hilbert space (vectors only)-
            who knew? */
         /* calculate explicitly rather than loop to save time */
         b1D1 = *(bptr + 1) * D1;
         b1D1 += b1D1;
         b2D2 = *(bptr + 2) * D2;
         b2D2 += b2D2;
         b4D4 = *(bptr + 4) * D4;
         b4D4 += b4D4;

         sumbD = *bptr * D0 + b1D1 + b2D2 +	/* bxxDxx + 2bxyDxy + 2bxzDxz + */
            (*(bptr + 3) * D3) +	/* byyDyy + */
            b4D4 +			/* 2byzDyz + */
            (*(bptr + 5) * D5);	/* bzzDzz */

         /*  exp (-bq.D) */
         *expbDptr = exp (-sumbD);
         wtexpbD = *(wtfactor + i) * *expbDptr;
         sum0 += wtexpbD * Powell_ts[i];
         sum1 += wtexpbD * *expbDptr;
         expbDptr++;
         wtfactorptr++;
         bptr += 6;		/* increment to next vector of bmatrix */
      }

   Powell_J = sum0 / sum1;
   /* Now compute error functional,E(D,J) and gradient of E with
      respect to D ,Ed or F in notes */
   /* E(D,J)= 1/2 Sum[wq (J exp(-bq.D) - Iq)^2] */
   sum0 = 0.0;
   sigma = 0.0;			/* standard deviation of noise for weight factors */
   expbDptr = expbD;
   wtfactorptr = wtfactor;
   Rptr = Rvector;		/* residuals calculated here - used in
                           Wt.factor calculations */
   for (i = 0; i < Powell_npts; i++)
      {
         *Rptr = tempcalc = (Powell_J * *expbDptr) - Powell_ts[i];
         tempcalc = tempcalc * tempcalc;
         sum0 += *wtfactorptr * tempcalc;	/* E(D,J) = Sum (wq temp^2) */
         sigma += tempcalc;	/* standard deviation of noise for
                                 weight factors */
         expbDptr++;
         wtfactorptr++;
         Rptr++;
      }

   /* sum0 is the error for this iteration */
   ED = sum0 / 2;		/* this is the error for this iteration */

   free (expbD);
   return(sum0);
}


/*! compute using optimization method by Powell, 2004*/
static int ComputeDwithPowell(float *ts, float *val, int npts, int nbriks) 
/* compute D tensor */
/* ts is input time-wise voxel data, val is output tensor data, npts
   is number of time points */
{
   /* assumes initial estimate for Dtensor already store in Dvector
      and Dmatrix above*/
   double *x, tx;
   int i, icalls;

   ENTRY("ComputeDwithPowell");

   Powell_npts = npts;
   Powell_ts = ts;

   x = (double *)malloc(sizeof(double)*6) ;

   /* move data into lower triangular format  */
   x[0] = sqrt(Dvector.elts[0]);
   x[1] = Dvector.elts[1] / x[0];
   x[2] = sqrt(Dvector.elts[3] - (x[1]*x[1]));
   x[3] = Dvector.elts[2] / x[0];
   x[4] = (Dvector.elts[4] - (x[1]*x[3]))/x[2];
   x[5] = sqrt(Dvector.elts[5] - (x[3]*x[3])-(x[4]*x[4]));

   /*printf("Dvector.elts[] %f %f %f %f %f %f\n",
     Dvector.elts[0],Dvector.elts[1],Dvector.elts[2],
     Dvector.elts[3],Dvector.elts[4],Dvector.elts[5]);*/
   if(debug_briks) {
      DT_Powell_optimize_fun(6, x);     /*  calculate original error */
      val[nbriks-2] = ED;                  /* store original error */
   }

   tx = TINYNUMBER;
   for(i=0;i<6;i++) {  /* find the largest element of the initial D tensor */
      if(x[i]>tx) tx = x[i];
   }

   icalls = powell_newuoa( 6 , x , 0.1*tx , 0.000001 * tx , 99999 , 
                           DT_Powell_optimize_fun ) ;


   if(reweight_flag) {
      ComputeWtfactors (npts);       /* compute new weight factors */
      tx = TINYNUMBER;
      for(i=0;i<6;i++) { /* find the largest element of the initial D tensor */
         if(x[i]>tx) tx = x[i];
      }
      /* parameters to powell_newuoa (not constrained)s
         ndim = 6   Solving for D tensor with 6 elements 
         x          variable for input and output (elements of D tensor)
         rstart = 0.1*tx size of search region aoround initial value of x
         rend = 0.001*tx size of final search region (desired accuracy)
         maxcall = 99999 maximum number times to call cost functin
         ufunc = DT_Powell_optimize_fun cost function 
      */
      i = powell_newuoa( 6 , x , 0.1*tx , 0.001 * tx , 
                         99999 , DT_Powell_optimize_fun ) ;
   }

   val[0] = x[0]*x[0];   /* D is used as upper triangular */
   val[1] = x[0]*x[1];
   val[2] = x[0]*x[3];
   val[3] = (x[1]*x[1])+(x[2]*x[2]);
   val[4] = (x[1]*x[3])+(x[2]*x[4]);
   val[5] = (x[3]*x[3]) + (x[4]*x[4]) + (x[5]*x[5]);
   /*
     printf("D tensor %f %f %f %f %f %f\n",
     val[0],val[1],val[2],val[3],val[4],val[5]);
   */ 
   if(debug_briks) {
      val[nbriks-4] = (float) icalls;
      if(icalls<1) { 
         printf("x values %12.9g %12.9g %12.9g %12.9g %12.9g %12.9g   tx %g\n", \
                x[0],x[1],x[2],x[3],x[4],x[5],tx );
         DT_Powell_optimize_fun(6, x);     /* compute J value if not
                                              already computed */
      }
      val[nbriks-3] = ED;
      val[nbriks-1] = Powell_J;            /* compute J value */;
   }
   free(x);

   RETURN(icalls);
}

// ========================================================================

// [PT: May, 2017] Calculate goodness-of-fit in two ways, following
// Papadakis et al. (2003, JMRI): Eqs. 1-4.

int ChiSq_GOF( THD_3dim_dataset *DWmeas, 
               THD_3dim_dataset *DWfit, 
               int *b0list,
               float **ccc,
               byte *maskp)
{
   int i,j,k;
   int Nvox = -1, Ndwi = -1;
   int Nwei = 0;
   double sumx=0., sumxx=0., diff=0.;
   float xm, xf;

   ENTRY("ChiSq_GOF");

   Nvox = DSET_NVOX(DWmeas);
   Ndwi = DSET_NVALS(DWmeas); // len of b0list

   // Count number of non-b0s
   for(j=0 ; j<Ndwi ; j++) 
      if( !b0list[j] ) 
         Nwei++;

   if( Nwei < 2 ) 
      ERROR_exit("Somehow only %d non-b0 values? "
                 "How can that be?", Nwei);

   // calc the chis
   for(i=0 ; i<Nvox ; i++) 
      if( *(maskp+i) ) {
         sumx = 0.;  
         sumxx = 0.;  
         diff = 0.;
         for(j=0 ; j<Ndwi ; j++) {
            if( !b0list[j] ) {
               xm = THD_get_voxel(DWmeas, i, j);
               xf = THD_get_voxel(DWfit, i, j) - xm;
               sumx+= xm;
               sumxx+= xm*xm;
               diff+= xf * xf;
            }
         }
         
         if( sumxx > SIG_EPS ) 
            ccc[0][i] = (float) (diff / sumxx); // fine chi_p
         else
            ccc[0][i] = -1;           // bad chi_p
         
         // Calc var; have already guarded against badness here.
         sumxx-= sumx*sumx/Nwei; 
         sumxx/= (Nwei - 1);
         
         if( sumxx > SIG_EPS ) 
            ccc[1][i] = (float) (diff / sumxx); // fine chi_c
         else
            ccc[1][i] = -1;           // bad chi_c
      }
   
   INFO_message("Calc'ed chi values. Writing out now.");
   
   
   RETURN(0);
}
