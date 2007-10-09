#include "mrilib.h"

static void NCO_help(void) ;  /* prototype */

/*---------------------------------------------------------------------------*/
#ifndef FLOATIZE
# include "matrix.h"          /* double precision */
# define MTYPE    double
# define NI_MTYPE NI_DOUBLE
# define QEPS 1.e-6
#else
# include "matrix_f.h"        /* single precision */
# define MTYPE    float
# define NI_MTYPE NI_FLOAT
# define QEPS 1.e-4
#endif

/*---------------------------------------------------------------------------*/
typedef struct {
  matrix x_all ;  /* the whole matrix (before censoring): nall X p */
  matrix x_full ; /* after censoring: nfull X p */

  matrix xtxinv_full ;    /* p X p */
  matrix xtxinvxt_full ;  /* p X nfull */

  matrix x_base ;         /* nfull X q */
  matrix xtxinvxt_base ;  /* q X nfull */
} linear_setup ;

#define INIT_linear_setup(ls)                    \
 do{ matrix_initialize(&((ls).x_all)) ;          \
     matrix_initialize(&((ls).x_full)) ;         \
     matrix_initialize(&((ls).xtxinv_full)) ;    \
     matrix_initialize(&((ls).xtxinvxt_full)) ;  \
     matrix_initialize(&((ls).x_base) ;          \
     matrix_initialize(&((ls).xtxinvxt_base) ;   \
 } while(0)

/*---------------------------------------------------------------------------*/
typedef struct {
  int parnum ;
  float tbot , ttop ;
  float *parval , *parbot , *partop ;
  void *qpar ;
  float (*f)(float,int,float *,void *) ;
} hrf_model ;

/*---------------------------------------------------------------------------*/
typedef struct {
  int polort, /* number of polynomial parameters PER run */
      qp,     /* total number of polynomial baseline parameters */
      q ,     /* total number of baseline parameters */
      p ,     /* total number of linear parameters (matrix columns) */
      nall ,  /* number of input data rows (before censoring) */
      nfull ; /* number of input data rows (after censoring) */

  int *goodlist ; /* goodlist[i] = index of i-th good row in all data */
  int  run_num ;  /* number of runs in total data */
  int *run_start; /* run_start[i] = data index of start of run #i */

  int          nprob ;      /* number of linear setups */
  linear_setup *prob ;
  float        *prob_toff ; /* time shift for each prob */

  int    num_hrf ;  /* number of HRF model functions we're finding */
  hrf_model *hrf ;  /* description of HRF model function */

  int     num_times ;        /* number of -stimtime options */
  char      **times_label ;  /* label for each option */
  MRI_IMAGE **times ;        /* times for each option */
  int        *times_hrfind ; /* which HRF model function to use */
} nonlinear_setup ;

/*---------------------------------------------------------------------------*/

void NCO_complete_linear_setup( linear_setup *ls, int ngood, int *goodlist, int qb )
{
   matrix *xf ;

ENTRY("complete_linear_setup") ;

   if( !ISVALID_MATRIX(ls->x_all) ){
     WARNING_message("Unprepared call to complete_linear_setup"); EXRETURN;
   }

   if( ISVALID_MATRIX(ls->x_full) ) matrix_destroy( &(ls->x_full) ) ;

   if( ngood > 0 && goodlist != NULL ){
     xf = &(ls->x_full) ;
     matrix_extract_rows( ls->x_all , ngood,goodlist , xf ) ;
   } else {
     xf = &(ls->x_all) ;
   }
   matrix_psinv( *xf , &(ls->xtxinv_full) , &(ls->xtxinvxt_full) ) ;

   if( qb > 0 && !ISVALID_MATRIX(ls->x_base) ){
     int *qlist=malloc(sizeof(int)*qb) , ii ;
     for( ii=0 ; ii < qb ; ii++ ) qlist[ii] = ii ;
     matrix_extract_cols( *xf , qb,qlist , &(ls->x_base) ) ;
     free((void *)qlist) ;
     matrix_psinv( ls->x_base , NULL , &(ls->xtxinvxt_base) ) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void NCO_clear_linear_setup( linear_setup *ls , int dobase )
{
   matrix_destroy( &(ls->x_full) ) ;
   matrix_destroy( &(ls->xtxinv_full) ) ;
   matrix_destroy( &(ls->xtxinvxt_full) ) ;
   if( dobase ){
     matrix_destroy( &(ls->x_base) ) ;
     matrix_destroy( &(ls->xtxinvxt_base) ) ;
   }
   return ;
}

/*===========================================================================*/
/*---------------------- Main program (not much here yet) -------------------*/
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

/*---------------------------------------------------------------------------*/

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
    " * Each time in the '-stimtime' inputs gets a separate amplitude estimate.\n"
    "    These need to be combined and/or contrasted in some other program,\n"
    "    such as 3dttest, to get a statistical map.\n"
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
    "    * 'IGAMVAR(d)' == similar to 'GAMVAR' but integrated over a duration\n"
    "        of 'd' seconds.  This HRF choice is designed for block-design\n"
    "        FMRI experiments.\n"
    "    * 'CSPLIN(b,t,n)' == same as in 3dDeconvolve, with the caveat that\n"
    "        the maximum amplitude of the HRF will be one.\n"
    "    * 'DITTO' or 'ditto' == use the same model AND the same parameter set\n"
    "        as the previous '-stimtime' argument.  In this way, the nonlinear\n"
    "        parameters for the HRFs for the two sets of time will be collapsed\n"
    "        into one set (e.g., both use the same value of 'tpeak' and 'fwhm').\n"
    "        Of course, the linear amplitudes for the two sets of times will be\n"
    "        separated.\n"
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

   printf("\nAUTHOR = Zhark the Experimental, October 2007\n\n" ) ;
   return ;
}
