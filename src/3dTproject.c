#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_blur3d_variable.c"
#include "cs_symeig.c"
#endif

/*----------------------------------------------------------------------------*/
/* Take a time series and a list of points to keep, and fill in the un-kept
   points with values linearly interpolated from the nearest kept points.
   -- This is an ad hoc fit to keep the Spanish Inquisition off my back. --
*//*--------------------------------------------------------------------------*/

void FILLIN_censored_time_series( int nt, int ntkeep, int *keep, float *xar )
{
   int ii , id,iu , qq , ntk1=ntkeep-1 ;
   float vd,vu ;

 ENTRY("FILLIN_censored_time_series") ;

   /* if 0 is not a kept point, then fill in from 0 up to the first keeper */

   iu = keep[0] ; vu = xar[iu] ;
   for( ii=0 ; ii < iu ; ii++ ) xar[ii] = xar[iu] ;

   /* loop over kept point pairs: from id..iu */

   for( qq=0 ; qq < ntk1 ; qq++ ){
     id = keep[qq]   ; vd = xar[id] ;     /* bottom of interval */
     iu = keep[qq+1] ; vu = xar[iu] ;        /* top of interval */
     if( iu-id > 1 ){                   /* any interior points? */
       float den , fu,fd ;
       den = 1.0f / (float)(iu-id) ;
       for( ii=id+1 ; ii < iu ; ii++ ){
         fu = (ii-id)*den ; fd = 1.0f-fu ; xar[ii] = fu*vu + fd*vd ;
       }
     }
   }

   /* if last point (nt-1) is not a kept point, fill in from the last keeper */

   id = keep[ntk1] ; vd = xar[id] ;
   for( ii=id+1 ; ii < nt ; ii++ ) xar[ii] = xar[id] ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

#define MIN_RUN 9

typedef struct {
   THD_3dim_dataset         *inset ;
   THD_3dim_dataset        *outset ;
   THD_3dim_dataset       *maskset ;
   int                    automask ;
   int                      polort ;
   int                  do_despike ;
   int                     do_norm ;
   MRI_IMARR                *ortar ;
   THD_3dim_dataset_array *dsortar ;
   float_pair            *stopband ;
   int                   nstopband ;
   float                        dt ;
   char                    *prefix ;
   float                      blur ;
   float                   *censar ;
   int                     ncensar ;
   int_triple          *abc_CENSOR ;
   int                  num_CENSOR ;
   int                     cenmode ;
   int                      nblock ;
   int             *blbeg , *blend ;
   int                        verb ;
} TPR_input ;

#define CEN_ZERO 1
#define CEN_KILL 2
#define CEN_NTRP 3

int   TPR_verb   = 0 ;
char *TPR_prefix = NULL ;

/*----------------------------------------------------------------------------*/

static TPR_input tin = { NULL,NULL,NULL , 0 ,
                         2 , 0 , 0 ,
                         NULL,NULL,
                         NULL,0 , 0.0f ,
                         "Tproject" ,
                         0.0f ,
                         NULL,0, NULL,0,CEN_KILL ,
                         0,NULL,NULL ,
                         1 } ;

static TPR_input *tinp = &tin ;

/*----------------------------------------------------------------------------*/

#undef  ADD_STOPBAND
#define ADD_STOPBAND(tp,bb,tt)                                                \
 do{ tp->stopband = (float_pair *)realloc(                                    \
                        tp->stopband, sizeof(float_pair)*(tp->nstopband+1)) ; \
     tp->stopband[tp->nstopband].a = (bb) ;                                   \
     tp->stopband[tp->nstopband].b = (tt) ; tp->nstopband++ ;                 \
 } while(0)

/*----------------------------------------------------------------------------*/

void TPR_help_the_pitiful_user(void)
{
  printf(
   "\n"
   "Usage:  3dTproject [options]\n"
   "\n"
   "This program projects (detrends) out various 'nuisance' time series from each\n"
   "voxel in the input dataset.  Note that all the projections are done via linear\n"
   "regression, including the frequency-based options such as '-passband'.  In this\n"
   "way, you can bandpass time-censored data, and at the same time, remove other\n"
   "time series of no interest (e.g., physiological estimates, motion parameters).\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -input dataset      = Specifies the input dataset.\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
#if 0
   "\n"
   " -despike            = Despike each input dataset time series before\n"
   "                       other processing.\n"
#endif
   "\n"
   " -censor cname       = As in 3dDeconvolve.\n"
   " -CENSORTR clist     = As in 3dDeconvolve.\n"
   " -cenmode mode       = 'mode' specifies how censored time points are treated in\n"
   "                       the output dataset:\n"
   "                       ++ mode = ZERO ==> put zero values in their place\n"
   "                                      ==> output datset is same length as input\n"
   "                       ++ mode = KILL ==> remove those time points\n"
   "                                      ==> output dataset is shorter than input\n"
   "                       ++ mode = NTRP ==> censored values are replaced by interpolated\n"
   "                                          neighboring (in time) non-censored values,\n"
   "                                          BEFORE any projections, and then the\n"
   "                                          analysis proceeds without actual removal\n"
   "                                          of any time points -- this feature is to\n"
   "                                          keep the Spanish Inquisition happy.\n"
   "                       ** The default mode is KILL !!!\n"
   "\n"
   " -concat ccc.1D      = The catenation file, as in 3dDeconvolve, containing the\n"
   "                       TR indexes of the start points for each contiguous run\n"
   "                       within the input dataset (the first entry should be 0).\n"
   "                       ++ Also as in 3dDeconvolve, if the input dataset is\n"
   "                          automatically catenated from a collection of datasets,\n"
   "                          then the run start indexes are determined directly,\n"
   "                          and '-concat' is not needed (and will be ignored).\n"
   "                       ++ Each run must have at least %d time points AFTER\n"
   "                          censoring, or the program will not work!\n"
   "                       ++ The only use made of this input is in setting up\n"
   "                          the bandpass/stopband regressors.\n"
   "                       ++ '-ort' and '-dsort' regressors run through all time\n"
   "                          points, as read in.  If you want separate projections\n"
   "                          in each run, then you must either break these ort files\n"
   "                          into appropriate components, OR you must run 3dTproject\n"
   "                          for each run separately, using the appropriate pieces\n"
   "                          from the ort files via the '{...}' selector for the\n"
   "                          1D files and the '[...]' selector for the datasets.\n"
   " -noblock            = Also as in 3dDeconvolve, if you want the program to treat\n"
   "                       an auto-catenated dataset as one long run, use this option.\n"
   "                       ++ However, '-noblock' will not affect catenation if you use\n"
   "                          the '-concat' option.\n"
   "\n"
   " -ort f.1D           = Remove each column in f.1D\n"
   "                       ++ Multiple -ort options are allowed.\n"
   "                       ++ Each column will have its mean removed.\n"
   " -polort pp          = Remove polynomials up to and including degree pp.\n"
   "                       ++ Default value is 2.\n"
   "                       ++ It makes no sense to use a value of pp greater than\n"
   "                          2, if you are bandpassing out the lower frequencies!\n"
   "                       ++ For catenated datasets, each run gets a separate set\n"
   "                          set of pp+1 Legendre polynomial regressors.\n"
   "                       ++ Use of -polort -1 is not advised (if data mean != 0),\n"
   "                          even if -ort contains constant terms, as all means are\n"
   "                          removed.\n"
   " -dsort fset         = Remove the 3D+time time series in dataset fset.\n"
   "                       ++ That is, 'fset' contains a different nuisance time\n"
   "                          series for each voxel (e.g., from AnatICOR).\n"
   "                       ++ Multiple -dsort options are allowed.\n"
#if 0
   "                       ++ These datasets are NOT despiked or blurred!\n"
#endif
   "\n"
   " -passband fbot ftop = Remove all frequencies EXCEPT those in the range\n"
   "  *OR* -bandpass       fbot..ftop.\n"
   "                       ++ Only one -passband option is allowed.\n"
   " -stopband sbot stop = Remove all frequencies in the range sbot..stop.\n"
   "                       ++ More than one -stopband option is allowed.\n"
   "                       ++ For example, '-passband 0.01 0.10' is equivalent to\n"
   "                          '-stopband 0 0.0099 -stopband 0.1001 9999'\n"
   " -dt dd              = Use time step dd for the frequency calculations,\n"
   " *OR* -TR              rather than the value stored in the dataset header.\n"
   "\n"
   " -mask mset          = Only operate on voxels nonzero in the mset dataset.\n"
   " *OR*                  ++ Use '-mask AUTO' to have the program generate the\n"
   " -automask                mask automatically (or use '-automask')\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
   "                       ++ If no masking option is given, then all voxels\n"
   "                          will be processed.\n"
   "\n"
   " -blur fff           = Blur (inside the mask only) with a filter that has\n"
   "                       width (FWHM) of fff millimeters.\n"
   "                       ++ Spatial blurring (if done) is after the time\n"
   "                          series filtering.\n"
   "\n"
   " -norm               = Normalize each output time series to have sum of\n"
   "                       squares = 1. This is the LAST operation.\n"
   "\n"
   " -quiet              = Hide the super-fun and thrilling progress messages.\n"
   "\n"
   " -verb               = The program will save the fixed ort matrix and its\n"
   "                       singular values into .1D files, for post-mortems.\n"
   "                       It will also print out more progress messages, which\n"
   "                       might help with figuring out what's happening when\n"
   "                       problems occur.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* The output dataset is in floating point format.\n"
   "\n"
   "* Removal of the various undesired components is via linear regression.\n"
   "   In particular, this method allows for bandpassing of censored time\n"
   "   series.\n"
   "\n"
   "* If you like technical math jargon (and who doesn't?), this program\n"
   "   performs orthogonal projection onto the null space of the set of 'ort'\n"
   "   vectors assembled from the various options '-polort', '-ort',\n"
   "   '-passband', '-stopband', and '-dsort'.\n"
   "\n"
   "* If A is a matrix whose column comprise the vectors to be projected\n"
   "   out, define the projection matrix Q(A) by\n"
   "    Q(A) = I - A psinv(A)\n"
   "   where psinv(A) is the pseudo-inverse of A [e.g., inv(A'A)A' -- but\n"
   "   the pseudo-inverse is actually calculated here via the SVD algorithm.]\n"
   "\n"
   "* If option '-dsort' is used, each voxel has a different matrix of\n"
   "   regressors -- encode this extra set of regressors in matrix B\n"
   "   (i.e., each column of B is a vector to be removed from its voxel's\n"
   "   time series). Then the projection for the compound matrix [A B] is\n"
   "      Q( Q(A)B ) Q(A) \n"
   "   that is, A is projected out of B, then the projector for that\n"
   "   reduced B is formed, and applied to the projector for the\n"
   "   voxel-independent A.  Since the number of columns in B is usually\n"
   "   many fewer than the number of columns in A, this technique can\n"
   "   be much faster than constructing the full Q([A B]) for each voxel.\n"
   "   (Since Q(A) only need to be constructed once for all voxels.)\n"
   "   A little fun linear algebra will show you that Q(Q(A)B)Q(A) = Q([A B]).\n"
   "\n"
   "* A similar regression could be done via the slower 3dTfitter program:\n"
   "    3dTfitter -RHS inputdataset+orig   \\\n"
   "              -LHS ort1.1D dsort2+orig \\\n"
   "              -polort 2 -prefix NULL   \\\n"
   "              -fitts Tfit\n"
   "    3dcalc -a inputdataset+orig -b Tfit+orig -expr 'a-b' \\\n"
   "           -datum float -prefix Tresidual\n"
   "  3dTproject should be MUCH more efficient, especially when using\n"
   "  voxel-specific regressors (i.e., '-dsort'), and of course, it also\n"
   "  offers internal generation of the bandpass/stopband regressors,\n"
   "  as well as censoring, blurring, and L2-norming.\n"
  "\n"
#ifdef USE_OMP
   "* This version of the program is compiled using OpenMP for speed.\n"
#else
   "* This version of the program is not compiled with OpenMP, but OpenMP\n"
   "   binaries DO exist, and using OpenMP will speed the program up.\n\n"
#endif
  "\n"
   "* Authored by RWCox in a fit of excessive linear algebra [summer 2013].\n"

    , MIN_RUN
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* get the workspace for compute_psinv() for a matrix of size m X n,
   with some extra if desired.
*//*--------------------------------------------------------------------------*/

static char * get_psinv_wsp( int m , int n , int extra )
{
   char *wsp ;

ENTRY("get_psinv_wsp") ;

#if 0
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */
   vmat = (double *)calloc( sizeof(double),n*n );
   umat = (double *)calloc( sizeof(double),m*n );   /* left singular vectors */
   sval = (double *)calloc( sizeof(double),n   );   /* singular values */
#endif

   if( extra < 0 ) extra = 0 ;
   wsp = (char *)malloc( sizeof(double) * (2*m*n + 2*n + n*n + extra) ) ;
   RETURN(wsp) ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float array
    (in column-major order).
    The input (rmat) is m X n, the output (pmat) is n X m.
    The workspace should be allocated by get_psinv_wsp(), and can be
    free()-ed when the need for it is over.
*//*--------------------------------------------------------------------------*/

static void compute_psinv( int m, int n, float *rmat, float *pmat, double *wsp )
{
   int ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , *wpp , smax,del ;
   register double sum ;

   /* deal with a single vector (of length m) */

   if( n == 1 ){
     for( sum=0.0,ii=0 ; ii < m ; ii++ ) sum += rmat[ii]*rmat[ii] ;
     if( sum > 0.0 ){
       sum = 1.0 / sum ;
       for( ii=0 ; ii < m ; ii++ ) pmat[ii] = sum * rmat[ii] ;
     } else {
       for( ii=0 ; ii < m ; ii++ ) pmat[ii] = 0.0f ;
     }
     return ;
   }

   /* OK, have a real matrix to handle here */

#if 0
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */
   vmat = (double *)calloc( sizeof(double),n*n );
   umat = (double *)calloc( sizeof(double),m*n );   /* left singular vectors */
   sval = (double *)calloc( sizeof(double),n   );   /* singular values */
#else
   wpp = wsp ; if( wpp == NULL ) wpp = (double *)get_psinv_wsp(m,n,0) ;
   amat = wpp ;            /* input matrix           (m*n) */
   xfac = amat + m*n ;     /* column norms of amat   (n)   */
   vmat = xfac + n ;       /* right singular vectors (n*n) */
   umat = vmat + n*n ;     /* left singular vectors  (n*m) */
   sval = umat + m*n ;     /* singular values        (n)   */
#endif

#undef  R
#undef  A
#undef  P
#undef  U
#undef  V
#define R(i,j) rmat[(i)+(j)*m]
#define A(i,j) amat[(i)+(j)*m]
#define P(i,j) pmat[(i)+(j)*n]
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = (double)R(ii,jj) ;

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     for( sum=0.0,ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ;
     xfac[jj] = sum ;
     if( sum > 0.0 ){
       for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
     }
   }

   /* compute SVD of scaled matrix */

   svd_double( m , n , amat , sval , umat , vmat ) ;

   /* find largest singular value */

   smax = sval[0] ; if( smax < 0.0 ) smax = 0.0 ;
   for( ii=1 ; ii < n ; ii++ ){
          if( sval[ii] > smax ) smax = sval[ii] ;
     else if( sval[ii] < 0.0  ) sval[ii] = 0.0 ;   /* should not happen */
   }

   if( smax <= 0.0 ){                        /* this is bad */
     static int first=1 ;
#pragma omp critical
     { if( first ){ ERROR_message("SVD fails in compute_psinv()!\n"); first=0; }}
     AAmemset( pmat , 0 , sizeof(float)*m*n ) ;
     if( wpp != wsp ) free(wpp) ;
     return ;
   }

   if( TPR_verb > 1 && TPR_prefix != NULL ){    /* save singular values? */
     char fname[256] ; FILE *fp ;
     sprintf(fname,"%s.sval.1D",TPR_prefix) ;
     fp = fopen( fname , "w" ) ;
     for( ii=0 ; ii < n ; ii++ ) fprintf(fp," %14.7g\n",sval[ii]) ;
     fclose(fp) ;
   }

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

#undef  PSINV_EPS
#define PSINV_EPS 1.e-6

   del = PSINV_EPS * smax*smax ;
   for( ii=0 ; ii < n ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       P(ii,jj) = (float)sum ;
     }
   }

   /** must now rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }

   if( wpp != wsp ) free(wpp) ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Project out ONE set of vectors:
     nr = number of rows = length of vectors
     nc = number of columns = number of vectors
    aar = nr X nc array of vectors
    par = nc X nr pseudo-inverse of aar
    var = nr length vector to have columns of aar projected out of
          (will be modified in place)
    wsp = workspace (at least nc floats long)
*//*--------------------------------------------------------------------------*/

static void project_out_once( int nr, int nc,
                              float *aar, float *par,
                              float *var, float *wsp )
{
   float *ap,*pp,vv ; int ii,jj,kk ;

   if( nr <= 0 || nc <= 0 ) return ;

   for( ii=0 ; ii < nc ; ii++ ) wsp[ii] = 0.0f ;
   for( kk=0 ; kk < nr ; kk++ ){
     pp = par + kk*nc ; vv = var[kk] ;
     for( ii=0 ; ii < nc ; ii++ ) wsp[ii] += pp[ii]*vv ;
   }
   for( kk=0 ; kk < nc ; kk++ ){
     ap = aar + kk*nr ; vv = wsp[kk] ;
     for( ii=0 ; ii < nr ; ii++ ) var[ii] -= ap[ii]*vv ;
   }
   return ;
}

/*----------------------------------------------------------------------------*/
/* Project out TWO sets of vectors, one of which is fixed (aar) and one
   of which varies with each voxel (bar):
     nr = number of rows = length of vectors
     nc = number of columns = number of vectors in the first set
    aar = nr X nc array of vectors in the first set
    par = nc X nr pseudo-inverse of aar
     nb = number of vectors in the second set
          if nb == 0, then this function just calls project_out_once()
    bar = nr x nb array of vectors in the second set
   pbar = nb x nr workspace of floats to hold the pseudo-inverse of bar
          (will be computed in here)
    var = nr length vector to have columns of aar projected out of,
          and then columns of bar also (will be modified in place)
    wsp = workspace (at least MAX(nc,nb) floats long)
*//*--------------------------------------------------------------------------*/

static void project_out_twice( int nr , int nc , float *aar , float *par  ,
                                        int nb , float *bar , float *pbar ,
                                                 float *var , char *wsp    )
{
   project_out_once( nr,nc , aar,par  , var , (float *)wsp ) ;

   if( nb > 0 ){
     int kk ;
     for( kk=0 ; kk < nb ; kk++ )
       project_out_once( nr,nc,aar,par , bar+kk*nr , (float *)wsp ) ;
     compute_psinv( nr,nb , bar , pbar , (double *)wsp ) ;
     project_out_once( nr,nb , bar,pbar , var , (float *)wsp ) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

static void vector_demean( int n , float *v )
{
   register int ii ; register float sm ;

   for( sm=0.0f,ii=0 ; ii < n ; ii++ ) sm += v[ii] ;
   for( sm/=n,  ii=0 ; ii < n ; ii++ ) v[ii] -= sm ;
   return ;
}

/*----------------------------------------------------------------------------*/

static int is_vector_zero( int n , float *v )
{
   int ii ;
   for( ii=0 ; ii < n && v[ii] == 0.0f ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/

static int is_vector_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/
/* Process all the data, as pointed to by tp */

void TPR_process_data( TPR_input *tp )
{
   int nt,ntkeep,ntuse , nort_fixed=0 , nort_voxel=0 , nbad=0 , qq,jj,qort ;
   byte *vmask=NULL ; int nvmask=0 , nvox , nvout ;
   float *ort_fixed=NULL , *ort_fixed_psinv=NULL , *ort_fixed_unc=NULL ;
   int *keep=NULL ;
   MRI_vectim *inset_mrv , **dsort_mrv=NULL ; int nort_dsort=0 ;
   int nbl , *bla , *blb , tt ;
   int **fmask=NULL , *nf=NULL ; float *df=NULL ;

ENTRY("TPR_process_data") ;

   /*----- structural constants -----*/

   nvox = DSET_NVOX(tp->inset) ;
   nt   = DSET_NVALS(tp->inset) ;

   nbl = tinp->nblock ; bla = tinp->blbeg ; blb = tinp->blend ;
   if( nbl < 1 ){
     nbl = 1 ;
     bla = (int *)malloc(sizeof(int)) ; bla[0] = 0 ;
     blb = (int *)malloc(sizeof(int)) ; blb[0] = nt-1 ;
   }

   /*----- make censor array -----*/

   if( tp->censar != NULL){
STATUS("make censor array") ;
     if( tp->ncensar < nt ){
       ERROR_message("-censor file is too short (%d) for dataset (%d)",tp->ncensar,nt) ;
       nbad++ ;
     } else if( tp->ncensar > nt ){
       WARNING_message("-censor file is too long (%d) for dataset (%d)",tp->ncensar,nt) ;
     }
   } else {
     tp->censar = (float *)malloc(sizeof(float)*nt) ;
     for( jj=0 ; jj < nt ; jj++ ) tp->censar[jj] = 1.0f ;
   }

   /*-- apply any -CENSORTR commands --*/

   if( tp->num_CENSOR > 0 ){
     int cc , r,a,b , bbot,btop ;
STATUS("apply -CENSORTR commands") ;
     if( TPR_verb > 1 ) INFO_message("-CENSORTR commands:") ;
     for( cc=0 ; cc < tp->num_CENSOR ; cc++ ){
       r = tp->abc_CENSOR[cc].i; a = tp->abc_CENSOR[cc].j; b = tp->abc_CENSOR[cc].k;
       if( nbl > 1 && r == -666 ){ /* wildcard run ==> create new triples */
         int_triple rab ;
         tp->abc_CENSOR = (int_triple *)realloc( tp->abc_CENSOR ,
                                                 sizeof(int_triple)*(tp->num_CENSOR+nbl) ) ;
         for( r=1 ; r <= nbl ; r++ ){
           rab.i = r ; rab.j = a ; rab.k = b ; tp->abc_CENSOR[tp->num_CENSOR++] = rab ;
         }
         if( TPR_verb > 1 )
           ININFO_message(" expand wildcard for %d..%d",a,b) ;
         continue ;  /* skip to next triple in newly expanded list */
       }
       if( nbl > 1 && r != 0 ){  /* convert local indexes to global */
         if( r < 0 || r > nbl ){
           WARNING_message("-CENSORTR %d:%d-%d has run index out of range 1..%d",
                           r,a,b , nbl ) ;
           a = -666666 ; b = -777777 ;
         } else {
           bbot = bla[r-1] ;  /* run indexes start at 1, but AFNI counts from 0 */
           btop = blb[r-1] ;
           if( a+bbot > btop ){
             WARNING_message("-CENSORTR %d:%d-%d has start index past end of run (%d)",
                             r,a,b,btop-bbot) ;
           }
           if( TPR_verb > 1 )
             ININFO_message(" run #%d: alter local %d..%d to global %d..%d",
                            r , a,b , a+bbot,MIN(b+bbot,btop) ) ;
           a += bbot ; b += bbot ; if( b > btop ) b = btop ;
         }
       }
       if( a < 0 || a >= nt || b < a ){
         if( TPR_verb > 1 )
           ININFO_message(" -- skip %d..%d as it is out of data range or means nothing",a,b) ;
         continue ;
       }
       if( b >= nt ) b = nt-1 ;
       if( TPR_verb > 1 )
         ININFO_message(" -- mark %d..%d as censored",a,b) ;
       for( jj=a ; jj <= b ; jj++ ) tp->censar[jj] = 0.0f ;
     }
   }

   /*-- count number of time points that are kept (NOT censored) --*/

STATUS("count un-censored points") ;
   for( jj=ntkeep=0 ; jj < nt ; jj++ ) if( tp->censar[jj] != 0.0f ) ntkeep++ ;

   if( ntkeep < nt )
     INFO_message("input time points = %d ; censored = %d ; remaining = %d",
                  nt , nt-ntkeep , ntkeep ) ;

   if( ntkeep < MIN_RUN )
     ERROR_exit(
       "only %d points left after censoring -- cannot continue (need at least %d)",
       ntkeep,MIN_RUN) ;

   /** The number of time points to use in regression [06 Dec 2013] **/

   ntuse = (tp->cenmode == CEN_NTRP) ? nt : ntkeep ;

   /* do the same for individual runs, if any */

   if( nbl > 1 ){
     int aa,bb, nnk , nerr=0 ;
STATUS("count un-censored points in each run") ;
     for( tt=0 ; tt < nbl ; tt++ ){
       aa = bla[tt] ; bb = blb[tt] ;
       for( nnk=0,jj=aa ; jj <= bb ; jj++ ) if( tp->censar[jj] != 0.0f ) nnk++ ;
       if( nnk < MIN_RUN ){
         ERROR_message(
           "run #%d has only %d points after censoring (need at least %d)",
           tt+1,nnk,MIN_RUN) ;
         nerr++ ;
       }
     }
     if( nerr > 0 ) ERROR_exit("Cannot continue with such over-censoring!") ;
   }

   /*-- make list of time indexes to keep --*/

STATUS("making list of keepers") ;
   keep = (int *)malloc( sizeof(int) * ntkeep ) ;
   for( qq=jj=0 ; jj < nt ; jj++ ) if( tp->censar[jj] != 0.0f ) keep[qq++] = jj ;

   (void)mcw_malloc_status("3dTproject.c",__LINE__) ;

   INFO_message("Setting up regressors") ;

   /*----- make stopband frequency mask, count number of stopband regressors -----*/

   if( tp->nstopband > 0 ){
     int ib , jbot,jtop , ntt , nbb ; float fbot,ftop , dff,qtop ;

STATUS("making stopband frequency mask") ;

     fmask = (int **) malloc(sizeof(int *)*nbl) ;
     nf    = (int *)  malloc(sizeof(int)  *nbl) ;
     df    = (float *)malloc(sizeof(float)*nbl) ;

     INFO_message("setting up stopband frequency mask") ;

     for( tt=0 ; tt < nbl ; tt++ ){    /* each run (block) is filtered separately */
       ntt = blb[tt] - bla[tt] + 1 ;
       dff = (tp->dt > 0.0f ) ? tp->dt : DSET_TR(tp->inset) ;
       if( dff <= 0.0f ) dff = 1.0f ;
       df[tt] = 1.0f / (ntt*dff) ; dff = 0.1666666f * df[tt] ;

       if( TPR_verb > 1 ) ININFO_message("run #%d has %d time points",tt,ntt) ;

       nf[tt] = ntt/2 ; fmask[tt] = (int *)malloc(sizeof(int)*(nf[tt]+2)) ;
       for( jj=0 ; jj <= nf[tt] ; jj++ ) fmask[tt][jj] = 0 ;
       qtop = (nf[tt]+0.1f)*df[tt] ;

       /* mark the frequencies to regress out */
STATUS("marking frequencies to delete") ;

       for( ib=0 ; ib < tp->nstopband ; ib++ ){
         fbot = tp->stopband[ib].a; if(fbot<0.0f) fbot=0.0f; else if(fbot>qtop) fbot=qtop;
         ftop = tp->stopband[ib].b; if(ftop<0.0f) ftop=0.0f; else if(ftop>qtop) ftop=qtop;
         jbot = (int)rintf((fbot+dff)/df[tt]); if( jbot < 0      ) jbot = 0     ;
         jtop = (int)rintf((ftop-dff)/df[tt]); if( jtop > nf[tt] ) jtop = nf[tt];
         if( TPR_verb > 1 )
           ININFO_message(" stopband %.4f..%.4f = indexes %d..%d = %.4f..%.4f",
                          fbot,ftop,jbot,jtop,jbot*df[tt],jtop*df[tt]) ;
         for( jj=jbot ; jj <= jtop ; jj++ ) fmask[tt][jj] = 1 ;
       }
       if( fmask[tt][0] ){                    /* always do freq=0 via polort */
         if( tp->polort < 0 ) tp->polort = 0 ;
         fmask[tt][0] = 0 ;
       }

       /* count the stopband regressors */

STATUS("counting stopband regressors") ;
       nbb = 0 ;
       for( jj=1 ; jj < nf[tt] ; jj++ ){
         if( fmask[tt][jj] ){ nort_fixed += 2 ; nbb += 2 ; }
       }

       /* even nt ==> top is Nyquist frequency (cosine only) */

       if( fmask[tt][nf[tt]] ){
         jj = (ntt%2==0) ? 1 : 2 ;
         nort_fixed += jj ; nbb += jj ;
       }

       ININFO_message("Block #%d: %d time points -- %d stopband regressors",
                      tt , ntt , nbb ) ;
#if 0
fprintf(stderr,"fmask: ") ;
for( jj=0 ; jj <= nf[tt] ; jj++ ) fprintf(stderr," %d",fmask[tt][jj]) ;
fprintf(stderr,"\n") ;
#endif
     }

     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;

     if( nort_fixed >= nt ){
       ERROR_message(
         "bandpass and/or stopbands ==> %d stopband regressors == too many for %d time points!",
         nort_fixed , nt ) ;
       nbad++ ;
     }
   }

   /*----- check for various other errors:
           ortar vectors the wrong length
           dsortar time series the wrong length
           just too many orts for the data length -----*/

   /*-- count polort regressors (N.B.: freq=0 and const polynomial don't BOTH occur) */

   if( tp->polort >= 0 ){
STATUS("counting polort regressors") ;
     nort_fixed += (tp->polort+1) * nbl ;  /* one set for each run */
     INFO_message("%d Blocks * %d polynomials -- %d polort regressors",
                  nbl , tp->polort+1 , (tp->polort+1)*nbl ) ;
   }

   /*-- check ortar for good-ositiness --*/

   if( tp->ortar != NULL ){
     MRI_IMAGE *qim ; int nbb=0 ;
STATUS("checking ortar for goodness") ;
     for( qq=0 ; qq < IMARR_COUNT(tp->ortar) ; qq++ ){
       qim = IMARR_SUBIM(tp->ortar,qq) ;
       nort_fixed += qim->ny ; nbb += qim->ny ;
       if( qim->nx != nt ){
         ERROR_message("-ort file #%d (%s) is %d long, but input dataset is %d",
                       qq+1 , qim->fname , qim->nx , nt ) ;
         nbad++ ;
       }
     }
     ININFO_message("-- %d other fixed ort regressors",nbb) ;
   }

   if( nort_fixed >= ntkeep ){
     ERROR_message(
       "total number of fixed regressors (%d) is too many for %d retained time points!",
       nort_fixed , ntkeep ) ;
     nbad++ ;
   }

   /*-- check dsortar for reasonabilitiness --*/

   if( tp->dsortar != NULL ){
STATUS("checking dsortar for goodness") ;
     for( jj=0 ; jj < tp->dsortar->num ; jj++ ){
       if( DSET_NVALS(tp->dsortar->ar[jj]) != nt ){
         ERROR_message("-dsort file #%d (%s) is %d long, but input dataset is %d",
                       qq+1 , DSET_BRIKNAME(tp->dsortar->ar[jj]) ,
                              DSET_NVALS(tp->dsortar->ar[jj])    , nt ) ;
         nbad++ ;
       }
       if( !EQUIV_GRIDXYZ(tp->inset,tp->dsortar->ar[jj]) ){
         ERROR_message("-dsort file #%d (%s) grid does not match input dataset",
                       qq+1 , DSET_BRIKNAME(tp->dsortar->ar[jj]) ) ;
         nbad++ ;
       }
     }
     nort_dsort = nort_voxel = tp->dsortar->num ;
     ININFO_message("-- %d voxel-wise dsort regressors",nort_dsort) ;
   }

   if( nort_voxel > 0 && nort_fixed+nort_voxel >= ntkeep ){
     ERROR_message(
       "number of fixed + voxel-wise regressors (%d+%d=%d) is too many for %d retained time points!",
       nort_fixed,nort_voxel,nort_fixed+nort_voxel , nt ) ;
     nbad++ ;
   }

   /***** refuse to continue if fatal errors have been observed in the wild *****/

   if( nbad > 0 ) ERROR_exit("Cannot continue after above errors :-( :-( :-( !!") ;

   { int ndof = ntkeep-nort_fixed-nort_voxel ;
     INFO_message("%d retained time points MINUS %d regressors ==> %d D.O.F. left",
                  ntkeep , nort_fixed+nort_voxel , ndof ) ;
     if( ndof < 30 ){
       if( ndof > 19 )
         WARNING_message("Be careful when your data has so few D.O.F.!") ;
       else if( ndof > 9 )
         WARNING_message("Statistics using data with so few D.O.F. might be meretricious!") ;
       else
         WARNING_message("Statistics using data with so few D.O.F. will be meretricious!") ;
     }
   }

   /*----- make voxel mask, if present -----*/

   if( tp->maskset != NULL ){  /*** explicit mask ***/

STATUS("making explicit mask") ;

     vmask = THD_makemask( tp->maskset , 0 , 1.0f,0.0f ) ;
     DSET_unload(tp->maskset) ;
     if( vmask == NULL )
       ERROR_exit("Can't make mask from -mask dataset '%s'",DSET_BRIKNAME(tp->maskset)) ;
     nvmask = THD_countmask( DSET_NVOX(tp->inset) , vmask ) ;
     INFO_message("%d voxels in the spatial mask",nvmask) ;
     if( nvmask == 0 )
       ERROR_exit("Mask from -mask dataset %s has 0 voxels",DSET_BRIKNAME(tp->maskset)) ;

   } else if( tp->automask ){  /*** AUTO mask ***/

STATUS("making automask") ;
     vmask = THD_automask( tp->inset ) ;
     if( vmask == NULL )
       ERROR_exit("Can't mask automask for some unknown reason :-( !!") ;
     nvmask = THD_countmask( DSET_NVOX(tp->inset) , vmask ) ;
     INFO_message("%d voxels in the spatial automask",nvmask) ;
     if( nvmask == 0 )
       ERROR_exit("automask from input dataset %s has 0 voxels",DSET_BRIKNAME(tp->inset)) ;

   } else {   /*** all voxels */

STATUS("making all-voxels mask") ;
     vmask = (byte *)malloc(sizeof(byte)*(nvox+2)) ; nvmask = nvox ;
     for( jj=0 ; jj < nvox ; jj++ ) vmask[jj] = 1 ;
     INFO_message("no -mask option ==> processing all %d voxels in dataset",nvox) ;

   }

   /*----- create array to hold all fixed orts (at this point, un-censored) -----*/

   if( nort_fixed > 0 )
     ort_fixed_unc = (float *)malloc( sizeof(float) * (nt * nort_fixed + 2 ) ) ;
     for( jj=0 ; jj < nt*nort_fixed ; jj++ ) ort_fixed_unc[jj] = 0.0f ;

   /*-- load the polort part of ort_fixed_unc ;
        note that the all 1s regressor will be first among equals --*/

   qort = 0 ;
   if( tp->polort >= 0 ){
     int ntt ; double fac ; float *opp ; int pp ;
STATUS("loading polort vectors") ;
     for( tt=0 ; tt < nbl ; tt++ ){
       ntt = blb[tt] - bla[tt] + 1 ;
       fac = 2.0/(ntt-1.0) ;
       for( pp=0 ; pp <= tp->polort ; pp++ ){
         opp = ort_fixed_unc + qort*nt ; qort++ ;
         for( jj=bla[tt] ; jj <= blb[tt] ; jj++ )
           opp[jj] = Plegendre( fac*(jj-bla[tt])-1.0 , pp ) ;
       }
     }
     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;
   }

   /*-- load cosine/sine (stopbands) part of ort_fixed_unc --*/

   if( fmask != NULL ){
     int pp , ntt ; float *opp , fq ;
STATUS("loading cos/sin vectors") ;
     for( tt=0 ; tt < nbl ; tt++ ){
       ntt = blb[tt] - bla[tt] + 1 ;
       for( pp=1 ; pp <= nf[tt] ; pp++ ){
         if( fmask[tt][pp] == 0 ) continue ;              /** keep this frequency! **/
STATUSi("cos qort",qort) ;
         opp = ort_fixed_unc + qort*nt ; qort++ ;
         fq = (2.0f * PI * pp) / (float)ntt ;
         for( jj=bla[tt] ; jj <= blb[tt] ; jj++ ) opp[jj] = cosf(fq*(jj-bla[tt])) ;
         if( pp < nf[tt] || ntt%2==1 ){  /* skip the Nyquist freq for sin() */
STATUSi("sin qort",qort) ;
           opp = ort_fixed_unc + qort*nt ; qort++ ;
           for( jj=bla[tt] ; jj <= blb[tt] ; jj++ ) opp[jj] = sinf(fq*(jj-bla[tt])) ;
         }
       }
       free(fmask[tt]) ;
     }
     free(fmask) ; free(nf) ; free(df) ;
     fmask = NULL ; nf = NULL ; df = NULL ;
     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;
   }

   /*-- load ortar part of ort_fixed_unc --*/

   if( tp->ortar != NULL ){
     MRI_IMAGE *qim ; float *qar , *opp , *qpp ; int pp ;
STATUS("loading ortar") ;
     for( qq=0 ; qq < IMARR_COUNT(tp->ortar) ; qq++ ){
       qim = IMARR_SUBIM(tp->ortar,qq) ; qar = MRI_FLOAT_PTR(qim) ;
       for( pp=0 ; pp < qim->ny ; pp++ ){
         qpp = qar + pp*qim->nx ;
         opp = ort_fixed_unc + qort*nt ; qort++ ;
         for( jj=0 ; jj < nt ; jj++ ) opp[jj] = qpp[jj] ;
         vector_demean( nt , opp ) ;
       }
     }
     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;
   }

   /*---- censor ort_fixed_unc into ort_fixed ----*/

   nort_fixed = qort ;

   if( ntuse == nt ){
     ort_fixed = ort_fixed_unc ; ort_fixed_unc = NULL ;   /* no censoring */
   } else {
     int pp,qort=0 ; float *opp , *upp ;
STATUS("censoring orts") ;
     ort_fixed = (float *)malloc(sizeof(float)*(ntkeep*nort_fixed+2)) ;
     for( pp=0 ; pp < nort_fixed ; pp++ ){
       upp = ort_fixed_unc + pp*nt ;
       opp = ort_fixed     + qort*ntkeep ; qort++ ;
       for( jj=0 ; jj < ntkeep ; jj++ ) opp[jj] = upp[keep[jj]] ;
       if( is_vector_zero(ntkeep,opp) ) qort-- ;
     }
     if( qort < nort_fixed ){  /* it might have shrunk above */
       INFO_message(
         "%d fixed ort vectors (out of %d original) discarded as all zero, after censoring",
         nort_fixed-qort,nort_fixed) ;
       nort_fixed = qort ;
     }
     if( nort_fixed == 0 )
       ERROR_exit("Censoring results in no nonzero fixed orts!") ;
     free(ort_fixed_unc) ; ort_fixed_unc = NULL ;
     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;
   }

   if( TPR_verb > 1 && nort_fixed > 0 ){
     MRI_IMAGE *qim = mri_new_vol_empty(ntuse,nort_fixed,1,MRI_float) ;
     char fname[256] ;
     mri_fix_data_pointer(ort_fixed,qim) ;
     sprintf(fname,"%s.ort.1D",tp->prefix) ;
     mri_write_1D(fname,qim) ; mri_clear_and_free(qim) ;
   }

   /*-- pseudo-inverse of the fixed orts --*/

   if( nort_fixed > 0 ){
     MRI_IMAGE *qim ; char fname[256] ;
STATUS("pseudo-inverse of fixed orts") ;
     if( tp->verb ) INFO_message("Compute pseudo-inverse of fixed orts") ;
     ort_fixed_psinv = (float *)malloc(sizeof(float)*(ntuse*nort_fixed+2)) ;
     TPR_prefix = tp->prefix ;
     compute_psinv( ntuse, nort_fixed, ort_fixed, ort_fixed_psinv, NULL ) ;
     TPR_prefix = NULL ;
     if( TPR_verb > 1 ){
       MRI_IMAGE *qim ; char fname[256] ;
       qim = mri_new_vol_empty(ntuse,nort_fixed,1,MRI_float) ;
       mri_fix_data_pointer(ort_fixed_psinv,qim) ;
       sprintf(fname,"%s.ort_psinv.1D",tp->prefix) ;
       mri_write_1D(fname,qim) ; mri_clear_and_free(qim) ;
     }
     (void)mcw_malloc_status("3dTproject.c",__LINE__) ;
   } else {
     ort_fixed_psinv = NULL ;
   }

   /*----- make vector images from all input datasets -----*/

   if( tp->verb ) INFO_message("Loading dataset%s",(tp->dsortar != NULL) ? "s" : "\0") ;
   if( ntuse == nt ){
STATUS("loading uncensored input datasets") ;
     inset_mrv = THD_dset_to_vectim( tp->inset , vmask , 0 ) ;
     if( ntkeep < ntuse ){               /* do the NTRP stuff now [06 Dec 2013] */
       for( jj=0 ; jj < inset_mrv->nvec ; jj++ )
         FILLIN_censored_time_series( nt,ntkeep,keep, VECTIM_PTR(inset_mrv,jj) ) ;
     }
   } else {
STATUS("loading censored datasets") ;
     inset_mrv = THD_dset_censored_to_vectim( tp->inset, vmask, ntkeep, keep ) ;
   }
   DSET_unload(tp->inset) ;
   THD_check_vectim(inset_mrv,"3dTproject input data") ;

   if( tp->dsortar != NULL ){
STATUS("loading dsortar") ;
     dsort_mrv = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*nort_dsort) ;
     for( jj=0 ; jj < nort_dsort ; jj++ ){
       if( ntuse == nt )
         dsort_mrv[jj] = THD_dset_to_vectim( tp->dsortar->ar[jj] , vmask , 0 ) ;
       else
         dsort_mrv[jj] = THD_dset_censored_to_vectim( tp->dsortar->ar[jj] ,
                                                      vmask, ntkeep, keep ) ;
       DSET_unload(tp->dsortar->ar[jj]) ;
       THD_vectim_applyfunc( dsort_mrv[jj] , vector_demean ) ;
       THD_check_vectim(dsort_mrv[jj],"3dTproject dsort data") ;
     }
   }

   /*----- do the actual work: filter time series !! -----*/

STATUS("start projection work") ;
   if( tp->verb ) INFO_message("Starting project-orization") ;

AFNI_OMP_START ;
#pragma omp parallel
{  int vv , kk , nds=nort_dsort+1 ;
   double *wsp ; float *dsar , *zar , *pdar ;

#pragma omp critical
   { wsp  = (double *)get_psinv_wsp( ntuse , nds , ntuse*2 ) ;
     dsar = (float *)malloc(sizeof(float)*(nds*ntuse+2)) ;
     pdar = (float *)malloc(sizeof(float)*(nds*ntuse+2)) ;
   }

#pragma omp for
   for( vv=0 ; vv < nvmask ; vv++ ){

     zar = VECTIM_PTR(inset_mrv,vv) ;   /* input data vector to be filtered */

     if( is_vector_zero(ntuse,zar) ) continue ;       /* skip all zero data */

     if( nort_dsort > 0 ){             /* collect the voxel-wise regressors */
       float *dar , *ear ;
       for( kk=0 ; kk < nort_dsort ; kk++ ){
         dar = VECTIM_PTR(dsort_mrv[kk],vv) ; ear = dsar+kk*ntuse ;
         AAmemcpy( ear , dar , sizeof(float)*ntuse ) ;
       }
     }

     project_out_twice( ntuse ,
                        nort_fixed , ort_fixed , ort_fixed_psinv ,
                        nort_dsort , dsar      , pdar            ,
                                     zar       , (char *)wsp      ) ;
   }

#pragma omp critical
   { free(wsp) ; free(dsar) ; free(pdar) ; }
}
AFNI_OMP_END ;

STATUS("projections done") ;

   /*-- get rid of some no-longer-needed stuff here --*/

   if( nort_dsort > 0 ){
STATUS("freeing dsortar data") ;
     for( jj=0 ; jj < nort_dsort ; jj++ ) VECTIM_destroy(dsort_mrv[jj]) ;
     free(dsort_mrv) ; dsort_mrv = NULL ;
   }
   free(vmask) ; vmask = NULL ;
   if( ort_fixed       != NULL ){ free(ort_fixed)       ; ort_fixed = NULL       ; }
   if( ort_fixed_psinv != NULL ){ free(ort_fixed_psinv) ; ort_fixed_psinv = NULL ; }

   /*----- blurring -----*/

STATUS("blurring?") ;

   MRILIB_verb = tp->verb ;
   mri_blur3D_vectim( inset_mrv , tp->blur ) ;

   /*----- norming -----*/

   if( tp->do_norm ){
STATUS("norming") ;
     if( tp->verb ) INFO_message("normalizing time series") ;
     THD_vectim_normalize(inset_mrv) ;
   }

   /*----- convert output time series into dataset -----*/

STATUS("convert results to output dataset") ;

   if( tp->verb ) INFO_message("Convert results to output dataset") ;

   tp->outset = EDIT_empty_copy( tp->inset ) ;
   nvout      = (tp->cenmode == CEN_KILL) ? ntkeep : nt ;

   EDIT_dset_items( tp->outset ,
                      ADN_prefix    , tp->prefix ,
                      ADN_nvals     , nvout      ,
                      ADN_ntt       , nvout      ,
                      ADN_brick_fac , NULL       ,
                    ADN_none ) ;
   for( jj=0 ; jj < nvout ; jj++ )
     EDIT_substitute_brick( tp->outset , jj , MRI_float , NULL ) ;

   if( tp->cenmode == CEN_ZERO )
     THD_vectim_to_dset_indexed( inset_mrv , tp->outset , keep ) ;
   else
     THD_vectim_to_dset        ( inset_mrv , tp->outset ) ;

   /*----- clean up whatever trash is still left -----*/

STATUS("all the work is done -- pour the Chablis") ;

   free(keep) ; VECTIM_destroy(inset_mrv) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ct , nact=0 ;
   int nbl=0 , *blist=NULL , tt , noblock=0 ;

   /*----------*/

   AFNI_SETUP_OMP(0) ;
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 )
     TPR_help_the_pitiful_user() ;

   /*----- bureaucracy -----*/

   mainENTRY("3dTproject"); machdep();
   AFNI_logger("3dTproject",argc,argv);
   PRINT_VERSION("3dTproject"); AUTHOR("Cox the Algebraic (Linear)") ;
   ct = NI_clock_time() ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*----- scan options -----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       tinp->verb = 0 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       tinp->verb++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-noblock") == 0 ){
       noblock++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-concat") == 0 ){
       MRI_IMAGE *cim ; float *car ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( nbl    > 0     ) ERROR_exit("You can't use '-concat' twice!") ;
       cim = mri_read_1D( argv[iarg] ) ;
       if( cim == NULL ) ERROR_exit("Can't read from -concat file '%s'",argv[iarg]) ;
       nbl = cim->nvox ;
       if( nbl <= 1 ){
         WARNING_message("-concat file '%s' has only 1 entry ==> ignoring this file",argv[iarg]) ;
         nbl = 0 ; blist = NULL ; mri_free(cim) ;
       } else {
         car   = MRI_FLOAT_PTR(cim) ;
         blist = (int *)malloc(sizeof(int)*(nbl+1)) ;
         for( tt=0 ; tt < nbl ; tt++ ) blist[tt] = (int)rintf(car[tt]) ;
         mri_free(cim) ;
         if( blist[0] != 0 ){
           WARNING_message("-concat file '%s' has first entry = %d ==> changing it to 0",
                           argv[iarg] , blist[0] ) ;
           blist[0] = 0 ;
         }
         for( tt=1 ; tt < nbl ; tt++ ){
           if( blist[tt] <= blist[tt-1] ){
             WARNING_message("-concat file '%s' has entries out of order ==> ignoring this file",argv[iarg]) ;
             nbl = 0 ; free(blist) ; blist = NULL ; break ;
           }
         }
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       char * dname;
       int    nname;
       if( tinp->inset != NULL )
          ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg      >= argc )
          ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;

       /* allow for multiple dataset names   12 May, 2014 [rickr] */
       for( nname=0; iarg+nname < argc && argv[iarg+nname][0] != '-'; nname++)
          ;
       dname = cat_strings(argv+iarg, nname, " ");
       if( ! dname )
          ERROR_exit("cannot cat %d dset names, starting with %d, '%s'",
                     nname, iarg, argv[iarg]);

       if( nname > 1 ) INFO_message("have %d input dataset names", nname);

       tinp->inset = THD_open_dataset(dname);
       CHECK_OPEN_ERROR(tinp->inset,dname);
       free(dname);
       iarg+=nname ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( tinp->maskset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg        >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( strcasecmp(argv[iarg],"AUTO") == 0 ){
         tinp->automask = 1 ;
       } else {
         tinp->maskset = THD_open_dataset(argv[iarg]) ;
         CHECK_OPEN_ERROR(tinp->maskset,argv[iarg]) ;
       }
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-automask") == 0 ){
       tinp->automask = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->polort = (int)strtod(argv[iarg],NULL) ;
       if( tinp->polort > 20 ) ERROR_exit("-polort value can't be over 20 :-(") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-norm") == 0 ){
       tinp->do_norm = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-ort") == 0 ){
       MRI_IMAGE *oim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->ortar == NULL ) INIT_IMARR(tinp->ortar) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         oim = mri_read_1D( argv[iarg] ) ;
         if( oim == NULL ) ERROR_exit("-ort: can't read file '%s'",argv[iarg]) ;
         ADDTO_IMARR(tinp->ortar,oim) ;
       }
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-dsort") == 0 ){
       THD_3dim_dataset *dsim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->dsortar == NULL ) INIT_3DARR(tinp->dsortar) ;
       dsim = THD_open_dataset(argv[iarg]) ; CHECK_OPEN_ERROR(dsim,argv[iarg]) ;
       ADDTO_3DARR(tinp->dsortar,dsim) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-stopband") == 0 ){
       float sbot,stop ;
       if( ++iarg >= argc-1 ) ERROR_exit("Need 2 values after option '%s'",argv[iarg-1]) ;
       sbot = (float)strtod(argv[iarg++],NULL) ;
       stop = (float)strtod(argv[iarg++],NULL) ;
       if( sbot <  0.0f ) sbot = 0.0f ;
       if( stop <= sbot ) ERROR_exit("-stopband: range %.5f %.5f is illegal :-(",sbot,stop) ;
       ADD_STOPBAND(tinp,sbot,stop) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-passband") == 0 ||
         strcasecmp(argv[iarg],"-bandpass") == 0   ){
       float fbot,ftop ;
       if( ++iarg >= argc-1 ) ERROR_exit("Need 2 values after option '%s'",argv[iarg-1]) ;
       fbot = (float)strtod(argv[iarg++],NULL) ;
       ftop = (float)strtod(argv[iarg++],NULL) ;
       if( fbot <  0.0f ) fbot = 0.0f ;
       if( ftop <= fbot ) ERROR_exit("-stopband: range %.5f %.5f is illegal :-(",fbot,ftop) ;
       ADD_STOPBAND(tinp,0.0f,fbot-0.0001f) ;
       ADD_STOPBAND(tinp,ftop+0.0001f,999999.9f) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-dt") == 0 || strcasecmp(argv[iarg],"-TR") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->dt = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-blur") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->blur = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(tinp->prefix) )
         ERROR_exit("-prefix '%s' is not acceptable :-(",tinp->prefix) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-cenmode") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
            if( strcasecmp(argv[iarg],"KILL") == 0 ) tinp->cenmode = CEN_KILL ;
       else if( strcasecmp(argv[iarg],"ZERO") == 0 ) tinp->cenmode = CEN_ZERO ;
       else if( strcasecmp(argv[iarg],"NTRP") == 0 ) tinp->cenmode = CEN_NTRP ;
       else ERROR_message("unknown value '%s' after -cenmode",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-censor") == 0 ){
       MRI_IMAGE *cenim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->censar != NULL ) ERROR_exit("Can't use -censor more than once!") ;
       cenim = mri_read_1D(argv[iarg]) ;
       if( cenim == NULL ) ERROR_exit("-censor can't read file '%s'",argv[iarg]) ;
       tinp->censar = MRI_FLOAT_PTR(cenim) ; tinp->ncensar = cenim->nx ;
       mri_clear_and_free(cenim) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-censorTR") == 0 ){
        NI_str_array *nsar ;
        char *src=malloc(4), *cpt, *dpt ;
        int ns, r,a,b, nerr=0 ; int_triple rab ;

        *src = '\0' ;   /* cat all following options until starts with '-' */
        for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
          ns = strlen(argv[iarg]) ; if( ns == 0 ) continue ;
          src = realloc(src,strlen(src)+ns+4) ;
          strcat(src," ") ; strcat(src,argv[iarg]) ;
        }
        if( *src == '\0' ) ERROR_exit("Bad (or no) argument after -CENSORTR") ;
        nsar = NI_decode_string_list( src , "," ) ; /* break into substrings */
        for( ns=0 ; ns < nsar->num ; ns++ ){ /* loop over substrings */
          cpt = nsar->str[ns] ; dpt = strchr(cpt,':') ; r = 0 ;
          if( *cpt == '\0' ) continue ;   /* skip an empty string */
          if( dpt != NULL ){              /* found 'run:' */
            if( *cpt == '*' ){ /* wildcard = all runs */
              r = -666 ;
            } else {
              r = (int)strtol(cpt,NULL,10) ;
              if( r <= 0 ){  /* skip out */
                ERROR_message("-CENSORTR %s -- run index '%d' is bad! [iarg=%d]",nsar->str[ns],r,iarg);
                nerr++ ; continue ;
              }
            }
            cpt = dpt+1 ;  /* skip to character after ':' */
            if( *cpt == '\0' ){  /* skip out */
              ERROR_message("-CENSORTR %s -- no data after run index! [iarg=%d]",nsar->str[ns],iarg);
              nerr++ ; continue ;
            }
          }
          a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
          if( a < 0 ){  /* skip out */
            ERROR_message("-CENSORTR %s -- time index '%d' is bad! [iarg=%d]",nsar->str[ns],a,iarg);
            nerr++ ; continue ;
          }
          if( *dpt == '\0' ){  /* no second number */
            b = a ;
          } else {             /* get second number */
            for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
            b = (int)strtol(dpt,NULL,10) ;
            if( b < a || b < 0 ){  /* skip out */
              ERROR_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad! [iarg=%d]",
                            nsar->str[ns],a,b,iarg);
              nerr++ ; continue ;
            }
          }
          if( r > 1 ){
            WARNING_message("-CENSORTR '%s' has run number > 1 ==> ignoring it",nsar->str[ns]) ;
          } else {
            tinp->abc_CENSOR = (int_triple *)realloc( tinp->abc_CENSOR ,
                                                sizeof(int_triple)*(tinp->num_CENSOR+1) );
            rab.i = r; rab.j = a; rab.k = b; tinp->abc_CENSOR[tinp->num_CENSOR++] = rab ;
          }
        } /* end of loop over -CENSORTR strings */
        if( nerr > 0 ) ERROR_exit("Can't proceed after -CENSORTR errors! [iarg=%d]",iarg) ;
        NI_delete_str_array(nsar) ; free(src) ;
        continue ;  /* next option */
     }

     /*--- error! ---*/

     ERROR_exit("Don't know what to do with option '%s' :-(",argv[iarg]) ;

   } /* end of option scanning loop */

   /*----- error checking -----*/

   if( tinp->inset == NULL )
     ERROR_exit("no input dataset?") ;

   if( DSET_NVALS(tinp->inset) < MIN_RUN )
     ERROR_exit("input dataset has fewer than %d time points?",MIN_RUN) ;

   if( tinp->maskset != NULL && !EQUIV_GRIDXYZ(tinp->inset,tinp->maskset) )
     ERROR_exit("mask and input datasets are NOT on the same 3D grid -- do you need 3dresample?") ;

   DSET_load(tinp->inset) ; CHECK_LOAD_ERROR(tinp->inset) ;
   if( tinp->maskset != NULL ){
     DSET_load(tinp->maskset) ; CHECK_LOAD_ERROR(tinp->maskset) ;
   }

   /* auto-catenated dataset ==> build blist */

   if( DSET_IS_TCAT(tinp->inset) ){
     int nerr=0 ;
     if( nbl > 0 ){
       WARNING_message("-concat option is ignored, since input dataset was automatically catenated") ;
       free(blist) ; blist = NULL ; nbl = 0 ;
     }
     if( !noblock ){
       nbl      = tinp->inset->tcat_num ;
       blist    = (int *)malloc(sizeof(int)*(nbl+1)) ;
       blist[0] = 0 ;
       for( tt=1 ; tt < nbl ; tt++ ){
         if( tinp->inset->tcat_len[tt-1] < MIN_RUN ) nerr++ ;
         blist[tt] = blist[tt-1] + tinp->inset->tcat_len[tt-1] ;
       }
       if( nerr > 0 ) ERROR_exit("Auto-catenated dataset has runs with length < %d",MIN_RUN) ;
       INFO_message("Auto-catenated dataset has %d runs",nbl) ;
     } else {
       INFO_message("Auto-catenated dataset treated as 1 run instead of %d",tinp->inset->tcat_num) ;
     }
   }

   if( nbl == 0 ){
     nbl = 1 ; blist = (int *)malloc(sizeof(int)*2) ; blist[0] = 0 ;
   }

   /* load nbl and blist into tinp */

   tinp->nblock = nbl ;
   tinp->blbeg  = (int *)malloc(sizeof(int)*nbl) ;
   tinp->blend  = (int *)malloc(sizeof(int)*nbl) ;
   for( tt=0 ; tt < nbl ; tt++ ){
     tinp->blbeg[tt] = blist[tt] ;
     tinp->blend[tt] = (tt < nbl-1) ? blist[tt+1]-1 : DSET_NVALS(tinp->inset)-1 ;
   }

   /* check CENSOR command for run indexes: should all have them, or none */

   if( tinp->abc_CENSOR != NULL ){
     int ic , rr , nzr=0 ;
     for( ic=0 ; ic < tinp->num_CENSOR ; ic++ ) /* count number with run != 0 */
       if( tinp->abc_CENSOR[ic].i ) nzr++ ;
     if( nzr > 0 && nzr < tinp->num_CENSOR )
       WARNING_message(
         "%d -CENSORTR commands have 'run:' numbers and %d do not!\n"
         "   (either all should have 'run:' numbers or none)",nzr,tinp->num_CENSOR-nzr);
   }

   /* check to see how many 'actions' are requested */

   nact = 0 ;
   if( tinp->polort >= 0      ) nact++ ;
   if( tinp->do_norm          ) nact++ ;
   if( tinp->ortar != NULL    ) nact++ ;
   if( tinp->dsortar != NULL  ) nact++ ;
   if( tinp->nstopband > 0    ) nact++ ;
   if( tinp->blur      > 0.0f ) nact++ ;
   if( tinp->censar != NULL   ) nact++ ;
   if( tinp->num_CENSOR > 0   ) nact++ ;
   if( nact == 0 )
     ERROR_exit("Don't you want to DO something? Please read the help!") ;

   /*----- process the data -----*/

   TPR_verb = tinp->verb ;

   TPR_process_data( tinp ) ;

   /* if this program weren't about to end, we'd cleanup the contents of tinp */

   if( tinp->outset != NULL ){
     tross_Copy_History( tinp->inset , tinp->outset ) ;
     tross_Make_History( "3dTproject" , argc,argv , tinp->outset ) ;
     DSET_write(tinp->outset) ;
     if( tinp->verb ) WROTE_DSET(tinp->outset) ;
   } else {
     ERROR_exit("Processing the data failed for unknown reasons :(") ;
   }

   if( tinp->verb )
     INFO_message("===== clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}
