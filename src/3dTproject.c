#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*----------------------------------------------------------------------------*/

typedef struct {
   THD_3dim_dataset         *inset ;
   THD_3dim_dataset        *outset ;
   THD_3dim_dataset       *maskset ;
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
   int                        verb ;
} TPR_input ;

#define CEN_ZERO 1
#define CEN_KILL 2

/*----------------------------------------------------------------------------*/

static TPR_input tin = { NULL,NULL,NULL ,
                         2 , 0 , 0 ,
                         NULL,NULL,
                         NULL,0 , 0.0f ,
                         "Tproject" ,
                         0.0f ,
                         NULL,0, NULL,0,CEN_KILL ,
                         1 } ;

static TPR_input *tinp = &tin ;

/*----------------------------------------------------------------------------*/

#undef  ADD_STOPBAND
#define ADD_STOPBAND(bb,tt)                                                       \
 do{ tinp->stopband = (float_pair *)realloc(                                      \
                        tinp->stopband, sizeof(float_pair)*(tinp->nstopband+1)) ; \
     tinp->stopband[tinp->nstopband].a = (bb) ;                                   \
     tinp->stopband[tinp->nstopband].b = (tt) ; tinp->nstopband++ ;               \
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
   "                       ** The default mode is KILL !!!\n"
   "\n"
   " -ort f.1D           = Remove each column in f.1D\n"
   "                       ++ Multiple -ort options are allowed.\n"
   " -polort pp          = Remove polynomials up to and including degree pp.\n"
   "                       ++ Default value is 2.\n"
   "                       ++ It makes no sense to use a value of pp greater than\n"
   "                          2, if you are bandpassing out the lower frequences!\n"
   " -dsort fset         = Remove the 3D+time time series in dataset fset.\n"
   "                       ++ That is, 'fset' contains a different nuisance time\n"
   "                          series for each voxel (e.g., from AnatICOR).\n"
   "                       ++ Multiple -dsort options are allowed.\n"
#if 0
   "                       ++ These datasets are NOT despiked or blurred!\n"
#endif
   "\n"
   " -passband fbot ftop = Remove all frequences EXCEPT those in the range\n"
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
   "                       ++ Use '-mask AUTO' to have the program generate the\n"
   "                          mask automatically.\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
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
   "------\n"
   "NOTES:\n"
   "------\n"
   "* The input file is treated as one continuous imaging 'run'; no time\n"
   "   discontinuities (breaks) are allowed -- that is, you can't use a\n"
   "   '-concat' option.\n"
   "\n"
   "* Removal of the various undesired components is via linear regression.\n"
   "   In particular, this method allows for bandpassing of censored time\n"
   "   series.\n"
   "\n"
   "* If A is a matrix whose column comprise the vectors to be projected\n"
   "   out, define the projection matrix Q(A) by\n"
   "    Q(A) = I - A psinv(A)\n"
   "   where psinv(A) is the pseudo-inverse of A [e.g., inv(A'A)A'].\n"
   "\n"
   "* If you like technical math jargon (and who doesn't?), this program\n"
   "   performs orthogonal projection onto the null space of the set of 'ort'\n"
   "   vectors assembled from the various options '-polort', '-ort',\n"
   "   '-passband', '-stopband', and '-dsort'.\n"
   "\n"
   "* If A is a matrix whose column comprise the vectors to be projected\n"
   "   out, define the projection matrix Q(A) by\n"
   "    Q(A) = I - A psinv(A)\n"
   "   where psinv(A) is the pseudo-inverse of A [e.g., inv(A'A)A'].\n"
   "\n"
   "* If option '-dsort' is used, each voxel has a different matrix of\n"
   "   regressors -- encode this extra set of regressors in matrix B.\n"
   "   Then the projection for the compound matrix [A B] is\n"
   "      Q( Q(A)B ) Q(A) ;\n"
   "   that is, A is projected out of B, then the projector for that\n"
   "   reduced B is formed, and applied to the projector for the\n"
   "   voxel-independent A.  Since the number of columns in B is usually\n"
   "   many fewer than the number of columns in A, this technique can\n"
   "   be much faster than constructing the full Q([A B]) for each voxel.\n"
  "\n"
#ifdef USE_OMP
   "* This version of the program is compiled using OpenMP for speed.\n"
#else
   "* This version of the program is not compiled with OpenMP, but OpenMP\n"
   "   binaries DO exist, and using OpenMP will speed the program up.\n\n"
#endif
  "\n"
   "* Authored by RWCox in a fit of excessive linear algebra [summer 2013].\n"
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

#if 0
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */
   vmat = (double *)calloc( sizeof(double),n*n );
   umat = (double *)calloc( sizeof(double),m*n );   /* left singular vectors */
   sval = (double *)calloc( sizeof(double),n   );   /* singular values */
#endif

   if( extra < 0 ) extra = 0 ;
   wsp = (char *)malloc( sizeof(double) * (2*m*n + 2*n + n*n + extra) ) ;
   return wsp ;
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

#undef  PSINV_EPS
#define PSINV_EPS 1.e-12

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
#pragma omp critical (STDERR)
     { if( first ) ERROR_message("SVD fails in compute_psinv()!\n"); }
     AAmemset( pmat , 0 , sizeof(float)*m*n ) ; first = 0 ;
     if( wpp != wsp ) free(wpp) ;
     return ;
   }

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

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
/* Process all the data */

void TPR_process_data( TPR_input *tp )
{
   int nt,nte,ntkeep , nort_fixed=0 , nort_voxel=0 , nbad=0 , qq,jj,qort ;
   int *fmask=NULL , nf=0 ; float df ;
   byte *vmask=NULL , nvmask=0 , nvox , *vlist=NULL ;
   float *ort_fixed , *ort_voxel , *ort_fixed_psinv ;
   int *keep=NULL ;
   MRI_vectim *inset_mrv , **dsort_mrv=NULL ; int nort_dsort=0 ;

   nt = DSET_NVALS(tp->inset) ; nte = ((nt%2)==0) ;  /* nte = even-ness of nt */

   nvox = DSET_NVOX(tp->inset) ;

   /*----- make censor array -----*/

   if( tp->censar != NULL){
     if( tp->ncensar < nt ){
       ERROR_message("-censor file is too short (%d) for dataset (%d)",tp->ncensar,nt) ;
       nbad++ ;
     } else if( tp->ncensar > nt ){
       WARNING_message("-censort file is too long (%d) for dataset (%d)",tp->ncensar,nt) ;
     }
   } else {
     tp->censar = (float *)malloc(sizeof(float)*nt) ;
     for( jj=0 ; jj < nt ; jj++ ) tp->censar[jj] = 1.0f ;
   }

   /*-- apply any -CENSORTR commands --*/

   if( tp->num_CENSOR > 0 ){
     int cc , r,a,b ;
     for( cc=0 ; cc < tp->num_CENSOR ; cc++ ){
       r = tp->abc_CENSOR[cc].i; a = tp->abc_CENSOR[cc].j; b = tp->abc_CENSOR[cc].k;
       if( r > 1 ) continue ; /* should not happen */
       if( a < 0   ) a = 0 ;
       if( b >= nt ) b = nt-1 ;
       for( jj=a ; jj <= b ; jj++ ) tp->censar[jj] = 0.0f ;
     }
   }

   /*-- count number of time points that are kept (NOT censored) --*/

   for( jj=ntkeep=0 ; jj < nt ; jj++ ) if( tp->censar[jj] != 0.0f ) ntkeep++ ;

   if( tp->verb )
     INFO_message("input time points = %d ; censored = %d ; remaining = %d",
                  nt , nt-ntkeep , ntkeep ) ;

   if( ntkeep < 9 )
     ERROR_exit("only %d points left after censoring -- cannot continue",ntkeep) ;

   /*-- make list of time indexes to keep --*/

   keep = (int *)malloc( sizeof(int) * ntkeep ) ;
   for( qq=jj=0 ; jj < nt ; jj++ ) if( tp->censar[jj] != 0.0f ) keep[qq++] = jj ;

   /*----- make stopband frequency mask, count number of frequency regressors -----*/

   if( tp->nstopband > 0 ){
     int ib , jbot,jtop ; float fbot,ftop ;

     df = (tp->dt > 0.0f ) ? tp->dt : DSET_TR(tp->inset) ;
     if( df <= 0.0f ) df = 1.0f ;
     df = 1.0f / (nt*df) ;

     nf = nt/2 ; fmask = (int *)calloc(sizeof(int),(nf+1)) ;

     for( ib=0 ; ib < tp->nstopband ; ib++ ){
       fbot = tp->stopband[ib].a ;
       ftop = tp->stopband[ib].b ;
       jbot = (int)rintf(fbot/df); if( jbot < 0  ) jbot = 0 ;
       jtop = (int)rintf(ftop/df); if( jbot > nf ) jbot = nf;
       for( jj=jbot ; jj <= jtop ; jj++ ) fmask[jj] = 1 ;
     }
     if( fmask[0] ){                    /* always do freq=0 via polort */
       if( tp->polort < 0 ) tp->polort = 0 ;
       fmask[0] = 0 ;
     }

     for( jj=1 ; jj < nf ; jj++ ) if( fmask[jj] ) nort_fixed += 2 ;
     if( fmask[nf] ) nort_fixed += (nte ? 1 : 2) ;

     if( nort_fixed >= nt ){
       ERROR_message(
         "bandpass / stopbands ==> %d frequency regressors -- too many for %d time points!",
         nort_fixed , nt ) ;
       nbad++ ;
     }
   }

   /*----- check for various errors:
           ortar vectors too short
           dsortar time series too short
           just too many orts for the data -----*/

   /*-- allow for polort (N.B.: freq=0 and const polynomial don't BOTH occur) */

   if( tp->polort >= 0 ) nort_fixed += tp->polort+1 ;

   /*-- check ortar --*/

   if( tp->ortar != NULL ){
     MRI_IMAGE *qim ;
     for( qq=0 ; qq < IMARR_COUNT(tp->ortar) ; qq++ ){
       qim = IMARR_SUBIM(tp->ortar,qq) ;
       nort_fixed += qim->ny ;
       if( qim->nx != nt ){
         ERROR_message("-ort file #%d (%s) is %d long, but dataset is %d",
                       qq+1 , qim->filename , qim->nx , nt ) ;
         nbad++ ;
       }
     }
   }

   if( nort_fixed >= ntkeep ){
     ERROR_message(
       "number of fixed regressors (%d) is too many for %d retained time points!",
       nort_fixed , nt ) ;
     nbad++ ;
   }

   /*-- check dsortar --*/

   if( tp->dsortar != NULL ){
     for( jj=0 ; jj < tp->dsortar->num ){
       if( DSET_NVALS(tp->dsortar->ar[jj]) != nt ){
         ERROR_message("-dsort file #%d (%s) is %d long, but dataset is %d",
                       qq+1 , DSET_BRIKNAME(tp->dsortar->ar[jj]) ,
                              DSET_NVALS(tp->dsortar->ar[jj])    , nt ) ;
         nbad++ ;
       }
     }
     nort_dsort = nort_voxel = tp->dsortar->num ;
   }

   if( nort_voxel > 0 && nort_fixed+nort_voxel >= ntkeep ){
     ERROR_message(
       "number of fixed + voxel-wise regressors (%d+%d=%d) is too many for %d retained time points!",
       nort_fixed,nort_voxel,nort_fixed+nort_voxel , nt ) ;
     nbad++ ;
   }

   /***** refuse to continue if fatal errors have been observed in the wild *****/

   if( nbad > 0 ) ERROR_exit("Cannot continue after above errors :-( :-( :-( !!") ;

   if( tp->verb )
     INFO_message("%d retained time points MINUS %d regressors ==> %d D.O.F. left",
                  ntkeep , nort_fixed+nort_voxel , ntkeep-nort_fixed-nort_voxel    ) ;

   /*----- make voxel mask, if present -----*/

   if( tp->maskset != NULL ){
     vmask = THD_make_mask( tp->maskset , 0 , 1.0f,0.0f ) ;
     DSET_unload(tp->maskset) ;
     if( vmask == NULL )
       ERROR_exit("Can't make mask from -mask dataset '%s'",DSET_BRIKNAME(tp->maskset) ;
     nvmask = THD_countmask( DSET_NVOX(tp->inset) , vmask ) ;
     if( tp->verb )
       INFO_message("%d voxels in the spatial mask",nvmask) ;
     if( nvmask == 0 )
       ERROR_exit("Mask from -mask dataset %s has 0 voxels",DSET_BRIKNAME(tp->maskset) ;
   } else {
     vmask = (byte *)malloc(sizeof(byte)*nvox) ; nvmask = nvox ;
     for( jj=0 ; jj < nvox ; jj++ ) vmask[jj] = 1 ;
     if( tp->verb )
       INFO_message("no -mask option ==> processing all %d voxels in dataset",nvox) ;
   }

   /*-- make list of voxels to filter --*/

   vlist = (int *)malloc(sizeof(int)*nvmask) ;
   for( jj=qq=0 ; jj < nvox ; jj++ ) if( vmask[jj] ) vlist[qq++] = jj ;

   /*----- create array of all fixed orts -----*/

   ort_fixed = (float *)malloc( sizeof(float) * ntkeep * nort_fixed ) ;

   /*-- polort part of ort_fixed --*/

   qort = 0 ;
   if( tp->polort >= 0 ){
     double fac=2.0/(nt-1.0) ; float *opp , sum ; int pp ;
     for( pp=0 ; pp <= tp->polort ; pp++ ){
       opp = ort_fixed + qort*ntkeep ; qort++ ;
       for( sum=0.0f,jj=0 ; jj < ntkeep ; jj++ ){
         opp[jj] = Plegendre(fac*keep[jj]-1.0,pp) ; sum += fabsf(opp[jj]) ;
       }
       if( sum == 0.0f ){  /* should not happen */
         WARNING_message("polort #%d is all zero: skipping",pp) ; qort-- ;
       }
     }
   }

   /*-- cosine/sine (stopbands) part of ort_fixed --*/

   if( fmask != NULL ){
     int pp ; float *opp , fq , sum ;
     for( pp=1 ; pp <= nf ; pp++ ){
       if( fmask[pp] == 0 ) continue ;
       opp = ort_fixed + qort*ntkeep ; qort++ ;
       fq = (2.0f * PI * pp) / (float)nt ;
       for( sum=0.0f,jj=0 ; jj < ntkeep ; jj++ ){
         opp[jj] = cosf(fq*keep[jj]) ; sum += fabsf(opp[jj]) ;
       }
       if( sum == 0.0f ){  /* should not happen */
         WARNING_message("cosine -ort #%d is all zero: skipping",pp) ; qort-- ;
       }
       if( pp < nf || nte == 0 ){  /* skip the Nyquist freq for sin() */
         opp = ort_fixed + qort*ntkeep ; qort++ ;
         for( sum=0.0f,jj=0 ; jj < ntkeep ; jj++ ){
           opp[jj] = sinf(fq*keep[jj]) ; sum += fabsf(opp[jj]) ;
         }
         if( sum == 0.0f ){  /* should not happen */
           WARNING_message("sine -ort #%d is all zero: skipping",pp) ; qort-- ;
         }
       }
     }
   }

   /*-- ortar part of ort_fixed --*/

   if( tp->ortar != NULL ){
     MRI_IMAGE *qim ; float *qar , *opp , *qpp , sum ; int pp ;
     for( qq=0 ; qq < IMARR_COUNT(tp->ortar) ; qq++ ){
       qim = IMARR_SUBIM(tp->ortar,qq) ; qar = MRI_FLOAT_PTR(qim) ;
       for( pp=0 ; pp < qim->ny ; pp++ ){
         qpp = qar + pp*qim->nx ;
         opp = ort_fixed + qort*ntkeep ; qort++ ;
         for( sum=0.0f,jj=0 ; jj < ntkeep ; jj++ ){
           opp[jj] = qpp[keep[jj]] ; sum += fabsf(opp[jj]) ;
         }
         if( sum == 0.0f ){
           WARNING_message("-ort #%d, column #%d is all zero: skipping",
                           qq+1 , pp ) ;
           qort-- ;
         }
       }
     }
     DESTROY_IMARR(tp->ortar) ; tp->ortar = NULL ;
   }

   nort_fixed = qort ;  /* in case it shrank above */

   /*-- pseudo-inverse of the fixed orts --*/

   if( nort_fixed > 0 ){
     ort_fixed_psinv = (float *)malloc(sizeof(float)*ntkeep*nort_fixed) ;
     compute_psinv( ntkeep, nort_fixed, ort_fixed, ort_fixed_psinv, NULL ) ;
   } else {
     ort_fixed_psinv = NULL ;
   }

   /*----- make vector images from all input datasets -----*/

   inset_mrv = THD_dset_censored_to_vectim( tp->inset, vmask, ntkeep, keep ) ;
   DSET_unload(tp->inset) ;

   if( tp->dsortar != NULL ){
     dsort_mrv = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*nort_dsort) ;
     for( jj=0 ; jj < nort_dsort ){
       dsort_mrv[jj] = THD_dset_censored_to_vectim( tp->dsortar->ar[jj] ,
                                                    vmask, ntkeep, keep ) ;
       DSET_unload(tp->dsortar->ar[jj]) ;
     }
   }

#if 0
   /*----- despike input dataset -----*/

   if( tp->do_despike ) (void)THD_vectim_despike9( inset_mrv ) ;
#endif

   /*----- filter time series -----*/

AFNI_OMP_START ;
#pragma omp parallel
{  int vv , kk , nds=nort_dsort+1 ;
   double *wsp ; float *dsar , *zar , *pdar ;

#pragma omp critical
   { wsp = (double *)get_psinv_wsp( ntkeep , nds , ntkeep*2 ) ;
     dsar = (float *)malloc(sizeof(float)*nds*ntkeep) ;
     pdar = (float *)malloc(sizeof(float)*nds*ntkeep) ; }

#pragma omp for
   for( vv=0 ; vv < nvmask ; vv++ ){
     zar = VECTIM_PTR(inset_mrv,vv) ;

     if( nort_dsort > 0 ){
       float *dar , *ear ;
       for( kk=0 ; kk < nort_dsort ; kk++ ){
         dar = VECTIM_PTR(dsort_mrv,vv) ; ear = dsar+kk*ntkeep ;
         AA_memcpy( ear , dar , sizeof(float)*ntkeep ) ;
       }
     }

     project_out_twice( ntkeep ,
                        nort_fixed , ort_fixed , ort_fixed_psinv ,
                        nort_dsort , dsar      , pdar            ,
   }

#pragma omp critical
   { free(wsp) ; free(dsar) ; free(pdar) ; }
}
AFNI_OMP_END ;

   /*----- blurring -----*/

   /*----- norming -----*/

   /*----- convert output time series into dataset -----*/

   /*----- clean up whatever trash is still left -----*/
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ct , nact=0 ;

   /*----------*/

   AFNI_SETUP_OMP(0) ;
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 )
     TPR_help_the_pitiful_user() ;

   /*----- scan options -----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       tinp->verb = 0 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( tinp->inset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg      >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->inset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(tinp->inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( tinp->maskset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg        >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->maskset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(tinp->maskset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->polort = (int)strtod(argv[iarg],NULL) ;
       if( tinp->polort > 20 ) ERROR_exit("-polort value can't be over 20 :-(") ;
       iarg++ ; continue ;
     }

#if 0
     /*-----*/

     if( strcasecmp(argv[iarg],"-despike") == 0 ){
       tinp->do_despike = 1 ; iarg++ ; continue ;
     }
#endif

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
       ADD_STOPBAND(sbot,stop) ;
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
       ADD_STOPBAND(0.0f,fbot-0.0001f) ;
       ADD_STOPBAND(ftop+0.0001f,666666.6f) ;
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
        char *src=malloc(1), *cpt, *dpt ;
        int ns, r,a,b, nerr=0 ; int_triple rab ;

        *src = '\0' ;   /* cat all following options until starts with '-' */
        for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
          ns = strlen(argv[iarg]) ; if( ns == 0 ) continue ;
          src = realloc(src,strlen(src)+ns+2) ;
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

   if( DSET_NVALS(tinp->inset) < 9 )
     ERROR_exit("input dataset has fewer than 9 time points?") ;

   if( tinp->maskset != NULL && !EQUIV_GRIDXYZ(tinp->inset,tinp->maskset) )
     ERROR_exit("mask and input datasets are NOT on the same 3D grid?") ;

   DSET_LOAD(tinp->inset) ; CHECK_LOAD_ERROR(tinp->inset) ;
   if( tinp->maskset != NULL ){
     DSET_LOAD(tinp->maskset) ; CHECK_LOAD_ERROR(tinp->maskset) ;
   }

   nact = 0 ;
   if( tinp->do_despike       ) nact++ ;
   if( tinp->polort >= 0      ) nact++ ;
   if( tinp->do_norm          ) nact++ ;
   if( tinp->ortar != NULL    ) nact++ ;
   if( tinp->dsortar != NULL  ) nact++ ;
   if( tinp->nstopband > 0    ) nact++ ;
   if( tinp->blur      > 0.0f ) nact++ ;
   if( tinp->censar != NULL   ) nact++ ;
   if( tinp->num_CENSOR > 0   ) nact++ ;
   if( nact == 0 )
     ERROR_exit("Don't you want to DO something?") ;

   /*----- bureaucracy -----*/

   mainENTRY("3dTproject"); machdep();
   AFNI_logger("3dTproject",argc,argv);
   PRINT_VERSION("3dTproject"); AUTHOR("Cox the Algebraic (Linear)") ;
   ct = NI_clock_time() ;

   /*----- process the data -----*/

   TPR_process_data( tinp ) ;

   if( tinp->outset != NULL ){
     tross_Copy_History( tinp->inset , tinp->outset ) ;
     tross_Make_History( "3dTproject" , argc,argv , tinp->outset ) ;
     DSET_write(tinp->outset) ;
     if( tinp->verb ) WROTE_DSET(tinp->outset) ;
   }

   exit(0) ;
}
