#include "mrilib.h"

#ifdef USE_OMP
#include "cs_symeig.c"
#endif

#undef MEMORY_CHECK
#if 0
#ifdef USING_MCW_MALLOC
# define MEMORY_CHECK(mm)                                               \
   do{ if( verb > 5 ) mcw_malloc_dump() ;                               \
       if( verb > 1 ){                                                  \
         long long nb = mcw_malloc_total() ;                            \
         if( nb > 0 ) INFO_message("Memory usage now = %s (%s): %s" ,   \
                      commaized_integer_string(nb) ,                    \
                      approximate_number_string((double)nb) , (mm) ) ;  \
         ININFO_message(" status = %s",mcw_malloc_status(NULL,0)) ;     \
       }                                                                \
   } while(0)
#endif
#endif

#ifndef MEMORY_CHECK
# define MEMORY_CHECK(mm) /*nada*/
#endif

static int   verb   = 1 ;
static char *prefix = "brainsync" ;
static char *matpre = NULL ;
static int do_joshi = 1 ;
static int do_norm  = 0 ;

static int ct ;
#define TIMER nice_time_string(NI_clock_time()-ct)

static THD_3dim_dataset *dsetB=NULL, *dsetC=NULL, *maskset=NULL, *outset=NULL ;

/*----------------------------------------------------------------------------*/

void TSY_help_the_pitiful_user(void)
{
  printf(
   "\n"
   "Usage:  3dBrainSync [options]\n"
   "\n"
   "This program 'synchronizes' the '-inset2' dataset to match the '-inset1'\n"
   "dataset, as much as possible. It uses the BrainSync algorithm of Joshi et al.\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -inset1 dataset     = Reference dataset\n"
   " -inset2 dataset     = Dataset to be matched to the reference dataset,\n"
   "                       as much as possible.\n"
   "                       ++ These 2 datasets must be on the same spatial grid,\n"
   "                          and must have the same number of time points!\n"
   "                       ++ These are MANDATORY 'options'.\n"
   "\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
   "                       This will be the -inset2 dataset transformed\n"
   "                       to be as correlated as possible (in time)\n"
   "                       with the -inset1 dataset.\n"
   "\n"
   " -normalize          = Normalize the output dataset so that each\n"
   "                       time series has sum-of-squares = 1.\n"
   "\n"
   " -mask mset          = Only operate on nonzero voxels in the mset dataset.\n"
   "                       ++ Voxels outside the mask will not be used in computing\n"
   "                          the transformation matrix Q, but WILL be transformed\n"
   "                          for your edification later.\n"
   "                       ++ For FMRI purposes, a gray matter mask would make\n"
   "                          sense here, or at least a brain mask.\n"
   "                       ++ If no masking option is given, then all voxels\n"
   "                          will be processed in computing Q.\n"
   "                       ++ Any voxel which is all constant in time\n"
   "                          (in either input) will be added to the mask.\n"
   "                       ++ This dataset must be on the same spatial grid\n"
   "                          as the other input datasets!\n"
   "\n"
   " -joshi              = Use the Joshi method, with an orthogonal matrix\n"
   " -orthogonal           for the transformation. [This is the default]\n"
   "                       ++ At present, there is no other method implemented,\n"
   "                          so this option is VERY optional :)\n"
   "\n"
#if 0
   " -non-orthgonal      = Allow the use of a non-orthgonal matrix for the\n"
   "                       transformation. [Experimental]\n"
   "                      **[not yet implemented]**\n"
#endif
   "\n"
   " -Qmatrix mmm        = Save the transformation matrix Q into a file\n"
   "                       with name 'mmm.qmat.1D', for perusal at your leisure.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* Is this program useful? Not even The Shadow knows!\n"
   "  (But don't call it BS.)\n"
   "\n"
   "* The output dataset is in floating point format.\n"
   "\n"
   "* Notation:\n"
   "    M = Number of time points\n"
   "    N = Number of voxels > M (N = size of mask)\n"
   "    B = MxN matrix of time series from -inset1\n"
   "    C = MxN matrix of time series from -inset2\n"
   "        Both matrices have each column normalized to\n"
   "        sum-of-squares = 1 (L2 normalized)\n"
   "    Q = Desired MxM matrix to transform C such that B-QC\n"
   "        is as small as possible (sum-of-squares = Frechet norm)\n"
   "        normF(A) = sum_{ij} A_{ij}^2 = trace(AA') = trace(A'A)\n"
   "        This norm is different from the matrix L2 norm.\n"
   "\n"
   "* Joshi method:\n"
   "   (a) compute MxM matrix B C'\n"
   "   (b) compute SVD of B C' = U S V' (U, S, V are MxM matrices)\n"
   "   (c) Q = U V'\n"
   "       [note: if B=C, then U=V, so Q=I, as it should]\n"
   "   (d) transform each time series from -inset2 using Q\n"
   "   This matrix Q is the solution to the restricted least squares\n"
   "   problem (i.e., restricted to have Q be an orthogonal matrix).\n"
   "\n"
   "   A pre-print of their method is available as:\n"
   "   AA Joshi, M Chong, RM Leahy.\n"
   "   BrainSync: An Orthogonal Transformation for Synchronization of fMRI\n"
   "   Data Across Subjects, Proc. MICCAI 2017\n"
   "   https://www.dropbox.com/s/tu4kuqqlg6r02kt/brainsync_miccai2017.pdf\n"
   "   https://www.google.com/search?q=joshi+brainsync\n"
#if 0
   "\n"
   "* non-orthogonal method:\n"
   "    Q = inv(C C') B C'\n"
   "      = solution to unrestricted least squares problem\n"
   "        (i.e., Q can be any MxM matrix).\n"
#endif
   "\n"
   "* The input datasets should be pre-processed first to remove\n"
   "  undesirable components (motions, baseline, spikes, etc.)\n"
   "  Otherwise, you'll be trying to match artifacts between the\n"
   "  datasets, which is not likely to be interesting or useful.\n"
   "  3dTproject would be one way to do this. Or afni_proc.py.\n"
   "\n"
   "* RWCox -- July 2017\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* Given m X n matrices bmat and cmat, compute the orthogonal m X m matrix
   qmat that 'best' transforms cmat to bmat.
    1) normalize all columns of bmat and cmat
    2) compute m X m matrix amat = [bmat] * [cmat]'
    3) SVD that to get amat = [umat] * [sigma] * [vmat]'
    4) qmat = [umat] * [vmat]'
*//*--------------------------------------------------------------------------*/

static void compute_joshi_matrix( int m , int n ,
                                  float *bmat , float *cmat , float *qmat )
{
   int ii,jj,kk , kbot ;
   double *bmatn , *cmatn ;
   double *amat , *umat , *vmat , *sval ;
   register double bsum , csum ;

ENTRY("compute_joshi_matrix") ;

   /* temp matrices */

   bmatn = (double *)calloc( sizeof(double) , m*n ) ; /* normalized bmat */
   cmatn = (double *)calloc( sizeof(double) , m*n ) ; /* normalized cmat */
   amat  = (double *)calloc( sizeof(double) , m*m ) ; /* [bmatn] * [cmatn]' */

   /* macros for 2D array indexing */

#undef  B
#undef  C
#undef  A
#undef  U
#undef  V
#undef  BN
#undef  CN

#define B(i,j)  bmat[(i)+(j)*m]
#define C(i,j)  cmat[(i)+(j)*m]
#define BN(i,j) bmatn[(i)+(j)*m]
#define CN(i,j) cmatn[(i)+(j)*m]
#define A(i,j)  amat[(i)+(j)*m]
#define U(i,j)  umat[(i)+(j)*m]
#define V(i,j)  vmat[(i)+(j)*m]
#define QQ(i,j) qmat[(i)+(j)*m]

   /* copy input matrices into bmatn and cmatn, normalizing as we go */

   ININFO_message("Normalizing time series [%s]",TIMER) ;

   for( jj=0 ; jj < n ; jj++ ){
     bsum = csum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ){
       bsum += B(ii,jj)*B(ii,jj) ;
       csum += C(ii,jj)*C(ii,jj) ;
     }
     if( bsum > 0.0 ) bsum = 1.0 / sqrt(bsum) ;
     if( csum > 0.0 ) csum = 1.0 / sqrt(csum) ;
     for( ii=0 ; ii < m ; ii++ ){
       BN(ii,jj) = B(ii,jj)*bsum ;
       CN(ii,jj) = C(ii,jj)*csum ;
     }
   }
MEMORY_CHECK("a") ;

   /* form A matrix = BN * CN' */

   ININFO_message("forming BC' matrix [%s]",TIMER) ;

   kbot = n%2 ; /* 1 if odd, 0 if even */
   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = (kbot) ? BN(ii,0)*CN(jj,0) : 0.0 ;
       for( kk=kbot ; kk < n ; kk+=2 ) /* unrolled by 2 */
         bsum += BN(ii,kk)*CN(jj,kk) + BN(ii,kk+1)*CN(ii,kk+1) ;
       A(ii,jj) = bsum ;
   }}
MEMORY_CHECK("b") ;

   free(bmatn) ; free(cmatn) ;
   umat  = (double *)calloc( sizeof(double),m*m ) ; /* SVD outputs */
   vmat  = (double *)calloc( sizeof(double),m*m ) ;
   sval  = (double *)calloc( sizeof(double),m   ) ;

   /* compute SVD of scaled matrix */

   ININFO_message("SVD-ing BC' matrix %dx%d [%s]",m,m,TIMER) ;

   svd_double( m , m , amat , sval , umat , vmat ) ;

   free(amat) ; free(sval) ;

   /* compute QQ = output matrix = U V' */

   ININFO_message("computing Q matrix [%s]",TIMER) ;

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < m ; kk++ ) bsum += U(ii,kk)*V(jj,kk) ;
       QQ(ii,jj) = (float)bsum ;
   }}
MEMORY_CHECK("d") ;

   free(umat) ; free(vmat) ;

   /* write the matrix out? [20 Jul 2017] */

   if( matpre != NULL ){
     char *fname ; MRI_IMAGE *qim ;
     fname = (char *)malloc(sizeof(char)*(strlen(matpre)+32)) ;
     strcpy(fname,matpre) ;
     if( strstr(fname,".1D") == NULL ) strcat(fname,".qmat.1D") ;
     qim = mri_new_vol_empty( m,m,1, MRI_float ) ;
     mri_fix_data_pointer( qmat , qim ) ;
     mri_write( fname , qim ) ;
     ININFO_message("wrote Q matrix to %s [%s]",fname,TIMER) ;
     mri_clear_data_pointer( qim ) ; mri_free(qim) ; free(fname) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Given m X n matrices bmat and cmat, compute the non-orthogonal m X m matrix
   qmat that 'best' transforms cmat to bmat.
    1) normalize all columns of bmat and cmat
    2) compute m X m matrix amat = [bmat] * [cmat]'
    3) compute m X m matrix dmat = [cmat] * [cmat]'
    4) qmat = inv[dmat] * [amat]
*//*--------------------------------------------------------------------------*/

static void compute_nonorth_matrix( int m , int n ,
                                    float *bmat , float *cmat , float *qmat )
{
ENTRY("compute_nonorth_matrix") ;

   /* a little more code needed here :) */

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static int is_vector_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*------------------- 08 Oct 2010: functions for despiking ----------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}
#undef SORT2
#undef SWAP

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+8 >= num ) jj = num-9;     \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }

/*-------------------------------------------------------------------------*/
/*! Remove spikes from a time series, in a very simplistic way.
    Return value is the number of spikes that were squashed [RWCox].
*//*-----------------------------------------------------------------------*/

static int despike9( int num , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return 0 ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   free(zme) ; return nsp ;
}
#undef mead9

/*----------------------------------------------------------------------------*/
/* compute the output dataset */

void TSY_process_data(void)
{
   byte *vmask ;
   int nvmask , ii,jj,kk , mm , nvox , ncut=0 ;
   MRI_IMAGE *bim, *cim ;
   float *bar, *car, *bmat, *cmat, *qmat, *cvec, csum ;

ENTRY("TSY_process_data") ;

   INFO_message("begin BrainSync [%s]",TIMER) ;

   /* build mask */

   nvox = DSET_NVOX(dsetB) ;
   mm   = DSET_NVALS(dsetB) ;

   if( maskset != NULL ){  /*** explicit mask ***/
     vmask = THD_makemask( maskset , 0 , 1.0f,0.0f ) ;
     DSET_unload(maskset) ;
     if( vmask == NULL )
       ERROR_exit("Can't make mask from -mask dataset '%s'",DSET_BRIKNAME(maskset)) ;
     nvmask = THD_countmask( nvox , vmask ) ;
     ININFO_message("%d voxels in the spatial mask [%s]",nvmask,TIMER) ;
     if( nvmask == 0 )
       ERROR_exit("Mask from -mask dataset %s has 0 voxels",DSET_BRIKNAME(maskset)) ;
   } else {               /*** all voxels */
     vmask = (byte *)malloc(sizeof(byte)*(nvox+2)) ; nvmask = nvox ;
     for( jj=0 ; jj < nvox ; jj++ ) vmask[jj] = 1 ;
     ININFO_message("no -mask option ==> processing all %d voxels in dataset [%s]",nvox,TIMER) ;
   }
MEMORY_CHECK("P") ;

   /* find and eliminate any voxels with all-constant vectors */

   ININFO_message("looking for all-constant time series [%s]",TIMER) ;
   bar = (float *)malloc(sizeof(float)*mm) ;
   for( kk=0 ; kk < nvox ; kk++ ){
     if( vmask[kk] ){
       THD_extract_array( kk , dsetB , 0 , bar ) ;
       if( is_vector_constant(mm,bar) ){ vmask[kk] = 0 ; ncut++ ; continue ; }
       THD_extract_array( kk , dsetC , 0 , bar ) ;
       if( is_vector_constant(mm,bar) ){ vmask[kk] = 0 ; ncut++ ; continue ; }
     }
   }
   free(bar) ;
   if( ncut > 0 ){
     nvmask = THD_countmask(nvox,vmask) ;
     ININFO_message("removed %d voxel%s for being constant in time [%s]" ,
                    ncut , (ncut > 1) ? "s" : "\0" , TIMER ) ;
   }
   if( nvmask <= 2*mm )
     ERROR_exit("not enough voxels in mask to process :(") ;

   /* load datasets into matrices */

   ININFO_message("loading datasets into matrices [%s]",TIMER) ;

   bmat = (float *)calloc( sizeof(float) , mm*nvmask ) ;
   cmat = (float *)calloc( sizeof(float) , mm*nvox   ) ; /* extra big */
   qmat = (float *)calloc( sizeof(float) , mm*mm     ) ;

   for( ii=0 ; ii < mm ; ii++ ){
     bim = THD_extract_float_brick( ii , dsetB ) ; bar = MRI_FLOAT_PTR(bim) ;
     cim = THD_extract_float_brick( ii , dsetC ) ; car = MRI_FLOAT_PTR(cim) ;
     for( kk=jj=0 ; jj < nvox ; jj++ ){
       if( vmask[jj] ){
         bmat[ii+kk*mm] = bar[jj] ;
         cmat[ii+kk*mm] = car[jj] ; kk++ ;
       }
     }
     mri_free(bim) ; mri_free(cim) ;
   }
MEMORY_CHECK("Q") ;
   DSET_unload(dsetB) ;
MEMORY_CHECK("R") ;

   /* despike matrix columns */

   ININFO_message("despiking time series [%s]",TIMER) ;

   for( ncut=kk=0 ; kk < nvmask ; kk++ ){
     ncut += despike9( mm , bmat+kk*mm ) ;
     ncut += despike9( mm , cmat+kk*mm ) ;
   }
   if( ncut > 0 )
     ININFO_message("removed %d spike%s from data vectors [%s]",
                    ncut , (ncut==1)?"\0":"s" , TIMER ) ;

   /* compute transform matrix qmat */

   if( do_joshi )
     compute_joshi_matrix  ( mm , nvmask , bmat , cmat , qmat ) ;
   else  /* not implemented */
     compute_nonorth_matrix( mm , nvmask , bmat , cmat , qmat ) ;
MEMORY_CHECK("S") ;

   free(bmat) ; /* not needed no more */

   /* reload input matrix C with ALL voxels, not just mask */

   if( nvmask < nvox ){
     ININFO_message("reloading C matrix with all voxels [%s]",TIMER) ;
     for( ii=0 ; ii < mm ; ii++ ){
       cim = THD_extract_float_brick( ii , dsetC ) ; car = MRI_FLOAT_PTR(cim) ;
       for( jj=0 ; jj < nvox ; jj++ ) cmat[ii+jj*mm] = car[jj] ;
       mri_free(cim) ;
     }
   }
   DSET_unload(dsetC) ;

   /* now transform input matrix C */

   ININFO_message("transforming dataset [%s]",TIMER) ;

   cvec = (float *)calloc( sizeof(float) , mm ) ;

#undef  QQ
#undef  C
#define QQ(i,j) qmat[(i)+(j)*mm]
#define C(i,j)  cmat[(i)+(j)*mm]

   for( jj=0 ; jj < nvox ; jj++ ){
     for( ii=0 ; ii < mm ; ii++ ){
       csum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) csum += QQ(ii,kk) * C(kk,jj) ;
       cvec[ii] = csum ;
     }
     if( do_norm ){  /* -normalize */
       csum = 0.0f ;
       for( ii=0 ; ii < mm ; ii++ ) csum += cvec[ii]*cvec[ii] ;
       if( csum > 0.0f ){
         csum = 1.0f/sqrtf(csum) ;
         for( ii=0 ; ii < mm ; ii++ ) cvec[ii] *= csum ;
       }
     }
     for( ii=0 ; ii < mm ; ii++ ) C(ii,jj) = cvec[ii] ;
   }
MEMORY_CHECK("T") ;

   free(cvec) ; free(qmat) ;

   /* load transformed C matrix into output dataset */

   ININFO_message("filling output dataset [%s]",TIMER) ;

   outset = EDIT_empty_copy( dsetB ) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL ,
                    ADN_none ) ;
   for( ii=0 ; ii < mm ; ii++ ){
     car = (float *)calloc(sizeof(float),nvox) ;
     for( jj=0 ; jj < nvox ; jj++ ) car[jj] = C(ii,jj) ;
     EDIT_substitute_brick( outset , ii , MRI_float , car ) ;
   }
MEMORY_CHECK("U") ;

   free(cmat) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , mm,nn ;

   /*----------*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 )
     TSY_help_the_pitiful_user() ;

   /*----- bureaucracy -----*/

   mainENTRY("3dBrainSync"); machdep();
   AFNI_logger("3dBrainSync",argc,argv);
   PRINT_VERSION("3dBrainSync"); AUTHOR("Cox the Algebraic (Linear)") ;
   ct = NI_clock_time() ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*----- scan options -----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-inset1") == 0 ){
       if( dsetB != NULL )
          ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
          ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;

       dsetB = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(dsetB,argv[iarg]);
       DSET_mallocize(dsetB) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-inset2") == 0 ){
       if( dsetC != NULL )
          ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
          ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;

       dsetC = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(dsetC,argv[iarg]);
       DSET_mallocize(dsetC) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( maskset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg  >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       maskset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(maskset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("-prefix '%s' is not acceptable :-(",prefix) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-Qmatrix") == 0 ||
         strcasecmp(argv[iarg],"-matrix")  == 0   ){

       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       matpre = strdup(argv[iarg]) ;
       if( !THD_filename_ok(matpre) )
         ERROR_exit("%s '%s' is not acceptable :-(",argv[iarg-1],prefix) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncasecmp(argv[iarg],"-norm",5) == 0 ){
       do_norm = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncasecmp(argv[iarg],"-josh",5) == 0 ||
         strncasecmp(argv[iarg],"-orth",5) == 0   ){

       do_joshi = 1 ; iarg++ ; continue ;
     }

#if 0
     if( strncasecmp(argv[iarg],"-non-ort",9) == 0 ||
         strncasecmp(argv[iarg],"-nonorth",9) == 0   ){

       do_joshi = 0 ; iarg++ ; continue ;
     }
#endif

     /*--- error! ---*/

     ERROR_exit("Don't know what to do with option '%s' :-(",argv[iarg]) ;

   } /* end of option scanning loop */

   /*----- error checking -----*/

   if( dsetB == NULL ) ERROR_exit("no -inset1 dataset?") ;
   if( dsetC == NULL ) ERROR_exit("no -inset2 dataset?") ;

   if( !EQUIV_GRIDXYZ(dsetB,dsetC) )
     ERROR_exit("-inset1 and -inset2 datasets are not on same 3D grid :(") ;

   mm = DSET_NVALS(dsetB) ;
   if( mm < 10 )
     ERROR_exit("-inset1 only has %d time points -- need at least 10 :(",mm) ;
   if( DSET_NVALS(dsetC) != mm )
     ERROR_exit("-inset1 has %d time points but -inset2 has %d -- they should match :(",
                mm , DSET_NVALS(dsetC) ) ;

   if( maskset != NULL && !EQUIV_GRIDXYZ(dsetB,maskset) )
     ERROR_exit("-mask and -inset1 datsets are NOT on the same 3D grid :(") ;

   INFO_message("reading datasets [%s]",TIMER) ;
   DSET_load(dsetB) ; CHECK_LOAD_ERROR(dsetB) ;
   DSET_load(dsetC) ; CHECK_LOAD_ERROR(dsetC) ;
   if( maskset != NULL ){ DSET_load(maskset) ; CHECK_LOAD_ERROR(maskset) ; }

   /*----- process the data, get output dataset -----*/

   TSY_process_data() ;

   if( outset != NULL ){
     tross_Copy_History( dsetB , outset ) ;
     tross_Make_History( "3dBrainSync" , argc,argv , outset ) ;
     DSET_write(outset) ;
     if( verb ) WROTE_DSET(outset) ;
   } else {
     ERROR_exit("Processing the data failed for unknown reasons :(") ;
   }

   if( verb )
     INFO_message("=== 3dBrainSync clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}
