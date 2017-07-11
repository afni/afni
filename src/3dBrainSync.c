#include "mrilib.h"

#ifdef USE_OMP
#include "cs_symeig.c"
#endif

#undef MEMORY_CHECK
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
#else
# define MEMORY_CHECK(mm) /*nada*/
#endif

static int   verb   = 1 ;
static char *prefix = "brainsync" ;
static char *matpre = NULL ;
static int do_joshi = 1 ;
static int do_norm  = 0 ;

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
   "                       ++ These 2 datasets must be on the same spatial\n"
   "                          grid, and must have the same number of time points!\n"
   "\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
   "                       This will be the -inset2 dataset transformed\n"
   "                       to be as correlated as possible (in time)\n"
   "                       with the -inset1 dataset.\n"
   "\n"
   " -normalize          = Normalize the output dataset so that each\n"
   "                       time series has sum-of-squares = 1.\n"
   "                      **[not yet implemented]**\n"
   "\n"
   " -mask mset          = Only operate on voxels nonzero in the mset dataset.\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
   "                       ++ If no masking option is given, then all voxels\n"
   "                          will be processed.\n"
   "                       ++ Any voxel which is all constant in time\n"
   "                          (in either input) will be added to the mask.\n"
   "                       ++ This dataset must be on the same spatial grid\n"
   "                          as the other input datasets.\n"
   "\n"
   " -joshi              = Use the Joshi method, with an orthogonal matrix\n"
   " -orthogonal           for the transformation. [This is the default]\n"
   "\n"
   " -non-orthgonal      = Allow the use of a non-orthgonal matrix for the\n"
   "                       transformation. [Experimental]\n"
   "                      **[not yet implemented]**\n"
   "\n"
   " -Qmatrix mmm        = Save the transformation matrix Q into a file\n"
   "                       with name 'mmm.qmat.1D', for perusal at your leisure.\n"
   "                      **[not yet implemented]**\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* Is this program useful? Not even The Shadow knows!\n"
   "\n"
   "* The output dataset is in floating point format.\n"
   "\n"
   "* Notation:\n"
   "    M = Number of time points\n"
   "    N = Number of voxels > M\n"
   "    B = MxN matrix of time series from -inset1\n"
   "    C = MxN matrix of time series from -inset2\n"
   "        Both matrices have each column normalized to\n"
   "        sum-of-squares = 1\n"
   "    Q = Desired MxM matrix to transform C such that\n"
   "        B-QC is as small as possible (sum-of-squares)\n"
   "\n"
   "* Joshi method:\n"
   "   (a) compute MxM matrix B C'\n"
   "   (b) compute SVD of B C' = U S V' (U, S, V are MxM matrices)\n"
   "   (c) Q = U V'\n"
   "   (d) transform each time series from -inset2 using Q\n"
   "   This matrix Q is the solution to the restricted least squares\n"
   "   problem (i.e., restricted to have Q be an orthogonal matrix).\n"
   "\n"
   "* non-orthogonal method:\n"
   "    Q = inv(C C') B C'\n"
   "      = solution to unrestricted least squares problem\n"
   "        (i.e., Q can be any MxM matrix).\n"
   "\n"
   "* The input datasets should be pre-processed first to remove\n"
   "  undesirable components (motions, baseline, spikes, etc.)\n"
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
   int ii,jj,kk ;
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

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < n ; kk++ ) bsum += BN(ii,kk) * CN(jj,kk) ;
       A(ii,jj) = bsum ;
   }}
MEMORY_CHECK("b") ;

   free(bmatn) ; free(cmatn) ;
   umat  = (double *)calloc( sizeof(double),m*m ) ; /* SVD outputs */
   vmat  = (double *)calloc( sizeof(double),m*m ) ;
   sval  = (double *)calloc( sizeof(double),m   ) ;

   /* compute SVD of scaled matrix */

MEMORY_CHECK("c1") ;
   svd_double( m , m , amat , sval , umat , vmat ) ;
MEMORY_CHECK("c2") ;

   free(amat) ; free(sval) ;

   /* compute QQ = output matrix = U V' */

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < m ; kk++ ) bsum += U(ii,kk)*V(jj,kk) ;
       QQ(ii,jj) = (float)bsum ;
   }}
MEMORY_CHECK("d") ;

   free(umat) ; free(vmat) ;
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

   /* a little more code needed here */

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static int is_vector_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/
/* compute the output dataset */

void TSY_process_data(void)
{
   byte *vmask ;
   int nvmask , ii,jj,kk , mm , nvox ;
   MRI_IMAGE *bim, *cim ;
   float *bar, *car, *bmat, *cmat, *qmat, *cvec, csum ;

ENTRY("TSY_process_data") ;

   /* build mask */

   nvox = DSET_NVOX(dsetB) ;

   if( maskset != NULL ){  /*** explicit mask ***/
     vmask = THD_makemask( maskset , 0 , 1.0f,0.0f ) ;
     DSET_unload(maskset) ;
     if( vmask == NULL )
       ERROR_exit("Can't make mask from -mask dataset '%s'",DSET_BRIKNAME(maskset)) ;
     nvmask = THD_countmask( nvox , vmask ) ;
     INFO_message("%d voxels in the spatial mask",nvmask) ;
     if( nvmask == 0 )
       ERROR_exit("Mask from -mask dataset %s has 0 voxels",DSET_BRIKNAME(maskset)) ;
   } else {               /*** all voxels */
     vmask = (byte *)malloc(sizeof(byte)*(nvox+2)) ; nvmask = nvox ;
     for( jj=0 ; jj < nvox ; jj++ ) vmask[jj] = 1 ;
     INFO_message("no -mask option ==> processing all %d voxels in dataset",nvox) ;
   }
MEMORY_CHECK("P") ;

   /* find and eliminate any voxels with all-constant vectors */

   /* load datasets into matrices */

   mm   = DSET_NVALS(dsetB) ;
   bmat = (float *)calloc( sizeof(float) , mm*nvmask ) ;
   cmat = (float *)calloc( sizeof(float) , mm*nvmask ) ;
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
   DSET_unload(dsetB) ; DSET_unload(dsetC) ;
MEMORY_CHECK("R") ;

   /* compute transform matrix qmat */

   if( do_joshi )
     compute_joshi_matrix  ( mm , nvmask , bmat , cmat , qmat ) ;
   else
     compute_nonorth_matrix( mm , nvmask , bmat , cmat , qmat ) ;

   /* transform input matric C */
MEMORY_CHECK("S") ;

   free(bmat) ;
   cvec = (float *)calloc( sizeof(float) , mm ) ;

#undef  QQ
#undef  C
#define QQ(i,j) qmat[(i)+(j)*mm]
#define C(i,j)  cmat[(i)+(j)*mm]

   for( jj=0 ; jj < nvmask ; jj++ ){
     for( ii=0 ; ii < mm ; ii++ ){
       csum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) csum += QQ(ii,kk) * C(kk,jj) ;
       cvec[ii] = csum ;
     }
     for( ii=0 ; ii < mm ; ii++ ) C(ii,jj) = cvec[ii] ;
   }
MEMORY_CHECK("T") ;

   free(cvec) ; free(qmat) ;

   /* load transformed C matrix into output dataset */

   outset = EDIT_empty_copy( dsetB ) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL ,
                    ADN_none ) ;
   for( ii=0 ; ii < mm ; ii++ ){
     car = (float *)calloc(sizeof(float),nvox) ;
     for( kk=jj=0 ; jj < nvox ; jj++ ){
       if( vmask[jj] ){ car[jj] = C(ii,kk) ; kk++ ; }
     }
     EDIT_substitute_brick( outset , ii , MRI_float , car ) ;
   }
MEMORY_CHECK("U") ;

   free(cmat) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , mm,nn , ct ;

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

     if( strncasecmp(argv[iarg],"-josh",5) == 0 ||
         strncasecmp(argv[iarg],"-orth",5) == 0   ){

       do_joshi = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-non-ort",9) == 0 ||
         strncasecmp(argv[iarg],"-nonorth",9) == 0   ){

       do_joshi = 0 ; iarg++ ; continue ;
     }

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
