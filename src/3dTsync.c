#include "mrilib.h"

#ifdef USE_OMP
#include "cs_symeig.c"
#endif

int   verb   = 0 ;
char *prefix = "Tsync" ;
THD_3dim_dataset *dsetB=NULL , *dsetC=NULL , *maskset=NULL ;

/*----------------------------------------------------------------------------*/

void TSY_help_the_pitiful_user(void)
{
  printf(
   "\n"
   "Usage:  3dsync [options]\n"
   "\n"
   "This program 'synchronizes' the '-inset2' dataset to match the '-inset1'\n"
   "dataset, as much as possible. It uses the BrainSync algorithm of Joshi et al.\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -inset1 dataset     =\n"
   " -inset2 dataset     =\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
   "\n"
   " -mask mset          = Only operate on voxels nonzero in the mset dataset.\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
   "                       ++ If no masking option is given, then all voxels\n"
   "                          will be processed.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* The output dataset is in floating point format.\n"
   "\n"
   "* The input datasets should be pre-processed first to remove\n"
   "  undesirable components (motions, baseline, spikes, etc.)\n"
   "  3dTproject would be one way to do this. Or afni_proc.py.\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* Given m X n matrices bmat and cmat, compute the orthogonal m X m matrix
   omat that 'best' transforms cmat to bmat.
    1) normalize all columns of bmat and cmat
    2) compute m X m matrix amat = [bmat] * [cmat]'
    3) SVD that to get amat = [umat] * [sigma] * [vmat]'
    4) omat = [vmat] * [umat]'
*//*--------------------------------------------------------------------------*/

static void compute_brainsync( int m , int n ,
                               float *bmat , float *cmat , float *omat )
{
   int ii,jj,kk ;
   double *bmatn , *cmatn ;
   double *amat , *umat , *vmat , *sval ;
   register double bsum , csum ;

   bmatn = (double *)calloc( sizeof(double),m*n ) ;
   cmatn = (double *)calloc( sizeof(double),m*n ) ;
   amat  = (double *)calloc( sizeof(double),m*m ) ;

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
#define OO(i,j) omat[(i)+(j)*m]

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

   /* form A matrix */

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < n ; kk++ ) bsum += BN(ii,kk) * CN(ii,kk) ;
       A(ii,jj) = bsum ;
   }}

   free(bmatn) ; free(cmatn) ;
   umat  = (double *)calloc( sizeof(double),m*m ) ;
   vmat  = (double *)calloc( sizeof(double),m*m ) ;
   sval  = (double *)calloc( sizeof(double),m   ) ;

   /* compute SVD of scaled matrix */

   svd_double( m , n , amat , sval , umat , vmat ) ;

   free(amat) ; free(sval) ;

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < mm ; kk++ ) bsum += V(ii,kk)*U(jj,kk) ;
       OO(ii,jj) = bsum ;
   }}

   free(umat) ; free(vmat) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static int is_vector_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ct , nact=0 ;
   int nbl=0 , *blist=NULL , tt , noblock=0 ;

   /*----------*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 )
     TSY_help_the_pitiful_user() ;

   /*----- bureaucracy -----*/

   mainENTRY("3dTsync"); machdep();
   AFNI_logger("3dTsync",argc,argv);
   PRINT_VERSION("3dTsync"); AUTHOR("Cox the Algebraic (Linear)") ;
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

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("-prefix '%s' is not acceptable :-(",prefix) ;
       iarg++ ; continue ;
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
                mm , DSET_NVALS(dsetC) ;

   if( maskset != NULL && !EQUIV_GRIDXYZ(dsetB,maskset) )
     ERROR_exit("-mask and -inset1 datsets are NOT on the same 3D grid :(") ;

   DSET_load(dsetB) ; CHECK_LOAD_ERROR(dsetB) ;
   DSET_load(dsetC) ; CHECK_LOAD_ERROR(dsetC) ;
   if( maskset != NULL ){ DSET_load(maskset) ; CHECK_LOAD_ERROR(maskset) ; }

   /*----- process the data -----*/

   TSY_process_data() ;

   if( outset != NULL ){
     tross_Copy_History( dsetB , outset ) ;
     tross_Make_History( "3dTsync" , argc,argv , outset ) ;
     DSET_write(outset) ;
     if( verb ) WROTE_DSET(outset) ;
   } else {
     ERROR_exit("Processing the data failed for unknown reasons :(") ;
   }

   if( verb )
     INFO_message("===== clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}
