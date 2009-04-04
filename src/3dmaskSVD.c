#include "mrilib.h"

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

void mri_principal_vector_params( int a , int b , int c , int d ) ;
MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar ) ;
float     * mri_principal_getev(void) ;
void        mri_principal_setev(int n) ;

/*------------------------------------------------------------------------*/
/* Adapted from 3dLocalSVD */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 , masknum ;
   int iarg=1 , verb=1 , ntype=0 , nev,kk,ii,nxyz,nt ;
   float na,nb,nc , dx,dy,dz ;
   MRI_IMARR *imar=NULL ; int *ivox ; MRI_IMAGE *pim ;
   int do_vmean=0 , do_vnorm=0 , do_vproj=0 , sval_index=0 ;
   int polort=-1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3maskSVD [options] inputdataset\n"
       "\n"
       "* Computes the principal singular vector of the time series\n"
       "    vectors extracted from the input dataset over the input mask.\n"
       "* You probably want to use 3dDetrend (or something similar) first,\n"
       "    to get rid of annoying artifacts, such as motion, breathing,\n"
       "    dark matter interactions with the brain, etc.\n"
       "* An alternative to this program would be 3dmaskdump followed\n"
       "    by 1dsvd, which could give you all the singular vectors you\n"
       "    could ever want, and more.\n"
       "* This program will be pretty slow if there are over about 1000\n"
       "    voxels in the mask.  It could be made more efficient for\n"
       "    such cases, but you'll have to give me some 'incentive'.\n"
       "* Result vector goes to stdout.  Redirect per your pleasures and needs.\n"
       "\n"
       "Options:\n"
       " -vnorm     = L2 normalize all time series before SVD (recommended)\n"
       " -sval n    = use the n-th singular vector, for n=0,1,... [default n=0]\n"
       " -mask mset = define the mask [default is entire dataset]\n"
       " -automask  = guess\n"
       " -polort p  = if you are lazy and didn't want to run 3dDetrend\n"
       " -input ddd = alternative way to give the input dataset name\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dmaskSVD"); mainENTRY("3dmaskSVD main"); machdep();
   AFNI_logger("3dmaskSVD",argc,argv); AUTHOR("Zhark the Singular");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-polort'") ;
       polort = (int)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sval") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-sval'") ;
       sval_index = (int)strtod(argv[iarg],NULL) ;
       if( sval_index < 0 ){
         WARNING_message("Replaced '-sval %d' with 0",sval_index) ;
         sval_index = 0 ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       masknum = mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have two mask inputs!") ;
       automask = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   nt = DSET_NVALS(inset) ;  /* vector lengths */
   if( nt < 9 )
     ERROR_exit("Must have at least 9 values per voxel") ;
   if( polort+1 >= nt )
     ERROR_exit("'-polort %d' too big for time series length = %d",polort,nt) ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   nxyz = DSET_NVOX(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     masknum = mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 9 ) ERROR_exit("Automask is too small to process") ;
   } else {
     mask = (byte *)malloc(sizeof(byte)*nxyz) ; masknum = nxyz ;
     memset( mask , 1 , sizeof(byte)*nxyz ) ;
     INFO_message("Using all %d voxels in dataset",nxyz) ;
   }

   nev = MIN(nt,masknum) ;
   if( sval_index >= nev ){
     WARNING_message("-sval '%d' reset to maximum allowed %d",sval_index,nev-1);
     sval_index = nev-1 ;
   }
   mri_principal_vector_params( 0 , do_vnorm , 0 , sval_index ) ;
   set_svd_sort(-1) ;  /* largest singular values first */
   mri_principal_setev(nev) ;

   ivox = (int *)malloc(sizeof(int)*masknum) ;
   for( kk=ii=0 ; ii < nxyz ; ii++ ) if( mask[ii] ) ivox[kk++] = ii ;
   INFO_message("Extracting data vectors") ;
   imar = THD_extract_many_series( masknum, ivox, inset ) ; DSET_unload(inset) ;
   if( imar == NULL ) ERROR_exit("Can't get data vector?!") ;

   if( polort >= 0 ){
     float **polref = THD_build_polyref( polort+1 , nt ) ;
     float *tsar ;
     INFO_message("Detrending data vectors") ;
     for( kk=0 ; kk < IMARR_COUNT(imar) ; kk++ ){
       tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,kk)) ;
       THD_generic_detrend_LSQ( nt , tsar , -1 , polort+1 , polref , NULL ) ;
     }
     for( kk=0 ; kk <= polort ; kk++ ) free(polref[kk]) ;
     free(polref) ;
   }

   INFO_message("Computing SVD") ;
   pim  = mri_principal_vector( imar ) ; DESTROY_IMARR(imar) ;
   if( pim == NULL ) ERROR_exit("SVD failure!") ;
   INFO_message("First singular value: %g",mri_principal_getev()[0]) ;
   mri_write_1D(NULL,pim) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

static int mpv_vmean = 0 ;
static int mpv_vnorm = 0 ;
static int mpv_vproj = 0 ;
static int mpv_sindx = 0 ;

void mri_principal_vector_params( int a , int b , int c , int d )
{
   mpv_vmean = a ; mpv_vnorm = b ; mpv_vproj = c ; mpv_sindx = d ;
}

/*------------------------------------------------------------------------*/

static int    mpv_evnum = 0 ;
static float *mpv_ev    = NULL ;

float * mri_principal_getev(void){ return mpv_ev ; }

void mri_principal_setev(int n){
   mpv_evnum = n ;
   mpv_ev    = (float *)malloc(sizeof(float)*n) ;
}

/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar )
{
   int nx , nvec , ii,jj ;
   double *amat , *umat , *vmat , *sval ;
   float *far ; MRI_IMAGE *tim ;
   float vmean=0.0f , vnorm=0.0f ;
   register double sum ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;

#define A(i,j) amat[(i)+(j)*nx]     /* nx X nvec matrix */
#define U(i,j) umat[(i)+(j)*nx]     /* ditto */
#define V(i,j) vmat[(i)+(j)*nvec]   /* nvec X nvec matrix */
#define X(i,j) amat[(i)+(j)*nvec]   /* nvec X nx matrix */

   amat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   umat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   vmat = (double *)malloc( sizeof(double)*nvec*nvec ) ;
   sval = (double *)malloc( sizeof(double)*nvec ) ;

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) = (double)far[ii] ;
   }

   if( mpv_vmean ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj) ;
       sum /= nx ; if( jj == 0 ) vmean = sum ;
       for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) -= sum ;
     }
   }
   if( mpv_vnorm ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
       if( sum > 0.0 ){
         sum = 1.0 / sqrt(sum) ; if( jj == 0 ) vnorm = 1.0/sum ;
         for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) *= sum ;
       }
     }
   }

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   if( mpv_evnum > 0 ){
     int itop = MIN(mpv_evnum,nvec) ;
     for( ii=0 ; ii < itop      ; ii++ ) mpv_ev[ii] = (float)sval[ii] ;
     for(      ; ii < mpv_evnum ; ii++ ) mpv_ev[ii] = 0.0f ;
   }

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;
   for( ii=0 ; ii < nx ; ii++ ) far[ii] = (float)U(ii,mpv_sindx) ;

   sum = 0.0 ;
   for( jj=0 ; jj < nvec ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,0)*far[ii] ;
   }
   if( sum < 0.0 ){
     for( ii=0 ; ii < nx ; ii++ ) far[ii] = -far[ii] ;
   }

   free(sval); free(vmat); free(umat); free(amat); return tim;
}
