#include "mrilib.h"

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

void mri_principal_vector_params( int , int , int ) ;
MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar ) ;
float     * mri_principal_getev(void) ;
void        mri_principal_setev(int n) ;

static int mpv_sign_meth = 0 ;

/*------------------------------------------------------------------------*/
/* Adapted from 3dLocalSVD */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 , masknum ;
   int iarg=1 , verb=1 , ntype=0 , nev,kk,ii,nxyz,nt ;
   float na,nb,nc , dx,dy,dz ;
   MRI_IMARR *imar=NULL ; int *ivox ; MRI_IMAGE *pim ;
   int do_vmean=0 , do_vnorm=0 , sval_itop=0 ;
   int polort=-1 ; float *ev ;
   MRI_IMARR *ortar ; MRI_IMAGE *ortim ; int nyort=0 ;
   float bpass_L=0.0f , bpass_H=0.0f , dtime ; int do_bpass=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage:  3dmaskSVD [options] inputdataset\n"
       "Author: Zhark the Gloriously Singular\n"
       "\n"
       "* Computes the principal singular vector of the time series\n"
       "    vectors extracted from the input dataset over the input mask.\n"
       "  ++ You can use the '-sval' option to change which singular\n"
       "     vectors are output.\n"
       "* The sign of the output vector is chosen so that the average\n"
       "    of arctanh(correlation coefficient) over all input data\n"
       "    vectors (from the mask) is positive.\n"
       "* The output vector is normalized: the sum of its components\n"
       "    squared is 1.\n"
       "* You probably want to use 3dDetrend (or something similar) first,\n"
       "    to get rid of annoying artifacts, such as motion, breathing,\n"
       "    dark matter interactions with the brain, etc.\n"
       "  ++ If you are lazy scum like Zhark, you might be able to get\n"
       "     away with using the '-polort' option.\n"
       "  ++ In particular, if your data time series has a nonzero mean,\n"
       "     then you probably want at least '-polort 0' to remove the\n"
       "     mean, otherwise you'll pretty much just get a constant\n"
       "     time series as the principal singular vector!\n"
       "* An alternative to this program would be 3dmaskdump followed\n"
       "    by 1dsvd, which could give you all the singular vectors you\n"
       "    could ever want, and much more -- enough to confuse you for days.\n"
       "  ++ In particular, although you COULD input a 1D file into\n"
       "     3dmaskSVD, the 1dsvd program would make much more sense.\n"
       "* This program will be pretty slow if there are over about 2000\n"
       "    voxels in the mask.  It could be made more efficient for\n"
       "    such cases, but you'll have to give Zhark some 'incentive'.\n"
       "* Result vector goes to stdout.  Redirect per your pleasures and needs.\n"
       "* Also see program 3dLocalSVD if you want to compute the principal\n"
       "    singular time series vector from a neighborhood of EACH voxel.\n"
       "  ++ (Which is a pretty slow operation!)\n"
       "* http://en.wikipedia.org/wiki/Singular_value_decomposition\n"
       "\n"
       "-------\n"
       "Options:\n"
       "-------\n"
       " -vnorm      = L2 normalize all time series before SVD [recommended!]\n"
       " -sval a     = output singular vectors 0 .. a [default a=0 = first one only]\n"
       " -mask mset  = define the mask [default is entire dataset == slow!]\n"
       " -automask   = you'll have to guess what this option does\n"
       " -polort p   = if you are lazy and didn't run 3dDetrend (like Zhark)\n"
       " -bpass L H  = bandpass [mutually exclusive with -polort]\n"
       " -ort xx.1D  = time series to remove from the data before SVD-ization\n"
       "               ++ You can give more than 1 '-ort' option\n"
       "               ++ 'xx.1D' can contain more than 1 column\n"
       " -input ddd  = alternative way to give the input dataset name\n"
       "\n"
       "-------\n"
       "Example:\n"
       "-------\n"
       " You have a mask dataset with discrete values 1, 2, ... 77 indicating\n"
       " some ROIs; you want to get the SVD from each ROI's time series separately,\n"
       " and then put these into 1 big 77 column .1D file.  You can do this using\n"
       " a csh shell script like the one below:\n"
       "\n"
       " # Compute the individual SVD vectors\n"
       " foreach mm ( `count 1 77` )\n"
       "   3dmaskSVD -vnorm -mask mymask+orig\"<${mm}..${mm}>\" epi+orig > qvec${mm}.1D\n"
       " end\n"
       " # Glue them together into 1 big file, then delete the individual files\n"
       " 1dcat qvec*.1D > allvec.1D\n"
       " /bin/rm -f qvec*.1D\n"
       " # Plot the results to a JPEG file, then compute their correlation matrix\n"
       " 1dplot -one -nopush -jpg allvec.jpg allvec.1D\n"
       " 1ddot -terse allvec.1D > allvec_COR.1D\n"
       "\n"
       " [[ If you use the bash shell,  you'll have to figure out the syntax ]]\n"
       " [[ yourself. Zhark has no sympathy for you bash shell infidels, and ]]\n"
       " [[ considers you only slightly better than those lowly Emacs users. ]]\n"
       " [[ And do NOT ever even mention 'nedit' in Zhark's august presence! ]]\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dmaskSVD"); mainENTRY("3dmaskSVD main"); machdep();
   AFNI_logger("3dmaskSVD",argc,argv); AUTHOR("Zhark the Singular");

   /*---- loop over options ----*/

   INIT_IMARR(ortar) ;

   mpv_sign_meth = AFNI_yesenv("AFNI_3dmaskSVD_meansign") ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-bpass") == 0 ){
       if( iarg+2 >= argc ) ERROR_exit("need 2 args after -bpass") ;
       bpass_L = (float)strtod(argv[++iarg],NULL) ;
       bpass_H = (float)strtod(argv[++iarg],NULL) ;
       if( bpass_L < 0.0f || bpass_H <= bpass_L )
         ERROR_exit("Illegal values after -bpass: %g %g",bpass_L,bpass_H) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ort") == 0 ){  /* 01 Oct 2009 */
       int nx,ny ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-ort'") ;
       ortim = mri_read_1D( argv[iarg] ) ;
       if( ortim == NULL ) ERROR_exit("-ort '%s': Can't read 1D file",argv[iarg]) ;
       nx = ortim->nx ; ny = ortim->ny ;
       if( nx == 1 && ny > 1 ){
         MRI_IMAGE *tim=mri_transpose(ortim); mri_free(ortim); ortim = tim; ny = 1;
       }
       ADDTO_IMARR(ortar,ortim) ; nyort += ny ;
       iarg++ ; continue ;
     }

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
       sval_itop = (int)strtod(argv[iarg],NULL) ;
       if( sval_itop < 0 ){ sval_itop = 0 ; WARNING_message("'-sval' reset to 0") ; }
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

   DSET_UNMSEC(inset) ;
   dtime = DSET_TR(inset) ;
   if( dtime <= 0.0f ) dtime = 1.0f ;
   do_bpass = (bpass_L < bpass_H) ;
   if( do_bpass ){
     kk = THD_bandpass_OK( nt , dtime , bpass_L , bpass_H , 1 ) ;
     if( kk <= 0 ) ERROR_exit("Can't continue since -bpass setup is illegal") ;
     polort = -1 ;
   }

   /*--- deal with the masking ---*/

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

   nev = MIN(nt,masknum) ;  /* max possible number of eigenvalues */
   if( sval_itop >= nev ){
     sval_itop = nev-1 ;
     WARNING_message("'-sval' reset to '%d'",sval_itop) ;
   }
   mri_principal_vector_params( 0 , do_vnorm , sval_itop ) ;
   mri_principal_setev(nev) ;

   /*-- get data vectors --*/

   ivox = (int *)malloc(sizeof(int)*masknum) ;
   for( kk=ii=0 ; ii < nxyz ; ii++ ) if( mask[ii] ) ivox[kk++] = ii ;
   INFO_message("Extracting data vectors") ;
   imar = THD_extract_many_series( masknum, ivox, inset ) ; DSET_unload(inset) ;
   if( imar == NULL ) ERROR_exit("Can't get data vector?!") ;

   /*-- detrending --*/

   if( polort >= 0 || nyort > 0 || do_bpass ){
     float **polref=NULL ; float *tsar ;
     int nort=IMARR_COUNT(ortar) , nref=0 ;

     if( polort >= 0 ){  /* polynomials */
       nref = polort+1 ; polref = THD_build_polyref(nref,nt) ;
     }

     if( nort > 0 ){     /* other orts */
       float *oar , *par ; int nx,ny , qq,tt ;
       for( kk=0 ; kk < nort ; kk++ ){  /* loop over input -ort files */
         ortim = IMARR_SUBIM(imar,kk) ;
         nx = ortim->nx ; ny = ortim->ny ;
         if( nx < nt ) ERROR_exit("-ort '%s' is too short for dataset",ortim->fname) ;
         polref = (float **)realloc(polref,(nref+ny)*sizeof(float *)) ;
         oar    = MRI_FLOAT_PTR(ortim) ;
         for( qq=0 ; qq < ny ; qq++,oar+=nx ){
           par = polref[nref+qq] = (float *)malloc(sizeof(float)*nt) ;
           for( tt=0 ; tt < nt ; tt++ ) par[tt] = oar[tt] ;
                if( polort == 0 ) THD_const_detrend (nt,par,NULL) ;
           else if( polort >  0 ) THD_linear_detrend(nt,par,NULL,NULL) ;
         }
         nref += ny ;
       }
       DESTROY_IMARR(ortar) ;
     }

     if( !do_bpass ){
       INFO_message("Detrending data vectors") ;
       for( kk=0 ; kk < IMARR_COUNT(imar) ; kk++ ){
         tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,kk)) ;
         THD_generic_detrend_LSQ( nt , tsar , -1 , nref , polref , NULL ) ;
       }
     } else {
       float **vec = (float **)malloc(sizeof(float *)*IMARR_COUNT(imar)) ;
       INFO_message("Bandpassing data vectors") ;
       for( kk=0 ; kk < IMARR_COUNT(imar) ; kk++ )
         vec[kk] = MRI_FLOAT_PTR(IMARR_SUBIM(imar,kk)) ;
       (void)THD_bandpass_vectors( nt    , IMARR_COUNT(imar) , vec     ,
                                   dtime , bpass_L           , bpass_H ,
                                   1     , nref              , polref   ) ;
       free(vec) ;
     }

     for( kk=0 ; kk < nref; kk++ ) free(polref[kk]) ;
     free(polref) ;
   } /* end of detrendization */

   /*--- the actual work ---*/

   INFO_message("Computing SVD") ;
   pim  = mri_principal_vector( imar ) ; DESTROY_IMARR(imar) ;
   if( pim == NULL ) ERROR_exit("SVD failure!?!") ;
   ev = mri_principal_getev() ;
   switch(sval_itop+1){
     case 1:
       INFO_message("First singular value: %g",ev[0]) ; break ;
     case 2:
       INFO_message("First 2 singular values: %g %g",ev[0],ev[1]) ; break ;
     case 3:
       INFO_message("First 3 singular values: %g %g %g",ev[0],ev[1],ev[2]) ; break ;
     case 4:
       INFO_message("First 4 singular values: %g %g %g %g",ev[0],ev[1],ev[2],ev[3]) ; break ;
     default:
     case 5:
       INFO_message("First 5 singular values: %g %g %g %g %g",ev[0],ev[1],ev[2],ev[3],ev[4]) ; break ;
   }
   mri_write_1D(NULL,pim) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

static int mpv_vmean = 0 ;
static int mpv_vnorm = 0 ;
static int mpv_sitop = 0 ;

void mri_principal_vector_params( int a , int b , int e )
{
   mpv_vmean = a; mpv_vnorm = b; mpv_sitop = e;
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
   int nx , nvec , ii,jj,kk , nev ;
   float *amat , *umat , *sval ;
   float *far , *tar ; MRI_IMAGE *tim ;
   float vmean=0.0f , vnorm=0.0f ;
   register float sum , zum ; float qum ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;

#define A(i,j) amat[(i)+(j)*nx]     /* nx X nvec matrix */
#define U(i,j) umat[(i)+(j)*nx]     /* ditto */
#define X(i,j) amat[(i)+(j)*nvec]   /* nvec X nx matrix */

   nev  = mpv_sitop + 1 ;

   amat = (float *)malloc( sizeof(float)*nx*nvec ) ;
   umat = (float *)malloc( sizeof(float)*nx*nev  ) ;
   sval = (float *)malloc( sizeof(float)*nev     ) ;

   /* load data vectors into A */

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) = far[ii] ;
   }

   /* remove mean of each vector? */

   if( mpv_vmean ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj) ;
       sum /= nx ; if( jj == 0 ) vmean = sum ;
       for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) -= sum ;
     }
   }

   /* normalize each vector? */

   if( mpv_vnorm ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0f ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
       if( sum > 0.0f ){
         sum = 1.0f / sqrtf(sum) ; if( jj == 0 ) vnorm = 1.0f/sum ;
         for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) *= sum ;
       }
     }
   }

   /* do the eigen-stuff */

   jj = first_principal_vectors( nx,nvec,amat , nev,sval,umat ) ;
   if( jj <= 0 ){ free(sval); free(umat); free(amat); return NULL; }
   nev = jj ;

   if( mpv_evnum > 0 ){
     int itop = MIN(mpv_evnum,nev) ;
     for( ii=0 ; ii < itop      ; ii++ ) mpv_ev[ii] = sval[ii] ;
     for(      ; ii < mpv_evnum ; ii++ ) mpv_ev[ii] = 0.0f ;
   }

   tim = mri_new( nx , nev , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;
   for( jj=0 ; jj < nev ; jj++ ){
     tar = far + jj*nx ;
     for( ii=0 ; ii < nx ; ii++ ) tar[ii] = U(ii,jj) ;
   }

   /* select sign of result vector(s), then scale */

   for( kk=0 ; kk < nev ; kk++ ){  /* loop over multiple output vectors */
     tar = far + kk*nx ;
     qum = 0.0f ;
     if( mpv_sign_meth == 0 ){         /* mean of dot products with data vectors */
       for( jj=0 ; jj < nvec ; jj++ ){
         zum = sum = 0.0f ;
         for( ii=0 ; ii < nx ; ii++ ){ sum += A(ii,jj)*tar[ii]; zum += A(ii,jj)*A(ii,jj); }
         if( zum > 0.0f ){
           sum = sum / zum ; sum = MIN(sum,0.9999f) ; sum = MAX(sum,-0.9999f) ;
           qum += atanhf(sum) ;
         }
       }
     } else {                          /* dot product with mean data vector */
       for( ii=0 ; ii < nx ; ii++ ){
         sum = 0.0f ;
         for( jj=0 ; jj < nvec ; jj++ ) sum += A(ii,jj) ;  /* mean vec at time ii */
         qum += sum * tar[ii] ;
       }
     }
     if( qum < 0.0f ){
       for( ii=0 ; ii < nx ; ii++ ) tar[ii] = -tar[ii] ;
     }
     sum = 0.0f ;
     for( ii=0 ; ii < nx ; ii++ ) sum += tar[ii]*tar[ii] ;
     if( sum > 0.0f && fabs(sum-1.0f) > 1.e-5f ){
       sum = 1.0f / sum ; for( ii=0 ; ii < nx ; ii++ ) tar[ii] *= sum ;
     }
   }

   free(sval); free(umat); free(amat); return tim;
}
