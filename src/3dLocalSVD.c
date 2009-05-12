#include "mrilib.h"

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

void mri_principal_vector_params( int a , int b ) ;
MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar ) ;
MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd ) ;
float * mri_principal_getev(void) ;
void    mri_principal_setev(int n) ;

static void vstep_print(void) ;

/*------------------------------------------------------------------------*/
/* Adapted from 3dLocalstat and 1dsvd */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL , *evset=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./LocalSVD" ;
   char *evprefix=NULL ; int nev ;
   int iarg=1 , verb=1 , ntype=0 , kk,nx,ny,nz,nxy,nxyz,nt , xx,yy,zz, vstep ;
   float na,nb,nc , dx,dy,dz ;
   MRI_IMARR *imar=NULL ; MRI_IMAGE *pim=NULL ;
   int do_vnorm=0 , do_vproj=0 , polort=-1 ;
   float **polyref , *tsar , *coef=NULL ; int vv,ii , rebase=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalSVD [options] inputdataset\n"
       "* You may want to use 3dDetrend before running this program,\n"
       "   or use the '-polort' option.\n"
       "* This program is highly experimental.  And slowish.\n"
       "* Computes the SVD of time series from a neighborhood of each\n"
       "   voxel.  An inricate way of 'smoothing' 3D+time datasets,\n"
       "   in some sense.\n"
       "\n"
       "Options:\n"
       " -mask mset           = restrict operations to this mask\n"
       " -automask            = create a mask from time series dataset\n"
       " -prefix ppp          = save SVD vector result in this dataset\n"
       " -evprefix ppp        = save eigenvalues in this dataset\n"
       " -input inputdataset  = input time series dataset\n"
       " -nbhd nnn            = e.g., 'SPHERE(5)'\n"
       " -polort p [+]        = detrending ['+' means to add trend back]\n"
       " -vnorm               = normalize vectors [strongly recommended]\n"
       " -vproj [ndim]        = project data vectors onto subspace of dimension 'ndim'\n"
       "                        [default: just output principal singular vector]\n"
       "                        [for most purposes, '-vnorm -vproj 2' is a good idea]\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalSVD"); mainENTRY("3dLocalSVD main"); machdep();
   AFNI_logger("3dLocalSVD",argc,argv); AUTHOR("Emperor Zhark the Singular");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vproj") == 0 ){
       if( iarg+1 < argc && isdigit(argv[iarg+1][0]) ){
         do_vproj = (int)strtod(argv[++iarg],NULL) ;
         if( do_vproj < 1 ) do_vproj = 1 ;
       } else {
         do_vproj = 1 ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-polort'") ;
       polort = (int)strtod(argv[iarg],&cpt) ;
       if( *cpt == '+' ){
         rebase = 1 ;
       } else if( iarg+1 < argc && strcmp(argv[iarg+1],"+") == 0 ){
         rebase = 1; iarg++;
       }
       iarg++ ; continue ;
     }


     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-evprefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-evprefix'") ;
       evprefix = strdup(argv[iarg]) ;
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
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a SPHERE of radius 0") ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else if( strncasecmp(cpt,"RHDD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a RHDD of radius 0") ;
         ntype = NTYPE_RHDD ;
       } else if( strncasecmp(cpt,"TOHD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a TOHD of radius 0") ;
         ntype = NTYPE_TOHD ;
       } else {
          ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /* send options to SVD routine below */

   mri_principal_vector_params( do_vnorm , do_vproj ) ;

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   nt = DSET_NVALS(inset) ;
   if( nt < 9 )
     ERROR_exit("Must have at least 9 values per voxel in time series dataset '%s'",
                DSET_BRIKNAME(inset) ) ;
   if( polort+1 >= nt )
     ERROR_exit("'-polort %d' too big for time series length = %d",polort,nt) ;

   if( polort >= 0 ){
     polyref = THD_build_polyref( polort+1 , nt ) ;
     if( rebase ) coef = (float *)malloc(sizeof(float)*(polort+1)) ;
   }

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   /*-- deal with mask --*/

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
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /*---- create neighborhood (as a cluster) -----*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = -1.01f ;
     INFO_message("Using default neighborhood = self + 6 neighbors") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;

     case NTYPE_SPHERE:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_spheremask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(inset));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(inset));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(inset));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
     }
     break ;

     case NTYPE_RHDD:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_rhddmask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_TOHD:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_tohdmask( dx,dy,dz , na ) ;
     }
     break ;
   }
   MCW_radsort_cluster( nbhd , dx,dy,dz ) ;  /* 26 Feb 2008 */

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
#if 0
   printf("Neighborhood offsets:\n") ;
   for( kk=0 ; kk < nbhd->num_pt ; kk++ )
     printf(" [%2d] = %2d %2d %2d\n",kk,nbhd->i[kk],nbhd->j[kk],nbhd->k[kk]) ;
#endif

   set_svd_sort(-1) ;  /* largest singular values first */

   /** create output dataset **/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset, ADN_prefix,prefix, ADN_brick_fac,NULL, ADN_none );
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalSVD" , argc,argv , outset ) ;
   for( kk=0 ; kk < nt ; kk++ )
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   nev = nbhd->num_pt ; mri_principal_setev(nev) ;
   if( evprefix != NULL ){
     evset = EDIT_empty_copy(outset) ;
     EDIT_dset_items( evset, ADN_prefix,evprefix, ADN_brick_fac,NULL, ADN_none );
     EDIT_dset_items( evset, ADN_nvals,nev , ADN_ntt,nbhd->num_pt , ADN_none ) ;
     for( kk=0 ; kk < nev ; kk++ )
       EDIT_substitute_brick( evset , kk , MRI_float , NULL ) ;
   }

   nx = DSET_NX(outset) ;
   ny = DSET_NY(outset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(outset) ; nxyz = nxy*nz ;
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;

   /*** the real work now begins ***/

   for( kk=0 ; kk < nxyz ; kk++ ){
     if( vstep && kk%vstep==vstep-1 ) vstep_print() ;
     if( !INMASK(kk) ) continue ;
     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;
     imar = THD_get_dset_nbhd_array( inset , mask , xx,yy,zz , nbhd ) ;
     if( imar == NULL ){ ERROR_message("get failure #%d",kk); continue; }
     if( polort >= 0 ){
       tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,0)) ;
       THD_generic_detrend_LSQ( nt , tsar , -1 , polort+1 , polyref , coef ) ;
       for( vv=1 ; vv < IMARR_COUNT(imar) ; vv++ ){
         tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,vv)) ;
         THD_generic_detrend_LSQ( nt , tsar , -1 , polort+1 , polyref , NULL ) ;
       }
     }
     pim  = mri_principal_vector( imar ) ; DESTROY_IMARR(imar) ;
     if( pim == NULL ){ ERROR_message("mpv failure #%d",kk); continue; }
     if( polort >= 0 && rebase && coef != NULL ){
       tsar = MRI_FLOAT_PTR(pim) ;
       for( vv=0 ; vv <= polort ; vv++ ){
         for( ii=0 ; ii < nt ; ii++ ) tsar[ii] += coef[vv]*polyref[vv][ii] ;
       }
     }
     THD_insert_series( kk, outset, nt, MRI_float, MRI_FLOAT_PTR(pim), 0 ) ;
     mri_free(pim) ;
     if( evset != NULL )
       THD_insert_series( kk, evset, nev, MRI_float, mri_principal_getev(), 0 );
   }
   if( vstep ) fprintf(stderr,"\n") ;

   /*** cleanup and exit ***/

   DSET_delete(inset) ;
   DSET_write(outset) ; WROTE_DSET(outset) ; DSET_delete(outset) ;

   if( evset != NULL ){
     DSET_write(evset) ; WROTE_DSET(evset) ; DSET_delete(evset) ;
   }

   exit(0) ;
}

/*------------------------------------------------------------------------*/

MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd )
{
   MRI_IMARR *imar ;
   int nvox, *ivox , nx,ny,nz , nxy,nxyz , npt, aa,bb,cc,kk,ii ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;

   kk = xx + yy*nx + zz*nxy ;
   if( kk < 0 || kk >= nxyz || !INMASK(kk) ) return NULL ;

   ivox = (int *)malloc(sizeof(int)*npt) ; nvox = 0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( INMASK(kk) ) ivox[nvox++] = kk ;
   }
   if( nvox == 0 ){ free(ivox) ; return NULL ; }

   imar = THD_extract_many_series( nvox, ivox, dset ) ;
   free(ivox) ; return imar ;
}

/*------------------------------------------------------------------------*/

static int mpv_vnorm = 0 ;
static int mpv_vproj = 0 ;

void mri_principal_vector_params( int a , int b )
{
   mpv_vnorm = a ; mpv_vproj = b ;
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
   int nx , nvec , ii,jj , itop ;
   double *amat , *umat , *vmat , *sval ;
   float *far ; MRI_IMAGE *tim ;
   float *vnorm=NULL ;
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

   /** assemble matrix for SVD-ization **/

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) = (double)far[ii] ;
   }

   /** normalize columns? **/

   if( mpv_vnorm ){
     vnorm = (float *)calloc(sizeof(float),nvec) ;
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
       vnorm[jj] = sqrt(sum) ;
       if( vnorm[jj] > 0.0 ){
         sum = 1.0 / vnorm[jj] ;
         for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) *= sum ;
       }
     }
   }

   /** all the CPU time dwells within **/

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   /** save eigenvalues **/

   itop = MIN(nvec,mpv_evnum) ;
   for( ii=0 ; ii < itop      ; ii++ ) mpv_ev[ii] = (float)sval[ii] ;
   for(      ; ii < mpv_evnum ; ii++ ) mpv_ev[ii] = 0.0f ;

   /** create output **/

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;            /* zero filled */

   if( mpv_vproj <= 0 ){  /* no projection: just save principal vector */

     for( ii=0 ; ii < nx ; ii++ ) far[ii] = (float)U(ii,0) ;
     sum = 0.0 ;
     for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,0)*far[ii] ;
     if( sum < 0.0 ) for( ii=0 ; ii < nx ; ii++ ) far[ii] = -far[ii] ;

   } else {  /* project input time series (1st column of A) onto subspace */

     int nproj = mpv_vproj ;
     if( nproj > nvec ) nproj = nvec ;
     if( nproj > nx   ) nproj = nx ;
     for( jj=0 ; jj < nproj ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += U(ii,jj) * A(ii,0) ;
       if( mpv_vnorm ) sum *= vnorm[0] ;
       for( ii=0 ; ii < nx ; ii++ ) far[ii] += sum * U(ii,jj) ;
     }

   }

   /** finito **/

   if( vnorm != NULL ) free(vnorm) ;
   free(sval); free(vmat); free(umat); free(amat); return tim;
}

/*------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}
