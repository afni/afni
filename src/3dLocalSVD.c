#include "mrilib.h"

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

void mri_principal_vector_params( int a , int b , int c ) ;
MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar ) ;
MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd ) ;
static void vstep_print(void) ;

/*------------------------------------------------------------------------*/
/* Adapted from 3dLocalstat and 1dsvd */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./LocalSVD" ;
   int iarg=1 , verb=1 , ntype=0 , kk,nx,ny,nz,nxy,nxyz,nt , xx,yy,zz, vstep ;
   float na,nb,nc , dx,dy,dz ;
   MRI_IMARR *imar=NULL ; MRI_IMAGE *pim=NULL ;
   int do_vmean=0 , do_vnorm=0 , do_vproj ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalSVD [options] inputdataset\n"
       "* You may want to use 3dDetrend before running this program.\n"
       "* This program is highly experimental.  And slowish.\n"
       "* Computes the SVD of time series from a neighborhood of each\n"
       "   voxel.  An inricate way of 'smoothing' 3D+time datasets.\n"
       "\n"
       "Options:\n"
       " -mask mset\n"
       " -automask\n"
       " -prefix ppp\n"
       " -input inputdataset\n"
       " -nbhd nnn\n"
       " -vmean\n"
       " -vnorm\n"
       " -vproj\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalSVD"); mainENTRY("3dLocalSVD main"); machdep();
   AFNI_logger("3dLocalSVD",argc,argv); AUTHOR("Emperor Zhark");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-vmean") == 0 ){
       do_vmean = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vproj") == 0 ){
       do_vproj = 1 ; iarg++ ; continue ;
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

   mri_principal_vector_params( do_vmean , do_vnorm , do_vproj ) ;

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   nt = DSET_NVALS(inset) ;
   if( nt < 2 )
     ERROR_exit("Must have at least 2 values per voxel") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

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

   nx = DSET_NX(outset) ;
   ny = DSET_NY(outset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(outset) ; nxyz = nxy*nz ;
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;

   for( kk=0 ; kk < nxyz ; kk++ ){
     if( vstep && kk%vstep==vstep-1 ) vstep_print() ;
     if( !INMASK(kk) ) continue ;
     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;
     imar = THD_get_dset_nbhd_array( inset , mask , xx,yy,zz , nbhd ) ;
     if( imar == NULL ){ ERROR_message("get failure #%d",kk); continue; }
     pim  = mri_principal_vector( imar ) ; DESTROY_IMARR(imar) ;
     if( pim == NULL ){ ERROR_message("mpv failure #%d",kk); continue; }
     THD_insert_series( kk, outset, nt, MRI_float, MRI_FLOAT_PTR(pim), 0 ) ;
     mri_free(pim) ;
   }
   if( vstep ) fprintf(stderr,"\n") ;

   DSET_delete(inset) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;

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

static int mpv_vmean = 0 ;
static int mpv_vnorm = 0 ;
static int mpv_vproj = 0 ;

void mri_principal_vector_params( int a , int b , int c )
{
   mpv_vmean = a ; mpv_vnorm = b ; mpv_vproj = c ;
}

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

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;
   for( ii=0 ; ii < nx ; ii++ ) far[ii] = (float)U(ii,0) ;

   sum = 0.0 ;
   for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,0)*far[ii] ;
   if( mpv_vproj && sum != 0.0 ){
     float fac = (float)sum ;
     for( ii=0 ; ii < nx ; ii++ ) far[ii] *= fac ;
   } else if( sum < 0.0 ){
     for( ii=0 ; ii < nx ; ii++ ) far[ii] = -far[ii] ;
   }

   if( vnorm != 0.0f )
     for( ii=0 ; ii < nx ; ii++ ) far[ii] *= vnorm ;
   if( vmean != 0.0f )
     for( ii=0 ; ii < nx ; ii++ ) far[ii] += vmean ;

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
