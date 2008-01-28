#include "mrilib.h"

MRI_IMAGE * mri_jointhist( MRI_IMAGE *imp , MRI_IMAGE *imq , byte *mmm ) ;

int main( int argc , char * argv[] )
{
   int narg , ndset , nvox , nvals,iv ;
   THD_3dim_dataset *xset , *yset , *mask_dset=NULL , *hset ;
   byte *mmm=NULL ;
   MRI_IMAGE *imh , *hdim ;
   float *har , *hdar ;
   char *prefix = "jhist" ;
   THD_ivec3 nxyz ;
   THD_fvec3 dxyz ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3JointHist [options] dset1 dset2\n"
             "Output = dataset of joint histogram between dset1[0] and\n"
             "         each sub-brick of dset2 (1 histogram per slice).\n"
             "Options:\n"
             "  -mask mset  = Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.\n"
             "  -prefix ppp = If you don't know this by now, you shouldn't\n"
             "                 even THINK about using this program!\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

   if( strncmp(argv[narg],"-mask",5) == 0 ){
     if( mask_dset != NULL ) ERROR_exit("Can't use -mask twice") ;
       if( narg+1 >= argc ) ERROR_exit("Need argument after -mask") ;
       mask_dset = THD_open_dataset( argv[++narg] ) ;
       if( mask_dset == NULL ) ERROR_exit("Can't open -mask %s",argv[narg]) ;
       narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-prefix") == 0 ){
       if( narg+1 >= argc ) ERROR_exit("Need argument after -prefix") ;
       prefix = argv[++narg] ;
       narg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s",argv[narg]) ;
   }

   /* should have at least 2 more arguments */

   ndset = argc - narg ;
   if( ndset <= 1 ) ERROR_exit("Need two input datasets") ;

   xset = THD_open_dataset( argv[narg++] ) ;
   yset = THD_open_dataset( argv[narg++] ) ;
   if( xset == NULL || yset == NULL )
     ERROR_exit("Cannot open both input datasets!\n") ;

   if( DSET_NVALS(xset) > 1 )
     WARNING_message("Will only use sub-brick #0 of 1st input dataset") ;
   nvals = DSET_NVALS(yset) ;

   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) )
     ERROR_exit("Input datasets grid dimensions don't match!\n") ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
     int mcount ;
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!\n");
     DSET_load(mask_dset) ; CHECK_LOAD_ERROR(mask_dset) ;
     mmm = THD_makemask( mask_dset , 0 , 1.0f,-1.0f ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     INFO_message("Have %d voxels in the mask\n",mcount) ;
     if( mcount <= 666 ) ERROR_exit("Mask is too small") ;
     DSET_delete(mask_dset) ;
   }

   INFO_message("loading 1st dataset") ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;
   INFO_message("loading 2nd dataset") ;
   DSET_load(yset) ; CHECK_LOAD_ERROR(yset) ;

   hset = EDIT_empty_copy(NULL) ;
   nxyz.ijk[0] = nxyz.ijk[1] = 256 ; nxyz.ijk[2] = nvals ;
   dxyz.xyz[0] = dxyz.xyz[1] = dxyz.xyz[2] = 1.0f ;
   EDIT_dset_items( hset ,
                      ADN_prefix     , prefix ,
                      ADN_datum_all  , MRI_float ,
                      ADN_nvals      , 1 ,
                      ADN_type       , HEAD_ANAT_TYPE ,
                      ADN_view_type  , xset->view_type ,
                      ADN_func_type  , ANAT_MRAN_TYPE ,
                      ADN_nxyz       , nxyz ,
                      ADN_xyzdel     , dxyz ,
                      ADN_malloc_type, DATABLOCK_MEM_MALLOC ,
                    ADN_none ) ;
   EDIT_substitute_brick( hset , 0 , MRI_float , NULL ) ;
   hdim = DSET_BRICK(hset,0) ; hdar = MRI_FLOAT_PTR(hdim) ;

   fprintf(stderr,"++ histogram-izing") ;
   for( iv=0 ; iv < nvals ; iv++ ){
     fprintf(stderr,".") ;
     imh = mri_jointhist( DSET_BRICK(xset,0), DSET_BRICK(yset,iv), mmm ) ;
     har = MRI_FLOAT_PTR(imh) ;
     memcpy( hdar+(256*256*iv) , har , 256*256*sizeof(float) ) ;
     mri_free(imh) ;
   }
   fprintf(stderr,"\n") ;

   DSET_write(hset) ;
   WROTE_DSET(hset) ;
   exit(0) ;
}

/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_jointhist( MRI_IMAGE *imp , MRI_IMAGE *imq , byte *mmm )
{
   int nvox , nmmm=0 ;
   float *rst ;
   byte *par, *qar ;
   float fac ;
   register int ii,jj,kk ;
   MRI_IMAGE *imqq, *impp , *imh ;

   if( imp == NULL || imq == NULL || imp->nvox != imq->nvox ) return NULL;

   nvox = imp->nvox ;

   impp = (imp->kind==MRI_byte) ? imp : mri_to_byte(imp) ;
   imqq = (imq->kind==MRI_byte) ? imq : mri_to_byte(imq) ;
   par  = MRI_BYTE_PTR(impp) ;
   qar  = MRI_BYTE_PTR(imqq) ;
   imh  = mri_new( 256,256,MRI_float ) ;
   rst  = MRI_FLOAT_PTR(imh) ;

   if( mmm != NULL ){
     for( kk=0 ; kk < nvox ; kk++ ) if( mmm[kk] ) nmmm++ ;
     fac = 1.0f / nmmm ;
     for( kk=0 ; kk < nvox ; kk++ ){
       if( mmm[kk] == 0 ) continue ;
       ii = par[kk] ; jj = qar[kk] ; rst[ii+256*jj] += fac ;
     }
   } else {
     fac = 1.0f / nvox ;
     for( kk=0 ; kk < nvox ; kk++ ){
       ii = par[kk] ; jj = qar[kk] ; rst[ii+256*jj] += fac ;
     }
   }

   if( impp != imp ) mri_free(impp) ;
   if( imqq != imq ) mri_free(imqq) ;

   return imh ;
}
