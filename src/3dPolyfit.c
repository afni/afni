#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float mrad=0.0f ;
   int nrep=1 ;
   char *prefix = "Polyfit" ;
   int iarg , verb=0 , do_automask=0 , nord=3 , meth=2 , do_mclip=0 ;
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *imout , *imin ;
   byte *mask=NULL ; int nvmask=0 , nmask=0 , do_mone=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dPolyfit [options] dataset\n"
             "Fits a polynomial in space to the dataset and outputs that.\n"
             "\n"
             "Options:\n"
             "  -nord n    = Maximum polynomial order (0..9) [default=3]\n"
             "  -mrad x    = Radius in voxels of preliminary median filter\n"
             "  -prefix pp = Use 'pp' for prefix of output dataset\n"
             "  -automask  = Create a mask (a la 3dAutomask)\n"
             "  -mask mset = Create a mask from nonzero voxels in 'mset'.\n"
             "  -mone      = Scale the mean value of the fit (inside the mask) to 1.\n"
             "  -mclip     = Clip values outside the box containing the mask\n"
             "               to the edge of the box, to avoid weird artifacts.\n"
             "  -meth mm   = Set 'mm' to 2 for least squares fit; set it\n"
             "               to 1 for L1 fit [default=2]\n"
             "  -verb      = Print fun and useful progress reports!\n"
             "\n"
             "Output dataset is always stored in float format.  If the input\n"
             "dataset has more than 1 sub-brick, only sub-brick #0 is processed.\n"
             "\n"
             "-- Dec 2010 - RWCox - for no good reason\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dPolyfit main"); machdep(); AFNI_logger("3dPolyfit",argc,argv);
   PRINT_VERSION("3dPolyfit") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || do_automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       nvmask = DSET_NVOX(mset) ;
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       nmask = THD_countmask( nvmask , mask ) ;
       if( nmask < 99 ) ERROR_exit("Too few voxels in mask (%d)",nmask) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nord") == 0 ){
       nord = (int)strtol( argv[++iarg], NULL , 10 ) ;
       if( nord < 0 || nord > 9 )
         ERROR_exit("Illegal value after -nord!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-meth") == 0 ){
       meth = (int)strtol( argv[++iarg], NULL , 10 ) ;
       if( nord < 1 || nord > 2 )
         ERROR_exit("Illegal value after -meth!") ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-automask",5) == 0 ){
       if( mask != NULL ) ERROR_exit("Can't use -mask and -automask together!") ;
       do_automask++ ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mclip",5) == 0 ){
       do_mclip++ ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mone",5) == 0 ){
       do_mone++ ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-verb",5) == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mrad") == 0 ){
       mrad = strtod( argv[++iarg] , NULL ) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal value after -prefix!\n");
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   if( iarg >= argc )
     ERROR_exit("No dataset name on command line?\n");

   /*-- read input --*/

   if( verb ) INFO_message("Load input dataset") ;

   inset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message("Only processing sub-brick #0 !!");

   if( mask != NULL ){
     if( nvmask != DSET_NVOX(inset) )
      ERROR_exit("-mask and input datasets don't match in voxel counts :-(") ;
   } else if( do_automask ){
     THD_automask_verbose( (verb > 1) ) ;
     THD_automask_extclip( 1 ) ;
     mask = THD_automask( inset ) ; nvmask = DSET_NVOX(inset) ;
     nmask = THD_countmask( nvmask , mask ) ;
     if( nmask < 99 ) ERROR_exit("Too few voxels in automask (%d)",nmask) ;
     if( verb ) ININFO_message("Number of voxels in automask = %d",nmask) ;
   }

   /* convert input to float, which is simpler to deal with */

   imin = THD_extract_float_brick(0,inset) ;
   if( imin == NULL ) ERROR_exit("Can't extract input dataset brick?! :-(") ;
   DSET_unload(inset) ;

   if( verb ) INFO_message("Start fitting process") ;
   mri_polyfit_verb(verb) ;
   imout = mri_polyfit( imin , nord , mask , mrad , meth ) ;

   if( imout == NULL )
     ERROR_exit("Can't compute polynomial fit :-( !?") ;
   free(imin) ;

   if( do_mone ){
     float sum=0.0f ; int nsum=0 , ii,nvox ; float *par=MRI_FLOAT_PTR(imout) ;
     nvox = imout->nvox ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mask != NULL && mask[ii] == 0 ) continue ;
       sum += par[ii] ; nsum++ ;
     }
     if( nsum > 0 && sum != 0.0f ){
       sum = nsum / sum ;
       if( verb ) INFO_message("-mone: scaling fit by %g",sum) ;
       for( ii=0 ; ii < nvox ; ii++ ) par[ii] *= sum ;
     }
   }

   /* if there's a mask, clip values outside of it */

#undef  PF
#define PF(i,j,k) par[(i)+(j)*nx+(k)*nxy]
   if( mask != NULL && do_mclip ){
     int xm,xp,ym,yp,zm,zp , ii,jj,kk , nx,ny,nz,nxy ; float *par ;
     MRI_IMAGE *bim = mri_empty_conforming( imout , MRI_byte ) ;
     mri_fix_data_pointer(mask,bim) ;
     if( verb ) INFO_message("-mclip: polynomial fit to autobox of mask") ;
     MRI_autobbox( bim , &xm,&xp , &ym,&yp , &zm,&zp ) ;
     mri_clear_data_pointer(bim) ; mri_free(bim) ;
     nx = imout->nx ; ny = imout->ny ; nz = imout->nz ; nxy = nx*ny ;
     par = MRI_FLOAT_PTR(imout) ;
     for( ii=0 ; ii < xm ; ii++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ ) PF(ii,jj,kk) = PF(xm,jj,kk) ;
     for( ii=xp+1 ; ii < nx ; ii++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ ) PF(ii,jj,kk) = PF(xp,jj,kk) ;
     for( jj=0 ; jj < ym ; jj++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,ym,kk) ;
     for( jj=yp+1 ; jj < ny ; jj++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,yp,kk) ;
     for( kk=0 ; kk < zm ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,jj,zm) ;
     for( kk=zp+1 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,jj,zp) ;
   }

   if( mask != NULL ) free(mask) ;

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                      ADN_prefix , prefix ,
                      ADN_nvals  , 1 ,
                      ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dPolyfit" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
