#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *imin, *imout ;
   THD_3dim_dataset *iset, *oset ;
   char *prefix = "SpatNorm" ;
   int iarg , verb=0 ;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;


   /*--- get help here or get help somewhere ---*/

   if( argc < 2 ){
     printf("Usage: 3dSpatNorm [options] dataset\n"
            "\n"
            "Options:\n"
            "  -prefix ppp = Write output dataset using 'ppp' for the prefix.\n"
            "  -verb       = Write out progress reports\n"
           ) ;
     exit(0) ;
   }

   /*--- options ---*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /* -prefix */

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ){
         fprintf(stderr,"**ERROR: -prefix requires another argument!\n") ;
         exit(1) ;
       }
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"**ERROR: -prefix value contains forbidden characters!\n") ;
         exit(1) ;
       }
       iarg++ ; continue ;
     }

     /* -verb */

     if( strncmp(argv[iarg],"-verb",5) == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     fprintf(stderr,"**ERROR: %s is unknown option!\n",argv[iarg]) ;
     exit(1) ;
   }

   if( iarg >= argc ){
     fprintf(stderr,"**ERROR: no input dataset name on command line?!\n") ;
     exit(1) ;
   }

   /*--- read dataset ---*/

   iset = THD_open_dataset( argv[iarg] ) ;
   if( !ISVALID_DSET(iset) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }

   /*--- get median brick --*/

   if( verb ) fprintf(stderr,"++3dSpatNorm: loading dataset\n") ;

   imin = THD_median_brick( iset ) ;
   if( imin == NULL ){
     fprintf(stderr,"**ERROR: can't load dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }
   imin->dx = fabs(iset->daxes->xxdel) ;
   imin->dy = fabs(iset->daxes->yydel) ;
   imin->dz = fabs(iset->daxes->zzdel) ;

   DSET_unload( iset ) ;  /* don't need this data no more */

   /*-- convert image to shorts, if appropriate --*/

   if( DSET_BRICK_TYPE(iset,0) == MRI_short ||
       DSET_BRICK_TYPE(iset,0) == MRI_byte    ){

     imout = mri_to_short(1.0,imin) ;
     mri_free(imin) ; imin = imout ;
   }

   /*--- normalize image spatially ---*/

   mri_brainormalize_verbose( verb ) ;
   imout = mri_brainormalize( imin , iset->daxes->xxorient,
                                     iset->daxes->yyorient,
                                     iset->daxes->zzorient ) ;
   mri_free( imin ) ;

   if( imout == NULL ){
     fprintf(stderr,"**ERROR: normalization fails!?\n"); exit(1);
   }

   if( AFNI_yesenv("WATERSHED") ){
     imin = mri_watershedize( imout , 0.25 ) ;
     if( imin != NULL ){ mri_free(imout); imout = imin; }
   }

   /*--- create output dataset ---*/

   oset = EDIT_empty_copy( NULL ) ;

   tross_Copy_History( iset , oset ) ;
   tross_Make_History( "3dSpatNorm" , argc,argv , oset ) ;

   LOAD_IVEC3( nxyz   , imout->nx    , imout->ny    , imout->nz    ) ;
   LOAD_FVEC3( dxyz   , imout->dx    , imout->dy    , imout->dz    ) ;
   LOAD_FVEC3( orgxyz , imout->xo    , imout->yo    , imout->zo    ) ;
   LOAD_IVEC3( orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE ) ;

   EDIT_dset_items( oset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , MRI_short ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , ANAT_BUCK_TYPE ,
                    ADN_none ) ;

   EDIT_substitute_brick( oset , 0 , imout->kind , mri_data_pointer(imout) ) ;

   if( verb )
     fprintf(stderr,"++3dSpatNorm: writing dataset with prefix=%s\n",prefix) ;

   DSET_write(oset) ;
   exit(0) ;
}
