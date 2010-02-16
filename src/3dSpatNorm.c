#include "mrilib.h"
#include "thd_brainormalize.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *imin, *imout , *imout_orig;
   THD_3dim_dataset *iset, *oset , *ooset;
   char *prefix = "SpatNorm" ;
   int iarg , verb=0, OrigSpace = 0 , specie = HUMAN;
   float SpatNormDxyz= 0.0, iset_scaled=1.0;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz, originRAIfv, fv2;


   /*--- get help here or get help somewhere ---*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dSpatNorm [options] dataset\n"
            "\n"
            "Options:\n"
            "  -prefix ppp = Write output dataset using 'ppp' for the prefix.\n"
            "  -orig_space = Write output dataset using the same grid as dataset.\n"
            "  -verb       = Write out progress reports\n"
            "--------------------------------------------------------------------\n"
            "* This program is obsolete, and should not be used by most people. *\n"
            "--------------------------------------------------------------------\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*--- options ---*/

   iarg = 1 ;
   OrigSpace = 0;
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

     if( strcmp(argv[iarg],"-dxyz") == 0 ){
       if( ++iarg >= argc ){
         fprintf(stderr,"**ERROR: -dxyz requires another argument!\n") ;
         exit(1) ;
       }
       SpatNormDxyz = atof(argv[iarg]) ;
       
       iarg++ ; continue ;
     }
     
     if( strncmp(argv[iarg],"-verb",5) == 0 ){
       verb++ ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-monkey",5) == 0 ){
       specie = MONKEY ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-marmoset",5) == 0 ){
       specie = MARMOSET ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-rat",5) == 0 ){
       specie = RAT ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-orig_space",10) == 0 ){
       OrigSpace = 1 ; iarg++ ; continue ;
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

   if (specie == MARMOSET) {
      iset_scaled = 2.5;
      THD_volDXYZscale(iset->daxes, iset_scaled, 0);
      specie = MONKEY;
   }
   imin = THD_median_brick( iset ) ;
   if( imin == NULL ){
     fprintf(stderr,"**ERROR: can't load dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }
   imin->dx = fabs(iset->daxes->xxdel) ;
   imin->dy = fabs(iset->daxes->yydel) ;
   imin->dz = fabs(iset->daxes->zzdel) ;
   
   
   mri_speciebusiness(specie);
   if (SpatNormDxyz) {
      if (verb) fprintf(stderr,"Overriding default resampling\n");
      mri_brainormalize_initialize(SpatNormDxyz, SpatNormDxyz, SpatNormDxyz);
   } else {
      float xxdel, yydel, zzdel, minres;
      if (specie == MONKEY) minres = 0.5;
      else if (specie == MARMOSET) minres = 0.2;
      else if (specie == RAT) minres = 0.1;
      else minres = 0.5;
      /* don't allow for too low a resolution, please */
      if (imin->dx < minres) xxdel = minres;
      else xxdel = imin->dx;
      if (imin->dy < minres) yydel = minres;
      else yydel = imin->dy;
      if (imin->dz < minres) zzdel = minres;
      else zzdel = imin->dz;
      if (verb) {
         fprintf(stderr,"%s:\n Original resolution %f, %f, %f\n SpatNorm resolution %f, %f, %f\n",
                     "3dSpatnorm", imin->dx, imin->dy, imin->dz, 
                     xxdel, yydel, zzdel);
      }   
      mri_brainormalize_initialize(xxdel, yydel, zzdel);
   }
   
   mri_brainormalize_initialize(imin->dz, imin->dy, imin->dz); /* To get around the #define for voxel counts and dimensions */
   
   /* me needs the origin of this dset in RAI world */
   LOAD_FVEC3(originRAIfv , iset->daxes->xxorg , iset->daxes->yyorg , iset->daxes->zzorg) ;
   originRAIfv = THD_3dmm_to_dicomm( iset , originRAIfv ) ;

   LOAD_FVEC3(fv2 , iset->daxes->xxorg + (iset->daxes->nxx-1)*iset->daxes->xxdel ,
                    iset->daxes->yyorg + (iset->daxes->nyy-1)*iset->daxes->yydel ,
                    iset->daxes->zzorg + (iset->daxes->nzz-1)*iset->daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( iset , fv2 ) ;

   if( originRAIfv.xyz[0] > fv2.xyz[0] ) { float tf; tf = originRAIfv.xyz[0]; originRAIfv.xyz[0] = fv2.xyz[0]; fv2.xyz[0] = tf; } 
   if( originRAIfv.xyz[1] > fv2.xyz[1] ) { float tf; tf = originRAIfv.xyz[1]; originRAIfv.xyz[1] = fv2.xyz[1]; fv2.xyz[1] = tf; }
   if( originRAIfv.xyz[2] > fv2.xyz[2] ) { float tf; tf = originRAIfv.xyz[2]; originRAIfv.xyz[2] = fv2.xyz[2]; fv2.xyz[2] = tf; }
   
   if (verb) {
      fprintf(stderr,"++3dSpatNorm (ZSS): RAI origin info: %f %f %f\n", 
                     originRAIfv.xyz[0], originRAIfv.xyz[1], originRAIfv.xyz[2]);
   }
   
   
   DSET_unload( iset ) ;  /* don't need this data no more */

   /*-- convert image to shorts, if appropriate --*/

   if( DSET_BRICK_TYPE(iset,0) == MRI_short ||
       DSET_BRICK_TYPE(iset,0) == MRI_byte    ){

     imout = mri_to_short(1.0,imin) ;
     mri_free(imin) ; imin = imout ;
   }

   /*--- normalize image spatially ---*/

   mri_brainormalize_verbose( verb ) ;
   if (OrigSpace) {
      imout = mri_brainormalize( imin , iset->daxes->xxorient,
                                     iset->daxes->yyorient,
                                     iset->daxes->zzorient , &imout_orig, NULL) ;
   } else {
      imout = mri_brainormalize( imin , iset->daxes->xxorient,
                                     iset->daxes->yyorient,
                                     iset->daxes->zzorient , NULL, NULL) ;
   }
   mri_free( imin ) ;

   if( imout == NULL ){
     fprintf(stderr,"**ERROR: normalization fails!?\n"); exit(1);
   }
   
   if (OrigSpace) {
      if( verb ) fprintf(stderr,"++3dSpatNorm: Output in Orignal space\n") ;
      mri_free( imout ) ;
      imout = imout_orig; imout->xo = originRAIfv.xyz[0]; imout->yo = originRAIfv.xyz[1]; imout->zo = originRAIfv.xyz[2]; 
      imout_orig = NULL;
   } else {
      if( verb ) fprintf(stderr,"++3dSpatNorm: Output in SpatNorm space\n") ;
   }
   
#if 0
   if( AFNI_yesenv("WATERSHED") ){
     imin = mri_watershedize( imout , 0.10 ) ;
     if( imin != NULL ){ mri_free(imout); imout = imin; }
   }
#endif

   /*--- create output dataset ---*/
   if( verb )
     fprintf(stderr,"++3dSpatNorm: Creating output dset\n") ;

   oset = EDIT_empty_copy( NULL ) ;

   tross_Copy_History( iset , oset ) ;
   tross_Make_History( "3dSpatNorm" , argc,argv , oset ) ;

   LOAD_IVEC3( nxyz   , imout->nx    , imout->ny    , imout->nz    ) ;
   LOAD_FVEC3( dxyz   , imout->dx    , imout->dy    , imout->dz    ) ;
   LOAD_FVEC3( orgxyz , imout->xo    , imout->yo    , imout->zo    ) ;
   LOAD_IVEC3( orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE ) ;

   if( verb )
     fprintf(stderr,"++3dSpatNorm: EDIT_dset_items\n") ;
   EDIT_dset_items( oset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , imout->kind ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , ANAT_BUCK_TYPE ,
                    ADN_none ) ;

   if( verb )
     fprintf(stderr,"++3dSpatNorm: EDIT_substitute_brick\n") ;
   EDIT_substitute_brick( oset , 0 , imout->kind , mri_data_pointer(imout) ) ;

   if (OrigSpace) {
      if( verb )
         fprintf(stderr,"++3dSpatNorm: Changing orientation from RAI\n") ;
      ooset = r_new_resam_dset ( oset, iset, 0, 0, 0, NULL, MRI_NN, NULL, 1);
      if (!ooset) {
         fprintf(stderr,"**ERROR: Failed to reslice!?\n"); exit(1);
      }
      DSET_delete(oset); oset = ooset; ooset = NULL;
   }

   if (iset_scaled != 1.0f) 
      THD_volDXYZscale(oset->daxes,
                      1/iset_scaled, 0);

   DSET_write(oset) ;
   if( verb )
     fprintf(stderr,"++3dSpatNorm: wrote dataset %s\n",DSET_BRIKNAME(oset)) ;
   
   exit(0) ;
}
