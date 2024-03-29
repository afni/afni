#include "mrilib.h"

// [PT, Dec, 2016] update to allow ijk output in local orientation

int main( int argc , char * argv[] )
{
   int narg=1, do_automask=0 , iv , nxyz , do_set=0 , 
      *rois=NULL, N_rois=0, all_rois = 0;
   THD_3dim_dataset *xset ;
   byte *mmm=NULL ; int nmask=0 , nvox_mask=0 ;
   THD_fvec3 cmv , setv ;

   int cmode = 0; // default: return xyz in DICOM
   int cm_cmode;
   int LocalHead = wami_lh();
   int Icent = 0;  // compute Icent internal center
   int Dcent = 0;  // compute Dcent distance center
        
   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dCM [options] dset\n"
"Output = center of mass of dataset, to stdout.\n"
"    Note: by default, the output is (x,y,z) values in DICOM\n"
"          coordinates.  But as of Dec, 2016, there are now\n"
"          command line switches for other options (see -local*\n"
"          below).\n\n"
"  -mask mset   Means to use the dataset 'mset' as a mask:\n"
"                 Only voxels with nonzero values in 'mset'\n"
"                 will be averaged from 'dataset'.  Note\n"
"                 that the mask dataset and the input dataset\n"
"                 must have the same number of voxels.\n"
"  -automask    Generate the mask automatically.\n"
"  -set x y z   After computing the CM of the dataset, set the\n"
"                 origin fields in the header so that the CM\n"
"                 will be at (x,y,z) in DICOM coords.\n"
"  -local_ijk   Output values as (i,j,k) in local orientation.\n"
"  -roi_vals v0 v1 v2 ... : Compute center of mass for each blob\n"
"                           with voxel value of v0, v1, v2, etc.\n"
"                           This option is handy for getting ROI \n"
"                           centers of mass.\n"
"  -all_rois     Don't bother listing the values of ROIs you want\n"
"                the program will find all of them and produce a \n"
"                full list.\n"
"  -Icent Compute Internal Center. For some shapes, the center can\n"
"          lie outside the shape. This option finds the location\n"
"          of the center of a voxel closest to the center of mass\n"
"          It will be the same or similar to a center of mass\n"
"          if the CM lies within the volume. It will lie necessarily\n"
"          on an edge voxel if the CMass lies outside the volume\n" 
"  -Dcent Compute Distance Center, i.e. the center of the voxel\n"
"          that has the shortest average distance to all the other\n"
"          voxels. This is much more computational expensive than\n"
"          Cmass or Icent centers\n"
"  NOTE: Masking options are ignored with -roi_vals and -all_rois\n"
             ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   LOAD_FVEC3(setv,0,0,0) ;   /* ZSS: To quiet init. warnings */
   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-set") == 0 ){
         float xset,yset,zset ;
         if( narg+3 >= argc ){
            fprintf(stderr,"*** -set need 3 args following!\n") ; exit(1) ;
         }
         xset = strtod( argv[++narg] , NULL ) ;
         yset = strtod( argv[++narg] , NULL ) ;
         zset = strtod( argv[++narg] , NULL ) ;
         LOAD_FVEC3(setv,xset,yset,zset) ; do_set = 1 ;
         THD_set_write_compression(COMPRESS_NONE); // do not alter compression
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         THD_3dim_dataset *mask_dset ;
         if( mmm != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; 
            exit(1) ;
         }
         if( do_automask ){
            fprintf(stderr,"*** Can't have -mask and -automask!\n") ;
            exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,
                    "*** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         CHECK_OPEN_ERROR(mask_dset,argv[narg]) ;
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,
                    "*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         mmm = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
         nvox_mask = DSET_NVOX(mask_dset) ;
         nmask = THD_countmask( nvox_mask , mmm ) ;
         if( mmm == NULL || nmask <= 0 ){
            fprintf(stderr,
                    "*** Can't make mask from dataset %s\n", argv[narg-1]);
            exit(1) ;
         }
         DSET_delete( mask_dset ) ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-roi_vals",5) == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,
                    "*** -mask option requires a following argument(s)!\n");
            exit(1) ;
         }
         rois = (int *)calloc(argc, sizeof(int));
         N_rois = 0;
         ++narg;
         while (narg < argc-1 && argv[narg][0] != '-') {
            rois[N_rois++] = atoi(argv[narg]);
            ++narg;
         }
        
         continue ;
      }
      if( strncmp(argv[narg],"-all_rois",5) == 0 ){
         all_rois = 1;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-Icent") == 0 ){
         Icent = 1;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-Dcent") == 0 ){
         Dcent = 1;
         narg++ ; continue ;
      }
      
      if( strcmp(argv[narg],"-automask") == 0 ){
         if( mmm != NULL ){
            fprintf(stderr,"*** Can't have -mask and -automask!\n") ; exit(1) ;
         }
         do_automask = 1 ; narg++ ; continue ;
      }

      // (PT, Dec, 2016) New opts
      if( strcmp(argv[narg],"-local_ijk") == 0 ){
         fprintf(stderr,"# Output: ijk in local orientation.\n");
         cmode = 1 ; narg++ ; continue ;
      }


      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have at least 1 more argument */

   if( argc <= narg ){
      fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

   for( ; narg < argc ; narg++ ){
      xset = THD_open_dataset( argv[narg] ) ;
      if( xset == NULL ){
         fprintf(stderr,"+++ Can't open dataset %s -- skipping\n",argv[narg]);
         continue ;
      }
      DSET_load(xset) ;
      if( !DSET_LOADED(xset) ){
         fprintf(stderr,"+++ Can't load dataset %s -- skipping\n",argv[narg]);
         DSET_delete(xset) ; continue ;
      }
      if( do_automask ){
         if( mmm != NULL ){ free(mmm); mmm = NULL; }
         mmm = THD_automask(xset) ;
         nvox_mask = DSET_NVOX(xset) ;
         nmask = THD_countmask( nvox_mask , mmm ) ;
         if( mmm == NULL || nmask <= 0 ){
            fprintf( stderr,
                     "+++ Can't make automask from dataset %s "
                     "-- skipping\n",argv[narg]) ;
            DSET_delete(xset) ; continue ;
         }
      }
      nxyz = DSET_NVOX(xset) ;
      if( mmm != NULL && nxyz != nvox_mask ){
         fprintf(stderr,
                 "+++ Mask/Dataset grid size mismatch at %s\n -- skipping\n",
                 argv[narg]) ;
         DSET_delete(xset) ; continue ;
      }

      if (all_rois) {
         if(!(rois = THD_unique_vals(xset, 0, &N_rois, NULL)) || N_rois == 0){
            ERROR_message("No rois or error in THD_unique_vals"); continue;
         }
         fprintf(stderr,"#%d distinct ROIs\n", N_rois);
      }
     
      if (!N_rois) {
         if(Icent || Dcent)
            cm_cmode = 0;
         else
            cm_cmode = cmode;    
         // [PT, Dec, 2016] allow integer ijk output
         cmv = THD_cmass( xset , 0 , mmm, cm_cmode ) ;
         /*if( cmode == 1 ) // integer valued
           printf("%d  %d  %d\n", (int) cmv.xyz[0], 
           (int) cmv.xyz[1], (int) cmv.xyz[2]) ;
           else*/
         if(!Icent && !Dcent)
            printf("%g  %g  %g\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
         else {
            if(LocalHead)
               printf("%g  %g  %g  Center of Mass\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
            if(Icent){
              cmv = THD_Icent( xset , 0 , mmm, cmode, cmv);
              printf("%g  %g  %g\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
            }
            if(Dcent){
              cmv = THD_Dcent( xset , 0 , mmm, cmode, cmv);
              printf("%g  %g  %g\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
            }
         }
         DSET_unload(xset) ;

         if( do_set ){
            THD_fvec3 dv , ov ;
            if(  DSET_IS_MASTERED(xset) ){
               fprintf(stderr, "+++ Can't modify CM of dataset %s\n",
                       argv[narg]) ;
            } else {
               /* lose obliquity */
               /* recompute Tc(Cardinal transformation matrix for new
                  grid output */
               THD_make_cardinal(xset);

               LOAD_FVEC3(ov,DSET_XORG(xset),DSET_YORG(xset),DSET_ZORG(xset)) ;
               ov = THD_3dmm_to_dicomm( xset , ov ) ;
               dv = SUB_FVEC3(setv,cmv) ;
               ov = ADD_FVEC3(dv,ov) ;
               ov = THD_dicomm_to_3dmm( xset , ov ) ;
               xset->daxes->xxorg = ov.xyz[0] ;
               xset->daxes->yyorg = ov.xyz[1] ;
               xset->daxes->zzorg = ov.xyz[2] ;
               /* allow overwriting header for all types of output data */
               putenv("AFNI_DECONFLICT=OVERWRITE") ;
               tross_Make_History("3dCM", argc, argv, xset );// ZSS Dec. 09 08
               if(DSET_IS_BRIK(xset)) {
                  INFO_message("Rewriting header %s",DSET_HEADNAME(xset)) ;
                  DSET_overwrite_header( xset ) ;
               }
               else { // for other dset types like NIFTI, rewrite whole dset
                  DSET_load( xset ) ;
                  DSET_overwrite(xset) ;
                  INFO_message("Wrote new dataset: %s",DSET_BRIKNAME(xset)) ;
               }   
            }
         }
      } else {
         float *xyz;
         if ((xyz = THD_roi_cmass(xset , 0 , rois, N_rois, cmode))) {
            printf("#Dset %s\n",DSET_BRIKNAME(xset));
            for (iv=0; iv<N_rois; ++iv) {
               printf("#ROI %d\n", rois[iv]);
               printf("%g  %g  %g\n",xyz[3*iv],xyz[3*iv+1],xyz[3*iv+2]) ;
            }
            free(xyz); free(rois); 
         } else {
            ERROR_message("Failed in THD_roi_cmass"); continue;
         }
      }
      DSET_delete(xset) ;
   }
   
   exit(0);
}
