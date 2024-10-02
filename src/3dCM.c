#include "mrilib.h"

// [PT, Dec, 2016] update to allow ijk output in local orientation

/*
  [PT: Apr 2024] So that XYZ coords will consistently in RAI-Dicom
  format by default, resample the input dset and then process it,
  *unless* '-local_ijk' is used.
*/
 
int dset_get_orient( THD_3dim_dataset *ddd, 
                     char *ori );

// Give all dsets a consistent origin, and be able to change
int dset_get_orient( THD_3dim_dataset *ddd, 
                     char *ori )
{
   
   // store original order
   ori[0]=ORIENT_typestr[ddd->daxes->xxorient][0];
   ori[1]=ORIENT_typestr[ddd->daxes->yyorient][0];
   ori[2]=ORIENT_typestr[ddd->daxes->zzorient][0];
   ori[3]='\0';

   //INFO_message(" DSET orient:  %s", ori);
   return 0;
}

void usage_3dCM() 
{
   printf("\n"
"Usage: 3dCM [options] dset\n"
"\n"
"Output = center of mass of dataset, to stdout.\n"
"    Note: by default, the output is (x,y,z) values in RAI-DICOM\n"
"          coordinates.  But as of Dec, 2016, there are now\n"
"          command line switches for other options (see -local*\n"
"          below).\n\n"
"\n"
"  -mask mset   :Means to use the dataset 'mset' as a mask:\n"
"                 Only voxels with nonzero values in 'mset'\n"
"                 will be averaged from 'dataset'.  Note\n"
"                 that the mask dataset and the input dataset\n"
"                 must have the same number of voxels.\n"
"\n"
"  -automask    :Generate the mask automatically.\n"
"\n"
"  -set x y z   :After computing the CM of the dataset, set the\n"
"                 origin fields in the header so that the CM\n"
"                 will be at (x,y,z) in DICOM coords.\n"
"\n"
"  -local_ijk   :Output values as (i,j,k) in local orientation.\n"
"\n"
"  -roi_vals v0 v1 v2 ... :Compute center of mass for each blob\n"
"                           with voxel value of v0, v1, v2, etc.\n"
"                           This option is handy for getting ROI \n"
"                           centers of mass.\n"
"\n"
"  -all_rois    :Don't bother listing the values of ROIs you want\n"
"                the program will find all of them and produce a \n"
"                full list.\n"
"\n"
"  -Icent :Compute Internal Center. For some shapes, the center can\n"
"          lie outside the shape. This option finds the location\n"
"          of the center of a voxel closest to the center of mass\n"
"          It will be the same or similar to a center of mass\n"
"          if the CM lies within the volume. It will lie necessarily\n"
"          on an edge voxel if the CMass lies outside the volume\n" 
"\n"
"  -Dcent :Compute Distance Center, i.e. the center of the voxel\n"
"          that has the shortest average distance to all the other\n"
"          voxels. This is much more computational expensive than\n"
"          Cmass or Icent centers\n"
"\n"
" -rep_xyz_orient RRR :when reporting (x,y,z) coordinates, use the\n"
"                specified RRR orientation (def: RAI).\n"
"                NB: this does not apply when using '-local_ijk',\n"
"                and will not change the orientation of the dset\n"
"                when using '-set ..'.\n"
"\n"
"  NOTE: Masking options are ignored with -roi_vals and -all_rois\n"
);
	return;
}

int main( int argc , char * argv[] )
{
   int narg=1, do_automask=0 , iv , nxyz , do_set=0 , 
      *rois=NULL, N_rois=0, all_rois = 0;
   THD_3dim_dataset *xset = NULL ;
   byte *mmm=NULL ; int nmask=0 , nvox_mask=0 ;
   THD_fvec3 cmv , setv ;

   int cmode = 0; // default: return xyz in DICOM
   int cm_cmode;
   int LocalHead = wami_lh();
   int Icent = 0;  // compute Icent internal center
   int Dcent = 0;  // compute Dcent distance center
   
   THD_3dim_dataset *mask_dset = NULL ;

   // [PT: Apr 23, 2024] for resampling internally
   char *dset_orient_ref   = NULL ;  // internally resampling all to this
	char dset_orient_inp[4] = "   ";  // [4]="---";
	char dset_orient_ext[4] = "   ";  // [4]="---";
   THD_3dim_dataset *tmpset = NULL;
   char tmppref[THD_MAX_PREFIX];
   int i = 0;

   mainENTRY("3dCM main") ; machdep() ;

   /*-- read command line arguments --*/
   if (argc == 1) { usage_3dCM(); exit(0); }

   LOAD_FVEC3(setv,0,0,0) ;   /* ZSS: To quiet init. warnings */
   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){
      if( strcmp(argv[narg],"-help") == 0 || 
          strcmp(argv[narg],"-h") == 0 ) {
         usage_3dCM();
         exit(0);
      }

      if( strcmp(argv[narg],"-set") == 0 ){
         float xin,yin,zin ;
         if( narg+3 >= argc ){
            fprintf(stderr,"*** -set need 3 args following!\n") ; exit(1) ;
         }
         xin = strtod( argv[++narg] , NULL ) ;
         yin = strtod( argv[++narg] , NULL ) ;
         zin = strtod( argv[++narg] , NULL ) ;
         LOAD_FVEC3(setv,xin,yin,zin) ; do_set = 1 ;
         THD_set_write_compression(COMPRESS_NONE); // do not alter compression
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
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
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-rep_xyz_orient") == 0 ){
			narg++ ; if( narg >= argc ) 
							ERROR_exit("Need argument after '-rep_xyz_orient'");
         dset_orient_ref = strdup(argv[narg]);
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

   /* set default orient, if not specified by user */ 

   if ( dset_orient_ref == NULL ) 
      dset_orient_ref = strdup("RAI");

   /* process the mask, as necessary */

   if ( mask_dset ) {
      // [PT: Apr 23, 2024] see note from same date below about
      // (maybe) initial resampling; use same 'if' condition as below
      i = dset_get_orient( mask_dset, dset_orient_inp);
      if ( cmode != 1 && strcmp(dset_orient_inp, dset_orient_ref) != 0 ) {
         MCW_strncpy( tmppref, DSET_PREFIX(mask_dset), THD_MAX_PREFIX ) ;
         tmpset = r_new_resam_dset( mask_dset, NULL, 0.0, 0.0, 0.0,
                                    dset_orient_ref, RESAM_NN_TYPE, 
                                    NULL, 1, 0);   
         DSET_delete(mask_dset);  mask_dset=tmpset;  tmpset=NULL;
         EDIT_dset_items( mask_dset, ADN_prefix, tmppref, ADN_none);
      }

      mmm = THD_makemask( mask_dset, 0, 1.0, 0.0 );
      nvox_mask = DSET_NVOX(mask_dset);
      nmask = THD_countmask( nvox_mask , mmm );
      if( mmm == NULL || nmask <= 0 ){
         fprintf(stderr, "** Mask dset input is not loading well.");
         exit(1) ;
      }
   }

   /* now go through all datasets and do the work */

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

      // [PT: Apr 23, 2024] Insert resampling as necessary. Here,
      // 'necessary' means that the input data wasn't already in the
      // 'correct' orientation, and also that '-local_ijk' was not
      // used. NB: use same 'if' condition and resampling for mask, above.
      i = dset_get_orient( xset, dset_orient_inp);
      if ( cmode != 1 && strcmp(dset_orient_inp, dset_orient_ref) != 0 ) {
         MCW_strncpy( tmppref, DSET_PREFIX(xset), THD_MAX_PREFIX ) ;
         tmpset = r_new_resam_dset( xset, NULL, 0.0, 0.0, 0.0,
                                    dset_orient_ref, RESAM_NN_TYPE, 
                                    NULL, 1, 0);   
         DSET_delete(xset);  xset=tmpset;  tmpset=NULL;
         EDIT_dset_items( xset, ADN_prefix, tmppref, ADN_none);
      }

      // [PT: Apr 23, 2024] check if grids match, *after* they have
      // both been resampled (if that is the case)
      if ( mask_dset && THD_dataset_mismatch( xset , mask_dset )) {
         ERROR_message("Mismatch between mask dset and input %s",argv[narg]);
         ERROR_message("For info, run '3dinfo -same_all_grid ...' on them");
         DSET_delete(xset) ; 
         exit(3);
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
               printf("%g  %g  %g  Center of Mass\n",
                      cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
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

               // [PT: Apr 23, 2024] if set above, then unset ref
               // orient for output
               if ( cmode != 1 && \
                    strcmp(dset_orient_inp, dset_orient_ref) != 0 ) {
                  MCW_strncpy( tmppref, DSET_PREFIX(xset), THD_MAX_PREFIX ) ;
                  tmpset = r_new_resam_dset( xset, NULL, 0.0, 0.0, 0.0,
                                             dset_orient_inp, RESAM_NN_TYPE, 
                                             NULL, 1, 0);   
                  DSET_delete(xset);  xset=tmpset;  tmpset=NULL;
                  INFO_message("OUT PREF: %s", tmppref);
                  EDIT_dset_items(  xset , ADN_prefix,  tmppref, ADN_none);
               }

               /* recompute Tc(Cardinal transformation matrix for new
                  grid output --- [PT: Apr 23, 2024] this
                  THD_make_cardinal() could be an 'else' of the above
                  'if' cond */
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
   
   if ( dset_orient_ref ) 
      free(dset_orient_ref);

   if ( mask_dset )
      DSET_delete( mask_dset );

   exit(0);
}
