#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg=1 , dcode=0 , maxgap=9 , nftot=0, i=0;
   char * prefix="rowfillin" , * dstr=NULL;
   THD_3dim_dataset * inset , * outset ;
   MRI_IMAGE * brim ;
   int verb=0, bin=0;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dRowFillin [options] dataset\n"
             "Extracts 1D rows in the given direction from a 3D dataset,\n"
             "searches for blank (zero) regions, and fills them in if\n"
             "the blank region isn't too large and it is flanked by\n"
             "the same value on either edge.  For example:\n"
             "     input row = 0 1 2 0 0 2 3 0 3 0 0 4 0\n"
             "    output row = 0 1 2 2 2 2 3 3 3 0 0 4 0\n"
             "\n"
             "OPTIONS:\n"
             " -maxgap N  = set the maximum length of a blank region that\n"
             "                will be filled in to 'N' [default=9].\n"
             " -dir D     = set the direction of fill to 'D', which can\n"
             "                be one of the following:\n"
             "                  A-P, P-A, I-S, S-I, L-R, R-L, x, y, z, \n"
             "                  XYZ.OR, XYZ.AND\n"
             "                The first 6 are anatomical directions;\n"
             "                x,y, and z, are reference to the dataset\n"
             "                internal axes. \n"
             "             XYZ.OR means do the fillin in x, followed by y,\n"
             "                followed by z directions.\n"
             "             XYZ.AND is like XYZ.OR but only accept voxels that\n"
             "                would have been filled in each of the three fill\n"
             "                calls. \n"
             "         Note that with XYZ* options, the fill value depends on\n"
             "         on the axis orientation. So you're better off sticking\n"
             "         to single valued dsets when using them. \n"
             "         See also -binary option below\n"
             " -binary: Turn input dataset to 0 and 1 before filling in.\n"
             "          Output will also be a binary valued dataset.\n"
             " -prefix P  = set the prefix to 'P' for the output dataset.\n"
             "\n"
             "N.B.: If the input dataset has more than one sub-brick,\n"
             "      only the first one will be processed.\n"
             "\n"
             "* The intention of this program is to let you fill in slice gaps\n"
             "  made when drawing ROIs with the 'Draw Dataset' plugin.  If you\n"
             "  draw every 5th coronal slice, say, then you could fill in using\n"
             "    3dRowFillin -maxgap 4 -dir A-P -prefix fredfill fred+orig\n"
             "* This program is moderately obsolescent, since I later added\n"
             "    the 'Linear Fillin' controls to the 'Draw Dataset' plugin.\n"
             "\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dRowFillin main"); machdep();
   AFNI_logger("3dRowFillin",argc,argv);
   PRINT_VERSION("3dRowFillin") ;

   /*-- scan args --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         verb++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-bin",4) == 0 ){
         bin=1; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-maxgap") == 0 ){
         maxgap = strtol( argv[++iarg] , NULL , 10 ) ;
         if( maxgap < 1 ){
            fprintf(stderr,"*** Illegal value after -maxgap!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dir") == 0 ){
         dstr = argv[++iarg] ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   if( dstr == NULL ){
      fprintf(stderr,"*** No -dir option on command line!\n"); exit(1);
   }
   if( iarg >= argc ){
      fprintf(stderr,"*** No input dataset on command line!\n"); exit(1);
   }

   inset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(inset,argv[iarg]) ;

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   if( THD_deathcon() && THD_is_file( DSET_HEADNAME(outset) ) ){
      fprintf(stderr,"** Output file %s exists -- cannot overwrite!\n",
              DSET_HEADNAME(outset) ) ;
      exit(1) ;
   }

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dRowFillin" , argc,argv , outset ) ;

   if( DSET_NVALS(inset) > 1 ){
      fprintf(stderr,"++ WARNING: input dataset has more than one sub-brick!\n");
      EDIT_dset_items( outset ,
                         ADN_ntt   , 0 ,
                         ADN_nvals , 1 ,
                       ADN_none ) ;
   }

   if( DSET_BRICK_TYPE(outset,0) != MRI_short &&
       DSET_BRICK_TYPE(outset,0) != MRI_byte    ){

      fprintf(stderr,"*** This program only works on byte and short dataset!\n");
      exit(1) ;
   }

   if (strcmp(dstr,"XYZ.OR")==0) dcode = 4;
   else if (strcmp(dstr,"XYZ.AND")==0) dcode = 5;
   else {
      switch( *dstr ){
         case 'x': dcode = 1 ; break ;
         case 'y': dcode = 2 ; break ;
         case 'z': dcode = 3 ; break ;
         default:
           if( *dstr == ORIENT_tinystr[outset->daxes->xxorient][0] ||
               *dstr == ORIENT_tinystr[outset->daxes->xxorient][1]  ) dcode = 1 ;

           if( *dstr == ORIENT_tinystr[outset->daxes->yyorient][0] ||
               *dstr == ORIENT_tinystr[outset->daxes->yyorient][1]  ) dcode = 2 ;

           if( *dstr == ORIENT_tinystr[outset->daxes->zzorient][0] ||
               *dstr == ORIENT_tinystr[outset->daxes->zzorient][1]  ) dcode = 3 ;
         break ;
      }
   }
   if( dcode == 0 ){
      fprintf(stderr,"*** Illegal -dir direction!\n") ; exit(1) ;
   }
   if( verb )
      fprintf(stderr,"++ Direction = axis %d in dataset\n",dcode) ;

   if (bin) DSET_mallocize(inset);
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;


   /* binarize input */
   if (bin) {
      switch(DSET_BRICK_TYPE(inset,0)) {
         case MRI_short: {
            short *dp;
            dp = DSET_ARRAY(inset, 0);
            for (i=0; i<DSET_NVOX(inset); ++i) {
               if (dp[i]) dp[i]=1;
            }
            break;
         }
         case MRI_byte: {
            byte *dp;
            dp = DSET_ARRAY(inset, 0);
            for (i=0; i<DSET_NVOX(inset); ++i) {
               if (dp[i]) dp[i]=1;
            }
            break;
         }
         default:
            fprintf(stderr,"*** Illegal brick type!\n") ; exit(1) ;
      }
   }

   if (dcode < 4) {
      brim = mri_copy( DSET_BRICK(inset,0) ) ;
      EDIT_substitute_brick( outset , 0 , brim->kind , mri_data_pointer(brim) ) ;
      DSET_unload(inset) ;
      nftot = THD_dataset_rowfillin( outset , 0 , dcode , maxgap ) ;
      fprintf(stderr,"++ Number of voxels filled = %d\n",nftot) ;
   } else if (dcode == 4) {
      brim = mri_copy( DSET_BRICK(inset,0) ) ;
      EDIT_substitute_brick( outset , 0 , brim->kind , mri_data_pointer(brim) ) ;
      DSET_unload(inset) ;
      nftot = 0; dcode=0;
      for (dcode=1; dcode<4; ++dcode) {
         nftot = nftot +
                  THD_dataset_rowfillin( outset , 0 , dcode , maxgap ) ;
      }
      fprintf(stderr,"++ Number of voxels OR filled = %d\n",nftot) ;
   }  else if (dcode == 5) {
      int nf=0;
      THD_3dim_dataset *otmp[3]={NULL, NULL, NULL};
      MRI_IMAGE *brimtmp=NULL;

      brim = mri_copy( DSET_BRICK(inset,0) ) ;
      EDIT_substitute_brick( outset , 0 , brim->kind , mri_data_pointer(brim) ) ;
      for (i=0; i<3; ++i) {
         otmp[i] = EDIT_empty_copy( outset ) ;
         brimtmp = mri_copy( DSET_BRICK(inset,0) ) ;
         EDIT_substitute_brick( otmp[i] , 0 ,
                        brimtmp->kind, mri_data_pointer(brimtmp) ) ;
         THD_dataset_rowfillin( otmp[i] , 0 , i+1 , maxgap);
      }
      DSET_unload(inset) ;

      switch(DSET_BRICK_TYPE(outset,0)) {
         case MRI_short: {
            short * dp[3], *doo;
            nftot = 0;
            doo = DSET_ARRAY(outset, 0);
            for (i=0; i<3; ++i) dp[i] = DSET_ARRAY(otmp[i], 0);
            for (i=0; i<DSET_NVOX(outset); ++i) {
               if ( !doo[i] &&  (dp[0][i] == dp[1][i]) &&
                    (dp[0][i] == dp[2][i]) && dp[0][i] ) {
                  doo[i] = dp[0][i]; ++nftot;
               }
            }
            break;
         }
         case MRI_byte: {
            byte * dp[3], *doo;
            nftot = 0;
            doo = DSET_ARRAY(outset, 0);
            for (i=0; i<3; ++i) dp[i] = DSET_ARRAY(otmp[i], 0);
            for (i=0; i<DSET_NVOX(outset); ++i) {
               if ( !doo[i] && (dp[0][i] == dp[1][i]) &&
                    (dp[0][i] == dp[2][i]) && dp[0][i] ) {
                  doo[i] = dp[0][i]; ++nftot;
               }
            }
            break;
         }
         default:
            fprintf(stderr,"*** Illegal brick type!\n") ; exit(1) ;
      }

      for (i=0; i<3; ++i) DSET_delete(otmp[i]); otmp[i] = NULL;
      fprintf(stderr,"++ Number of voxels AND filled = %d\n",nftot) ;
   }

   DSET_write(outset) ;
   exit(0) ;
}
