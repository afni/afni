#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 , idsetarg;
   char *prefix = NULL, ax[7], tx[7], ext[17], oprefix[900] ;
   void *row ;
   char *p2;
   int nx,ny,nz , ii,jj,kk , dcode , dcodeu, ival, D1, D2 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dLRflip [-LR|-AP|-IS|-X|-Y|-Z] [-prefix ppp] dataset dataset dataset ...\n"
             "Flips the rows of a dataset along one of the three\n"
             "axis.\n"
             "This is useful in the case where you, or some fast\n"
             "program, constructed a dataset with one of the \n"
             "directions incorrectly labeled. \n"
             " Optional options:\n"
             " -LR | -AP | -IS: Axis about which to flip the data\n"
             "                  Default is -LR.\n"
             "      or\n"
             " -X | -Y | -Z: Flip about 1st, 2nd or 3rd directions,\n"
             "               respectively. \n"
             " Note: Only one of these 6 options can be  used at a time.\n"
             "        \n"
             " -prefix ppp: Prefix to use for output. If you have \n"
             "              multiple datasets as input, you are better\n"
             "              off letting the program choose a prefix for\n"
             "              each output.\n"
             "\n" 
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dLRflip main"); machdep(); AFNI_logger("3dLRflip",argc,argv);

    D1 = ORI_R2L_TYPE;
    D2 = ORI_L2R_TYPE;
    sprintf(ax,"L-R");
    sprintf(tx,"Lf");
    dcodeu = -1;
   while( argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** Illegal prefix: %s\n",prefix); exit(1);
       }
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-LR") == 0 ){
       D1 = ORI_R2L_TYPE;
       D2 = ORI_L2R_TYPE;
       sprintf(ax,"L-R");
       sprintf(tx,"Lf");
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-AP") == 0 ){
       D1 = ORI_A2P_TYPE;
       D2 = ORI_P2A_TYPE;
       sprintf(ax,"A-P");
       sprintf(tx,"Af");
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-IS") == 0 ){
       D1 = ORI_I2S_TYPE;
       D2 = ORI_S2I_TYPE;
       sprintf(ax,"I-S");
       sprintf(tx,"If");
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-X") == 0 ){
       dcodeu = 1;
       sprintf(ax,"X");
       sprintf(tx,"Xf");
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-Y") == 0 ){
       dcodeu = 2;
       sprintf(ax,"Y");
       sprintf(tx,"Yf");
       iarg++ ; continue ;
     }else if( strcmp(argv[iarg],"-Z") == 0 ){
       dcodeu = 3;
       sprintf(ax,"Z");
       sprintf(tx,"Zf");
       iarg++ ; continue ;
     }else {
      fprintf(stderr,"Janez! Ne razumem %s.\n", argv[iarg]); exit(1);
     }
   }

   idsetarg = iarg;
   while (iarg < argc) {
      /*-- read data --*/
      fprintf(stderr,"++ processing %s ...\n", argv[iarg]);
      dset = THD_open_dataset(argv[iarg]) ;
      CHECK_OPEN_ERROR(dset,argv[iarg]) ;
      DSET_mallocize(dset) ; DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

      nx=DSET_NX(dset); ny=DSET_NY(dset); nz=DSET_NZ(dset);

      
      if (dcodeu > 0) {
         dcode = dcodeu;
         
      } else {
         if( dset->daxes->xxorient == D1 ||
             dset->daxes->xxorient == D2   ) dcode = 1 ;
         else
         if( dset->daxes->yyorient == D1 ||
             dset->daxes->yyorient == D2   ) dcode = 2 ;
         else
         if( dset->daxes->zzorient == D1 ||
             dset->daxes->zzorient == D2   ) dcode = 3 ;
         else
         { fprintf(stderr,"** Dataset has no %s axis!\n", ax); exit(1); }
      }
      /* modify dataset name */

      dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ; /* 14 Jan 2004 */
      dset->idcode = MCW_new_idcode() ;  /* 24 Aug 2003 - ooops */
      /* set prefix */
      if (prefix) { /* user set */
         if (argc -idsetarg == 1) { /* one dset only */
            sprintf(oprefix, "%s", prefix);
         } else {
            if (PREFIX_IS_NIFTI(prefix)) { 
               if (STRING_HAS_SUFFIX(prefix,".nii")) {
                  p2 = strstr(prefix, ".nii"); 
                  prefix[p2-prefix]='\0';
                  sprintf(ext,".nii");
               } else if (STRING_HAS_SUFFIX(prefix,".nii.gz")) {
                  p2 = strstr(prefix, ".nii.gz");
                  prefix[p2-prefix]='\0';
                  sprintf(ext,".nii.gz");
               } else {
                  ext[0] = '\0';
               }
            } 
            sprintf(oprefix, "%s_%d%s", prefix, iarg - idsetarg, ext);
         }
      } else {
         prefix = DSET_PREFIX(dset);
         if (PREFIX_IS_NIFTI(prefix)) {   /* Note, this ruins prefix in dset! But not me worry here.*/
            if (PREFIX_IS_NIFTI(prefix)) {
               if (STRING_HAS_SUFFIX(prefix,".nii")) {
                  p2 = strstr(prefix, ".nii");
                  prefix[p2-prefix]='\0';
                  sprintf(ext,".nii");
               } else if (STRING_HAS_SUFFIX(prefix,".nii.gz")) {
                  p2 = strstr(prefix, ".nii.gz");
                  prefix[p2-prefix]='\0';
                  sprintf(ext,".nii.gz");
               } else {
                  ext[0] = '\0';
               }
            } 
            
         }
         sprintf(oprefix, "%s_%s%s", prefix, tx, ext);
         prefix = NULL;
      }
      EDIT_dset_items( dset , ADN_prefix , oprefix , ADN_none ) ;
      tross_Make_History( "3dLRflip", argc,argv, dset ) ;

      /* loop over bricks */

      for( ival=0 ; ival < DSET_NVALS(dset) ; ival++ ){

        switch( dcode ){

          case 1:   /* flip x-axis rows */
            for( kk=0 ; kk < nz ; kk++ )
             for( jj=0 ; jj < ny ; jj++ ){
               row = THD_get_dset_row( dset , ival , -dcode , 0,jj,kk ) ;
                     THD_put_dset_row( dset , ival ,  dcode , 0,jj,kk , row ) ;
               free(row) ;
             }
          break ;

          case 2:   /* flip y-axis rows */
            for( kk=0 ; kk < nz ; kk++ )
             for( ii=0 ; ii < nx ; ii++ ){
               row = THD_get_dset_row( dset , ival , -dcode , ii,0,kk ) ;
                     THD_put_dset_row( dset , ival ,  dcode , ii,0,kk , row ) ;
               free(row) ;
             }
          break ;

          case 3:   /* flip z-axis rows */
            for( jj=0 ; jj < ny ; jj++ )
             for( ii=0 ; ii < nx ; ii++ ){
               row = THD_get_dset_row( dset , ival , -dcode , ii,jj,0 ) ;
                     THD_put_dset_row( dset , ival ,  dcode , ii,jj,0 , row ) ;
               free(row) ;
             }
          break ;

        }
      }

      /* done */

      DSET_write(dset) ; DSET_delete(dset); dset = NULL;
   
      ++iarg;
   }
   
   exit(0) ;
}
