#include "mrilib.h"

int THD_copy_file( char *old , char *newFile ) ; /* prototype */

/*---------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nopt=1 , old_view , new_view , vbot,vtop ;
   char *old_name , *new_name ;
   char old_prefix[THD_MAX_PREFIX] , new_prefix[THD_MAX_PREFIX] ;
   int ii,jj , old_len , new_len, non_afni_out ;
   THD_3dim_dataset *dset[LAST_VIEW_TYPE+1] ;
   MCW_idcode        idc [LAST_VIEW_TYPE+1] ;
   char dname[THD_MAX_NAME] ;
   char old_brikname[THD_MAX_NAME] ;
   char new_brikname[THD_MAX_NAME] ;
   char *old_bname=NULL ;
   int brick_ccode , verb=0, nfound=0 ;
   int denote=0 ;   /* 08 Jul 2005 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 || strcmp(argv[1],"-h") == 0){
      printf(
       "Usage 1: 3dcopy [-verb] [-denote] old_prefix new_prefix ~1~\n"
       "  Will copy all datasets using the old_prefix to use the new_prefix;\n"
       "\n"
       "    3dcopy fred ethel\n"
       "\n"
       "  will copy   fred+orig.HEAD    to ethel+orig.HEAD\n"
       "              fred+orig.BRIK    to ethel+orig.BRIK\n"
       "              fred+tlrc.HEAD    to ethel+tlrc.HEAD\n"
       "              fred+tlrc.BRIK.gz to ethel+tlrc.BRIK.gz\n"
       "\n"
       "Usage 2: 3dcopy old_prefix+view new_prefix ~1~\n"
       "  Will copy only the dataset with the given view (orig, acpc, tlrc).\n"
       "\n"
       "Usage 3: 3dcopy old_dataset new_prefix ~1~\n"
       "  Will copy the non-AFNI formatted dataset (e.g., MINC, ANALYZE, CTF)\n"
       "  to the AFNI formatted dataset with the given new prefix.\n"
       "\n"
       "\n"
       "Notes: ~1~\n"
       "* This is to copy entire datasets, possibly with multiple views.\n"
       "  So sub-brick selection is not allowed.  Please use 3dbucket or\n"
       "  3dTcat for that purpose.\n"
       "\n"
       "* The new datasets have new ID codes.  If you are renaming\n"
       "   multiple datasets (as in Usage 1), then if the old +orig\n"
       "   dataset is the warp parent of the old +acpc and/or +tlrc\n"
       "   datasets, then the new +orig dataset will be the warp\n"
       "   parent of the new +acpc and +tlrc datasets.  If any other\n"
       "   datasets point to the old datasets as anat or warp parents,\n"
       "   they will still point to the old datasets, not these new ones.\n"
       "\n"
       "* The BRIK files are copied if they exist, keeping the compression\n"
       "   suffix unchanged (if any).\n"
       "\n"
       "* The old_prefix may have a directory name attached in front,\n"
       "   as in 'gerard/manley/hopkins'.\n"
       "\n"
       "* If the new_prefix does not have a directory name attached\n"
       "   (i.e., does NOT look like 'homer/simpson'), then the new\n"
       "   datasets will be written in the current directory ('./').\n"
       "\n"
       "* The new can JUST be a directory now (like the Unix\n"
       "   utility 'cp'); in this case the output has the same prefix\n"
       "   as the input.\n"
       "\n"
       "* The '-verb' option will print progress reports; otherwise, the\n"
       "   program operates silently (unless an error is detected).\n"
       "\n"
       "* The '-denote' option will remove any Notes from the file.\n"
      ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dcopy") ; machdep() ; AFNI_logger("3dcopy",argc,argv) ;
   PRINT_VERSION("3dcopy") ;
   set_obliquity_report(0); /* silence obliquity */

   /* input arguments */

   while( nopt < argc && argv[nopt][0] == '-' ){
      if ( strcmp(argv[nopt], "-echo_edu") == 0 ) {
         int jj;
         fprintf(stdout,"\n+++ Now running:\n   ");
         for (jj=0; jj<argc; ++jj) {
            if (jj != nopt) {
               fprintf(stdout,"%s ", argv[jj]);
            }
         }
         fprintf(stdout,"\n+++\n");
         nopt++; continue;
     }
     if( strcmp(argv[nopt],"-verb")   == 0 ){ verb   = 1; nopt++; continue; }
     if( strcmp(argv[nopt],"-denote") == 0 ){ denote = 1; nopt++; continue; }
     ERROR_exit("unknown option: %s\n", argv[nopt]);
   }
   if( nopt+1 >= argc )
     ERROR_exit("3dcopy needs input AND output filenames!\n") ;

   if (nopt+2 < argc)
      ERROR_exit(
   "3dcopy only takes one input AND one output filename after options.\n"
   "Have at least one too many parameters: \n %s \n %s \n %s\n"
         , argv[nopt], argv[nopt+1], argv[nopt+2]);

   old_name = argv[nopt++] ; old_len = strlen(old_name) ;
   new_name = argv[nopt++] ; new_len = strlen(new_name) ;

   if( old_len < 1 || old_len > THD_MAX_PREFIX || !THD_filename_ok(old_name) )
     ERROR_exit("Illegal old dataset name! - EXIT\n"
                "(note: sub-brick selection is not allowed in this context)") ;

   if( !strcmp(new_name,".") ){        /* turn . to ./ ZSS Oct 2010*/
     char *str = malloc(new_len+16) ;
     strcpy(str,"./") ;
     new_name = str ;
   }

   if( strstr(new_name,"/") == NULL ){        /* put cwd on new name, if no */
     char *str = malloc(new_len+16) ;         /* directory present at all   */
     strcpy(str,"./") ; strcat(str,new_name) ;
     new_name = str ;
   }

   if( new_len < 1 || new_len > THD_MAX_PREFIX || !THD_filename_ok(new_name) )
     ERROR_exit("Illegal new dataset name! - EXIT\n") ;


   /* check old_name for a +view suffix somewhere */

   MCW_strncpy(old_prefix,old_name,THD_MAX_PREFIX) ;
   if( strstr(old_name,"+") == NULL ){
      old_view = ILLEGAL_TYPE ;               /* no +view ==> do all views */
   } else {
      char *qq ;
      for( qq=old_name+old_len ; *qq != '+' ; qq-- ) ; /* find last '+' */
      qq++ ;                                           /* point to view */
      for( old_view=0 ; old_view <= LAST_VIEW_TYPE ; old_view++ )
         if( strncmp(qq,VIEW_codestr[old_view],
                        strlen(VIEW_codestr[old_view])) == 0 ) break ;

      if( old_view > LAST_VIEW_TYPE ){
         old_view = ILLEGAL_TYPE ;               /* the '+' is a fake! */
      } else {
         old_prefix[ (qq-old_name)-1 ] = '\0' ;  /* truncate prefix */
      }
   }

   /* check new_name for a +view suffix, also */

   MCW_strncpy(new_prefix,new_name,THD_MAX_PREFIX) ;
   if( strstr(new_name,"+") == NULL ){
      new_view = ILLEGAL_TYPE ;               /* no +view */
   } else {
      char *qq ;
      for( qq=new_name+new_len ; *qq != '+' ; qq-- ) ; /* find last '+' */
      qq++ ;                                           /* point to view */
      for( new_view=0 ; new_view <= LAST_VIEW_TYPE ; new_view++ )
         if( strncmp(qq,VIEW_codestr[new_view],
                        strlen(VIEW_codestr[new_view])) == 0 ) break ;

      if( new_view > LAST_VIEW_TYPE ){
         new_view = ILLEGAL_TYPE ;               /* the '+' is a fake! */
      } else {
         new_prefix[ (qq-new_name)-1 ] = '\0' ;  /* truncate prefix */
      }
   }
   if( !THD_filename_ok( new_prefix ) )  /* 28 Jan 2003 */
     ERROR_exit("Illegal new prefix: %s\n",new_prefix) ;
#if 0
   if( strstr(new_prefix,".nii") != NULL ||   /* 06 Apr 2005 */
       strstr(new_prefix,".hdr") != NULL   )  /* 11 Oct 2005 */
     ERROR_exit("You can't use 3dcopy to create a NIfTI-1.1 file!\n"
                " *        Use program 3dAFNItoNIFTI for that purpose.\n") ;
#endif

   /* 28 Jan 2003:
      to allow for non-AFNI datasets input,
      we now check if we can read the input dataset without a view */

   non_afni_out = has_known_non_afni_extension( new_prefix );

   if( old_view == ILLEGAL_TYPE || non_afni_out ){
     THD_3dim_dataset *qset , *cset ;
     qset = THD_open_one_dataset( old_name ) ;
     if( qset != NULL ){
       if( verb ) INFO_message("Opened dataset %s\n",old_prefix) ;
       /* make sure prefix is not just a path     ZSS Oct 2010 */
       if (new_prefix[strlen(new_prefix)-1] == '/') {
          strncat(new_prefix, DSET_PREFIX(qset),
                  THD_MAX_PREFIX-strlen(new_prefix)-1);
       }
       cset = EDIT_empty_copy( qset ) ;
       if( new_view < 0 ) new_view = qset->view_type ;
       EDIT_dset_items( cset ,
                          ADN_prefix    , new_prefix ,
                          ADN_view_type , new_view   ,
                        ADN_none ) ;
       if (!THD_copy_labeltable_atr( cset->dblk,  qset->dblk)) {
          WARNING_message("Failed trying to preserve labeltables");
       }
       tross_Make_History( "3dcopy" , argc,argv , cset ) ;

       DSET_mallocize(qset); DSET_load(qset); CHECK_LOAD_ERROR(qset);
       for( ii=0 ; ii < DSET_NVALS(qset) ; ii++ )
         EDIT_substitute_brick( cset , ii ,
                                DSET_BRICK_TYPE(qset,ii) ,
                                DSET_BRICK_ARRAY(qset,ii) ) ;
       if( verb ) INFO_message("Writing %s and %s\n",
                          DSET_HEADNAME(cset) , DSET_BRIKNAME(cset) ) ;

       if( cset->type      == HEAD_ANAT_TYPE     &&
           cset->view_type == VIEW_ORIGINAL_TYPE &&
           DSET_NUM_TIMES(cset) == 1               ){  /* add markers? */

         THD_marker_set *markers ;
         int ii , jj ;

         markers = cset->markers = myXtNew( THD_marker_set ) ;
         markers->numdef = 0 ;

         for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){       /* null all data out */
           markers->valid[ii] = 0 ;
           for( jj=0 ; jj < MARKS_MAXLAB  ; jj++ )
             markers->label[ii][jj] = '\0';
           for( jj=0 ; jj < MARKS_MAXHELP ; jj++ )
             markers->help[ii][jj]  = '\0';
         }

         for( ii=0 ; ii < NMARK_ALIGN ; ii++ ){       /* copy strings in */
           MCW_strncpy( &(markers->label[ii][0]) ,
                        THD_align_label[ii] , MARKS_MAXLAB ) ;
           MCW_strncpy( &(markers->help[ii][0]) ,
                        THD_align_help[ii] , MARKS_MAXHELP ) ;
         }

         for( ii=0 ; ii < MARKS_MAXFLAG ; ii++ )     /* copy flags in */
           markers->aflags[ii] = THD_align_aflags[ii] ;
       }

       if( denote ) THD_anonymize_write(1) ;  /* 08 Jul 2005 */
       if ( DSET_write(cset) != True )  exit(1) ;
       else exit (0);
     } else if ( non_afni_out ) { /* fail */
       fprintf(stderr,"** failed to open input '%s' on non-AFNI write\n"
                "Try including the view of the input dataset %s\n"
                "or use its fullname. That might take care of the error.\n",
               old_prefix, old_name);
       exit(1);
     }
   }

   /* of course, we don't actually use the +view suffix on the output */

   if( new_view >= 0 ){
     if( verb ) INFO_message("ignoring +view on new_prefix.\n") ;
     new_view = ILLEGAL_TYPE ;
   }

   if( old_view >= 0 ){
     vbot = vtop = old_view ;
   } else {
     vbot = 0 ; vtop = LAST_VIEW_TYPE ;
   }

   /*-- loop over views, open and rename datasets internally --*/

   nfound = 0 ;
   for( ii=vbot ; ii <= vtop ; ii++ ){

      /*-- open the ii-th view dataset --*/

      sprintf(dname,"%s+%s" , old_prefix , VIEW_codestr[ii] ) ;

      if( verb ) INFO_message("Opening dataset %s\n",dname) ;

      dset[ii] = THD_open_one_dataset( dname ) ;
      if( dset[ii] == NULL ){
        /* changed from error to warning       3 Jun 2009 [rickr] */
        /* make this warning show up only if verb 27 Apr 2016 [drg] */
        if (verb)
           WARNING_message("missing view dataset: %s - SKIPPING\n",dname) ;
        continue ;
      }
      nfound ++;                   /* found some dataset */
      idc[ii] = dset[ii]->idcode ; /* save old ID code */

      /*-- rename it (but don't save to disk yet) --*/

      dset[ii]->idcode = MCW_new_idcode() ;
      dset[ii]->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ; /* 14 Jan 2004 */
      EDIT_dset_items( dset[ii] , ADN_prefix,new_prefix , ADN_none ) ;

      /*-- check if new header already exists --*/

      if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(dset[ii]) ) )
         ERROR_exit("Output dataset %s already exists! - EXIT\n",
                 DSET_HEADNAME(dset[ii]) ) ;

      /*-- check if current dataset has a warp
           parent in one of the previous datasets;
           if so, alter it to be that datasets new idcode;
           otherwise, leave the warp parent idcode unchanged --*/

      if( ii > 0 && !ISZERO_IDCODE(dset[ii]->warp_parent_idcode) ){
        for( jj=vbot ; jj < ii ; jj++ ){
          if( EQUIV_IDCODES(idc[jj],dset[ii]->warp_parent_idcode) ){
             if( verb )
               INFO_message("Changing warp parent of %s to %s\n",
                       DSET_HEADNAME(dset[ii]) , DSET_HEADNAME(dset[jj]) ) ;

             dset[ii]->warp_parent_idcode = idc[jj] ; break ;
          }
        }
      }
   }

   if( nfound == 0 )
      ERROR_exit("no datasets found!"); /* ZSS, changed from ERROR_message */

   /*-- get to here means all datasets are ready to be
        written back out to disk under their new names --*/

   /*-- loop over views, rewrite dataset HEADs and copy dataset BRIKs --*/

   for( ii=vbot ; ii <= vtop ; ii++ ){

      if( dset[ii] == NULL ) continue ;  /* skip */

      tross_Make_History( "3dcopy" , argc,argv , dset[ii] ) ;

      if( verb )
        INFO_message("Writing new header %s\n",DSET_HEADNAME(dset[ii])) ;

      if( denote ) THD_anonymize_write(1) ;  /* 08 Jul 2005 */

      DSET_write_header( dset[ii] ) ;  /* new header is written */

      /*-- create the .BRIK filename for the old and new datasets --*/

      sprintf( old_brikname , "%s+%s.%s" ,
               old_prefix , VIEW_codestr[ii] , DATASET_BRICK_SUFFIX ) ;

      /*-- however, the .BRIK might have a compression suffix on it,
           which affects both its old name and its new name         --*/
      brick_ccode = COMPRESS_filecode(old_brikname) ;
      if( brick_ccode == COMPRESS_NOFILE ){
         new_brikname[0] = '\0' ;  /* flag to do nothing */
      } else {
         old_bname = COMPRESS_filename( old_brikname ) ;
         if( old_bname == NULL )  /* should not happen */
           ERROR_exit("COMPRESS_filename() fails! - EXIT\n");
         if (!strstr(new_prefix,DSET_HEADNAME(dset[ii]))) {
            /* this would happen in old cases of 3dcopy Joe+tlrc ./
                                                         ZSS Oct 2010*/
            snprintf( new_brikname , THD_MAX_NAME, "%s/%s+%s.%s" ,
               DSET_DIRNAME(dset[ii]), DSET_PREFIX(dset[ii]),
                     VIEW_codestr[ii] , DATASET_BRICK_SUFFIX) ;
         } else { /* the old way, for minimal changes*/
            sprintf( new_brikname , "%s+%s.%s" ,
                  new_prefix , VIEW_codestr[ii] , DATASET_BRICK_SUFFIX ) ;
         }
         if( brick_ccode >= 0 )
            strcat(new_brikname,COMPRESS_suffix[brick_ccode]) ;

         if( !THD_ok_overwrite() && THD_is_file(new_brikname) )
            ERROR_exit("New brick %s already exists! - EXIT\n",
                    new_brikname) ;
      }

      if( new_brikname[0] == '\0' ){ /* skip BRIK copy */
         if( verb )
           ERROR_message("Old brick file %s does not exist - SKIPPING\n",
                   old_brikname ) ;
         continue ;
      }

      if( verb )
        INFO_message("Copying file %s to %s\n",old_bname,new_brikname) ;

      THD_copy_file( old_bname , new_brikname ) ;
      free(old_bname) ;
   }

   exit(0) ;
}

/*------------------------------------------------------------*/

#undef NBUF
#define NBUF 1048576  /* 2^20 */

int THD_copy_file( char *old , char *newFile )
{
   FILE *fold , *fnew ;
   char *buf ;
   int ii,jj ;

   if( old == NULL || old[0] == '\0' ||
       newFile == NULL || newFile[0] == '\0'   ) return 1 ;

   fold = fopen( old , "rb" ) ;
   if( fold == NULL ){ perror("Old open fails"); return 1; }

   fnew = fopen( newFile , "wb" ) ;
   if( fnew == NULL ){ perror("New open fails"); fclose(fold); return 1; }

   buf = malloc(NBUF) ;
   if( buf == NULL )
      ERROR_exit("Can't malloc() buffer for file copy! - EXIT\n");
   while(1){  /*- loop: read buffer, write buffer -*/

     ii = fread( buf , 1 , NBUF , fold ) ; /* read buffer  */
     if( ii <= 0 ) break ;                 /* read failed  */
     jj = fwrite( buf , 1 , ii , fnew ) ;  /* write buffer */
     if( jj != ii ){
        perror("New write fails") ; break ;
     }
   }

   fsync(fileno(fnew)) ;
   free(buf); fclose(fnew); fclose(fold) ; return 0 ;
}

