#include "mrilib.h"

int THD_copy_file( char *old , char *new ) ; /* prototype */

/*---------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int nopt=1 , old_view , new_view , vbot,vtop ;
   char * old_name , * new_name ;
   char old_prefix[THD_MAX_PREFIX] , new_prefix[THD_MAX_PREFIX] ;
   int ii,jj , old_len , new_len ;
   THD_3dim_dataset *dset[LAST_VIEW_TYPE+1] ;
   MCW_idcode        idc [LAST_VIEW_TYPE+1] ;
   char dname[THD_MAX_NAME] ;
   char old_brikname[THD_MAX_NAME] ;
   char new_brikname[THD_MAX_NAME] ;
   char *old_bname=NULL ;
   int brick_ccode , verb=0 ;

   /*-- help? --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf(
       "Usage 1: 3dcopy [-verb] old_prefix new_prefix\n"
       "  Will copy all datasets using the old_prefix to use the new_prefix;\n"
       "    3dcopy fred ethel\n"
       "  will copy   fred+orig.HEAD    to ethel+orig.HEAD\n"
       "              fred+orig.BRIK    to ethel+orig.BRIK\n"
       "              fred+tlrc.HEAD    to ethel+tlrc.HEAD\n"
       "              fred+tlrc.BRIK.gz to ethel+tlrc.BRIK.gz\n"
       "\n"
       "Usage 2: 3dcopy old_prefix+view new_prefix\n"
       "  Will copy only the dataset with the given view (orig, acpc, tlrc).\n"
       "\n"
       "Notes:\n"
       "* The new datasets have new ID codes.  If you are renaming\n"
       "   multiple datasets (as in Usage 1), then if the old +orig\n"
       "   dataset is the warp parent of the old +acpc and/or +tlrc\n"
       "   datasets, then the new +orig dataset will be the warp\n"
       "   parent of the new +acpc and +tlrc datasets.  If any other\n"
       "   datasets point to the old datasets as anat or warp parents,\n"
       "   they will still point to the old datasets, not these new ones.\n"
       "* The BRIK files are copied if they exist, keeping the compression\n"
       "   suffix unchanged (if any).\n"
       "* The old_prefix may have a directory name attached in front,\n"
       "   as in 'gerard/manley/hopkins'.\n"
       "* If the new_prefix does not have a directory name attached\n"
       "   (i.e., does NOT look like 'homer/simpson'), then the new\n"
       "   datasets will be written in the current directory ('./').\n"
       "* The new_prefix cannot JUST be a directory (unlike the Unix\n"
       "   utility 'cp'); you must supply a filename prefix, even if\n"
       "   is identical to the filename prefix in old_prefix.\n"
       "* The -verb option will print progress reports; otherwise, the\n"
       "   program operates silently (unless an error is detected).\n"
      ) ;
      exit(0) ;
   }

   mainENTRY("3dcopy") ; machdep() ; AFNI_logger("3dcopy",argc,argv) ;

   /* input arguments */

   if( strcmp(argv[nopt],"-verb") == 0 ){ verb = 1; nopt++; }

   old_name = argv[nopt++] ; old_len = strlen(old_name) ;
   new_name = argv[nopt++] ; new_len = strlen(new_name) ;

   if( old_len < 1 || old_len > THD_MAX_PREFIX || !THD_filename_ok(old_name) ){
      fprintf(stderr,"** Illegal old dataset name! - EXIT\n") ; exit(1) ;
   }
   if( new_len < 1 || new_len > THD_MAX_PREFIX || !THD_filename_ok(new_name) ){
      fprintf(stderr,"** Illegal new dataset name! - EXIT\n") ; exit(1) ;
   }

#ifdef ALLOW_MINC
   if( STRING_HAS_SUFFIX(old_name,".mnc") ){
      fprintf(stderr,"** Old dataset name can't be a MINC file\n"); exit(1);
   }
   if( STRING_HAS_SUFFIX(new_name,".mnc") ){
      fprintf(stderr,"** New dataset name can't be a MINC file\n"); exit(1);
   }
#endif

   if( strstr(new_name,"/") == NULL ){        /* put cwd on new name, if no */
     char *str = malloc(new_len+16) ;         /* directory present at all   */
     strcpy(str,"./") ; strcat(str,new_name) ;
     new_name = str ;
   }

   /* check old_name for a +view suffix somewhere */

   MCW_strncpy(old_prefix,old_name,THD_MAX_PREFIX) ;
   if( strstr(old_name,"+") == NULL ){
      old_view = ILLEGAL_TYPE ;               /* no +view ==> do all views */
   } else {
      char * qq ;
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
      char * qq ;
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

   /* of course, we don't actually use the +view suffix on the output */

   if( new_view >= 0 ){
      fprintf(stderr,"++ WARNING: ignoring +view on new_prefix.\n") ;
      new_view = ILLEGAL_TYPE ;
   }

   if( old_view >= 0 ){
      vbot = vtop = old_view ;
   } else {
      vbot = 0 ; vtop = LAST_VIEW_TYPE ;
   }

   /*-- loop over views, open and rename datasets internally --*/

   for( ii=vbot ; ii <= vtop ; ii++ ){

      /*-- open the ii-th view dataset --*/

      sprintf(dname,"%s+%s" , old_prefix , VIEW_codestr[ii] ) ;

      if( verb )
        fprintf(stderr,"++ Opening dataset %s\n",dname) ;

      dset[ii] = THD_open_one_dataset( dname ) ;
      if( dset[ii] == NULL ){
        fprintf(stderr,"** Can't open dataset %s - SKIPPING \n",dname) ; continue ;
      }
      idc[ii] = dset[ii]->idcode ; /* save old ID code */

      /*-- rename it (but don't save to disk yet) --*/

      EDIT_dset_items( dset[ii] , ADN_prefix,new_prefix , ADN_none ) ;
      dset[ii]->idcode = MCW_new_idcode() ;

      /*-- check if new header already exists --*/

      if( THD_is_file( DSET_HEADNAME(dset[ii]) ) ){
         fprintf(stderr,"** Output dataset %s already exists! - EXIT\n",
                 DSET_HEADNAME(dset[ii]) ) ;
         exit(1) ;
      }

      /*-- check if current dataset has a warp
           parent in one of the previous datasets;
           if so, alter it to be that datasets new idcode;
           otherwise, leave the warp parent idcode unchanged --*/

      if( ii > 0 && !ISZERO_IDCODE(dset[ii]->warp_parent_idcode) ){
        for( jj=vbot ; jj < ii ; jj++ ){
          if( EQUIV_IDCODES(idc[jj],dset[ii]->warp_parent_idcode) ){
             if( verb )
               fprintf(stderr,"++ Changing warp parent of %s to %s\n",
                       DSET_HEADNAME(dset[ii]) , DSET_HEADNAME(dset[jj]) ) ;

             dset[ii]->warp_parent_idcode = idc[jj] ; break ;
          }
        }
      }
   }

   /*-- get to here means all datasets are ready to be
        written back out to disk under their new names --*/

   /*-- loop over views, rewrite dataset HEADs and copy dataset BRIKs --*/

   for( ii=vbot ; ii <= vtop ; ii++ ){

      if( dset[ii] == NULL ) continue ;  /* skip */

      tross_Make_History( "3dcopy" , argc,argv , dset[ii] ) ;

      if( verb )
        fprintf(stderr,"++ Writing new header %s\n",DSET_HEADNAME(dset[ii])) ;

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
         if( old_bname == NULL ){  /* should not happen */
            fprintf(stderr,"** COMPRESS_filename() fails! - EXIT\n"); exit(1);
         }
         sprintf( new_brikname , "%s+%s.%s" ,
                  new_prefix , VIEW_codestr[ii] , DATASET_BRICK_SUFFIX ) ;
         if( brick_ccode >= 0 )
            strcat(new_brikname,COMPRESS_suffix[brick_ccode]) ;

         if( THD_is_file(new_brikname) ){
            fprintf(stderr,"** New brick %s already exists! - EXIT\n",
                    new_brikname) ;
            exit(1) ;
         }
      }

      if( new_brikname[0] == '\0' ){ /* skip BRIK copy */
         if( verb )
           fprintf(stderr,"** Old brick file %s does not exist - SKIPPING\n",
                   old_brikname ) ;
         continue ;
      }

      if( verb )
        fprintf(stderr,"++ Copying file %s to %s\n",old_bname,new_brikname) ;

      THD_copy_file( old_bname , new_brikname ) ;
      free(old_bname) ;
   }

   exit(0) ;
}

/*------------------------------------------------------------*/

#undef NBUF
#define NBUF 1048576  /* 2^20 */

int THD_copy_file( char *old , char *new )
{
   FILE *fold , *fnew ;
   char *buf ;
   int ii,jj ;

   if( old == NULL || old[0] == '\0' ||
       new == NULL || new[0] == '\0'   ) return 1 ;

   fold = fopen( old , "rb" ) ;
   if( fold == NULL ){ perror("Old open fails"); return 1; }

   fnew = fopen( new , "wb" ) ;
   if( fnew == NULL ){ perror("New open fails"); fclose(fold); return 1; }

   buf = malloc(NBUF) ;
   if( buf == NULL ){
      fprintf(stderr,"** Can't malloc() buffer for file copy! - EXIT\n"); exit(1);
   }
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
