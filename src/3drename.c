/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

#undef DEBUG

static int THD_rename_dataset_files( char * , char * , int ) ;

int main( int argc , char * argv[] )
{
   int nopt=1 , old_view , new_view , vbot,vtop ;
   char * old_name , * new_name ;
   char old_prefix[THD_MAX_PREFIX] , new_prefix[THD_MAX_PREFIX] ;
   int ii , old_len , new_len ;

   if( argc != 3 || strcmp(argv[1],"-help") == 0 ){
      printf(
       "Usage 1: 3drename old_prefix new_prefix\n"
       "  Will rename all datasets using the old_prefix to use the new_prefix;\n"
       "    3drename fred ethel\n"
       "  will change fred+orig.HEAD    to ethel+orig.HEAD\n"
       "              fred+orig.BRIK    to ethel+orig.BRIK\n"
       "              fred+tlrc.HEAD    to ethel+tlrc.HEAD\n"
       "              fred+tlrc.BRIK.gz to ethel+tlrc.BRIK.gz\n"
       "\n"
       "Usage 2: 3drename old_prefix+view new_prefix\n"
       "  Will rename only the dataset with the given view (orig, acpc, tlrc).\n"
      ) ;
      exit(0) ;
   }

   /* input arguments */

   old_name = argv[nopt++] ; old_len = strlen(old_name) ;
   new_name = argv[nopt++] ; new_len = strlen(new_name) ;

   if( old_len < 1 || old_len > THD_MAX_PREFIX || !THD_filename_pure(old_name) ){
      fprintf(stderr,"** Illegal old dataset name!\n") ; exit(1) ;
   }
   if( new_len < 1 || new_len > THD_MAX_PREFIX || !THD_filename_pure(new_name) ){
      fprintf(stderr,"** Illegal new dataset name!\n") ; exit(1) ;
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
#ifdef DEBUG
fprintf(stderr,"++ old_view = %d\n",old_view) ;
#endif

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
#ifdef DEBUG
fprintf(stderr,"++ new_view = %d\n",new_view) ;
#endif

   /* of course, we don't actually use the +view suffix on the output */

   if( new_view >= 0 ){
      fprintf(stderr,"++ Warning: ignoring +view on new_prefix.\n") ;
      new_view = ILLEGAL_TYPE ;
   }

   if( old_view >= 0 ){
      vbot = vtop = old_view ;
   } else {
      vbot = 0 ; vtop = LAST_VIEW_TYPE ;
   }

   for( ii=vbot ; ii <= vtop ; ii++ )
      (void) THD_rename_dataset_files( old_prefix , new_prefix , ii ) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

int THD_rename_dataset_files( char * old_prefix , char * new_prefix , int view )
{
   char old_headname[THD_MAX_NAME] , old_brikname[THD_MAX_NAME] ;
   char new_headname[THD_MAX_NAME] , new_brikname[THD_MAX_NAME] ;
   char * old_bname=NULL ;
   int brick_ccode , val ;

   if( old_prefix == NULL || old_prefix[0] == '\0' ||
       new_prefix == NULL || new_prefix[0] == '\0' ||
       view < 0           || view > LAST_VIEW_TYPE   ){

      fprintf(stderr,"** THD_rename_dataset_files: illegal inputs!\n") ;
      return 0 ;
   }

#ifdef DEBUG
fprintf(stderr,"++ THD_rename_dataset_files: %s %s %d\n",old_prefix,new_prefix,view);
#endif

   /*-- create the old .HEAD filename --*/

   sprintf( old_headname , "%s+%s.%s" ,
            old_prefix , VIEW_codestr[view] , DATASET_HEADER_SUFFIX ) ;

#ifdef DEBUG
fprintf(stderr,"++ THD_rename_dataset_files: old_headname=%s\n",old_headname) ;
#endif

   if( !THD_is_file(old_headname) ){
      fprintf(stderr,"** THD_rename_dataset_files: old header %s doesn't exist!\n",
              old_headname) ;
      return 0 ;
   }

   /*-- create the new .HEAD filename --*/

   sprintf( new_headname , "%s+%s.%s" ,
            new_prefix , VIEW_codestr[view] , DATASET_HEADER_SUFFIX ) ;

#ifdef DEBUG
fprintf(stderr,"++ THD_rename_dataset_files: new_headname=%s\n",new_headname) ;
#endif

   if( THD_is_file(new_headname) ){
      fprintf(stderr,"** THD_rename_dataset_files: new header %s already exists!\n",
              new_headname) ;
      return 0 ;
   }

   /*-- create the old .BRIK filename --*/

   sprintf( old_brikname , "%s+%s.%s" ,
            old_prefix , VIEW_codestr[view] , DATASET_BRICK_SUFFIX ) ;

#ifdef DEBUG
fprintf(stderr,"++ THD_rename_dataset_files: old_brikname=%s\n",old_brikname) ;
#endif

   /*-- however, the .BRIK might have a compression suffix on it,
        which affects both its old name and its new name         --*/

   brick_ccode = COMPRESS_filecode(old_brikname) ;
   if( brick_ccode == COMPRESS_NOFILE ){
      fprintf(stderr,"++ THD_rename_dataset_files: old brick %s doesn't exist.\n",
              old_brikname) ;
      new_brikname[0] = '\0' ;  /* flag to do nothing */
   } else {
      old_bname = COMPRESS_filename( old_brikname ) ;
      if( old_bname == NULL ){  /* should not happen */
         fprintf(stderr,"** THD_rename_dataset_files: COMPRESS_filename fails!\n") ;
         return 0 ;
      }
      sprintf( new_brikname , "%s+%s.%s" ,
               new_prefix , VIEW_codestr[view] , DATASET_BRICK_SUFFIX ) ;
      if( brick_ccode >= 0 )
         strcat(new_brikname,COMPRESS_suffix[brick_ccode]) ;

#ifdef DEBUG
fprintf(stderr,"++ THD_rename_dataset_files: new_brikname=%s\n",new_brikname) ;
#endif

      if( THD_is_file(new_brikname) ){
         fprintf(stderr,"** THD_rename_dataset_files: new brick %s already exists!\n",
                 new_brikname) ;
         if( old_bname != NULL ) free(old_bname) ;
         return 0 ;
      }
   }

   /*-- actually do the renaming --*/

   fprintf(stderr,"++ THD_rename_dataset_files: rename %s -> %s\n",
           old_headname,new_headname) ;
   val = rename( old_headname , new_headname ) ;

   if( val != 0 ){
      perror("** THD_rename_dataset_files: rename() failed") ;
      if( old_bname != NULL ) free(old_bname) ;
      return 0 ;
   }

   if( new_brikname[0] != '\0' && old_bname != NULL ){
      fprintf(stderr,"++ THD_rename_dataset_files: rename %s -> %s\n",
              old_bname,new_brikname) ;
      val = rename( old_bname , new_brikname ) ;
      free(old_bname) ;

      if( val != 0 ){
         perror("** THD_rename_dataset_files: rename() failed") ;
         return 0 ;
      }
   }

   return 1 ;
}
