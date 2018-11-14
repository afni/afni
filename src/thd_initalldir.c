/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------------
   given a directory name, create datablocks from all entries possible.
   return an array of datablock arrays
------------------------------------------------------------------------*/

RwcPointer_array * THD_init_alldir_datablocks( char * dirname )
{
   RwcPointer_array     * super_array ;
   THD_datablock_array * dblk_arr ;
   THD_string_array    * flist , * rlist ;
   char prefix[THD_MAX_NAME] ;
   char * fname ;
   int ifile ;

   /* initialize answer */

   INIT_XTARR( super_array ) ;

   /* get list of all regular files in the directory */

   flist = THD_get_all_filenames( dirname ) ;
   if( flist == NULL || flist->num <= 0 ){
      DESTROY_SARR(flist) ;
      return super_array ;
   }

   rlist = THD_extract_regular_files( flist ) ;
   DESTROY_SARR(flist) ;
   if( rlist == NULL || rlist->num <= 0 ){
      DESTROY_SARR(rlist) ;
      return super_array ;
   }

   /* for each header/prefix combination
      in the list, try to get all datablocks within */

   for( ifile=0 ; ifile < rlist->num ; ifile++ ){

      fname = rlist->ar[ifile] ;
      if( fname == NULL ||
          strstr(fname,DATASET_HEADER_SUFFIX) == NULL ) continue ;

      FILENAME_TO_PREFIX(fname,prefix) ;
      if( strlen(prefix) == 0 ) continue ;

      dblk_arr = THD_init_prefix_datablocks( prefix , rlist ) ;

      if( dblk_arr != NULL && dblk_arr->num > 0 ){
         ADDTO_XTARR(super_array,dblk_arr) ;       /* store in output */
      } else {
         FREE_DBARR(dblk_arr) ;                    /* throw it away */
      }

   }

   /* done! */

   DESTROY_SARR(rlist) ;
   return super_array ;
}
