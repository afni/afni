/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"


/*----------------------------------------------------------------
  given a prefix and a list of files,
  create a set of datablocks for all datasets that correspond;
  return an array of datablocks.
------------------------------------------------------------------*/

THD_datablock_array * THD_init_prefix_datablocks(
                           char * prefixname , THD_string_array * regfile_list )
{
   THD_datablock_array * dblk_arr ;
   THD_datablock       * dblk ;
   int nlast , nnext , plen , ii ;
   char prefixcode[THD_MAX_NAME] , dirname[THD_MAX_NAME] , ptemp[THD_MAX_NAME] ;
   char * fname ;

   /* initialize answer */

   INIT_DBARR( dblk_arr ) ;

   if( prefixname   == NULL || strlen(prefixname) == 0 ||
       regfile_list == NULL || regfile_list->num  <= 0   ) return dblk_arr ;

   strcpy(prefixcode,prefixname) ; strcat(prefixcode,"+") ;

   /* for each file that could be a dataset header, try to make a datablock */

   nlast = 0 ;  /* start first search at beginning of list */

   do{

      /* look for the correct type of filename */

      nnext = SARR_lookfor_substring( regfile_list ,
                                      DATASET_HEADER_SUFFIX , nlast ) ;

      if( nnext < 0 ) break ;  /* no more header files */
      nlast = nnext + 1 ;      /* start next search here */

      fname = regfile_list->ar[nnext] ;
      if( strstr(fname,prefixcode) == NULL ) continue ; /* wrong prefix */
      FILENAME_TO_PREFIX(fname,ptemp) ;
      if( strcmp(prefixname,ptemp) != 0 ) continue ;    /* wrong prefix */

      /* try to make datablock from this file */

      /*-- find directory in fname --*/

      plen = strlen(fname) ;
      for( ii=plen-1 ; ii >= 0 ; ii-- ) if( fname[ii] == '/' ) break ;

      if( ii < 0 ){
         strcpy( dirname , "./" ) ;      /* fake directory name */
      } else {
         strcpy( dirname , fname ) ;
         dirname[ii+1] = '\0' ;
      }

      /* try to make datablock from this file */

      dblk = THD_init_one_datablock( dirname , fname ) ;
      if( dblk != NULL ) ADDTO_DBARR(dblk_arr,dblk) ;
      REMOVEFROM_SARR( regfile_list , nnext ) ;

   } while(1) ;

   return dblk_arr ;
}
