/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"


/*---------------------------------------------------------------------
   Given a directory name, read in all the datasets and make
   a session data structure from them.
-----------------------------------------------------------------------*/

THD_session * THD_init_session( char * sessname )
{
   THD_session            * sess ;
   XtPointer_array        * dblk_arrarr ;
   THD_datablock_array    * dblk_arr ;
   THD_3dim_dataset       * dset ;
   THD_3dim_dataset_array * dset_arr ;

   int ibar , idset , iview  ;
   Boolean all_anat , all_func ;

   /*-- sanity check --*/

   if( sessname == NULL || strlen(sessname) == 0 || ! THD_is_directory(sessname) )
      return NULL ;

   /*-- initialize session --*/

   sess         = myXtNew( THD_session ) ;
   sess->type   = SESSION_TYPE ;
   sess->parent = NULL ;
   BLANK_SESSION(sess) ;  /* null out all entries */

   /* save directory name, with a trailing slash */

   MCW_strncpy( sess->sessname , sessname , THD_MAX_NAME ) ;
   iview = strlen(sess->sessname) ;
   if( sess->sessname[iview-1] != '/' ){  /* tack trailing / onto sessname */
      sess->sessname[iview]   = '/' ;
      sess->sessname[iview+1] = '\0' ;
   } else {
      iview-- ;  /* iview now points to last non-NUL character in string */
   }

   /* save last name from sessname */
#if 1
   { char * env = my_getenv( "AFNI_SESSTRAIL" ) ; int tt = 0 ;
     if( env != NULL ) tt = strtol(env,NULL,10) ;
     env = THD_trailname(sess->sessname,tt) ;
     tt = 1+strlen(env) - THD_MAX_LABEL ; if( tt < 0 ) tt = 0 ;
     strcpy( sess->lastname , env+tt ) ;
   }
#else
     for( iview-- ; iview >= 0 ; iview-- ) if( sess->sessname[iview] == '/' ) break ;
     MCW_strncpy( sess->lastname , &(sess->sessname[iview+1]) , THD_MAX_LABEL ) ;
#endif

   /*-- read all datablocks --*/

   dblk_arrarr = THD_init_alldir_datablocks( sess->sessname ) ;
   if( dblk_arrarr->num <= 0 ){ myXtFree( sess ) ; return NULL ; }

   /*-- for each datablock array ... --*/

   for( ibar=0 ; ibar < dblk_arrarr->num ; ibar++ ){

      /*-- get the current array of datablocks --*/

      dblk_arr = (THD_datablock_array *) dblk_arrarr->ar[ibar] ;
      if( dblk_arr == NULL || dblk_arr->num <= 0 ) continue ;

      /*-- convert it into an array of datasets --*/

      dset_arr = THD_array_3dim_from_block( dblk_arr ) ;
      if( dset_arr == NULL || dset_arr->num <= 0 ) continue ;

      /*-- are we dealing with anatomy or function? --*/

      all_anat = ISANAT(dset_arr->ar[0]) ;
      all_func = ISFUNC(dset_arr->ar[0]) ;

      if( !all_anat && !all_func ){
         fprintf(stderr,
            "\n*** THD_init_session: %s - illegal dataset types encountered ***\n",
            sessname ) ;
         for( idset=0 ; idset < dset_arr->num ; idset++ )
            THD_delete_3dim_dataset( dset_arr->ar[idset] , False ) ;
         FREE_3DARR(dset_arr) ;
         continue ;
      }

      /*-- function case: place into next row of function table --*/

      if( all_func ){
         int nf = sess->num_func ;  /* next row index */

         if( nf >= THD_MAX_SESSION_FUNC ){
            fprintf(stderr,
             "\n*** Session %s function table overflow with dataset %s ***\n",
             sessname , dset_arr->ar[0]->self_name) ;
            for( idset=0 ; idset < dset_arr->num ; idset++ )
               THD_delete_3dim_dataset( dset_arr->ar[idset] , False ) ;
            FREE_3DARR(dset_arr) ;
            continue ;  /* skip to next dblk_arr (ibar loop) */
         }

         /*-- put each dataset into this row in its view place --*/

         for( idset=0 ; idset < dset_arr->num ; idset++ ){
            dset  = dset_arr->ar[idset] ;
            iview = dset->view_type ;

            if( sess->func[nf][iview] != NULL ){
               fprintf(stderr,
                "\n*** Session %s has duplicate function views of %s ***\n",
                sessname , dset->self_name) ;
               THD_delete_3dim_dataset( dset , False ) ;
            } else {
               sess->func[nf][iview] = dset ;
            }
         }

         (sess->num_func)++ ;  /* increment functional row count */
      }

      /*-- anat case: place into next row of anatomy table --*/

      if( all_anat ){
         int na = sess->num_anat ;  /* anatomy row index */

         if( na >= THD_MAX_SESSION_ANAT ){
            fprintf(stderr,
             "\n*** Session %s anatomy table overflow with dataset %s ***\n",
             sessname , dset_arr->ar[0]->self_name) ;
            for( idset=0 ; idset < dset_arr->num ; idset++ )
               THD_delete_3dim_dataset( dset_arr->ar[idset] , False ) ;
            FREE_3DARR(dset_arr) ;
            continue ;  /* skip to next dblk_arr (ibar loop) */
         }

         /*-- put each dataset into this row in its view place --*/

         for( idset=0 ; idset < dset_arr->num ; idset++ ){
            dset  = dset_arr->ar[idset] ;
            iview = dset->view_type ;

            if( sess->anat[na][iview] != NULL ){
               fprintf(stderr,
                "\n*** Session %s has duplicate anatomical views of %s ***\n",
                sessname , dset->self_name) ;
               THD_delete_3dim_dataset( dset , False ) ;
            } else {
               sess->anat[na][iview] = dset ;
            }
         }

         (sess->num_anat)++ ;  /* increment anatomical row count */
      }

      FREE_3DARR(dset_arr) ;

   } /* end of loop over each datablock array (ibar) */

   /*-- throw away the datablock arrays at this point --*/

   for( ibar=0 ; ibar < dblk_arrarr->num ; ibar++ ){
      dblk_arr = (THD_datablock_array *) dblk_arrarr->ar[ibar] ;
      FREE_DBARR( dblk_arr ) ;
   }
   FREE_XTARR( dblk_arrarr ) ;

   /*-- done! --*/

   return sess ;
}
