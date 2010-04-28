/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
    Reconcile warp and anatomy pointers between datasets that
    have been read in from multiple sessions.
    [28 Jul 2003] Modified for new THD_session struct.
------------------------------------------------------------------*/

# define IFNOANAT(ds)                                           \
   if( needed && (ds)->anat_parent == NULL )                    \
      fprintf(stderr, "\n** Can't find anat parent %s of %s",   \
             (ds)->anat_parent_idcode.str , DSET_HEADNAME(ds) )

# define IFNOWARP(ds)                                             \
   if( needed && (ds)->warp_parent == NULL && ! DSET_ONDISK(ds) ) \
      fprintf(stderr, "\n** Can't find warp parent %s of %s",     \
             (ds)->warp_parent_idcode.str , DSET_HEADNAME(ds) )

#if 0
# define SHOW_PARENTING(str,ds,dsp)                                            \
  ( printf("THD_reconcile_parents: %s of %s to %s\n",                          \
           str,(ds)->dblk->diskptr->filecode,(dsp)->dblk->diskptr->filecode) , \
    fflush(stdout) )
#else
# define SHOW_PARENTING(str,ds,dsp) /* nada */
#endif

void THD_reconcile_parents( THD_sessionlist * ssl )
{
   int iss , idd , ivv , jss , imm , needed ;
   THD_session      * sess ;
   THD_3dim_dataset * dset_orph ;
   THD_slist_find   find ;

ENTRY("THD_reconcile_parents") ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSIONLIST(ssl) || ssl->num_sess <= 0 ) EXRETURN ;

   /*-- for each session in the list --*/

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      sess = ssl->ssar[iss] ;

      /*-- for each dataset in the session --*/

      for( idd=0 ; idd < sess->num_dsset ; idd++ ){
         for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){

          dset_orph = sess->dsset[idd][ivv] ;
          if( dset_orph == NULL ) continue ;

            /*-- if it needs an anatomy parent --*/

          if( dset_orph->anat_parent == NULL ){  /* 28 Dec 2002 */
            needed = 0 ;
            if( ! ISZERO_IDCODE(dset_orph->anat_parent_idcode) ){
               needed = 1 ;
               find = THD_dset_in_sessionlist( FIND_IDCODE ,
                                               &(dset_orph->anat_parent_idcode),
                                               ssl , iss ) ;
               dset_orph->anat_parent = find.dset ;
               if( dset_orph->anat_parent != NULL )
                  SHOW_PARENTING("(ID) anat_parent",dset_orph,dset_orph->anat_parent) ;
            }
            if( dset_orph->anat_parent == NULL && strlen(dset_orph->anat_parent_name) > 0 ){
               needed = 1 ;
               find = THD_dset_in_sessionlist( FIND_NAME ,
                                               dset_orph->anat_parent_name,
                                               ssl , iss ) ;
               dset_orph->anat_parent = find.dset ;
               if( dset_orph->anat_parent != NULL )
                  SHOW_PARENTING("(NAME) anat_parent",dset_orph,dset_orph->anat_parent) ;
            }
            /** IFNOANAT(dset_orph) ; **/
          }

            /*-- if it needs a warp parent --*/

          if( dset_orph->warp_parent == NULL ){  /* 28 Dec 2002 */
            needed = 0 ;
            if( ! ISZERO_IDCODE(dset_orph->warp_parent_idcode) ){
               needed = 1 ;
               find = THD_dset_in_sessionlist( FIND_IDCODE ,
                                               &(dset_orph->warp_parent_idcode),
                                               ssl , iss ) ;
               dset_orph->warp_parent = find.dset ;
               if( dset_orph->warp_parent != NULL )
                  SHOW_PARENTING("(ID) warp_parent",dset_orph,dset_orph->warp_parent) ;
            }
            if( dset_orph->warp_parent == NULL && strlen(dset_orph->warp_parent_name) > 0 ){
               needed = 1 ;
               find = THD_dset_in_sessionlist( FIND_NAME ,
                                               dset_orph->warp_parent_name,
                                               ssl , iss ) ;
               dset_orph->warp_parent = find.dset ;
               if( dset_orph->warp_parent != NULL )
                  SHOW_PARENTING("(NAME) warp_parent",dset_orph,dset_orph->warp_parent) ;
            }
            IFNOWARP(dset_orph) ;
          }

         }
      }  /* end of loop over anat datasets */

   }  /* end of loop over sessions */

   EXRETURN ;
}
