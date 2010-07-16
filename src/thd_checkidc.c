/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
    Check to see if there are any duplicate ID codes in the
    datasets stored herein.
    [28 Jul 2003] Modified for new THD_session struct.
------------------------------------------------------------------*/

void THD_check_idcodes( THD_sessionlist *ssl )
{
   int iss , idd,jdd , ivv , dsnum , nd ;
   THD_session *sess ;
   THD_3dim_dataset * dset , **dsl ;

ENTRY("THD_check_idcodes") ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSIONLIST(ssl) || ssl->num_sess <= 0 ) EXRETURN ;

   /*-- count number of datasets --*/

   for( dsnum=iss=0 ; iss < ssl->num_sess ; iss++ ){
     sess = ssl->ssar[iss] ; if( !ISVALID_SESSION(sess) ) continue ;
     for( idd=0 ; idd < sess->num_dsset ; idd++ ){
       for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){
         dset = GET_SESSION_DSET(sess, idd, ivv);
/*         dset = sess->dsset_xform_table[idd][ivv] ;*/
         if( ISVALID_DSET(dset) ) dsnum++ ;
       }
     }
   }
   STATUS("counted datasets") ;
   if( dsnum < 2 ) EXRETURN ;  /* 21 Feb 2007 */

   /*-- make list of datasets --*/

   dsl = (THD_3dim_dataset **) malloc( sizeof(THD_3dim_dataset *) * dsnum ) ;

   for( nd=iss=0 ; iss < ssl->num_sess ; iss++ ){
     sess = ssl->ssar[iss] ; if( !ISVALID_SESSION(sess) ) continue ;
     for( idd=0 ; idd < sess->num_dsset ; idd++ ){
       for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){
         dset = GET_SESSION_DSET(sess, idd, ivv);
/*         dset = sess->dsset_xform_table[idd][ivv] ;*/
         if( ISVALID_DSET(dset) ) dsl[nd++] = dset ;
       }
     }
   }
   STATUS("collected datasets") ;

   /*-- check list for duplicates --*/

   for( iss=idd=0 ; idd < dsnum-1 ; idd++ ){
     nd = 0 ;
     for( jdd=idd+1 ; jdd < dsnum ; jdd++ ){
       if( DUPLICATE_DSETS(dsl[idd],dsl[jdd]) ){ /* 20 Dec 2001: change EQUIV_IDCODES() to DUPLICATE_DSETS() */
         fprintf(stderr,
                 "\n*** WARNING: Identical ID codes in %s and %s",
                 DSET_HEADNAME(dsl[idd]) , DSET_HEADNAME(dsl[jdd]) ) ;
         iss++ ;
       }
     }
   }

   if( iss > 0 ) fprintf(stderr,"\n") ;

   free(dsl) ; EXRETURN ;
}
