#include "mrilib.h"

/*---------------------------------------------------------------------------*/

void AFNI_vedit( THD_3dim_dataset *dset , VEDIT_settings vednew )
{
   THD_datablock *dblk ;
   int ival ;
   MRI_IMAGE *dim ;

ENTRY("AFNI_vedit") ;

   if( !ISVALID_DSET(dset) ) EXRETURN ;

   dblk = dset->dblk ;

   /* check if we are to clear any existing edits,
      and also set future editing status to 'do nothing' */

   if( vednew.code <= 0 || vednew.code > VEDIT_LASTCODE ){
     if( dblk->vedim != NULL ){ mri_free(dblk->vedim); dblk->vedim=NULL; }
     dblk->vedset.code = 0 ;
     EXRETURN ;
   }

   /* if we have existing editing results,
      and they correspond to the same parameters as before, then are done.
      otherwise, we clear out the existing results so they can be replaced. */

   if( dblk->vedim != NULL ){
     if( memcmp(&dblk->vedset,&vednew,sizeof(VEDIT_settings)) == 0 ) EXRETURN ;
     mri_free(dblk->vedim); dblk->vedim=NULL;
   }

   /* at this point, must edit dataset brick */

   dblk->vedset = vednew ;  /* save settings for next time in */

   /* get volume to edit */

   ival = vednew.ival ;
   if( ival < 0 || ival > DSET_NVALS(dset) ) EXRETURN ;
   dim = DBLK_BRICK(dblk,ival) ;
   if( dim == NULL ){
     DSET_load(dset) ;
     dim = DBLK_BRICK(dblk,ival) ;
     if( dim == NULL ) EXRETURN ;
   }

   /* edit volume into the temporary result instead */

   switch( vednew.code ){

     case VEDIT_CLUST:{
       MRI_IMAGE *tim=NULL ; int ithr ; float thr,rmm,vmul ;

       ithr = (int)vednew.param[0] ;
       if( ithr >= 0 && ithr < DSET_NVALS(dset) )
         tim = DBLK_BRICK(dblk,ithr) ;
       thr  = vednew.param[1] ;
       rmm  = vednew.param[2] ;
       vmul = vednew.param[3] ;
       dblk->vedim = mri_clusterize( vednew.param[2], vednew.param[3], dim,
                                                      vednew.param[1], tim  );
     }
     break ;

   }

   /*--- done ---*/

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_vedit_clear( THD_3dim_dataset *dset )
{
   VEDIT_settings vs ;
ENTRY("AFNI_vedit_clear") ;
   memset(&vs,0,sizeof(VEDIT_settings)) ;
   AFNI_vedit( dset , vs ) ;
   EXRETURN ;
}
