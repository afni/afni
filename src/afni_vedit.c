#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Volume edit on demand: produce a new volume for display based on the
    parameters stored in dset->dblk->vedset.  [05 Sep 2006]
-----------------------------------------------------------------------------*/

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
     dblk->vedset.code = 0 ; dblk->vedset.ival = -1 ;
     EXRETURN ;
   }

   /* if we have existing editing results,
      and they correspond to the same parameters as before, then are done.
      otherwise, we clear out the existing results so they can be replaced. */

   if( dblk->vedim != NULL ){
     if( memcmp(&dblk->vedset,&vednew,sizeof(VEDIT_settings)) == 0 ){
       EXRETURN ;
     }
     mri_free(dblk->vedim); dblk->vedim=NULL;
   }

   /* at this point, must edit dataset brick */

   dblk->vedset = vednew ;  /* save settings for next time in */

   /* get volume to edit */

   ival = vednew.ival ;
   if( ival < 0 || ival > DSET_NVALS(dset) ) EXRETURN ;
   dim = DBLK_BRICK(dblk,ival) ;
   if( dim == NULL || mri_data_pointer(dim) == NULL ){
     DSET_load(dset) ;
     dim = DBLK_BRICK(dblk,ival) ;
     if( dim == NULL || mri_data_pointer(dim) == NULL ) EXRETURN ;
   }
   dim->dx = fabs(DSET_DX(dset));
   dim->dy = fabs(DSET_DY(dset));
   dim->dz = fabs(DSET_DZ(dset));

   /* edit volume into the temporary result instead */

   switch( vednew.code ){

     case VEDIT_CLUST:{
       MRI_IMAGE *tim=NULL ; int ithr ; float thr,rmm,vmul ;

       ithr = (int)vednew.param[0] ;
       if( ithr >= 0 && ithr < DSET_NVALS(dset) )
         tim = DBLK_BRICK(dblk,ithr) ;
       thr  = vednew.param[1] ;
       if( DSET_BRICK_FACTOR(dset,ithr) > 0.0f )
         thr /= DSET_BRICK_FACTOR(dset,ithr) ;
       rmm  = vednew.param[2] ; vmul = vednew.param[3] ;
       dblk->vedim = mri_clusterize( rmm, vmul, dim, thr, tim  );
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
