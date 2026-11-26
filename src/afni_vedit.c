#include "mrilib.h"

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+9f
#define THBOT(t) ((thrsign==0 || thrsign==2) ? (-(t)) : (-THBIG))
#define THTOP(t) ((thrsign==0 || thrsign==1) ? (t)    :  (THBIG))

/*---------------------------------------------------------------------------*/
/*! Volume edit on demand: produce a new volume for display based on the
    parameters stored in dset->dblk->vedset.  [05 Sep 2006]

    At one time, I thought there would be more than one "edit on demand"
    option, but 15 years out, there is still only the clustering. [Jul 2021]
-----------------------------------------------------------------------------*/

int AFNI_vedit( THD_3dim_dataset *dset , VEDIT_settings vednew , byte *mask )
{
   THD_datablock *dblk ;
   int ival ;
   MRI_IMAGE *dim ;

ENTRY("AFNI_vedit") ;

   if( !ISVALID_DSET(dset) ) RETURN(0) ;

   dblk = dset->dblk ;

   /*--- check if we are to clear any existing edits,
         and also set future editing status to 'do nothing' ---*/

   if( vednew.code <= 0 || vednew.code > VEDIT_LASTCODE ){
     if( dblk->vedim != NULL ){ mri_free(dblk->vedim); dblk->vedim=NULL; }
     dblk->vedset.code = 0; dblk->vedset.ival = -1; dblk->vedset.exinfo = NULL;
     RETURN(0) ;
   }

   /* if we have existing editing results,
      and they correspond to the same parameters as before, then are done.
      otherwise, we clear out the existing results so they can be replaced. */

   if( dblk->vedim != NULL && (vednew.flags&1) == 0 ){
     if( memcmp(&(dblk->vedset),&vednew,sizeof(VEDIT_settings)) == 0 )
       RETURN(0) ;                            /* exactly the same as before */

     mri_free(dblk->vedim); dblk->vedim=NULL; /* clear old results */
   }

   /*--- at this point, must edit dataset brick ---*/

   dblk->vedset = vednew ;  /* save settings for next time in */
   dblk->vedset.flags = 0 ;

   /*--- create edited volume ---*/

   if( vednew.code == VEDIT_CLUST ){  /*----- Clusterize -----*/

     MRI_IMAGE *tim=NULL ;
     float thr,rmm,vmul,thb,tht ;
     int thrsign,posfunc,ithr,bisid ;

     ival = vednew.ival ;
     if( ival < 0 || ival > DSET_NVALS(dset) ) RETURN(0) ;
     dim = DBLK_BRICK(dblk,ival) ;
     if( dim == NULL || mri_data_pointer(dim) == NULL ){
       DSET_load(dset) ;
       dim = DBLK_BRICK(dblk,ival) ;
       if( dim == NULL || mri_data_pointer(dim) == NULL ) RETURN(0) ;
     }
     dim->dx = fabs(DSET_DX(dset));
     dim->dy = fabs(DSET_DY(dset));
     dim->dz = fabs(DSET_DZ(dset));

     ithr    = (int)vednew.param[0] ;
     thrsign = (int)vednew.param[4] ;
     posfunc = (int)vednew.param[5] ;

     bisid   = (posfunc||thrsign) ? 0 : (int)vednew.param[6] ;  /* 30 Jan 2015 */

     if( ithr >= 0 && ithr < DSET_NVALS(dset) ){
       tim = DBLK_BRICK(dblk,ithr) ;
       if( bisid && THD_stat_is_2sided(DSET_BRICK_STATCODE(dset,ithr),0)==0 ) bisid = 0 ;
     }
     thr = vednew.param[1] ;
     if( DSET_BRICK_FACTOR(dset,ithr) > 0.0f )
       thr /= DSET_BRICK_FACTOR(dset,ithr) ;
     thb = THBOT(thr) ; tht = THTOP(thr) ;

     rmm  = vednew.param[2] ; vmul = vednew.param[3] ;

     /* now check for whether the overlay is RGB, which has special behavior
        here. The original/simpler/more common application is in the 'else' */
     if( dim->kind == MRI_rgb ){
       MRI_IMAGE *sim, *sout, *rgbout ;
       float *sar ;
       byte *rgb ;
       int ii ;

       /* Use AFNI brightness for clustering; preserve the RGB colors. */
       sim = mri_to_float(dim) ;
       if( sim == NULL ) RETURN(0) ;
       sim->dx = dim->dx ;
       sim->dy = dim->dy ;
       sim->dz = dim->dz ;

       if( bisid )
         sout = mri_bi_clusterize(
                  rmm,vmul,sim,thb,tht,tim,mask ) ;
       else
         sout = mri_clusterize(
                  rmm,vmul,sim,thb,tht,tim,posfunc,mask ) ;

       mri_free(sim) ;
       if( sout == NULL ) RETURN(0) ;

       /* Keep exact original RGB triplets only at surviving voxels. */
       rgbout = mri_copy(dim) ;
       if( rgbout == NULL ){
         mri_free(sout) ;
         RETURN(0) ;
       }

       sar = MRI_FLOAT_PTR(sout) ;
       rgb = MRI_RGB_PTR(rgbout) ;
       for( ii=0 ; ii < rgbout->nvox ; ii++ ){
         if( sar[ii] == 0 )
           rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = 0 ;
       }

       mri_free(sout) ;
       dblk->vedim = rgbout ;

     } else {

       /* Primary/original behavior for non-RGB overlays. */
       if( bisid )
         dblk->vedim = mri_bi_clusterize( rmm,vmul,dim,thb,tht,tim,          mask );
       else
         dblk->vedim = mri_clusterize   ( rmm,vmul,dim,thb,tht,tim,posfunc , mask );
     }

   }

   /*--- done ---*/

   RETURN(1) ;
}

/*---------------------------------------------------------------------------*/

void AFNI_vedit_clear( THD_3dim_dataset *dset )
{
   VEDIT_settings vs ;
ENTRY("AFNI_vedit_clear") ;
   memset(&vs,0,sizeof(VEDIT_settings)) ;
   (void)AFNI_vedit( dset , vs , NULL ) ;
   EXRETURN ;
}
