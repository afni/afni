#include "mrilib.h"

/*---------------------------------------------------------------------
  Make a byte mask for a 3D+time dataset -- 13 Aug 2001 - RWCox
  (Compare to thd_makemask.c)
-----------------------------------------------------------------------*/

byte * THD_automask( THD_3dim_dataset * dset )
{
   MRI_IMAGE *medim ;
   float clip_val , *mar ;
   byte *mmm = NULL ;
   int nvox , ii ;

ENTRY("THD_automask") ;

   medim    = THD_median_brick(dset) ; if( medim == NULL ) RETURN(NULL);
   clip_val = THD_cliplevel(medim,0.5) ;
   nvox     = medim->nvox ;
   mar      = MRI_FLOAT_PTR(medim) ;
   mmm      = (byte *) calloc( sizeof(byte)*nvox , 1 ) ;
   for( ii=0 ; ii < nvox ; ii++ )
      if( mar[ii] >= clip_val ) mmm[ii] = 1 ;

   mri_free(medim) ; RETURN(mmm) ;
}
