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
   int nvox , ii , nmm ;

   MCW_cluster_array *clar ;
   int iclu , kclu ;

ENTRY("THD_automask") ;

   medim    = THD_median_brick(dset) ; if( medim == NULL ) RETURN(NULL);
   clip_val = THD_cliplevel(medim,0.5) ;
   nvox     = medim->nvox ;
   mar      = MRI_FLOAT_PTR(medim) ;
   mmm      = (byte *) calloc( sizeof(byte)*nvox , 1 ) ;
   for( nmm=ii=0 ; ii < nvox ; ii++ )
      if( mar[ii] >= clip_val ){ mmm[ii] = 1; nmm++; }

   mri_free(medim) ;
   if( nmm == 0 ) RETURN(mmm) ;  /* should not happen */

   /*-- 10 Apr 2002: only keep the largest component --*/

   clar = MCW_find_clusters( DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset) ,
                             1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01 ) ;

   /* at this point, all nonzero data in mmm has been transferred to clar */

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   /* find largest cluster */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   /* put that cluster back into the volume */

   MCW_cluster_to_vol( DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset) ,
                       MRI_byte , mmm , clar->clar[kclu]          ) ;

   DESTROY_CLARR(clar) ;

   RETURN(mmm) ;
}
