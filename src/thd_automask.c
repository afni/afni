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
   int nvox , ii , nmm , nx,ny,nz ;

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

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;

   clar = MCW_find_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01   ) ;

   /* at this point, all nonzero data in mmm has been transferred to clar */

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   /* find largest cluster */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   /* put that cluster back into the volume */

   MCW_cluster_to_vol( nx,ny,nz , MRI_byte,mmm , clar->clar[kclu] ) ;

   DESTROY_CLARR(clar) ;

#if 1
   /* 18 Apr 2002: now erode the resulting volume */

   MCW_erode_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                       MRI_byte,mmm , 1.42 , 0.90 , 1 ) ;

   /* now recluster it, and again keep only the largest survivor */

   clar = MCW_find_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01   ) ;

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   MCW_cluster_to_vol( nx,ny,nz , MRI_byte,mmm , clar->clar[kclu] ) ;

   DESTROY_CLARR(clar) ;
#endif

   RETURN(mmm) ;
}
