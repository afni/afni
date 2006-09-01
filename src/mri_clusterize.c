#include "mrilib.h"

MRI_IMAGE * mri_clusterize( float rmm , float vmul , MRI_IMAGE *bim ,
                                        float thr  , MRI_IMAGE *tim  )
{
   float dx,dy,dz , dbot ;
   int   nx,ny,nz , ptmin,iclu ;
   MRI_IMAGE *cim ; void *car ;
   MCW_cluster *cl ; MCW_cluster_array*clar ;

ENTRY("mri_clusterize") ;

   if( bim == NULL ) RETURN(NULL) ;

   nx = bim->nx; ny = bim->ny; nz = bim->nz;
   dx = bim->dx; dy = bim->dy; dz = bim->nz;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   dbot = MIN(dx,dy) ; dbot = MIN(dbot,dz) ;

   if( rmm < dbot ){ dx = dy = dz = 1.0f; rmm = 1.01f; }
   if( vmul <= 2.0*dx*dy*dz ) RETURN(NULL) ;

   cim = mri_copy(bim) ; car = mri_data_pointer(cim) ;

   if( thr > 0.0f && tim != NULL )
     mri_threshold( -thr , thr , tim , cim ) ;

   ptmin = (int)( vmul/(dx*dy*dz) + 0.99f ) ;

   clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz , cim->kind,car , rmm ) ;
   if( clar != NULL ){
     for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
       cl = clar->clar[iclu] ;
       if( cl->num_pt >= ptmin )  /* put back into array */
          MCW_cluster_to_vol( nx,ny,nz , cim->kind,car , cl ) ;
     }
     DESTROY_CLARR(clar) ;
   }

   RETURN(cim) ;
}
