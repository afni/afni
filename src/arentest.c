#include "aren.h"
#include <stdlib.h>

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * oset ;
   MRI_IMAGE * grim , * opim , * renim ;
   float theta , phi , sx,sy,sz ;
   void * arh ;
   char fname[128] ;
   int nopt ;

   dset = THD_open_dataset( argv[1] ) ;
   if( dset == NULL ) exit(1) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) exit(1) ;
   grim = DSET_BRICK(dset,0) ;
   sx = DSET_NX(dset) * DSET_DX(dset) ;
   sy = DSET_NY(dset) * DSET_DY(dset) ;
   sz = DSET_NZ(dset) * DSET_DZ(dset) ;

   oset = THD_open_dataset( argv[2] ) ;
   if( oset == NULL ) exit(1) ;
   DSET_load(oset) ; if( !DSET_LOADED(oset) ) exit(1) ;
   opim = DSET_BRICK(oset,0) ;

   { int nvox = DSET_NVOX(oset) , ii ;
     byte * opar = MRI_BYTE_PTR(opim) ;
     for( ii=0        ; ii < nvox/4 ; ii++ ) opar[ii] = 0 ;
     for( ii=3*nvox/4 ; ii < nvox   ; ii++ ) opar[ii] = 0 ;
   }

   arh = new_AREN_renderer() ; AREN_be_verbose(arh) ;

#if 1
   AREN_set_graybrick( arh , grim ) ;
#else
   AREN_set_rgbbricks( arh , grim,grim,grim ) ;
#endif

   AREN_set_opabrick ( arh , opim ) ;
   AREN_set_size     ( arh , sx,sy,sz ) ;

#if 0
   DSET_delete(dset) ; DSET_delete(oset) ;
#endif

   for( nopt=3 ; nopt < argc-1 ; nopt+=2 ){
      theta = strtod( argv[nopt]   , NULL ) ;
      phi   = strtod( argv[nopt+1] , NULL ) ;
      AREN_set_viewpoint( arh , theta , phi ) ;
      renim = AREN_render( arh , 256 ) ;
      sprintf(fname,"aren.%03d.pnm",(nopt-1)/2) ;
      mri_write_pnm( fname , renim ) ;
      mri_free(renim) ;
   }

   exit(0) ;
}
