#include "mrilib.h"

#ifndef ALLOW_AGNI
# error "Can't compile - AGNI not enabled"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *iset , *oset ;
   char *prefix = "agni" ;
   int iarg=1 , *vmap , ii,nxyz ;
   byte *bmap ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: agni_to_afni [-prefix ppp] dset\n") ;
      exit(0) ;
   }

   if( strcmp(argv[iarg],"-prefix") == 0 ){
      prefix = argv[++iarg] ; iarg++ ;
   }

   iset = THD_open_dataset( argv[iarg] ) ;
   if( iset == NULL ){
      fprintf(stderr,"Can't open %s\n",argv[iarg]); exit(1);
   }
   if( iset->ag_sname == NULL ){
      fprintf(stderr,"%s has no .SURF file\n",argv[iarg]); exit(1);
   }
   AGNI_load( iset ) ;
   if( iset->ag_surf == NULL ){
      fprintf(stderr,"%s can't load .SURF file\n",argv[iarg]); exit(1);
   }

   oset = EDIT_empty_copy(iset) ;
   EDIT_dset_items( oset ,
                      ADN_prefix , prefix ,
                      ADN_ntt    , 0 ,
                      ADN_nvals  , 1 ,
                      ADN_type   , HEAD_FUNC_TYPE ,
                      ADN_func_type,FUNC_FIM_TYPE ,
                      ADN_datum_all, MRI_byte ,
                    ADN_none ) ;
   EDIT_substitute_brick( oset , 0 , MRI_byte , NULL ) ;

   vmap = iset->ag_vmap ;
   if( vmap == NULL ){
     vmap = AGNI_map_dset_to_surf( iset->ag_surf , iset ) ;
     if( vmap == NULL ){
       fprintf(stderr,"Can't AGNI_map_dset_to_surf\n"); exit(1);
     }
   }

   nxyz = DSET_NX(oset) * DSET_NY(oset) * DSET_NZ(oset) ;
   bmap = DSET_ARRAY(oset,0) ;
   for( ii=0 ; ii < nxyz ; ii++ )
      bmap[ii] = (vmap[ii] >= 0) ;
   free(vmap) ;
   DSET_write(oset) ; exit(0) ;
}
