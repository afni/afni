#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAutobox dataset\n"
             "Computes size of a box that fits around the volume.\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAutobox main"); machdep(); AFNI_logger("3dAutobox",argc,argv);

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   ){
      fprintf(stderr,"** ILLEGAL dataset type\n"); exit(1);
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   { int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
     int ii,jj,kk ;

     { int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1 ;
       THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp ) ;
       fprintf(stderr,"++ Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
               xm,xp,ym,yp,zm,zp ) ;
     }
   }

   exit(0) ;
}
