#define FLOAT_TYPE double  /* must be same as in thd_rot3d.c */
#include "vecmat.h"
#include "mrilib.h"

/**********************************************************************************/

#define ERREX(str) (fprintf(stderr,"*** %s\n",str),exit(1))

int main( int argc , char * argv[] )
{
   MRI_IMAGE * vecim ;
   float * vecar ;
   THD_fvec3 * xx , * yy ;
   int nvec , ii ;
   THD_vecmat rt ;
   char a[32],b[32],c[32],d[32] , *s ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTagalign vecfile1 vecfile2\n") ; exit(0) ;
   }

   /*-*/

   vecim = mri_read_ascii( argv[1] ) ;
   if( vecim == NULL ) ERREX("Can't open vecfile1\n") ;
   if( vecim->nx != 3 || vecim->ny < 4 ) ERREX("vecfile1 badly formed") ;
   nvec = vecim->ny ;
   vecar = MRI_FLOAT_PTR(vecim) ;

   xx = (THD_fvec3 *) malloc( sizeof(THD_fvec3) * nvec ) ;
   for( ii=0 ; ii < nvec ; ii++ ){
      LOAD_FVEC3( xx[ii] , vecar[3*ii+0],vecar[3*ii+1],vecar[3*ii+2]) ;
   }
   mri_free(vecim) ;

   /*-*/

   vecim = mri_read_ascii( argv[2] ) ;
   if( vecim == NULL ) ERREX("Can't open vecfile2\n") ;
   if( vecim->nx != 3 || vecim->ny != nvec ) ERREX("vecfile2 badly formed") ;
   vecar = MRI_FLOAT_PTR(vecim) ;

   yy = (THD_fvec3 *) malloc( sizeof(THD_fvec3) * nvec ) ;
   for( ii=0 ; ii < nvec ; ii++ ){
      LOAD_FVEC3( yy[ii] , vecar[3*ii+0],vecar[3*ii+1],vecar[3*ii+2]) ;
   }
   mri_free(vecim) ;

   rt = LSQ_rot_trans( nvec , xx , yy , NULL ) ;

   for( ii=0 ; ii < 3 ; ii++ ){
      s = MV_format_fval( rt.mm.mat[ii][0] ) ; strcpy(a,s) ;
      s = MV_format_fval( rt.mm.mat[ii][1] ) ; strcpy(b,s) ;
      s = MV_format_fval( rt.mm.mat[ii][2] ) ; strcpy(c,s) ;
      s = MV_format_fval( rt.vv.xyz[ii]    ) ; strcpy(d,s) ;
      printf("%s %s %s %s\n",a,b,c,d) ;
   }

   exit(0) ;
}
