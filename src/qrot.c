#include "mrilib.h"

#define ERREX(str) (fprintf(stderr,"*** %s\n",str),exit(1))

int main( int argc , char * argv[] )
{
   THD_mat33 mat ;
   THD_fvec3 tran , vec ;
   MRI_IMAGE * matim , * vecim ;
   float     * matar , * vecar ;
   int nvec , ii ;
   char a[32],b[32],c[32] , *s ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: qrot matfile vecfile > outvecfile \n") ; exit(0) ;
   }

   matim = mri_read_ascii( argv[1] ) ;
   if( matim == NULL ) ERREX("Can't open matfile!") ;
   if( matim->nx != 4 || matim->ny != 3 ) ERREX("matfile not 4x3!") ;

   matar = MRI_FLOAT_PTR(matim) ;
   LOAD_MAT(mat,matar[0],matar[1],matar[2],
                matar[4],matar[5],matar[6],
                matar[8],matar[9],matar[10] ) ;
   LOAD_FVEC3(tran,matar[3],matar[7],matar[11]) ;

   vecim = mri_read_ascii( argv[2] ) ;
   if( vecim == NULL ) ERREX("Can't open vecfile!") ;
   if( vecim->nx != 3 ) ERREX("vecfile not 3xN!") ;

   vecar = MRI_FLOAT_PTR(vecim) ;
   nvec = vecim->ny ;
   for( ii=0 ; ii < nvec ; ii++ ){
      LOAD_FVEC3(vec,vecar[3*ii+0],vecar[3*ii+1],vecar[3*ii+2]) ;
      vec = MATVEC(mat,vec) ;
      vec = ADD_FVEC3(vec,tran) ;
      s = MV_format_fval( vec.xyz[0] ) ; strcpy(a,s) ;
      s = MV_format_fval( vec.xyz[1] ) ; strcpy(b,s) ;
      s = MV_format_fval( vec.xyz[2] ) ; strcpy(c,s) ;
      printf("%s %s %s\n",a,b,c) ;
   }
   exit(0) ;
}
