/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*============ 08 Feb 2001: actually implement this code - RWCox ============*/

#include "mrilib.h"
#include "vecmat.h"

int main( int argc , char * argv[] )
{
   int iarg=1 , nn , invert,nadd ;
   THD_dmat33 tmat , qmat , imat ;
   THD_dfvec3 tvec , qvec , ivec ;
   FILE * fp ;
   double dd[12] ;
   THD_dvecmat dvm ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: cat_matvec matvec_spec matvec_spec ...\n"
             "\n"
             "Catenates 3D rotation+shift matrix+vector transformations.\n"
             "Each matvec_spec is of the form\n"
             "\n"
             "  mfile [-opkey]\n"
             "\n"
             "'mfile' can take 2 forms:\n"
             "\n"
             "=== FORM 1 ===\n"
             "mfile is the name of an ASCII file with 12 numbers arranged\n"
             "in 3 lines:\n"
             "      u11 u12 u13 v1\n"
             "      u21 u22 u23 v2\n"
             "      u31 u32 u33 u3\n"
             "where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij]\n"
             "is the matrix of the transform, and the 3-vector [vi] is the\n"
             "shift.  The transform is [xnew] = [uij]*[xold] + [vi].\n"
             "\n"
             "=== FORM 2 ===\n"
             "mfile is of the form 'dataset::attribute', where 'dataset'\n"
             "is the name of an AFNI dataset, and 'attribute' is the name\n"
             "of an attribute in the dataset's header that contains a\n"
             "matrix+vector (e.g., 'fred+orig::VOLREG_MATVEC_000000').\n"
             "\n"
             "=== COMPUTATIONS ===\n"
             "If [U] [v] are the matrix/vector for the first mfile, and\n"
             "   [A] [b] are the matrix/vector for the second mfile, then\n"
             "the catenated transformation is\n"
             "  matrix = [A][U]   vector = [A][v] + [b]\n"
             "That is, the second mfile transformation follows the first.\n"
             "\n"
             "The optional 'opkey' (operation key) following each mfile\n"
             "starts with a '-', and then is a set of letters telling how\n"
             "to treat the input.  The only opkey currently defined is\n"
             "\n"
             "  -I = invert the transformation:\n"
             "                     -1              -1\n"
             "       [xold] = [uij]  [xnew] - [uij]  [vi]\n"
             "\n"
             "The transformation resulting by catenating the transformations\n"
             "is written to stdout in the sam ASCII file format.  This can be\n"
             "used as input to 3drotate -matvec_dicom (provided [uij] is a\n"
             "proper orthogonal matrix).\n"
             "\n"
             "N.B.: If exactly 9 numbers can be read from an mfile, then those\n"
             "      values form the matrix, and the vector is set to zero.\n"
           ) ;
      exit(0) ;
   }

   /* initialize identity transformation into tmat,tvec */

   LOAD_DIAG_DMAT(tmat,1.0,1.0,1.0) ;
   LOAD_DFVEC3(tvec,0.0,0.0,0.0) ;

   /* loop and read arguments, process them */

   while( iarg < argc ){

      nadd = 1 ; invert = 0 ;
      if( iarg+1 < argc && strcmp(argv[iarg+1],"-I") == 0 ){
         invert = 1 ; nadd = 2 ;
      }
      dvm = THD_read_dvecmat( argv[iarg] , invert ) ;
      qmat = dvm.mm ; qvec = dvm.vv ;

      if( SIZE_DMAT(qmat) == 0.0 ){
         fprintf(stderr,"*** ERROR: can't read mfile %s\n",argv[iarg]) ;
         exit(1) ;
      }

      iarg += nadd ;

      /* multiply into accumulating transformation */

      imat = DMAT_MUL( qmat , tmat ) ;
      ivec = DMATVEC( qmat , tvec ) ;
      tvec = ADD_DFVEC3( qvec , ivec ) ;
      tmat = imat ;
   }

   /* write results to stdout */

   printf("%13.6g %13.6g %13.6g %13.6g\n"
          "%13.6g %13.6g %13.6g %13.6g\n"
          "%13.6g %13.6g %13.6g %13.6g\n" ,
     tmat.mat[0][0] , tmat.mat[0][1] , tmat.mat[0][2] , tvec.xyz[0] ,
     tmat.mat[1][0] , tmat.mat[1][1] , tmat.mat[1][2] , tvec.xyz[1] ,
     tmat.mat[2][0] , tmat.mat[2][1] , tmat.mat[2][2] , tvec.xyz[2]  ) ;

   exit(0) ;
}
