/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "vecmat.h"

/*-------------------------------------------------------------------------------*/
void usage_cat_matvec(int detail) {

      printf(
"Usage: cat_matvec [-MATRIX | -ONELINE] matvec_spec matvec_spec ...\n"
 "\n"
 "Catenates 3D rotation+shift matrix+vector transformations.\n"
 "Each matvec_spec is of the form\n"
 "\n"
 "  mfile [-opkey]\n"
 "\n"
 "'mfile' specifies the matrix, and can take 4(ish) forms:\n"
 "\n"
 "=== FORM 1 ===\n"
 "mfile is the name of an ASCII file with 12 numbers arranged\n"
 "in 3 lines:\n"
 "      u11 u12 u13 v1\n"
 "      u21 u22 u23 v2\n"
 "      u31 u32 u33 v3\n"
 "where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij]\n"
 "is the matrix of the transform, and the 3-vector [vi] is the\n"
 "shift.  The transform is [xnew] = [uij]*[xold] + [vi].\n"
 "\n"
 "=== FORM 1a === [added 24 Jul 2007]\n"
 "mfile is the name of an ASCII file with multiple rows, each\n"
 "containing 12 numbers in the order\n"
 "  u11 u12 u13 v1 u21 u22 u23 v2 u31 u32 u33 v3\n"
 "The filename must end in the characters '.aff12.1D', as output\n"
 "by the '-1Dmatrix_save' option in 3dAllineate and 3dvolreg.\n"
 "Each row of this file is treated as a separate matrix, and\n"
 "multiple matrices will be computed.\n"
 "** N.B.: At most ONE input matrix can be in this format! **\n"
 "\n"
 "=== FORM 2 ===\n"
 "mfile is of the form 'dataset::attribute', where 'dataset'\n"
 "is the name of an AFNI dataset, and 'attribute' is the name\n"
 "of an attribute in the dataset's header that contains a\n"
 "matrix+vector.  Examples:\n"
 " 'fred+orig::VOLREG_MATVEC_000000'        = fred+orig from 3dvolreg\n"
 " 'fred+acpc::WARP_DATA'                   = fred+acpc warped in AFNI\n"
 " 'fred+orig::WARPDRIVE_MATVEC_FOR_000000' = fred+orig from 3dWarpDrive\n"
 " 'fred+orig::ROTATE_MATVEC_000000'        = fred+orig from 3drotate\n"
 " For matrices to turn voxel coordinates to dicom:\n"
 " 'fred+orig::IJK_TO_CARD_DICOM'   \n"
 " 'fred+orig::IJK_TO_DICOM_REAL'        \n"                        
 "\n"
 "Note that both of VOLREG_MATVEC_ and ROTATE_MATVEC_ are usually\n"
 "accompanied with VOLREG_CENTER_OLD and VOLREG_CENTER_BASE or\n"
 "ROTATE_CENTER_OLD and ROTATE_CENTER_BASE attributes.\n"
 "These center attributes are automatically taken into account in\n"
 "cat_matvec's output.\n" 
 "\n"
 "=== FORM 3 ===\n"
 "mfile is of the form\n"
 " 'MATRIX(u11,u12,u13,v1,u21,u22,u23,v2,u31,u32,u33,v3)'\n"
 "directly giving all 12 numbers on the command line.  You will\n"
 "need the 'forward single quotes' around this argument.\n"
 "\n"
 "=== FORM 4 ===\n"
 "mfile is of the form\n"
 " '-rotate xI yR zA'\n"
 "where 'x', 'y', and 'z' are angles in degrees, specifying rotations\n"
 "about the I, R, and A axes respectively.  The letters 'I', 'R', 'A'\n"
 "specify the axes, and can be altered as in program 3drotate.\n"
 "(The 'quotes' are mandatory here because the argument contains spaces.)\n"
 "\n"
 "\n"
 "=== COMPUTATIONS ===\n"
 "If [U] [v] are the matrix/vector for the first mfile, and\n"
 "   [A] [b] are the matrix/vector for the second mfile, then\n"
 "the catenated transformation is\n"
 "  matrix = [A][U]   vector = [A][v] + [b]\n"
 "That is, the second mfile transformation follows the first.\n"
 "** Thus, the order of matrix multiplication is exactly the  **\n"
 "** opposite of the order of the inputs on the command line! **\n"
 "\n"
 "The optional 'opkey' (operation key) following each mfile\n"
 "starts with a '-', and then is a set of letters telling how\n"
 "to treat the input.  The opkeys currently defined are:\n"
 "\n"
 "  -I = invert the transformation:\n"
 "                     -1              -1\n"
 "       [xold] = [uij]  [xnew] - [uij]  [vi]\n"
 "\n"
 "  -P = Do a polar decomposition on the 3x3 matrix part \n"
 "       of the mfile. This would result in an orthogonal\n"
 "       matrix (rotation only, no scaling) Q that is closest,\n"
 "       in the Frobenius distance sense, to the input matrix A.\n"
 "    Note: if A = R * S * E, where R, S and E are the Rotation,\n"
 "       Scale, and shEar matrices, respctively, Q does not \n"
 "       necessarily equal R because of interaction; Each of R,\n"
 "       S and E affects most of the columns in matrix A.\n"
 "\n"
 "  -IP = -I followed by -P\n"
 "\n"
 "  -S = square root of the matrix\n"
 "    Note: Not all matrices have square roots!\n"
 "       The square root of a matrix will do 'half' the transformation.\n"
 "       One application: 3dLRflip + 3dAllineate to register a volume\n"
 "       to its mirror image, then apply half the transformation to\n"
 "       bring it into vertical alignment.\n"
 "\n"
 "The transformation resulting by catenating the transformations\n"
 "is written to stdout in the same 3x4 ASCII file format.  This can\n"
 "be used as input to '3drotate -matvec_dicom' (provided [uij] is a\n"
 "proper orthogonal matrix), or to '3dWarp -matvec_xxx'.\n"
 "\n"
 "  -MATRIX: indicates that the resulting matrix will\n"
 "      be written to stdout in the 'MATRIX(...)' format (FORM 3).\n"
 "      This feature could be used, with clever scripting, to input\n"
 "      a matrix directly on the command line to program 3dWarp.\n"
 "  -ONELINE: option indicates that the resulting matrix\n"
 "      will simply be written as 12 numbers on one line.\n"
 "  -4x4: Output matrix in augmented form (last row is 0 0 0 1)\n"
 "        This option does not work with -MATRIX or -ONELINE\n"
 "N.B.: If only 9 numbers can be read from an mfile, then those\n"
 "      values form the [uij] matrix, and the vector is set to zero.\n"
 "N.B.: If form 1a (.aff12.1D) is used to compute multiple matrices,\n"
 "      then the output matrices are written to stdout, one matrix\n"
 "      per line.\n"
           ) ;
   
   return;
}

int main( int argc , char * argv[] )
{
   int iarg=1 , nn , invert,nadd , polar, do_sqrt, i, j;
   THD_dmat33 *tmat , qmat , imat ;
   THD_dfvec3 *tvec , qvec , ivec ;
   int ntmat, augmat = 0 ;
   FILE *fp ;
   THD_dvecmat *dvm , qvm ; int ndvm ;
   int matout=0 , oneline=0 ;
   MRI_IMAGE *multi_im=NULL ;
   float     *multi_far=NULL ;
   int        multi_nx, multi_ny , dd ;  /* nx=# of values per row, ny=# of rows */

   /* initialize identity transformation into tmat,tvec */

   mainENTRY("cat_matvec"); machdep();

   ntmat = 1 ;
   tmat = (THD_dmat33 *)malloc(sizeof(THD_dmat33)) ;
   tvec = (THD_dfvec3 *)malloc(sizeof(THD_dfvec3)) ;
   LOAD_DIAG_DMAT(tmat[0],1.0,1.0,1.0) ;
   LOAD_DFVEC3   (tvec[0],0.0,0.0,0.0) ;

   /* loop and read arguments, process them */

   while( iarg < argc ){
      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         usage_cat_matvec(strlen(argv[iarg])>3?2:1);
         exit(0) ;
      }


      if( strcmp(argv[iarg],"-MATRIX") == 0 ){
        oneline = 0 ; matout = 1 ; augmat = 0; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-4x4") == 0 ){
        oneline = 0 ; matout = 0 ; ntmat = 1 ; augmat = 1; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-ONELINE") == 0 ){
        oneline = 1 ; matout = 0 ; iarg++ ; continue ;
      }

      /* check for opcodes that follow the next argument */
 
      nadd = 1; invert = 0; polar = 0; do_sqrt = 0;
      if( iarg+1 < argc ) {   
         if (strcmp(argv[iarg+1],"-I") == 0 ){
            invert = 1 ; nadd = 2 ;
         } else if (strcmp(argv[iarg+1],"-P") == 0 ){
            polar = 1 ; nadd = 2 ;
         } else if (strcmp(argv[iarg+1],"-IP") == 0 ){
            invert = 1; polar = 1 ; nadd = 2 ;
         } else if( strcmp(argv[iarg+1],"-S") == 0 ){
            do_sqrt = 1 ; nadd = 2 ;
         }
      }

      /* read matrix (or matrices) */

      if( STRING_HAS_SUFFIX(argv[iarg],".aff12.1D") ){  /* 24 Jul 2007: multiple */
        MRI_IMAGE *qim ;
        qim = mri_read_1D(argv[iarg]) ;
        if( qim == NULL ) ERROR_exit("Can't read file '%s'",argv[iarg]) ;
        multi_im  = mri_transpose(qim); mri_free(qim);
        multi_far = MRI_FLOAT_PTR(multi_im) ;
        multi_nx  = multi_im->nx ;  /* # of values per row */
        multi_ny  = multi_im->ny ;  /* number of rows */
        if( multi_nx < 12 || multi_nx > 12 )
          ERROR_exit("File '%s' has %d values per row, which isn't 12",argv[iarg],multi_nx) ;

#undef  APL
#define APL(i,j) multi_far[(i)+(j)*multi_nx] /* i=param index, j=row index */

        /* load matrices from data just read */

        if( ntmat > 1 && multi_ny > 1 )
          ERROR_exit("%s : can't have 2 multi-line '.aff12.1D' files!",argv[iarg]) ;

        ndvm = multi_ny ;
        dvm  = (THD_dvecmat *)malloc(sizeof(THD_dvecmat)*ndvm) ;
        for( dd=0 ; dd < ndvm ; dd++ ){
          LOAD_DMAT(qvm.mm , APL(0,dd) , APL(1,dd) , APL( 2,dd) ,
                             APL(4,dd) , APL(5,dd) , APL( 6,dd) ,
                             APL(8,dd) , APL(9,dd) , APL(10,dd)  ) ;
          LOAD_DFVEC3(qvm.vv , APL(3,dd) , APL(7,dd) , APL(11,dd) ) ;

               if( invert  ) dvm[dd] = invert_dvecmat(qvm) ;
          else if( do_sqrt ) dvm[dd] = sqrt_dvecmat(qvm) ;
          else               dvm[dd] = qvm ;
        }

        /* replicate tmat/tvec matrix/vector */

        if( ntmat == 1 && ndvm > 1 ){
          qmat = tmat[0] ; qvec = tvec[0] ;
          tmat = (THD_dmat33 *)realloc(tmat,sizeof(THD_dmat33)*ndvm) ;
          tvec = (THD_dfvec3 *)realloc(tvec,sizeof(THD_dfvec3)*ndvm) ;
          for( dd=1 ; dd < ndvm ; dd++ ){ tmat[dd] = qmat; tvec[dd] = qvec; }
          ntmat = ndvm ;
        }

      } else {  /* one matrix (ye olde way) */

        qvm = THD_read_dvecmat( argv[iarg] , invert ) ;
        if( SIZE_DMAT(qvm.mm) == 0.0 )
          ERROR_exit("Can't read matrix from '%s'\n",argv[iarg]) ;
        if( do_sqrt ) qvm = sqrt_dvecmat(qvm) ;

        ndvm   = 1 ;
        dvm    = (THD_dvecmat *)malloc(sizeof(THD_dvecmat)) ;
        dvm[0] = qvm ;
      }

      if (polar) {   /* do a polar decomposition */
        mat33 A, M;
        for( dd=0 ; dd < ndvm ; dd++ ){
          for (i=0;i<3;++i) for (j=0;j<3;++j) A.m[i][j] = (float)dvm[dd].mm.mat[i][j];
          M = nifti_mat33_polar( A );
          for (i=0;i<3;++i) for (j=0;j<3;++j) dvm[dd].mm.mat[i][j] = (double)M.m[i][j];
        }
      }

      /* multiply into accumulating transformation */

      if( ndvm == ntmat ){
        for( dd=0 ; dd < ndvm ; dd++ ){
          qmat = dvm[dd].mm ; qvec = dvm[dd].vv ;
          imat = DMAT_MUL( qmat , tmat[dd] ) ;
          ivec = DMATVEC ( qmat , tvec[dd] ) ;
          tvec[dd] = ADD_DFVEC3( qvec , ivec ) ;
          tmat[dd] = imat ;
        }
      } else if( ndvm == 1 ){
        for( dd=0 ; dd < ntmat ; dd++ ){
          qmat = dvm[0].mm ; qvec = dvm[0].vv ;
          imat = DMAT_MUL( qmat , tmat[dd] ) ;
          ivec = DMATVEC ( qmat , tvec[dd] ) ;
          tvec[dd] = ADD_DFVEC3( qvec , ivec ) ;
          tmat[dd] = imat ;
        }
      } else {
        ERROR_exit("WTF?! ndvm=%d ntmat=%d",ndvm,ntmat) ;
      }

      iarg += nadd ;  /* move on, nothing to see here */
   }
   
   if( argc < 2){
      usage_cat_matvec(1);
      exit(0) ;
   }


   /* write results to stdout */

   if( matout ){
     char buf[1024] ; int ii, lb ;
     for( dd=0 ; dd < ntmat ; dd++ ){
       sprintf(buf,"MATRIX(%13.6g,%13.6g,%13.6g,%13.6g,"
                          "%13.6g,%13.6g,%13.6g,%13.6g,"
                          "%13.6g,%13.6g,%13.6g,%13.6g)" ,
         tmat[dd].mat[0][0], tmat[dd].mat[0][1], tmat[dd].mat[0][2], tvec[dd].xyz[0],
         tmat[dd].mat[1][0], tmat[dd].mat[1][1], tmat[dd].mat[1][2], tvec[dd].xyz[1],
         tmat[dd].mat[2][0], tmat[dd].mat[2][1], tmat[dd].mat[2][2], tvec[dd].xyz[2] ) ;
       for( ii=0 ; buf[ii] != '\0' ; ii++ )
         if( !isspace(buf[ii]) ) putchar(buf[ii]) ;
       putchar('\n') ;
     }
   } else if( ntmat == 1 && !oneline ){
     printf("%13.6g %13.6g %13.6g %13.6g\n"
            "%13.6g %13.6g %13.6g %13.6g\n"
            "%13.6g %13.6g %13.6g %13.6g\n" ,
       tmat[0].mat[0][0], tmat[0].mat[0][1], tmat[0].mat[0][2], tvec[0].xyz[0],
       tmat[0].mat[1][0], tmat[0].mat[1][1], tmat[0].mat[1][2], tvec[0].xyz[1],
       tmat[0].mat[2][0], tmat[0].mat[2][1], tmat[0].mat[2][2], tvec[0].xyz[2] ) ;
     if (augmat) {
      printf("%13.6g %13.6g %13.6g %13.6g\n", 0.0, 0.0, 0.0, 1.0);
     }
   } else {
     for( dd=0 ; dd < ntmat ; dd++ )
       printf("%13.6g %13.6g %13.6g %13.6g "
              "%13.6g %13.6g %13.6g %13.6g "
              "%13.6g %13.6g %13.6g %13.6g\n" ,
         tmat[dd].mat[0][0], tmat[dd].mat[0][1], tmat[dd].mat[0][2], tvec[dd].xyz[0],
         tmat[dd].mat[1][0], tmat[dd].mat[1][1], tmat[dd].mat[1][2], tvec[dd].xyz[1],
         tmat[dd].mat[2][0], tmat[dd].mat[2][1], tmat[dd].mat[2][2], tvec[dd].xyz[2] ) ;
   }
   exit(0) ;
}
