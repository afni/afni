#ifndef _MCW_3DVECMAT_
#define _MCW_3DVECMAT_

#include <math.h>

#ifndef FLOAT_TYPE
#define FLOAT_TYPE float
#endif

/*-------------------------------------------------------------------*/
/*-----             3-vector and matrix structures              -----*/

typedef struct { int ijk[3] ;        }    THD_ivec3 ;
typedef struct { FLOAT_TYPE xyz[3] ; }    THD_fvec3 ;
typedef struct { FLOAT_TYPE mat[3][3] ; } THD_mat33 ;

typedef struct {    /* 3x3 matrix + 3-vector [16 Jul 2000] */
   THD_fvec3 vv ;
   THD_mat33 mm ;
} THD_vecmat ;

/*-------------------------------------------------------------------*/
/*-----       macros that operate on 3 vectors and matrices     -----*/

#define LOAD_IVEC3(iv,i,j,k) ( (iv).ijk[0]=(i), \
                               (iv).ijk[1]=(j), \
                               (iv).ijk[2]=(k)    )

#define UNLOAD_IVEC3(iv,i,j,k) ( (i)=(iv).ijk[0], \
                                 (j)=(iv).ijk[1], \
                                 (k)=(iv).ijk[2]   )

#define LOAD_FVEC3(fv,x,y,z) ( (fv).xyz[0]=(x), \
                               (fv).xyz[1]=(y), \
                               (fv).xyz[2]=(z)   )

#define UNLOAD_FVEC3(fv,x,y,z) ( (x)=(fv).xyz[0], \
                                 (y)=(fv).xyz[1], \
                                 (z)=(fv).xyz[2]   )

static THD_ivec3 tempA_ivec3 , tempB_ivec3 ;  /* temps for macros below */
static THD_fvec3 tempA_fvec3 , tempB_fvec3 ;
static THD_mat33 tempA_mat33 , tempB_mat33 ;
static FLOAT_TYPE tempRWC ;

#define TEMP_IVEC3(i,j,k) ( tempB_ivec3.ijk[0]=(i), \
                            tempB_ivec3.ijk[1]=(j), \
                            tempB_ivec3.ijk[2]=(k), tempB_ivec3 )

#define TEMP_FVEC3(x,y,z) ( tempB_fvec3.xyz[0]=(x), \
                            tempB_fvec3.xyz[1]=(y), \
                            tempB_fvec3.xyz[2]=(z), tempB_fvec3 )

#define DUMP_IVEC3(str,iv) \
   printf("%s: %d %d %d\n",(str),(iv).ijk[0],(iv).ijk[1],(iv).ijk[2])

#define DUMP_FVEC3(str,fv) \
   printf("%s: %13.6g %13.6g %13.6g\n",(str),(fv).xyz[0],(fv).xyz[1],(fv).xyz[2])

#define DUMP_MAT33(str,A)                                  \
   printf("%10.10s: [ %13.6g %13.6g %13.6g ]\n"            \
       "            [ %13.6g %13.6g %13.6g ]\n"            \
       "            [ %13.6g %13.6g %13.6g ]\n" ,          \
     str , (A).mat[0][0] , (A).mat[0][1] , (A).mat[0][2] , \
           (A).mat[1][0] , (A).mat[1][1] , (A).mat[1][2] , \
           (A).mat[2][0] , (A).mat[2][1] , (A).mat[2][2]  )

/*--- macros for operations on floating 3 vectors,
      with heavy use of the comma operator and structure assignment! ---*/

  /* negation */

#define NEGATE_FVEC3(a) ( (a).xyz[0] = -(a).xyz[0] , \
                          (a).xyz[1] = -(a).xyz[1] , \
                          (a).xyz[2] = -(a).xyz[2]    )

  /* subtraction */

#define SUB_FVEC3(a,b) \
   ( tempA_fvec3.xyz[0] = (a).xyz[0] - (b).xyz[0] , \
     tempA_fvec3.xyz[1] = (a).xyz[1] - (b).xyz[1] , \
     tempA_fvec3.xyz[2] = (a).xyz[2] - (b).xyz[2] , tempA_fvec3 )

  /* addition */

#define ADD_FVEC3(a,b) \
   ( tempA_fvec3.xyz[0] = (a).xyz[0] + (b).xyz[0] , \
     tempA_fvec3.xyz[1] = (a).xyz[1] + (b).xyz[1] , \
     tempA_fvec3.xyz[2] = (a).xyz[2] + (b).xyz[2] , tempA_fvec3 )

  /* make into a unit vector */

#define NORMALIZE_FVEC3(a) \
   ( tempRWC =   (a).xyz[0] * (a).xyz[0]                 \
               + (a).xyz[1] * (a).xyz[1]                 \
               + (a).xyz[2] * (a).xyz[2]               , \
     tempRWC = (tempRWC > 0) ? (1.0/sqrt(tempRWC)) : 0 , \
     tempA_fvec3.xyz[0] = (a).xyz[0] * tempRWC         , \
     tempA_fvec3.xyz[1] = (a).xyz[1] * tempRWC         , \
     tempA_fvec3.xyz[2] = (a).xyz[2] * tempRWC         , tempA_fvec3 )

  /* cross product */

#define CROSS_FVEC3(a,b) \
  ( tempA_fvec3.xyz[0] = (a).xyz[1]*(b).xyz[2] - (a).xyz[2]*(b).xyz[1] , \
    tempA_fvec3.xyz[1] = (a).xyz[2]*(b).xyz[0] - (a).xyz[0]*(b).xyz[2] , \
    tempA_fvec3.xyz[2] = (a).xyz[0]*(b).xyz[1] - (a).xyz[1]*(b).xyz[0] , \
    tempA_fvec3 )

  /* L2 norm */

#define SIZE_FVEC3(a) \
   sqrt((a).xyz[0]*(a).xyz[0]+(a).xyz[1]*(a).xyz[1]+(a).xyz[2]*(a).xyz[2])

  /* dot product */

#define DOT_FVEC3(a,b) \
   ((a).xyz[0]*(b).xyz[0] + (a).xyz[1]*(b).xyz[1] + (a).xyz[2]*(b).xyz[2])

  /* scale and add two vectors: fa * a + fb * b */

#define SCLADD_FVEC3(fa,a,fb,b) \
  ( tempA_fvec3.xyz[0] = (fa)*(a).xyz[0] + (fb)*(b).xyz[0] , \
    tempA_fvec3.xyz[1] = (fa)*(a).xyz[1] + (fb)*(b).xyz[1] , \
    tempA_fvec3.xyz[2] = (fa)*(a).xyz[2] + (fb)*(b).xyz[2] , tempA_fvec3 )

  /* round to an integer vector (assume non-negative) */

#define INT_FVEC3(a) \
  ( tempA_ivec3.ijk[0] = (a).xyz[0] + 0.5 , \
    tempA_ivec3.ijk[1] = (a).xyz[1] + 0.5 , \
    tempA_ivec3.ijk[1] = (a).xyz[2] + 0.5 , tempA_ivec3 ) ;

  /* matrix-vector multiply */

#define MATVEC(A,x) \
  ( tempA_fvec3.xyz[0] = (A).mat[0][0] * (x).xyz[0]  \
                        +(A).mat[0][1] * (x).xyz[1]  \
                        +(A).mat[0][2] * (x).xyz[2] ,\
    tempA_fvec3.xyz[1] = (A).mat[1][0] * (x).xyz[0]  \
                        +(A).mat[1][1] * (x).xyz[1]  \
                        +(A).mat[1][2] * (x).xyz[2] ,\
    tempA_fvec3.xyz[2] = (A).mat[2][0] * (x).xyz[0]  \
                        +(A).mat[2][1] * (x).xyz[1]  \
                        +(A).mat[2][2] * (x).xyz[2] ,  tempA_fvec3 )

  /* matrix-vector multiply with subtract: output = A (x-b) */

#define VECSUB_MAT(A,x,b) \
  ( tempA_fvec3.xyz[0] = (A).mat[0][0] * ((x).xyz[0]-(b).xyz[0])  \
                        +(A).mat[0][1] * ((x).xyz[1]-(b).xyz[1])  \
                        +(A).mat[0][2] * ((x).xyz[2]-(b).xyz[2]) ,\
    tempA_fvec3.xyz[1] = (A).mat[1][0] * ((x).xyz[0]-(b).xyz[0])  \
                        +(A).mat[1][1] * ((x).xyz[1]-(b).xyz[1])  \
                        +(A).mat[1][2] * ((x).xyz[2]-(b).xyz[2]) ,\
    tempA_fvec3.xyz[2] = (A).mat[2][0] * ((x).xyz[0]-(b).xyz[0])  \
                        +(A).mat[2][1] * ((x).xyz[1]-(b).xyz[1])  \
                        +(A).mat[2][2] * ((x).xyz[2]-(b).xyz[2]) ,\
    tempA_fvec3 )

   /* matrix vector multiply with subtract after: A x - b */

#define MATVEC_SUB(A,x,b) \
  ( tempA_fvec3.xyz[0] = (A).mat[0][0] * (x).xyz[0]               \
                        +(A).mat[0][1] * (x).xyz[1]               \
                        +(A).mat[0][2] * (x).xyz[2] - (b).xyz[0] ,\
    tempA_fvec3.xyz[1] = (A).mat[1][0] * (x).xyz[0]               \
                        +(A).mat[1][1] * (x).xyz[1]               \
                        +(A).mat[1][2] * (x).xyz[2] - (b).xyz[1] ,\
    tempA_fvec3.xyz[2] = (A).mat[2][0] * (x).xyz[0]               \
                        +(A).mat[2][1] * (x).xyz[1]               \
                        +(A).mat[2][2] * (x).xyz[2] - (b).xyz[2] ,\
    tempA_fvec3 )

  /* matrix-matrix multiply */

#define ROW_DOT_COL(A,B,i,j) (  (A).mat[i][0] * (B).mat[0][j] \
                              + (A).mat[i][1] * (B).mat[1][j] \
                              + (A).mat[i][2] * (B).mat[2][j]   )

#define MAT_MUL(A,B) \
  ( tempA_mat33.mat[0][0] = ROW_DOT_COL((A),(B),0,0) , \
    tempA_mat33.mat[1][0] = ROW_DOT_COL((A),(B),1,0) , \
    tempA_mat33.mat[2][0] = ROW_DOT_COL((A),(B),2,0) , \
    tempA_mat33.mat[0][1] = ROW_DOT_COL((A),(B),0,1) , \
    tempA_mat33.mat[1][1] = ROW_DOT_COL((A),(B),1,1) , \
    tempA_mat33.mat[2][1] = ROW_DOT_COL((A),(B),2,1) , \
    tempA_mat33.mat[0][2] = ROW_DOT_COL((A),(B),0,2) , \
    tempA_mat33.mat[1][2] = ROW_DOT_COL((A),(B),1,2) , \
    tempA_mat33.mat[2][2] = ROW_DOT_COL((A),(B),2,2) , tempA_mat33 )

   /* matrix determinant */

#define MAT_DET(A) \
 (  (A).mat[0][0]*(A).mat[1][1]*(A).mat[2][2] \
  - (A).mat[0][0]*(A).mat[1][2]*(A).mat[2][1] \
  - (A).mat[1][0]*(A).mat[0][1]*(A).mat[2][2] \
  + (A).mat[1][0]*(A).mat[0][2]*(A).mat[2][1] \
  + (A).mat[2][0]*(A).mat[0][1]*(A).mat[1][2] \
  - (A).mat[2][0]*(A).mat[0][2]*(A).mat[1][1]   )

   /* matrix trace [5 Oct 1998] */

#define MAT_TRACE(A) ( (A).mat[0][0] + (A).mat[1][1] + (A).mat[2][2] )

   /* matrix inverse */

#define MAT_INV(A) \
 ( tempRWC = 1.0 / MAT_DET(A) , \
    tempA_mat33.mat[1][1] = \
     ( (A).mat[0][0]*(A).mat[2][2] - (A).mat[0][2]*(A).mat[2][0]) * tempRWC,\
    tempA_mat33.mat[2][2] = \
     ( (A).mat[0][0]*(A).mat[1][1] - (A).mat[0][1]*(A).mat[1][0]) * tempRWC,\
    tempA_mat33.mat[2][0] = \
     ( (A).mat[1][0]*(A).mat[2][1] - (A).mat[1][1]*(A).mat[2][0]) * tempRWC,\
    tempA_mat33.mat[1][2] = \
     (-(A).mat[0][0]*(A).mat[1][2] + (A).mat[0][2]*(A).mat[1][0]) * tempRWC,\
    tempA_mat33.mat[0][1] = \
     (-(A).mat[0][1]*(A).mat[2][2] + (A).mat[0][2]*(A).mat[2][1]) * tempRWC,\
    tempA_mat33.mat[0][0] = \
     ( (A).mat[1][1]*(A).mat[2][2] - (A).mat[1][2]*(A).mat[2][1]) * tempRWC,\
    tempA_mat33.mat[2][1] = \
     (-(A).mat[0][0]*(A).mat[2][1] + (A).mat[0][1]*(A).mat[2][0]) * tempRWC,\
    tempA_mat33.mat[1][0] = \
     (-(A).mat[1][0]*(A).mat[2][2] + (A).mat[1][2]*(A).mat[2][0]) * tempRWC,\
    tempA_mat33.mat[0][2] = \
     ( (A).mat[0][1]*(A).mat[1][2] - (A).mat[0][2]*(A).mat[1][1]) * tempRWC,\
    tempA_mat33 )

  /* load a matrix from scalars [3 Oct 1998] */

#define LOAD_MAT(A,a11,a12,a13,a21,a22,a23,a31,a32,a33) \
 ( (A).mat[0][0] = (a11) , (A).mat[0][1] = (a12) ,      \
   (A).mat[0][2] = (a13) , (A).mat[1][0] = (a21) ,      \
   (A).mat[1][1] = (a22) , (A).mat[1][2] = (a23) ,      \
   (A).mat[2][0] = (a31) , (A).mat[2][1] = (a32) , (A).mat[2][2] = (a33) )

  /* unload a matrix into scalars [3 Oct 1998] */

#define UNLOAD_MAT(A,a11,a12,a13,a21,a22,a23,a31,a32,a33) \
 ( (a11) = (A).mat[0][0] , (a12) = (A).mat[0][1] ,        \
   (a13) = (A).mat[0][2] , (a21) = (A).mat[1][0] ,        \
   (a22) = (A).mat[1][1] , (a23) = (A).mat[1][2] ,        \
   (a31) = (A).mat[2][0] , (a32) = (A).mat[2][1] , (a33) = (A).mat[2][2] )

   /* diagonal matrix */

#define LOAD_DIAG_MAT(A,x,y,z) \
 ( (A).mat[0][0] = (x) , \
   (A).mat[1][1] = (y) , \
   (A).mat[2][2] = (z) , \
   (A).mat[0][1] = (A).mat[0][2] = (A).mat[1][0] = \
   (A).mat[1][2] = (A).mat[2][0] = (A).mat[2][1] = 0.0 )

   /* zero matrix */

#define LOAD_ZERO_MAT(A) LOAD_DIAG_MAT((A),0,0,0)

   /* elementary rotation matrices:
      rotate about axis #ff, from axis #aa toward #bb,
      where ff, aa, and bb are a permutation of {0,1,2} */

#define LOAD_ROTGEN_MAT(A,th,ff,aa,bb)             \
 ( (A).mat[aa][aa] = (A).mat[bb][bb] = cos((th)) , \
   (A).mat[aa][bb] = sin((th)) ,                   \
   (A).mat[bb][aa] = -(A).mat[aa][bb] ,            \
   (A).mat[ff][ff] = 1.0 ,                         \
   (A).mat[aa][ff] = (A).mat[bb][ff] = (A).mat[ff][aa] = (A).mat[ff][bb] = 0.0 )

#define LOAD_ROTX_MAT(A,th) LOAD_ROTGEN_MAT(A,th,0,1,2)
#define LOAD_ROTY_MAT(A,th) LOAD_ROTGEN_MAT(A,th,1,2,0)
#define LOAD_ROTZ_MAT(A,th) LOAD_ROTGEN_MAT(A,th,2,0,1)

#define LOAD_ROT_MAT(A,th,i)                  \
  do{ switch( (i) ){                          \
        case 0: LOAD_ROTX_MAT(A,th) ; break ; \
        case 1: LOAD_ROTY_MAT(A,th) ; break ; \
        case 2: LOAD_ROTZ_MAT(A,th) ; break ; \
       default: LOAD_ZERO_MAT(A)    ; break ; \
      } } while(0)

   /* shear matrices [3 Oct 1998] */

#define LOAD_SHEARX_MAT(A,f,b,c) ( LOAD_DIAG_MAT(A,(f),1,1) , \
                                   (A).mat[0][1] = (b) , (A).mat[0][2] = (c) )

#define LOAD_SHEARY_MAT(A,f,a,c) ( LOAD_DIAG_MAT(A,1,(f),1) , \
                                   (A).mat[1][0] = (a) , (A).mat[1][2] = (c) )

#define LOAD_SHEARZ_MAT(A,f,a,b) ( LOAD_DIAG_MAT(A,1,1,(f)) , \
                                   (A).mat[2][0] = (a) , (A).mat[2][1] = (b) )

   /* matrix transpose */

#define TRANSPOSE_MAT(A) \
 ( tempA_mat33.mat[0][0] = (A).mat[0][0] , \
   tempA_mat33.mat[1][0] = (A).mat[0][1] , \
   tempA_mat33.mat[2][0] = (A).mat[0][2] , \
   tempA_mat33.mat[0][1] = (A).mat[1][0] , \
   tempA_mat33.mat[1][1] = (A).mat[1][1] , \
   tempA_mat33.mat[2][1] = (A).mat[1][2] , \
   tempA_mat33.mat[0][2] = (A).mat[2][0] , \
   tempA_mat33.mat[1][2] = (A).mat[2][1] , \
   tempA_mat33.mat[2][2] = (A).mat[2][2] , tempA_mat33 )

   /* component-wise min and max of 3-vectors */

#define MAX_FVEC3(a,b) \
( tempA_fvec3.xyz[0] = (((a).xyz[0] > (b).xyz[0]) ? (a).xyz[0] : (b).xyz[0]) ,\
  tempA_fvec3.xyz[1] = (((a).xyz[1] > (b).xyz[1]) ? (a).xyz[1] : (b).xyz[1]) ,\
  tempA_fvec3.xyz[2] = (((a).xyz[2] > (b).xyz[2]) ? (a).xyz[2] : (b).xyz[2]) ,\
  tempA_fvec3 )

#define MIN_FVEC3(a,b) \
( tempA_fvec3.xyz[0] = (((a).xyz[0] < (b).xyz[0]) ? (a).xyz[0] : (b).xyz[0]) ,\
  tempA_fvec3.xyz[1] = (((a).xyz[1] < (b).xyz[1]) ? (a).xyz[1] : (b).xyz[1]) ,\
  tempA_fvec3.xyz[2] = (((a).xyz[2] < (b).xyz[2]) ? (a).xyz[2] : (b).xyz[2]) ,\
  tempA_fvec3 )

#endif /* _MCW_3DVECMAT_ */
