/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _THD_SHEAR3D_HEADER_
#define _THD_SHEAR3D_HEADER_

#include "vecmat.h"

#include <stdio.h>
#include <stdlib.h>
#include "mrilib.h"

#undef  DB
#define DB fprintf(stderr,"in %s at line %d\n",__FILE__,__LINE__)

/*------------------------------------------------------------------------
   Struct to contain a set of 4 3-shears, used to represent an arbitrary
   transformation; for i=0..3:
      3-shear #i is along axis # ax[i] (0=x, 1=y, 2=z),
      with scaling parameter scl[i][j] for direction j (j=0,1,2),
      and shift parameter sft[i] (of course in the ax[i] direction).
   In addition, a preliminary flipping about two axes may be present.
   These axes are denoted by flip0 and flip1, if flip0 >= 0.
--------------------------------------------------------------------------*/

typedef struct {
   int    ax[4] , flip0,flip1;
   double scl[4][3] , sft[4] ;
} MCW_3shear ;

#define DUMP_3SHEAR(str,sss)                                                      \
  fprintf(stderr,"shear %s: flip0=%d flip1=%d\n"                                  \
         " #0: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #1: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #2: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #3: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n" ,                     \
   str , (sss).flip0 , (sss).flip1 ,                                              \
   (sss).ax[0], (sss).scl[0][0], (sss).scl[0][1], (sss).scl[0][2], (sss).sft[0],  \
   (sss).ax[1], (sss).scl[1][0], (sss).scl[1][1], (sss).scl[1][2], (sss).sft[1],  \
   (sss).ax[2], (sss).scl[2][0], (sss).scl[2][1], (sss).scl[2][2], (sss).sft[2],  \
   (sss).ax[3], (sss).scl[3][0], (sss).scl[3][1], (sss).scl[3][2], (sss).sft[3]  )

#define ISVALID_3SHEAR(sss)    ((sss).ax[0] >= 0)
#define INVALIDATE_3SHEAR(sss) ((sss).ax[0] = -1)

/*-------- Prototypes -------------*/

extern double norm_3shear( MCW_3shear sh ) ;
extern THD_dmat33 make_shear_matrix( int ax , double scl[3] ) ;
extern MCW_3shear permute_3shear( MCW_3shear shin , int ox1, int ox2, int ox3 ) ;
extern THD_dmat33 permute_dmat33( THD_dmat33 q , int ox1, int ox2, int ox3 ) ;
extern THD_dfvec3 permute_dfvec3( THD_dfvec3 q , int ox1, int ox2, int ox3 ) ;
extern MCW_3shear shear_xzyx( THD_dmat33 *q , THD_dfvec3 *xyzdel ) ;
extern MCW_3shear shear_arb( THD_dmat33 *q , THD_dfvec3 *xyzdel , int ox1,int ox2,int ox3 ) ;
extern MCW_3shear shear_best( THD_dmat33 * q , THD_dfvec3 * xyzdel ) ;
extern THD_dmat33 rot_to_matrix( int ax1 , double th1 ,
                                 int ax2 , double th2 , int ax3 , double th3  ) ;
extern MCW_3shear rot_to_shear( int ax1 , double th1 ,
                         int ax2 , double th2 ,
                         int ax3 , double th3 , 
                         int dcode , double dx , double dy , double dz ,
                         double xdel , double ydel , double zdel ) ;
extern MCW_3shear rot_to_shear_matvec( THD_dmat33 rmat , THD_dfvec3 tvec , 
                                double xdel , double ydel , double zdel )  ;
extern THD_dmat33 DMAT_xt_x( THD_dmat33 inmat ) ;
extern THD_dmat33 DMAT_x_xt( THD_dmat33 inmat ) ;   /* 09 Apr 2003 */
extern THD_dvecmat DMAT_symeig( THD_dmat33 inmat ) ;
extern THD_dmat33 DMAT_pow( THD_dmat33 inmat , double pp ) ;
extern THD_dmat33 DMAT_svdrot_old( THD_dmat33 inmat ) ;
extern THD_dmat33 DMAT_svdrot_new( THD_dmat33 inmat ) ;  /* 09 Apr 2003 */
extern THD_dmat33 DMAT_svdrot_newer( THD_dmat33 inmat ) ;  /* 22 Oct 2004 */
extern THD_dvecmat DLSQ_rot_trans( int n, THD_dfvec3 * xx, THD_dfvec3 * yy, double * ww ) ;

#endif /* _THD_SHEAR3D_HEADER_ */
