/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _RWCOX_PARSER_HEADER_
#define _RWCOX_PARSER_HEADER_

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

#define MAX_PARCODE 999

typedef struct {
   int num_code ;
   char c_code[8*MAX_PARCODE] ;
} PARSER_code ;

extern void PARSER_set_printout( int ) ;

extern PARSER_code * PARSER_generate_code( char * ) ;

extern double PARSER_evaluate_one( PARSER_code *, double atoz[] ) ;

extern void PARSER_evaluate_vector( PARSER_code * pc, double  * atoz[],
                                    int nv, double vout[] ) ;

extern int PARSER_has_symbol( char * sym , PARSER_code * pc ) ;
extern void PARSER_mark_symbols( PARSER_code * pc , int * sl ) ;

extern int PARSER_1deval( char *, int, float, float, float * ) ; /* 17 Nov 1999 */

#ifdef NEED_PARSER_INTERNALS
#include "f2c.h"

extern int parser_(char *c_expr__, logical *l_print__,
                   integer * num_code__, char *c_code__,
                   ftnlen c_expr_len, ftnlen c_code_len) ;

extern doublereal pareval_(integer *num_code__, char *c_code__,
                           doublereal *r8val, ftnlen c_code_len) ;

extern int parevec_(integer *num_code__, char *c_code__,
                    doublereal *va, doublereal *vb, doublereal *vc,
		    doublereal *vd, doublereal *ve, doublereal *vf, 
		    doublereal *vg, doublereal *vh, doublereal *vi,
		    doublereal *vj, doublereal *vk, doublereal *vl,
		    doublereal *vm, doublereal *vn, doublereal *vo,
		    doublereal *vp, doublereal *vq, doublereal *vr,
		    doublereal *vs, doublereal *vt, doublereal *vu,
		    doublereal *vv, doublereal *vw, doublereal *vx,
		    doublereal *vy, doublereal *vz, integer *lvec, 
		    doublereal *vout, ftnlen c_code_len) ;

extern doublereal dbesj0_( doublereal * ) ;
extern doublereal dbesj1_( doublereal * ) ;
extern doublereal dbesy0_( doublereal * ) ;
extern doublereal dbesy1_( doublereal * ) ;

extern doublereal derf_ ( doublereal * ) ;
extern doublereal derfc_( doublereal * ) ;

extern doublereal unif_( doublereal * ) ;

#endif /* NEED_PARSER_INTERNALS */
#endif /* _RWCOX_PARSER_HEADER_ */
