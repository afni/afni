/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_CS_STUFF_HEADER_
#define _MCW_CS_STUFF_HEADER_

/*----- Miscellaneous "computer science" stuff
        [legacy of when I was a professor of Computer Science -- RWCox] -----*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>

#include "mcw_malloc.h"

#ifdef  __cplusplus
extern "C" {
#endif

/***** Quicksort routines in various flavonoids *****/

extern void qsort_floatint  ( int , float *  , int * ) ;
extern void qsort_doubleint ( int , double * , int * ) ;
extern void qsort_intint    ( int , int *    , int * ) ;
extern void qsort_floatfloat( int , float *  , float * ) ;  /* 30 Jan 2000 */
extern void qsort_floatstuff( int , float *  , void ** ) ;  /* 06 Feb 2000 */

/***** Quicksort-ish median *****/

extern float qmed_float( int , float * ) ;
extern void  qmedmad_float( int,float *,float *,float * ) ; /* 08 Mar 2001 */

/***** Eigensolutions *****/

extern void symeig_double( int , double * , double * ) ;
extern void symeigval_double( int , double * , double * ) ;
extern void svd_double( int, int, double *, double *, double *, double * ) ;
extern void svd_float ( int, int, float *, float *, float *, float * ) ;

extern void symeig_3( double *, double *, int ) ;  /* 30 Sep 2005 */
extern void symeig_2( double *, double *, int ) ;
extern void symeig_forbid_23( int ) ;

/***** Argument list mangling *****/

extern void addto_args( int , char ** , int * , char *** ) ;
extern void append_string_to_args ( char *, int, char **, int *, char *** ) ;
extern void prepend_string_to_args( char *, int, char **, int *, char *** ) ;

/***** Misc stuff *****/

extern void get_laguerre_table( int , double ** , double ** ) ;  /* 12 Mar 2000 */

extern int qhull_wrap( int , float * , int ** ) ; /* 07 Jun 2001 */
extern int sphere_voronoi_angles( int , float *, float *, float ** ) ;
extern int sphere_voronoi_vectors( int , float *, float ** ) ;

extern float cl1_solve    ( int, int, float *, float **, float *,int ) ; /* cl1.c */
extern float cl1_solve_res( int, int, float *, float **, float *,int, float*,int ) ; /* cl1.c */

extern int powell_newuoa( int ndim , double *x ,
                          double rstart , double rend ,
                          int maxcall , double (*ufunc)(int,double *) ) ;
extern void powell_set_mfac( float mm , float aa ) ;

extern char *approximate_number_string( double ) ;  /* 16 Jan 2004 */

typedef struct {
      float x;
      int Index;
} Z_QSORT_FLOAT;

typedef struct {
      int x;
      int Index;
} Z_QSORT_INT;

extern int compare_Z_IQSORT_FLOAT (Z_QSORT_FLOAT *a, Z_QSORT_FLOAT *b );
extern int compare_Z_IQSORT_INT (Z_QSORT_INT *a, Z_QSORT_INT *b );
extern int compare_double (double *a, double *b );
extern int compare_float (float *a, float *b );
extern int compare_int (int *a, int *b );
extern int compare_short (short *a, short *b );
extern int compare_char (char *a, char *b );

#ifdef  __cplusplus
}
#endif

#endif
