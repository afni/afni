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

/***** Argument list mangling *****/

extern void addto_args( int , char ** , int * , char *** ) ;
extern void append_string_to_args ( char *, int, char **, int *, char *** ) ;
extern void prepend_string_to_args( char *, int, char **, int *, char *** ) ;

/***** Misc stuff *****/

extern void get_laguerre_table( int , double ** , double ** ) ;  /* 12 Mar 2000 */

extern int qhull_wrap( int , float * , int ** ) ; /* 07 Jun 2001 */
extern int sphere_voronoi_angles( int , float *, float *, float ** ) ;
extern int sphere_voronoi_vectors( int , float *, float ** ) ;

extern int cl1_solve( int , int , float * , float ** , float * ) ; /* cl1.c */

#endif
