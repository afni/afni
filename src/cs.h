#ifndef _MCW_CS_STUFF_HEADER_
#define _MCW_CS_STUFF_HEADER_

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*----- Miscellaneous "computer science" stuff
        [legacy of when I was a professor of Computer Science -- RWCox] -----*/

#include <stdlib.h>
#include <stdio.h>

/***** Quicksort routines in various flavonoids *****/

extern void qsort_floatint ( int , float *  , int * ) ;
extern void qsort_doubleint( int , double * , int * ) ;
extern void qsort_intint   ( int , int *    , int * ) ;

/***** Eigensolutions *****/

extern void symeig_double( int , double * , double * ) ;

/***** Argument list mangling *****/

extern void addto_args( int , char ** , int * , char *** ) ;

#endif
