#ifndef _MCW_QSRT_H_
#define _MCW_QSRT_H_

#include <stdlib.h>
#include <stdio.h>

extern void isort_sh( int , short * ) ;
extern void qsrec_sh( int , short * , int ) ;
extern void qsort_sh( int , short * ) ;
extern void qsort_partial_sh( int , short * , int ) ;

extern int MCW_inverse_histogram( int , short * , int , short * ) ;

#endif /* _MCW_QSRT_H_ */
