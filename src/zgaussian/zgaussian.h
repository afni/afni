#ifndef _ZGAUSSIAN2_HEADER_
#define _ZGAUSSIAN2_HEADER_

/****************************************************************************
 *
 * Generate Gaussian-distribution numbers via the Ziggurat algorithm
 * of Marsaglia and Tsang. The library of functions for this is by
 * J. Burkhardt (ziggurat.*); it is MIT-licensed and portable.
 *
 * Usage in each AFNI program:
 *   - at top:  #include <stdint.h>
 *              #include "zgaussian2.h"
 *   - in file: call zgaussian_init(rseed) once near the top of
 *              main(), before any call to zgaussian().
 *   - in file, with parallelization: use zgaussian_sss() version
 *
 ****************************************************************************/

/*  initialize RNG for zgaussian() usage  */
void  zgaussian_init( uint32_t seed ) ;

/*  Generate random num from Gaussian distribution N(0,1)  */
float zgaussian( void ) ; 

/*  thread-safe variant of zgaussian(); caller owns jsr state  */
float zgaussian_sss( uint32_t *jsr ) ;
/*  derive a per-thread seed from the base seed; use with zgaussian_sss()  */
uint32_t zgaussian_thread_seed( int tid ) ;  

#endif /* _ZGAUSSIAN2_HEADER_ */
