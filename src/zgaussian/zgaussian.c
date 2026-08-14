#include <stdint.h>
#include <time.h>
#include "ziggurat.h"
#include "zgaussian.h"

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

static uint32_t zigg_jsr ;      /* RNG seed / state                */
static uint32_t zigg_kn[128] ;  /* ziggurat table (integer bounds) */
static float    zigg_fn[128] ;  /* ziggurat table (PDF values)     */
static float    zigg_wn[128] ;  /* ziggurat table (strip widths)   */

/*---------------------------------------------------------------------------
 * zgaussian2_init() - must be called once per program before zgaussian().
 *   Seeds the RNG from the wall clock and builds the ziggurat lookup tables.
 *   Calling it more than once is harmless but unnecessary.
 *
 *   Pass seed=0 to seed from the wall clock (non-reproducible).
 *   Pass any nonzero seed for a reproducible sequence. 
 *--------------------------------------------------------------------------*/
void zgaussian2_init( uint32_t seed )
{
  zigg_jsr = (seed != 0) ? seed : (uint32_t)time(NULL) ;
  r4_nor_setup( zigg_kn , zigg_fn , zigg_wn ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2() - returns one N(0,1) random deviate.
 * 
 * This function is basically a wrapper for an actual library function
 * (see ziggurat.*), matching the name and usage of an earlier one.
 *--------------------------------------------------------------------------*/
float zgaussian2( void )
{
  return r4_nor( &zigg_jsr , zigg_kn , zigg_fn , zigg_wn ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2_sss() - thread-safe variant of zgaussian2(), e.g., for use
 *   in OpenMP parallelization; caller owns and passes the seed.
 *
 *   Note that zgaussian2_init() must still be called once in main()
 *   before any parallel region, to build the shared lookup
 *   tables. Additionally, zgaussian2_thread_seed() is also required.
 *
 *   Each thread should declare its own uint32_t and seed it before the
 *   parallel region, for example:
 *
 *     uint32_t jsr = zgaussian2_thread_seed(omp_get_thread_num()) ;
 *     #pragma omp parallel firstprivate(jsr)
 *     {
 *       float z = zgaussian2_sss(&jsr) ;
 *       ...
 *     }
 *
 *   The lookup tables (zigg_kn, zigg_fn, zigg_wn) are read-only after
 *   zgaussian2_init() and are safely shared across threads.
 *--------------------------------------------------------------------------*/
float zgaussian2_sss( uint32_t *jsr )
{
  return r4_nor( jsr , zigg_kn , zigg_fn , zigg_wn ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2_thread_seed() - convenience function to derive a distinct,
 *   reproducible seed for each thread from the base seed supplied to
 *   zgaussian_init().  Pass the thread index (e.g. omp_get_thread_num()).
 *--------------------------------------------------------------------------*/
uint32_t zgaussian2_thread_seed( int thread_id )
{
  /* Simple hash to spread thread indices across the seed space, avoiding
     the degenerate case where adjacent thread IDs produce similar seeds. */
  return zigg_jsr ^ (uint32_t)(1664525u * thread_id + 1013904223u) ;
}
