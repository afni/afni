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
 *   - at top:  #include "mrilib.h"
 *              ... because these files are now compiled into libmri
 *   - in file: call zgaussian2_init(rseed) once near the top of
 *              main(), before any call to zgaussian2().
 *   - in file, with parallelization: use zgaussian2_sss() version
 *
 ****************************************************************************/

/* LCG constants for mixing the seed into a full 64-bit state.
   These are a well-validated multiplier/increment pair for 64-bit
   LCG generators (L'Ecuyer 1999), and are identical across all
   platforms since they are fixed-width uint64_t values. */
#define ZIGG_LCG_MULT  6364136223846793005ULL
#define ZIGG_LCG_INCR  1442695040888963407ULL

static uint64_t zigg_jsr64 ;    /* 64-bit master RNG state (period 2^64-1) */
static uint32_t zigg_kn[128] ;  /* ziggurat table (integer bounds) */
static float    zigg_fn[128] ;  /* ziggurat table (PDF values)     */
static float    zigg_wn[128] ;  /* ziggurat table (strip widths)   */

/*---------------------------------------------------------------------------
 * next_jsr32() - advance the 64-bit XOR-shift master state and return the
 *   upper 32 bits as a local seed for r4_nor(). Using a 64-bit master state
 *   avoids the period exhaustion that would occur with a bare uint32_t when
 *   the simulation makes billions of calls (large nvals * ntrial).
 *--------------------------------------------------------------------------*/
static uint32_t next_jsr32( void )
{
  zigg_jsr64 ^= zigg_jsr64 << 13 ;
  zigg_jsr64 ^= zigg_jsr64 >>  7 ;
  zigg_jsr64 ^= zigg_jsr64 << 17 ;
  return (uint32_t)( zigg_jsr64 >> 32 ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2_init() - must be called once per program before zgaussian2().
 *   Seeds the RNG from the wall clock and builds the ziggurat lookup tables.
 *   Calling it more than once is harmless but unnecessary.
 *
 *   Pass seed=0 to seed from the wall clock (non-reproducible).
 *   Pass any nonzero seed for a reproducible sequence. 
 * 
 *   This has now been configured to handle more states than the
 *   original implementation's uint32_t would allow, via this wrapping
 *--------------------------------------------------------------------------*/
void zgaussian2_init( uint32_t seed )
{
  /* promote to 64 bits with a LCG mix to avoid degenerate near-zero state */
  zigg_jsr64 = (seed != 0)
             ? ( (uint64_t)seed      * ZIGG_LCG_MULT + ZIGG_LCG_INCR )
             : ( (uint64_t)time(NULL) * ZIGG_LCG_MULT + ZIGG_LCG_INCR ) ;
  r4_nor_setup( zigg_kn , zigg_fn , zigg_wn ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2() - returns one N(0,1) random deviate.
 * 
 * This function is basically a wrapper for an actual library function
 * (see ziggurat.*), matching the name and usage of an earlier one.
 * 
 * The *current* form just makes the single thread call (this one) be a
 * special case of the multithread, OpenMP version, below. The idea is
 * to just make the maintenance/updates inherently unified.
 *
 * The earlier/initial form of this function was just comprised of the
 * following 2 lines: 
 * { 
 *   uint32_t jsr = next_jsr32() ;
 *   return r4_nor( &jsr , zigg_kn , zigg_fn , zigg_wn ) ; 
 * }
 * 
 *--------------------------------------------------------------------------*/
float zgaussian2( void )
{
  return zgaussian2_sss( &zigg_jsr64 ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2_sss() - thread-safe variant of zgaussian2(), e.g., for use
 *   in OpenMP parallelization; caller owns and passes the 64-bit seed.
 *   This function takes a pointer to a caller-owned 64-bit seed
 *   instead of using the shared global zigg_jsr64 state. Each thread
 *   maintains its own independent seed variable, advances it with the
 *   same XOR-shift sequence, extracts the upper 32 bits, and passes
 *   them to r4_nor() along with the shared read-only ziggurat lookup
 *   tables.
 *
 *   Note that zgaussian2_init() must still be called once in main()
 *   before any parallel region, to build the shared lookup
 *   tables. Additionally, zgaussian2_thread_seed() is also required.
 *
 *   Each thread should declare its own uint64_t and seed it before the
 *   parallel region, for example:
 *
 *     uint64_t jsr = zgaussian2_thread_seed(omp_get_thread_num()) ;
 *     #pragma omp parallel firstprivate(jsr)
 *     {
 *       float z = zgaussian2_sss(&jsr) ;
 *       ...
 *     }
 *
 *   The lookup tables (zigg_kn, zigg_fn, zigg_wn) are read-only after
 *   zgaussian2_init() and are safely shared across threads.
 *--------------------------------------------------------------------------*/
float zgaussian2_sss( uint64_t *jsr )
{
  uint32_t jsr32 ;
  *jsr ^= *jsr << 13 ;
  *jsr ^= *jsr >>  7 ;
  *jsr ^= *jsr << 17 ;
  jsr32 = (uint32_t)( *jsr >> 32 ) ;
  return r4_nor( &jsr32 , zigg_kn , zigg_fn , zigg_wn ) ;
}

/*---------------------------------------------------------------------------
 * zgaussian2_thread_seed() - convenience function to derive a distinct,
 *   reproducible 64-bit seed for each thread from the base seed supplied to
 *   zgaussian_init().  Pass the thread index (e.g. omp_get_thread_num()).
 * 
 *   This function derives a distinct starting 64-bit seed for each
 *   thread from the base seed established by zgaussian2_init(). It
 *   does this by XORing the global zigg_jsr64 state with a hash of
 *   the thread index using the same LCG multiplier, ensuring that
 *   adjacent thread indices produce well-separated starting points in
 *   the random number sequence rather than similar or correlated
 *   ones.
 *--------------------------------------------------------------------------*/
uint64_t zgaussian2_thread_seed( int thread_id )
{
  /* Simple hash to spread thread indices across the seed space, avoiding
     the degenerate case where adjacent thread IDs produce similar seeds. */
  return zigg_jsr64 ^ ( (uint64_t)(1664525u * thread_id + 1013904223u)
                        * 6364136223846793005ULL ) ;
}
