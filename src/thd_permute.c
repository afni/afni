/*----------------------------------------------------------------------------
  thd_permute.c -- permutation inference for the general linear model.

  This is the shared engine behind permutation-based group statistics: it
  builds sets of relabelings that respect a design's exchangeability
  structure, and runs a statistic over them to get empirical uncorrected and
  max-statistic FWE-corrected p-values.  It knows nothing about datasets, so
  any program that can hand it "nobs observations x nelem elements" can use
  it.

  ---------------------------------------------------------------------------
  The Freedman-Lane procedure
  ---------------------------------------------------------------------------

  For a model Y = X*beta + Z*gamma + e, where X holds the regressors under
  test and Z the nuisance regressors, the labels attached to X are no longer
  freely exchangeable once Z is present: shuffling them also destroys the
  covariate structure that Z is supposed to account for.  Freedman & Lane
  (1983) get exchangeability back by shuffling only the part of the data that
  the nuisance model does not explain:

     1. Fit the reduced model Y = Z*gamma, giving fitted values Zg and
        residuals e_z = Y - Zg.
     2. For relabeling p, form a synthetic dataset Y_p = Zg + P_p*e_z, where
        P_p permutes and/or sign-flips the residuals.
     3. Refit the full model [X Z] to Y_p and evaluate the contrast.
     4. Collect the resulting statistics as the null distribution.

  Because Zg is held fixed, the nuisance structure survives intact while the
  association between X and the data is broken.  The identity relabeling
  reconstructs Y exactly, so the observed statistic is a genuine member of
  its own null distribution.

  ---------------------------------------------------------------------------
  Exchangeability blocks
  ---------------------------------------------------------------------------

  Freely shuffling every observation is only valid when the errors really are
  exchangeable across all of them.  Repeated measures, sibling pairs,
  multi-site studies and the like restrict which swaps are admissible.  This
  file follows PALM's model: observations carry a block label, and the caller
  says whether relabeling happens inside blocks (each block is an independent
  little permutation problem), between whole blocks (blocks trade places as
  rigid units, which requires equal block sizes), or both.  Sign flips follow
  the same structure -- per observation for within-block designs, per block
  when whole blocks are exchanged.

  The scheme also distinguishes "exchangeable errors" (EE, permutation) from
  "independent and symmetric errors" (ISE, sign flipping), and can form the
  product of the two.

  ---------------------------------------------------------------------------
  References
  ---------------------------------------------------------------------------

  Freedman D, Lane D.  A nonstochastic interpretation of reported
    significance levels.  Journal of Business & Economic Statistics,
    1(4):292-298, 1983.

  Winkler AM, Ridgway GR, Webster MA, Smith SM, Nichols TE.  Permutation
    inference for the general linear model.  NeuroImage, 92:381-397, 2014.
    doi:10.1016/j.neuroimage.2014.01.060
    -- the reference implementation of Freedman-Lane for neuroimaging (PALM),
       the EE/ISE terminology, and the design partitioning used by
       THD_perm_partition_design() below.

  Winkler AM, Webster MA, Vidaurre D, Nichols TE, Smith SM.  Multi-level
    block permutation.  NeuroImage, 123:253-268, 2015.
    doi:10.1016/j.neuroimage.2015.05.092
    -- the exchangeability-block scheme this file implements a single level
       of (within-block, whole-block, and the two combined).

  Nichols TE, Holmes AP.  Nonparametric permutation tests for functional
    neuroimaging: a primer with examples.  Human Brain Mapping,
    15(1):1-25, 2002.
    -- the max-statistic FWE correction used by THD_perm_result_finish().

  Phipson B, Smyth GK.  Permutation p-values should never be zero.
    Statistical Applications in Genetics and Molecular Biology, 9(1), 2010.
    -- why the identity relabeling belongs in the null distribution.
------------------------------------------------------------------------------*/

#include "matrix.h"    /* must precede mrilib.h: picks double, not float */
#include "mrilib.h"
#include "thd_permute.h"

#include <limits.h>
#include <math.h>
#include <float.h>
#include <stdint.h>

#ifdef USE_OMP
#include <omp.h>
#endif

/*--------------------------------------------------------------------------*/
/*! Rank tolerance for deciding how many singular values of the design count. */

#define PERM_RANK_EPS 1.0e-10

/*! Largest number of independent sign-flip units we will enumerate exactly. */

#define PERM_MAX_FLIP_BITS 30

/*! Largest relabeling group we will ENUMERATE exactly by default.  Exact
    enumeration materializes the whole set (nperm*nobs ints + signs), so an
    astronomically exact group is a memory bomb: a group of 1e8 with nobs=20 is
    ~2 GB.  Above this cap we fall back to random sampling even when exact
    enumeration was requested, so ps->exact never silently allocates gigabytes.
    Small designs -- where exact enumeration is the point (n=10 sign flip = 1024,
    a 12-subject two-group test = 924) -- are well under it. */

#ifndef PERM_MAX_EXACT
#define PERM_MAX_EXACT 200000
#endif

/*--------------------------------------------------------------------------*/
/* Small arithmetic helpers, all saturating at PERM_COUNT_TOOBIG so that a
   design with an astronomically large relabeling group reports "too big"
   instead of silently wrapping around.                                     */
/*--------------------------------------------------------------------------*/

/* Multiply two counts, saturating once the product exceeds INT_MAX. */

static long long perm_mul( long long a, long long b )
{
   long long out ;
   if( a == PERM_COUNT_TOOBIG || b == PERM_COUNT_TOOBIG ) return PERM_COUNT_TOOBIG ;
   if( a == 0 || b == 0 ) return 0 ;
   out = a * b ;
   if( out / b != a || out > (long long)INT_MAX ) return PERM_COUNT_TOOBIG ;
   return out ;
}

/* Count the ways to choose kk of nn items, saturating past INT_MAX. */

static long long perm_binom( int nn, int kk )
{
   int ii ;
   long long out = 1 ;
   if( kk < 0 || kk > nn ) return 0 ;
   if( kk > nn-kk ) kk = nn-kk ;
   for( ii=1 ; ii <= kk ; ii++ ){
      long long factor = nn-kk+ii ;
      if( out > LLONG_MAX/factor ) return PERM_COUNT_TOOBIG ;
      out = (out*factor)/ii ;
      if( out > (long long)INT_MAX ) return PERM_COUNT_TOOBIG ;
   }
   return out ;
}

/* Count nn!, saturating past INT_MAX. */

static long long perm_factorial( int nn )
{
   int ii ;
   long long out = 1 ;
   for( ii=2 ; ii <= nn ; ii++ ){
      out *= ii ;
      if( out > (long long)INT_MAX ) return PERM_COUNT_TOOBIG ;
   }
   return out ;
}

/*--------------------------------------------------------------------------*/
/* Exchangeability scheme                                                   */
/*--------------------------------------------------------------------------*/

/* Create a scheme for nobs observations: one block, permutation only,
   exact if feasible.  Returns NULL on a bad argument. */

PERM_scheme * THD_perm_scheme_new( int nobs )
{
   PERM_scheme *ps ;
   int ii ;

   if( nobs < 2 ) return NULL ;

   ps = (PERM_scheme *)calloc(1,sizeof(PERM_scheme)) ;
   if( ps == NULL ) return NULL ;

   ps->nobs      = nobs ;
   ps->exchange  = PERM_EE ;
   ps->blockmode = PERM_WITHIN_BLOCK ;
   ps->exact     = 1 ;
   ps->nperm     = 0 ;
   ps->seed      = 1234567L ;
   ps->nblock    = 1 ;
   ps->eqclass   = NULL ;

   ps->block = (int *)calloc((size_t)nobs,sizeof(int)) ;
   if( ps->block == NULL ){ free(ps) ; return NULL ; }
   for( ii=0 ; ii < nobs ; ii++ ) ps->block[ii] = 0 ;

   return ps ;
}

void THD_perm_scheme_free( PERM_scheme *ps )
{
   if( ps == NULL ) return ;
   if( ps->block   != NULL ) free(ps->block) ;
   if( ps->eqclass != NULL ) free(ps->eqclass) ;
   free(ps) ;
}

/* Renumber arbitrary integer labels to 0..nlab-1 in order of first
   appearance, writing into out.  Returns the number of distinct labels. */

static int perm_relabel( int *lab, int nn, int *out )
{
   int ii, jj, nlab = 0 ;
   int *seen = (int *)malloc(sizeof(int)*(size_t)nn) ;
   if( seen == NULL ) return 0 ;

   for( ii=0 ; ii < nn ; ii++ ){
      for( jj=0 ; jj < nlab ; jj++ ) if( seen[jj] == lab[ii] ) break ;
      if( jj == nlab ){ seen[nlab] = lab[ii] ; nlab++ ; }
      out[ii] = jj ;
   }

   free(seen) ;
   return nlab ;
}

int THD_perm_scheme_set_blocks( PERM_scheme *ps, int *blk )
{
   int ii ;

   if( ps == NULL ) return 0 ;

   if( blk == NULL ){                    /* back to a single block */
      for( ii=0 ; ii < ps->nobs ; ii++ ) ps->block[ii] = 0 ;
      ps->nblock = 1 ;
      return 1 ;
   }

   ps->nblock = perm_relabel(blk,ps->nobs,ps->block) ;
   return ps->nblock ;
}

int THD_perm_scheme_set_eqclass( PERM_scheme *ps, int *cls )
{
   if( ps == NULL ) return 0 ;

   if( ps->eqclass != NULL ){ free(ps->eqclass) ; ps->eqclass = NULL ; }
   if( cls == NULL ) return 1 ;

   ps->eqclass = (int *)malloc(sizeof(int)*(size_t)ps->nobs) ;
   if( ps->eqclass == NULL ) return 0 ;

   return perm_relabel(cls,ps->nobs,ps->eqclass) ;
}

/*--------------------------------------------------------------------------*/
/* Block bookkeeping used while a relabeling set is being built.            */
/*--------------------------------------------------------------------------*/

typedef struct {
   int size ;
   int *dst ;   /* [size] observations belonging to this block, ascending */
   int *src ;   /* [size] observation currently feeding each slot of dst  */
   int *key ;   /* [size] equivalence key travelling alongside src        */
} perm_block ;

typedef struct {
   PERM_scheme *ps ;
   int nblock ;
   perm_block *blk ;
   int *barr ;           /* [nblock] source block feeding each block slot  */
   int nsign ;           /* number of independent sign-flip units          */
   signed char *sunit ;  /* [nsign] current sign of each unit              */
   unsigned long sctr ;  /* exact-enumeration counter over the sign units  */
   unsigned long smax ;  /* 2^nsign, only used when enumerating exactly    */
   int per_block_sign ;  /* 1 when a sign unit covers a whole block        */
   int use_ee, use_ise ;
   int use_within, use_whole ;
} perm_state ;

/* Sort a block's (key,src) pairs into ascending key order, ties keeping
   ascending src.  Insertion sort: blocks are small, and stability matters
   more than speed here. */

static void perm_block_sort( perm_block *pb )
{
   int ii, jj, k, s ;
   for( ii=1 ; ii < pb->size ; ii++ ){
      k = pb->key[ii] ; s = pb->src[ii] ;
      for( jj=ii-1 ; jj >= 0 && (pb->key[jj] > k ||
                                (pb->key[jj] == k && pb->src[jj] > s)) ; jj-- ){
         pb->key[jj+1] = pb->key[jj] ; pb->src[jj+1] = pb->src[jj] ;
      }
      pb->key[jj+1] = k ; pb->src[jj+1] = s ;
   }
}

/* Put a block back to the first arrangement of its enumeration. */

static void perm_block_reset( perm_block *pb, int *eqclass )
{
   int kk ;
   for( kk=0 ; kk < pb->size ; kk++ ){
      pb->src[kk] = pb->dst[kk] ;
      pb->key[kk] = (eqclass != NULL) ? eqclass[pb->dst[kk]] : pb->dst[kk] ;
   }
   perm_block_sort(pb) ;
}

/* Step a block to the next arrangement in lexicographic key order, or return
   0 once the last one has been passed.

   Running the classic next-permutation algorithm on the *keys* rather than on
   the observation indices is what makes equivalence classes pay off: keys
   that repeat collapse the redundant arrangements, so a two-group design with
   na and nb members visits choose(na+nb,na) relabelings instead of
   (na+nb)! of them, with no change to the resulting null distribution. */

static int perm_block_next( perm_block *pb )
{
   int ii, jj, lo, hi, t ;

   for( ii=pb->size-2 ; ii >= 0 && pb->key[ii] >= pb->key[ii+1] ; ii-- ) ; /* nada */
   if( ii < 0 ) return 0 ;

   for( jj=pb->size-1 ; pb->key[jj] <= pb->key[ii] ; jj-- ) ; /* nada */

   t = pb->key[ii] ; pb->key[ii] = pb->key[jj] ; pb->key[jj] = t ;
   t = pb->src[ii] ; pb->src[ii] = pb->src[jj] ; pb->src[jj] = t ;

   for( lo=ii+1, hi=pb->size-1 ; lo < hi ; lo++, hi-- ){
      t = pb->key[lo] ; pb->key[lo] = pb->key[hi] ; pb->key[hi] = t ;
      t = pb->src[lo] ; pb->src[lo] = pb->src[hi] ; pb->src[hi] = t ;
   }
   return 1 ;
}

/* Step an all-distinct index array to its next lexicographic arrangement. */

static int perm_array_next( int *arr, int nn )
{
   int ii, jj, lo, hi, t ;

   for( ii=nn-2 ; ii >= 0 && arr[ii] >= arr[ii+1] ; ii-- ) ; /* nada */
   if( ii < 0 ) return 0 ;
   for( jj=nn-1 ; arr[jj] <= arr[ii] ; jj-- ) ; /* nada */

   t = arr[ii] ; arr[ii] = arr[jj] ; arr[jj] = t ;
   for( lo=ii+1, hi=nn-1 ; lo < hi ; lo++, hi-- ){
      t = arr[lo] ; arr[lo] = arr[hi] ; arr[hi] = t ;
   }
   return 1 ;
}

/* Shuffle an int array in place (Fisher-Yates, drand48 like the rest of
   AFNI). */

static void perm_array_shuffle( int *arr, int nn )
{
   int ii, jj, t ;
   for( ii=nn-1 ; ii > 0 ; ii-- ){
      jj = (int)(drand48()*(ii+1)) ;
      if( jj > ii ) jj = ii ;
      t = arr[ii] ; arr[ii] = arr[jj] ; arr[jj] = t ;
   }
}

/* Release everything hanging off a perm_state. */

static void perm_state_free( perm_state *st )
{
   int bb ;
   if( st == NULL ) return ;
   if( st->blk != NULL ){
      for( bb=0 ; bb < st->nblock ; bb++ ){
         if( st->blk[bb].dst != NULL ) free(st->blk[bb].dst) ;
         if( st->blk[bb].src != NULL ) free(st->blk[bb].src) ;
         if( st->blk[bb].key != NULL ) free(st->blk[bb].key) ;
      }
      free(st->blk) ;
   }
   if( st->barr  != NULL ) free(st->barr) ;
   if( st->sunit != NULL ) free(st->sunit) ;
   free(st) ;
}

/* Turn a scheme into the block lists and counters the builders walk over.
   Returns NULL if the scheme is inconsistent. */

static perm_state * perm_state_init( PERM_scheme *ps )
{
   perm_state *st ;
   int ii, bb, *fill ;

   if( ps == NULL || ps->nobs < 2 ) return NULL ;
   if( (ps->exchange & PERM_BOTH) == 0 ) return NULL ;

   st = (perm_state *)calloc(1,sizeof(perm_state)) ;
   if( st == NULL ) return NULL ;

   st->ps      = ps ;
   st->nblock  = ps->nblock ;
   st->use_ee  = (ps->exchange & PERM_EE ) != 0 ;
   st->use_ise = (ps->exchange & PERM_ISE) != 0 ;

   st->use_within = st->use_ee &&
                    (ps->blockmode == PERM_WITHIN_BLOCK ||
                     ps->blockmode == PERM_BOTH_BLOCK) ;
   st->use_whole  = st->use_ee &&
                    (ps->blockmode == PERM_WHOLE_BLOCK ||
                     ps->blockmode == PERM_BOTH_BLOCK) ;

   /* One sign per block when whole blocks move as units, otherwise one per
      observation.  A block-level flip keeps the members of a block together,
      which is the point of declaring the block in the first place. */

   st->per_block_sign = st->use_ise && (ps->blockmode == PERM_WHOLE_BLOCK) ;
   if( st->use_ise )
      st->nsign = st->per_block_sign ? st->nblock : ps->nobs ;
   else
      st->nsign = 0 ;

   st->blk = (perm_block *)calloc((size_t)st->nblock,sizeof(perm_block)) ;
   st->barr = (int *)calloc((size_t)st->nblock,sizeof(int)) ;
   fill = (int *)calloc((size_t)st->nblock,sizeof(int)) ;
   if( st->blk == NULL || st->barr == NULL || fill == NULL ){
      if( fill != NULL ) free(fill) ;
      perm_state_free(st) ; return NULL ;
   }

   for( ii=0 ; ii < ps->nobs ; ii++ ) st->blk[ps->block[ii]].size++ ;
   for( bb=0 ; bb < st->nblock ; bb++ ){
      int sz = st->blk[bb].size ;
      if( sz < 1 ){ free(fill) ; perm_state_free(st) ; return NULL ; }
      st->blk[bb].dst = (int *)calloc((size_t)sz,sizeof(int)) ;
      st->blk[bb].src = (int *)calloc((size_t)sz,sizeof(int)) ;
      st->blk[bb].key = (int *)calloc((size_t)sz,sizeof(int)) ;
      if( st->blk[bb].dst == NULL || st->blk[bb].src == NULL ||
          st->blk[bb].key == NULL ){
         free(fill) ; perm_state_free(st) ; return NULL ;
      }
      st->barr[bb] = bb ;
   }
   for( ii=0 ; ii < ps->nobs ; ii++ ){
      int bx = ps->block[ii] ;
      st->blk[bx].dst[fill[bx]] = ii ;
      fill[bx]++ ;
   }
   free(fill) ;

   /* Trading whole blocks only makes sense if they are the same shape. */

   if( st->use_whole ){
      for( bb=1 ; bb < st->nblock ; bb++ )
         if( st->blk[bb].size != st->blk[0].size ){
            perm_state_free(st) ; return NULL ;
         }
   }

   for( bb=0 ; bb < st->nblock ; bb++ )
      perm_block_reset(&st->blk[bb],ps->eqclass) ;

   if( st->nsign > 0 ){
      st->sunit = (signed char *)malloc((size_t)st->nsign) ;
      if( st->sunit == NULL ){ perm_state_free(st) ; return NULL ; }
      for( ii=0 ; ii < st->nsign ; ii++ ) st->sunit[ii] = 1 ;
   }
   st->sctr = 0 ;
   st->smax = (st->nsign > 0 && st->nsign <= PERM_MAX_FLIP_BITS)
              ? (1UL << st->nsign) : 1UL ;

   return st ;
}

/*--------------------------------------------------------------------------*/

long long THD_perm_group_size( PERM_scheme *ps )
{
   perm_state *st ;
   long long total = 1 ;
   int bb ;

   st = perm_state_init(ps) ;
   if( st == NULL ) return PERM_COUNT_TOOBIG ;

   if( st->use_within ){
      for( bb=0 ; bb < st->nblock ; bb++ ){
         perm_block *pb = &st->blk[bb] ;
         long long cnt = 1 ;
         int kk = 0 ;
         /* keys are sorted, so equal keys are adjacent: the arrangement count
            is the multinomial coefficient over the run lengths */
         while( kk < pb->size ){
            int run = 1 ;
            while( kk+run < pb->size && pb->key[kk+run] == pb->key[kk] ) run++ ;
            cnt = perm_mul(cnt,perm_binom(pb->size-kk,run)) ;
            kk += run ;
         }
         total = perm_mul(total,cnt) ;
      }
   }
   if( st->use_whole )
      total = perm_mul(total,perm_factorial(st->nblock)) ;

   if( st->use_ise ){
      if( st->nsign > PERM_MAX_FLIP_BITS ) total = PERM_COUNT_TOOBIG ;
      else total = perm_mul(total,1LL << st->nsign) ;
   }

   perm_state_free(st) ;
   return total ;
}

/*--------------------------------------------------------------------------*/
/* Write the relabeling described by the current state into slot ip.        */

static void perm_state_emit( perm_state *st, PERM_set *pset, int ip )
{
   int bb, kk, nobs = pset->nobs ;
   int *pp = pset->perm + (size_t)ip*nobs ;
   signed char *sg = pset->sign + (size_t)ip*nobs ;

   for( bb=0 ; bb < st->nblock ; bb++ ){
      perm_block *dstb = &st->blk[bb] ;
      /* which block's contents land in this block's slots */
      perm_block *srcb = st->use_whole ? &st->blk[st->barr[bb]] : dstb ;
      for( kk=0 ; kk < dstb->size ; kk++ ){
         /* within-block reordering is read from the source block, so that
            "both" mode composes the two moves rather than picking one */
         pp[dstb->dst[kk]] = st->use_within ? srcb->src[kk] : srcb->dst[kk] ;
      }
   }

   if( st->nsign <= 0 ){
      for( kk=0 ; kk < nobs ; kk++ ) sg[kk] = 1 ;
   } else if( st->per_block_sign ){
      for( bb=0 ; bb < st->nblock ; bb++ )
         for( kk=0 ; kk < st->blk[bb].size ; kk++ )
            sg[st->blk[bb].dst[kk]] = st->sunit[bb] ;
   } else {
      for( kk=0 ; kk < nobs ; kk++ ) sg[kk] = st->sunit[kk] ;
   }
}

/* Advance the odometer: sign pattern fastest, then each block's internal
   arrangement, then the arrangement of the blocks themselves.  Returns 0
   once the whole group has been visited. */

static int perm_state_next( perm_state *st )
{
   int bb ;

   /* Exact enumeration only ever gets here with nsign <= PERM_MAX_FLIP_BITS,
      because a larger group is rejected as unenumerable up front. */

   if( st->nsign > 0 ){
      st->sctr++ ;
      if( st->sctr < st->smax ){
         for( bb=0 ; bb < st->nsign ; bb++ )
            st->sunit[bb] = ((st->sctr >> bb) & 1UL) ? -1 : 1 ;
         return 1 ;
      }
      st->sctr = 0 ;
      for( bb=0 ; bb < st->nsign ; bb++ ) st->sunit[bb] = 1 ;
   }

   if( st->use_within ){
      for( bb=0 ; bb < st->nblock ; bb++ ){
         if( perm_block_next(&st->blk[bb]) ) return 1 ;
         perm_block_reset(&st->blk[bb],st->ps->eqclass) ;
      }
   }

   if( st->use_whole ){
      if( perm_array_next(st->barr,st->nblock) ) return 1 ;
      for( bb=0 ; bb < st->nblock ; bb++ ) st->barr[bb] = bb ;
   }

   return 0 ;
}

/* Put the state on the identity relabeling: everything in its own slot, no
   sign flipped.  perm_state_init() leaves each block sorted by equivalence
   key instead, which is the right starting point for enumeration but is not
   literally the identity when the keys do not already ascend. */

static void perm_state_identity( perm_state *st )
{
   int bb, kk ;

   for( bb=0 ; bb < st->nblock ; bb++ ){
      perm_block *pb = &st->blk[bb] ;
      for( kk=0 ; kk < pb->size ; kk++ ) pb->src[kk] = pb->dst[kk] ;
      st->barr[bb] = bb ;
   }
   for( kk=0 ; kk < st->nsign ; kk++ ) st->sunit[kk] = 1 ;
}

/* Draw one relabeling uniformly from the group. */

static void perm_state_random( perm_state *st )
{
   int bb, kk ;

   if( st->use_whole ) perm_array_shuffle(st->barr,st->nblock) ;

   if( st->use_within ){
      for( bb=0 ; bb < st->nblock ; bb++ ){
         perm_block *pb = &st->blk[bb] ;
         for( kk=0 ; kk < pb->size ; kk++ ) pb->src[kk] = pb->dst[kk] ;
         perm_array_shuffle(pb->src,pb->size) ;
      }
   }

   /* Draw the signs straight into the unit array rather than through the
      enumeration counter, so that designs with far more sign units than a
      machine word has bits still work in random mode. */

   for( kk=0 ; kk < st->nsign ; kk++ )
      st->sunit[kk] = (drand48() < 0.5) ? -1 : 1 ;
}

/*--------------------------------------------------------------------------*/

PERM_set * THD_perm_set_build( PERM_scheme *ps )
{
   PERM_set *pset ;
   perm_state *st ;
   long long gsize ;
   int nperm, ip, exact ;

ENTRY("THD_perm_set_build") ;

   if( ps == NULL || ps->nobs < 2 ) RETURN(NULL) ;

   gsize = THD_perm_group_size(ps) ;
   exact = (gsize != PERM_COUNT_TOOBIG && gsize > 0) &&
           (gsize <= (long long)PERM_MAX_EXACT) &&
           (ps->exact || (long long)ps->nperm >= gsize) ;

   /* A group too large to enumerate but exact was requested: warn and fall
      through to random sampling, which needs a positive nperm. */
   if( !exact && ps->exact && gsize > (long long)PERM_MAX_EXACT &&
       gsize != PERM_COUNT_TOOBIG )
      INFO_message("permutation set: group holds %lld relabelings, above the "
                   "exact-enumeration cap of %d; sampling %d at random instead",
                   gsize,PERM_MAX_EXACT,ps->nperm) ;

   if( exact ){
      nperm = (int)gsize ;
      /* Asking for at least as many random draws as the group holds is a
         request for the exact answer, whether or not it was phrased that
         way: enumerating is both cheaper and free of sampling error. */
      if( !ps->exact )
         INFO_message("permutation set: %d draws requested but the group holds "
                      "only %lld relabelings, so enumerating it exactly",
                      ps->nperm,gsize) ;
   } else {
      if( ps->nperm < 1 ){
         ERROR_message("permutation set: need a positive nperm when the "
                       "relabeling group cannot be enumerated exactly") ;
         RETURN(NULL) ;
      }
      nperm = ps->nperm ;
   }

   st = perm_state_init(ps) ;
   if( st == NULL ){
      ERROR_message("permutation set: inconsistent exchangeability scheme") ;
      RETURN(NULL) ;
   }

   pset = (PERM_set *)calloc(1,sizeof(PERM_set)) ;
   if( pset == NULL ){ perm_state_free(st) ; RETURN(NULL) ; }

   pset->nperm    = nperm ;
   pset->nobs     = ps->nobs ;
   pset->is_exact = exact ;
   pset->perm = (int *)calloc((size_t)nperm*ps->nobs,sizeof(int)) ;
   pset->sign = (signed char *)calloc((size_t)nperm*ps->nobs,sizeof(signed char)) ;
   if( pset->perm == NULL || pset->sign == NULL ){
      perm_state_free(st) ; THD_perm_set_free(pset) ;
      ERROR_message("permutation set: cannot allocate %d x %d relabelings",
                    nperm,ps->nobs) ;
      RETURN(NULL) ;
   }

   if( exact ){
      /* Enumerate the group.  The identity is always one of the arrangements
         visited (up to a swap of equivalent observations, which by definition
         leaves the statistic alone), so the observed statistic is part of its
         own null distribution without any special casing. */
      for( ip=0 ; ip < nperm ; ip++ ){
         perm_state_emit(st,pset,ip) ;
         if( ip+1 < nperm && !perm_state_next(st) ){
            ERROR_message("permutation set: enumeration ended after %d of %d",
                          ip+1,nperm) ;
            pset->nperm = ip+1 ; break ;
         }
      }
   } else {
      srand48(ps->seed) ;
      /* Slot 0 is the identity on purpose: an empirical p-value must be able
         to count the observed statistic among the null values, which is what
         keeps it from ever reaching zero (Phipson & Smyth, 2010). */
      perm_state_identity(st) ;
      perm_state_emit(st,pset,0) ;
      for( ip=1 ; ip < nperm ; ip++ ){
         perm_state_random(st) ;
         perm_state_emit(st,pset,ip) ;
      }
   }

   perm_state_free(st) ;
   RETURN(pset) ;
}

void THD_perm_set_free( PERM_set *pset )
{
   if( pset == NULL ) return ;
   if( pset->perm != NULL ) free(pset->perm) ;
   if( pset->sign != NULL ) free(pset->sign) ;
   free(pset) ;
}

void THD_perm_shuffle_r( int *arr, int n, unsigned short xs[3] )
{
   int ii, jj, t ;
   if( arr == NULL || n < 2 ) return ;
   for( ii=n-1 ; ii > 0 ; ii-- ){
      jj = (int)( nrand48(xs) % (long)(ii+1) ) ;
      t = arr[ii] ; arr[ii] = arr[jj] ; arr[jj] = t ;
   }
}

void THD_perm_set_apply( PERM_set *pset, int ip, float *yin, float *yout )
{
   int ii, nobs ;
   int *pp ; signed char *sg ;

   if( pset == NULL || yin == NULL || yout == NULL ) return ;
   if( ip < 0 || ip >= pset->nperm ) return ;

   nobs = pset->nobs ;
   pp   = pset->perm + (size_t)ip*nobs ;
   sg   = pset->sign + (size_t)ip*nobs ;
   for( ii=0 ; ii < nobs ; ii++ ) yout[ii] = sg[ii] * yin[pp[ii]] ;
}

/*--------------------------------------------------------------------------*/
/* SplitMix64 gives bootstrap, time-shift, and phase-null sets a small, explicit,
   reproducible RNG independent of the process-global drand48 stream used by
   PERM_set.  Stream separators keep their draws mutually independent.  The
   rejection step avoids modulo bias when the range is not a power of two. */

static uint64_t resample_u64( uint64_t *state )
{
   uint64_t z ;
   *state += UINT64_C(0x9e3779b97f4a7c15) ;
   z = *state ;
   z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9) ;
   z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb) ;
   return z ^ (z >> 31) ;
}

static int resample_index( uint64_t *state, int nobs )
{
   uint64_t x, lim = UINT64_MAX - (UINT64_MAX % (uint64_t)nobs) ;
   do { x = resample_u64(state) ; } while( x >= lim ) ;
   return (int)(x % (uint64_t)nobs) ;
}

THD_resample_set * THD_resample_set_build( int nobs, int nresample, long seed )
{
   THD_resample_set *rset ;
   uint64_t state ;
   size_t ii, ntot ;

   if( nobs < 2 || nresample < 1 ) return NULL ;
   if( (size_t)nresample > ((size_t)-1)/((size_t)nobs*sizeof(int)) ) return NULL ;

   rset = (THD_resample_set *)calloc(1,sizeof(THD_resample_set)) ;
   if( rset == NULL ) return NULL ;
   rset->nobs = nobs ; rset->nresample = nresample ;
   rset->index = (int *)malloc(sizeof(int)*(size_t)nobs*nresample) ;
   if( rset->index == NULL ){ free(rset) ; return NULL ; }

   /* A fixed stream separator makes the bootstrap independent even when the
      caller intentionally gives it the same user-facing seed as permutation. */
   state = (uint64_t)(unsigned long long)seed ^ UINT64_C(0xd1b54a32d192ed03) ;
   ntot = (size_t)nobs*nresample ;
   for( ii=0 ; ii < ntot ; ii++ ) rset->index[ii] = resample_index(&state,nobs) ;
   return rset ;
}

THD_resample_set * THD_resample_set_build_stratified( int nobs, int nresample,
                                                       long seed, int *block )
{
   THD_resample_set *rset ;
   uint64_t state ;
   int *label=NULL, *size=NULL, *fill=NULL, *member=NULL, *offset=NULL ;
   int ii,jj,bb,ir,nblock=0 ;

   if( block == NULL ) return THD_resample_set_build(nobs,nresample,seed) ;
   if( nobs < 2 || nresample < 1 ) return NULL ;
   if( (size_t)nresample > ((size_t)-1)/((size_t)nobs*sizeof(int)) ) return NULL ;

   label=(int *)malloc(sizeof(int)*(size_t)nobs) ;
   size =(int *)calloc((size_t)nobs,sizeof(int)) ;
   fill =(int *)calloc((size_t)nobs,sizeof(int)) ;
   member=(int *)malloc(sizeof(int)*(size_t)nobs) ;
   offset=(int *)malloc(sizeof(int)*(size_t)(nobs+1)) ;
   if( label==NULL || size==NULL || fill==NULL || member==NULL || offset==NULL ) goto bad ;

   /* Relabel arbitrary integers by first appearance, matching PERM_scheme. */
   for( ii=0 ; ii<nobs ; ii++ ){
      label[ii]=-1 ;
      for( jj=0 ; jj<ii ; jj++ ) if( block[jj]==block[ii] ){
         label[ii]=label[jj] ; break ;
      }
      if( label[ii]<0 ) label[ii]=nblock++ ;
      size[label[ii]]++ ;
   }
   offset[0]=0 ;
   for( bb=0 ; bb<nblock ; bb++ ) offset[bb+1]=offset[bb]+size[bb] ;
   for( ii=0 ; ii<nobs ; ii++ ){
      bb=label[ii] ; member[offset[bb]+fill[bb]++]=ii ;
   }

   rset=(THD_resample_set *)calloc(1,sizeof(THD_resample_set)) ;
   if( rset==NULL ) goto bad ;
   rset->nobs=nobs ; rset->nresample=nresample ;
   rset->index=(int *)malloc(sizeof(int)*(size_t)nobs*nresample) ;
   if( rset->index==NULL ){ free(rset) ; rset=NULL ; goto bad ; }

   state=(uint64_t)(unsigned long long)seed ^ UINT64_C(0xd1b54a32d192ed03) ;
   for( ir=0 ; ir<nresample ; ir++ ) for( ii=0 ; ii<nobs ; ii++ ){
      bb=label[ii] ;
      rset->index[(size_t)ir*nobs+ii]
        =member[offset[bb]+resample_index(&state,size[bb])] ;
   }
   free(label);free(size);free(fill);free(member);free(offset) ;
   return rset ;

bad:
   free(label);free(size);free(fill);free(member);free(offset) ;
   return NULL ;
}

void THD_resample_set_free( THD_resample_set *rset )
{
   if( rset == NULL ) return ;
   free(rset->index) ; free(rset) ;
}

THD_timeshift_set * THD_timeshift_set_build( int nobs, int ntime,
                                              int nshift, int min_shift,
                                              long seed )
{
   THD_timeshift_set *tset ;
   uint64_t state ;
   int nallow, ss, jj ; long long na ;

   if( nobs < 2 || ntime < 3 || nshift < 1 || min_shift < 1 ) return NULL ;
   na = (long long)ntime - 2LL*min_shift + 1LL ;
   if( na < 2 || na > INT_MAX ) return NULL ;
   nallow = (int)na ;
   if( (size_t)nshift > ((size_t)-1)/((size_t)nobs*sizeof(int)) ) return NULL ;

   tset = (THD_timeshift_set *)calloc(1,sizeof(THD_timeshift_set)) ;
   if( tset == NULL ) return NULL ;
   tset->nshift=nshift ; tset->nobs=nobs ; tset->ntime=ntime ;
   tset->min_shift=min_shift ;
   tset->offset=(int *)calloc((size_t)nshift*nobs,sizeof(int)) ;
   if( tset->offset == NULL ){ free(tset) ; return NULL ; }

   /* A third stream separator keeps shift draws independent of both the
      permutation and ordinary-bootstrap streams at the same user seed. */
   state = (uint64_t)(unsigned long long)seed ^ UINT64_C(0xa0761d6478bd642f) ;
   for( ss=1 ; ss < nshift ; ss++ )
      for( jj=0 ; jj < nobs ; jj++ )
         tset->offset[(size_t)ss*nobs+jj]
            = min_shift + resample_index(&state,nallow) ;
   return tset ;
}

void THD_timeshift_set_free( THD_timeshift_set *tset )
{
   if( tset == NULL ) return ;
   free(tset->offset) ; free(tset) ;
}

THD_phase_set * THD_phase_set_build( int nobs, int ntime, int nphase,
                                      long seed )
{
   THD_phase_set *pset ;
   if( nobs < 2 || ntime < 3 || nphase < 1 ) return NULL ;
   pset=(THD_phase_set *)calloc(1,sizeof(THD_phase_set)) ;
   if( pset == NULL ) return NULL ;
   pset->nphase=nphase ; pset->nobs=nobs ; pset->ntime=ntime ;
   pset->nfreq=(ntime-1)/2 ;
   pset->stream=(unsigned long long)seed ^ UINT64_C(0xe7037ed1a0b428db) ;
   return pset ;
}

void THD_phase_set_factor( const THD_phase_set *pset, int iphase,
                           int iobs, int freq, float *co, float *si )
{
   uint64_t state,z,flat ; double ang ;
   if( co == NULL || si == NULL ) return ;
   *co=1.0f ; *si=0.0f ;
   if( pset == NULL || iphase <= 0 || iphase >= pset->nphase ||
       iobs < 0 || iobs >= pset->nobs || freq < 1 || freq > pset->nfreq ) return ;

   flat=((uint64_t)(iphase-1)*(uint64_t)pset->nobs+(uint64_t)iobs)
        *(uint64_t)pset->nfreq+(uint64_t)(freq-1) ;
   state=(uint64_t)pset->stream
        +UINT64_C(0x9e3779b97f4a7c15)*flat ;
   z=resample_u64(&state) ;
   ang=6.283185307179586476925286766559
      *(double)(z >> 11)*0x1.0p-53 ;
   *co=(float)cos(ang) ; *si=(float)sin(ang) ;
}

void THD_phase_set_free( THD_phase_set *pset )
{
   free(pset) ;
}

/*--------------------------------------------------------------------------*/
/* General linear model setup                                               */
/*--------------------------------------------------------------------------*/

/* Numerical rank of a matrix, from its singular values. */

static int perm_matrix_rank( matrix X )
{
   double *sv = matrix_singvals(X) ;
   double smax = 0.0 ;
   int ii, rank = 0 ;

   if( sv == NULL ) return X.cols ;
   for( ii=0 ; ii < X.cols ; ii++ ) if( sv[ii] > smax ) smax = sv[ii] ;
   if( smax > 0.0 )
      for( ii=0 ; ii < X.cols ; ii++ ) if( sv[ii] > PERM_RANK_EPS*smax ) rank++ ;
   free(sv) ;
   return rank ;
}

/*! Replace a matrix by an orthonormal basis of its column space.

    Only the span of the nuisance regressors matters to a GLM -- residuals,
    error sum of squares, and the variance of the effect of interest are all
    unchanged by a change of basis (Frisch-Waugh-Lovell) -- so trimming the
    dependent columns away costs nothing and buys a design that (M'M) can
    actually be inverted for.  The design partitioner needs this because it
    hands back a nuisance block with more columns than it has rank.

    Returns the number of basis columns, or 0 on failure.                   */

static int perm_orth_basis( matrix Zin, matrix *Zout )
{
   int mm = Zin.rows, nn = Zin.cols, ii, jj, kk, keep ;
   double *amat, *umat, *vmat, *sval, smax ;

   if( mm < 1 || nn < 1 || mm < nn ) return 0 ;

   amat = (double *)calloc((size_t)mm*nn,sizeof(double)) ;
   umat = (double *)calloc((size_t)mm*nn,sizeof(double)) ;
   vmat = (double *)calloc((size_t)nn*nn,sizeof(double)) ;
   sval = (double *)calloc((size_t)nn,sizeof(double)) ;
   if( amat == NULL || umat == NULL || vmat == NULL || sval == NULL ){
      if( amat != NULL ) free(amat) ;
      if( umat != NULL ) free(umat) ;
      if( vmat != NULL ) free(vmat) ;
      if( sval != NULL ) free(sval) ;
      return 0 ;
   }

   /* svd_double() wants column-major storage */
   for( ii=0 ; ii < mm ; ii++ )
      for( jj=0 ; jj < nn ; jj++ ) amat[ii+jj*mm] = Zin.elts[ii][jj] ;

   svd_double(mm,nn,amat,sval,umat,vmat) ;

   smax = 0.0 ;
   for( jj=0 ; jj < nn ; jj++ ) if( sval[jj] > smax ) smax = sval[jj] ;

   keep = 0 ;
   if( smax > 0.0 )
      for( jj=0 ; jj < nn ; jj++ ) if( sval[jj] > PERM_RANK_EPS*smax ) keep++ ;

   if( keep > 0 ){
      matrix_create(mm,keep,Zout) ;
      for( jj=0, kk=0 ; jj < nn ; jj++ ){
         if( sval[jj] <= PERM_RANK_EPS*smax ) continue ;
         for( ii=0 ; ii < mm ; ii++ ) Zout->elts[ii][kk] = umat[ii+jj*mm] ;
         kk++ ;
      }
   }

   free(sval) ; free(vmat) ; free(umat) ; free(amat) ;
   return keep ;
}

/* Invert a small square matrix, falling back to the pseudo-inverse. */

static int perm_small_inverse( matrix a, matrix *ainv )
{
   if( matrix_inverse_dsc(a,ainv) ) return 1 ;
   matrix_psinv(a,NULL,ainv) ;
   return ISVALID_MATRIX(*ainv) ;
}

void THD_perm_glm_free( PERM_glm *glm )
{
   if( glm == NULL ) return ;
   matrix_destroy(&glm->M) ;
   matrix_destroy(&glm->Z) ;
   matrix_destroy(&glm->pinvM) ;
   matrix_destroy(&glm->pinvZ) ;
   matrix_destroy(&glm->Ct) ;
   matrix_destroy(&glm->cmc_inv) ;
   free(glm) ;
}

PERM_glm * THD_perm_glm_setup( matrix X, matrix Z, matrix *Ct )
{
   PERM_glm *glm ;
   matrix MtMinv, tmp, Cq, cmc ;
   int nobs, nreg, nnuis, npar, ncon, ii, jj ;

ENTRY("THD_perm_glm_setup") ;

   if( !ISVALID_MATRIX(X) ){
      ERROR_message("permutation GLM: no regressors of interest") ;
      RETURN(NULL) ;
   }
   nobs = X.rows ; nreg = X.cols ;
   nnuis = ISVALID_MATRIX(Z) ? Z.cols : 0 ;
   if( nnuis > 0 && Z.rows != nobs ){
      ERROR_message("permutation GLM: X has %d rows but Z has %d",nobs,Z.rows) ;
      RETURN(NULL) ;
   }
   npar = nreg + nnuis ;
   if( nobs <= npar ){
      ERROR_message("permutation GLM: %d observations cannot support %d regressors",
                    nobs,npar) ;
      RETURN(NULL) ;
   }

   glm = (PERM_glm *)calloc(1,sizeof(PERM_glm)) ;
   if( glm == NULL ) RETURN(NULL) ;

   matrix_initialize(&glm->M) ;      matrix_initialize(&glm->Z) ;
   matrix_initialize(&glm->pinvM) ;  matrix_initialize(&glm->pinvZ) ;
   matrix_initialize(&glm->Ct) ;     matrix_initialize(&glm->cmc_inv) ;
   matrix_initialize(&MtMinv) ; matrix_initialize(&tmp) ;
   matrix_initialize(&Cq) ;     matrix_initialize(&cmc) ;

   glm->nobs = nobs ; glm->nreg = nreg ; glm->nnuis = nnuis ; glm->npar = npar ;

   /* full design: interest columns first, nuisance columns after */

   matrix_create(nobs,npar,&glm->M) ;
   for( ii=0 ; ii < nobs ; ii++ ){
      for( jj=0 ; jj < nreg ; jj++ )  glm->M.elts[ii][jj] = X.elts[ii][jj] ;
      for( jj=0 ; jj < nnuis ; jj++ ) glm->M.elts[ii][nreg+jj] = Z.elts[ii][jj] ;
   }
   if( nnuis > 0 ){
      matrix_equate(Z,&glm->Z) ;
      matrix_psinv(glm->Z,NULL,&glm->pinvZ) ;
      if( !ISVALID_MATRIX(glm->pinvZ) ){
         ERROR_message("permutation GLM: nuisance matrix Z is degenerate") ;
         goto BAILOUT ;
      }
   }

   matrix_psinv(glm->M,&MtMinv,&glm->pinvM) ;
   if( !ISVALID_MATRIX(glm->pinvM) || !ISVALID_MATRIX(MtMinv) ){
      ERROR_message("permutation GLM: full design matrix is degenerate") ;
      goto BAILOUT ;
   }

   glm->rank = perm_matrix_rank(glm->M) ;
   glm->dof  = nobs - glm->rank ;
   if( glm->dof < 1 ){
      ERROR_message("permutation GLM: no residual degrees of freedom") ;
      goto BAILOUT ;
   }
   if( glm->rank < npar )
      WARNING_message("permutation GLM: design has rank %d but %d columns; "
                      "collinear regressors make the contrast hard to interpret",
                      glm->rank,npar) ;

   /* contrast: caller's, or "all of X at once" */

   if( Ct != NULL && ISVALID_MATRIX(*Ct) ){
      if( Ct->cols != npar ){
         ERROR_message("permutation GLM: contrast has %d columns but the design has %d",
                       Ct->cols,npar) ;
         goto BAILOUT ;
      }
      matrix_equate(*Ct,&glm->Ct) ;
   } else {
      matrix_create(nreg,npar,&glm->Ct) ;
      for( ii=0 ; ii < nreg ; ii++ ) glm->Ct.elts[ii][ii] = 1.0 ;
   }
   ncon = glm->Ct.rows ;
   glm->ncon = ncon ;
   glm->is_ftest = (ncon > 1) ;

   /* cmc = Ct (M'M)^+ Ct' -- the contrast's variance factor */

   matrix_transpose(glm->Ct,&Cq) ;
   matrix_multiply(MtMinv,Cq,&tmp) ;
   matrix_multiply(glm->Ct,tmp,&cmc) ;
   if( !perm_small_inverse(cmc,&glm->cmc_inv) ){
      ERROR_message("permutation GLM: contrast is not estimable") ;
      goto BAILOUT ;
   }
   glm->cvar = (ncon == 1) ? cmc.elts[0][0] : 0.0 ;
   if( ncon == 1 && glm->cvar <= 0.0 ){
      ERROR_message("permutation GLM: contrast has non-positive variance factor") ;
      goto BAILOUT ;
   }

   matrix_destroy(&MtMinv) ; matrix_destroy(&tmp) ;
   matrix_destroy(&Cq) ;     matrix_destroy(&cmc) ;
   RETURN(glm) ;

BAILOUT:
   matrix_destroy(&MtMinv) ; matrix_destroy(&tmp) ;
   matrix_destroy(&Cq) ;     matrix_destroy(&cmc) ;
   THD_perm_glm_free(glm) ;
   RETURN(NULL) ;
}

/*--------------------------------------------------------------------------*/
/*! Beckmann partitioning of a design against a contrast, as used by PALM
    (Winkler et al. 2014, section 2.3 and eq. 6-8):

       D  = (M'M)^+
       X  = M D C  (C' D C)^+          effect of interest, one column per
                                       contrast constraint
       Cv = I - C C^+                  a basis for the null space of C'
       Z  = M Cv                       everything the contrast does not touch

    Writing psi = D C (C' D C)^+ beta + Cv gamma makes C' psi = beta, since
    C' Cv = 0, so testing all of X in [X Z] is exactly testing C' psi = 0.

    Z as written has as many columns as M but only rank(M) - rank(C)
    independent ones, so it is reduced here to an orthonormal basis of the
    same column space.  A nuisance block contributes nothing but its span, and
    the reduction is what keeps [X Z] full rank.                            */

int THD_perm_partition_design( matrix M, matrix Ct, matrix *X, matrix *Z )
{
   matrix D, C, Dc, cdc, cdcinv, MD, Cpinv, Ccp, Cv, ident, Zraw ;
   int ok = 0, pp ;

ENTRY("THD_perm_partition_design") ;

   if( !ISVALID_MATRIX(M) || !ISVALID_MATRIX(Ct) || X == NULL || Z == NULL )
      RETURN(0) ;
   if( Ct.cols != M.cols ){
      ERROR_message("design partition: contrast has %d columns but design has %d",
                    Ct.cols,M.cols) ;
      RETURN(0) ;
   }
   pp = M.cols ;

   matrix_initialize(&D) ;     matrix_initialize(&C) ;
   matrix_initialize(&Dc) ;    matrix_initialize(&cdc) ;
   matrix_initialize(&cdcinv); matrix_initialize(&MD) ;
   matrix_initialize(&Cpinv) ; matrix_initialize(&Ccp) ;
   matrix_initialize(&Cv) ;    matrix_initialize(&ident) ;
   matrix_initialize(&Zraw) ;

   matrix_psinv(M,&D,NULL) ;                  /* D = (M'M)^+ */
   if( !ISVALID_MATRIX(D) ) goto DONE ;
   matrix_transpose(Ct,&C) ;                  /* C = Ct' , p x s */

   /* X = M D C (C' D C)^+ */
   matrix_multiply(D,C,&Dc) ;
   matrix_multiply(Ct,Dc,&cdc) ;
   if( !perm_small_inverse(cdc,&cdcinv) ) goto DONE ;
   matrix_multiply(M,Dc,&MD) ;
   matrix_multiply(MD,cdcinv,X) ;

   /* Cv = I - C C^+ spans the null space of C', so C'Cv = 0 and the nuisance
      part of the reparametrization drops out of the contrast exactly.
      matrix_subtract() writes its result through matrix_create(), which frees
      the destination first, so it must not alias either input. */
   matrix_psinv(C,NULL,&Cpinv) ;              /* Cpinv = C^+ , s x p */
   if( !ISVALID_MATRIX(Cpinv) ) goto DONE ;
   matrix_multiply(C,Cpinv,&Ccp) ;            /* C C^+ , p x p */
   matrix_identity(pp,&ident) ;
   matrix_subtract(ident,Ccp,&Cv) ;

   /* Z = M Cv */
   matrix_multiply(M,Cv,&Zraw) ;

   /* Zraw has as many columns as M but only rank(M)-rank(C) of them are
      independent, which would leave [X Z] rank deficient and its contrast
      variance ill-defined.  Only the span matters, so hand back a basis. */
   if( !ISVALID_MATRIX(Zraw) ) goto DONE ;
   if( perm_orth_basis(Zraw,Z) < 1 ) goto DONE ;

   ok = ISVALID_MATRIX(*X) && ISVALID_MATRIX(*Z) ;
   if( ok && Z->cols < Zraw.cols )
      INFO_message("design partition: nuisance space reduced from %d columns to "
                   "a rank-%d basis",Zraw.cols,Z->cols) ;

DONE:
   matrix_destroy(&D) ;      matrix_destroy(&C) ;
   matrix_destroy(&Dc) ;     matrix_destroy(&cdc) ;
   matrix_destroy(&cdcinv) ; matrix_destroy(&MD) ;
   matrix_destroy(&Cpinv) ;  matrix_destroy(&Ccp) ;
   matrix_destroy(&Cv) ;     matrix_destroy(&ident) ;
   matrix_destroy(&Zraw) ;
   RETURN(ok) ;
}

/*--------------------------------------------------------------------------*/
/* Results                                                                  */
/*--------------------------------------------------------------------------*/

PERM_result * THD_perm_result_new( int nelem, int nperm )
{
   PERM_result *pr ;

   if( nelem < 1 || nperm < 1 ) return NULL ;

   pr = (PERM_result *)calloc(1,sizeof(PERM_result)) ;
   if( pr == NULL ) return NULL ;

   pr->nelem = nelem ; pr->nperm = nperm ; pr->dof = -1 ;
   pr->stat     = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->effect   = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->p_unc    = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->p_fwe    = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->z_unc    = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->z_fwe    = (float *)calloc((size_t)nelem,sizeof(float)) ;
   pr->cnt_unc  = (int   *)calloc((size_t)nelem,sizeof(int)) ;
   pr->max_null = (float *)calloc((size_t)nperm,sizeof(float)) ;

   if( pr->stat == NULL || pr->effect == NULL || pr->p_unc == NULL ||
       pr->p_fwe == NULL || pr->z_unc == NULL || pr->z_fwe == NULL ||
       pr->cnt_unc == NULL || pr->max_null == NULL ){
      THD_perm_result_free(pr) ; return NULL ;
   }
   return pr ;
}

void THD_perm_result_free( PERM_result *pr )
{
   if( pr == NULL ) return ;
   if( pr->stat     != NULL ) free(pr->stat) ;
   if( pr->effect   != NULL ) free(pr->effect) ;
   if( pr->p_unc    != NULL ) free(pr->p_unc) ;
   if( pr->p_fwe    != NULL ) free(pr->p_fwe) ;
   if( pr->z_unc    != NULL ) free(pr->z_unc) ;
   if( pr->z_fwe    != NULL ) free(pr->z_fwe) ;
   if( pr->cnt_unc  != NULL ) free(pr->cnt_unc) ;
   if( pr->max_null != NULL ) free(pr->max_null) ;
   free(pr) ;
}

/* Ascending comparison for qsort of the max-statistic null. */

static int perm_cmp_float( const void *a, const void *b )
{
   float aa = *((const float *)a), bb = *((const float *)b) ;
   return (aa > bb) - (aa < bb) ;
}

float THD_perm_emp_pvalue( float *sorted, int nperm, float obs )
{
   int lo = 0, hi = nperm ;
   if( sorted == NULL || nperm < 1 ) return 1.0f ;
   while( lo < hi ){
      int mid = (lo + hi) / 2 ;
      if( sorted[mid] >= obs ) hi = mid ;
      else                     lo = mid + 1 ;
   }
   return (float)(nperm - lo) / (float)nperm ;
}

/*--------------------------------------------------------------------------*/
/*! Signed z-score with the same tail probability as pval.

    AFNI's GUI can show a correct p-value on the threshold slider when it is
    handed a FIZT-coded brick, which a raw permutation p-value is not.  The
    sign of the observed statistic is carried over so that overlay direction
    still means something.  p is nudged away from 0 and 1 to keep z finite. */

float THD_perm_signed_z( float pval, float observed_stat, int tail )
{
   double p, z, pmin = 1.0e-15 ;

   p = (double)pval ;
   if( p < pmin )       p = pmin ;
   if( p > 1.0 - pmin ) p = 1.0 - pmin ;

   if( tail == PERM_TAIL_TWO ){
      z = qginv(p/2.0) ;
      if( observed_stat < 0.0f ) z = -z ;
   } else {
      z = qginv(p) ;
   }
   return (float)z ;
}

void THD_perm_result_finish( PERM_result *pr, unsigned char *mask )
{
   int ii, tail ;

   if( pr == NULL ) return ;

   tail = pr->is_ftest ? PERM_TAIL_ONE : pr->tail ;
   qsort(pr->max_null,pr->nperm,sizeof(float),perm_cmp_float) ;

   for( ii=0 ; ii < pr->nelem ; ii++ ){
      float cmpval ;
      if( mask != NULL && !mask[ii] ){
         pr->p_unc[ii] = pr->p_fwe[ii] = 1.0f ;
         pr->z_unc[ii] = pr->z_fwe[ii] = 0.0f ;
         continue ;
      }
      /* Every relabeling set contains the identity, so cnt_unc is at least 1
         and the p-value can never be zero. */
      pr->p_unc[ii] = (float)pr->cnt_unc[ii] / (float)pr->nperm ;
      cmpval = (tail == PERM_TAIL_TWO) ? fabsf(pr->stat[ii]) : pr->stat[ii] ;
      pr->p_fwe[ii] = THD_perm_emp_pvalue(pr->max_null,pr->nperm,cmpval) ;
      pr->z_unc[ii] = THD_perm_signed_z(pr->p_unc[ii],pr->stat[ii],tail) ;
      pr->z_fwe[ii] = THD_perm_signed_z(pr->p_fwe[ii],pr->stat[ii],tail) ;
   }
}

/*--------------------------------------------------------------------------*/
/* The permutation drivers                                                  */
/*--------------------------------------------------------------------------*/

/* Scratch space reused for every element and every relabeling.  The matrix.c
   vector routines reallocate their output on each call, which is fine for
   the one-time setup algebra above but not inside a loop that runs
   nelem*nperm times, so the hot path works on plain double arrays taken from
   the (already matrix.c-computed) design matrices. */

typedef struct {
   double *y, *zfit, *ez, *ep, *yp, *beta, *cb, *gz ;
} perm_work ;

static void perm_work_free( perm_work *w )
{
   if( w == NULL ) return ;
   if( w->y    != NULL ) free(w->y) ;
   if( w->zfit != NULL ) free(w->zfit) ;
   if( w->ez   != NULL ) free(w->ez) ;
   if( w->ep   != NULL ) free(w->ep) ;
   if( w->yp   != NULL ) free(w->yp) ;
   if( w->beta != NULL ) free(w->beta) ;
   if( w->cb   != NULL ) free(w->cb) ;
   if( w->gz   != NULL ) free(w->gz) ;
   memset(w,0,sizeof(perm_work)) ;
}

static int perm_work_alloc( perm_work *w, int nobs, int npar, int ncon, int nnuis )
{
   memset(w,0,sizeof(perm_work)) ;
   w->y    = (double *)calloc((size_t)nobs,sizeof(double)) ;
   w->zfit = (double *)calloc((size_t)nobs,sizeof(double)) ;
   w->ez   = (double *)calloc((size_t)nobs,sizeof(double)) ;
   w->ep   = (double *)calloc((size_t)nobs,sizeof(double)) ;
   w->yp   = (double *)calloc((size_t)nobs,sizeof(double)) ;
   w->beta = (double *)calloc((size_t)npar,sizeof(double)) ;
   w->cb   = (double *)calloc((size_t)ncon,sizeof(double)) ;
   w->gz   = (double *)calloc((size_t)(nnuis > 0 ? nnuis : 1),sizeof(double)) ;
   if( w->y == NULL || w->zfit == NULL || w->ez == NULL || w->ep == NULL ||
       w->yp == NULL || w->beta == NULL || w->cb == NULL || w->gz == NULL ){
      perm_work_free(w) ; return 0 ;
   }
   return 1 ;
}

/* Fit the full model to yp and return the contrast's t (ncon == 1) or F
   (ncon > 1).  effect, when not NULL, receives the contrast estimate. */

static float perm_glm_stat( PERM_glm *glm, double *yp, perm_work *w, float *effect )
{
   int ii, jj, nobs = glm->nobs, npar = glm->npar, ncon = glm->ncon ;
   double sse = 0.0, sigma2, num ;

   /* beta = pinvM * yp */
   for( ii=0 ; ii < npar ; ii++ ){
      double *row = glm->pinvM.elts[ii], ss = 0.0 ;
      for( jj=0 ; jj < nobs ; jj++ ) ss += row[jj]*yp[jj] ;
      w->beta[ii] = ss ;
   }

   /* residual sum of squares of yp - M*beta */
   for( ii=0 ; ii < nobs ; ii++ ){
      double *row = glm->M.elts[ii], ss = yp[ii] ;
      for( jj=0 ; jj < npar ; jj++ ) ss -= row[jj]*w->beta[jj] ;
      sse += ss*ss ;
   }

   /* contrast estimate */
   for( ii=0 ; ii < ncon ; ii++ ){
      double *row = glm->Ct.elts[ii], ss = 0.0 ;
      for( jj=0 ; jj < npar ; jj++ ) ss += row[jj]*w->beta[jj] ;
      w->cb[ii] = ss ;
   }

   sigma2 = sse / (double)glm->dof ;
   if( !(sigma2 > 0.0) || !isfinite(sigma2) ){
      if( effect != NULL ) *effect = 0.0f ;
      return 0.0f ;
   }

   if( ncon == 1 ){
      double den = sqrt(sigma2 * glm->cvar) ;
      if( effect != NULL ) *effect = (float)w->cb[0] ;
      if( !(den > 0.0) || !isfinite(den) ) return 0.0f ;
      return (float)(w->cb[0]/den) ;
   }

   /* F = (Cb)' [C (M'M)^+ C']^-1 (Cb) / (ncon * sigma^2) */
   num = 0.0 ;
   for( ii=0 ; ii < ncon ; ii++ ){
      double *row = glm->cmc_inv.elts[ii], ss = 0.0 ;
      for( jj=0 ; jj < ncon ; jj++ ) ss += row[jj]*w->cb[jj] ;
      num += w->cb[ii]*ss ;
   }
   if( effect != NULL ) *effect = (float)(num/(double)ncon) ;
   if( !isfinite(num) || num < 0.0 ) return 0.0f ;
   return (float)(num/((double)ncon*sigma2)) ;
}

/*--------------------------------------------------------------------------*/

PERM_result * THD_permute_freedman_lane( float **data, int nobs, int nelem,
                                         unsigned char *mask, PERM_glm *glm,
                                         PERM_set *pset, int tail,
                                         PERM_progfunc pfunc, void *pdata )
{
   PERM_result *pr ;
   int ip, nperm, npar, ncon, nnuis ;

ENTRY("THD_permute_freedman_lane") ;

   if( data == NULL || glm == NULL || pset == NULL ) RETURN(NULL) ;
   if( nobs != glm->nobs || nobs != pset->nobs ){
      ERROR_message("Freedman-Lane: %d observations, but the design has %d "
                    "and the relabeling set has %d",nobs,glm->nobs,pset->nobs) ;
      RETURN(NULL) ;
   }
   if( nelem < 1 ) RETURN(NULL) ;

   nperm = pset->nperm ; npar = glm->npar ; ncon = glm->ncon ; nnuis = glm->nnuis ;

   pr = THD_perm_result_new(nelem,nperm) ;
   if( pr == NULL ){ ERROR_message("Freedman-Lane: cannot allocate results") ; RETURN(NULL) ; }
   pr->dof      = glm->dof ;
   pr->is_ftest = glm->is_ftest ;
   pr->tail     = glm->is_ftest ? PERM_TAIL_ONE : tail ;

   for( ip=0 ; ip < nperm ; ip++ ) pr->max_null[ip] = -FLT_MAX ;

   /* Elements on the outside, relabelings on the inside: the reduced-model fit
      is per element, so this way it is done once rather than nperm times.  The
      loop is parallel over elements; each thread keeps its own workspace and a
      private copy of the max-statistic null, combined at the end.  cnt_unc,
      stat and effect are per element, so those need no synchronization. */

 AFNI_OMP_START ;
#pragma omp parallel
 {
   perm_work w ; float *my_max ; int e, p, i, j ;

   if( perm_work_alloc(&w,nobs,npar,ncon,nnuis) &&
       (my_max = (float *)malloc(sizeof(float)*(size_t)nperm)) != NULL ){

     for( p=0 ; p < nperm ; p++ ) my_max[p] = -FLT_MAX ;

#pragma omp for
     for( e=0 ; e < nelem ; e++ ){
       float obs ;

       if( mask != NULL && !mask[e] ){ if(pfunc) pfunc(e,nelem,pdata) ; continue ; }

       for( i=0 ; i < nobs ; i++ ) w.y[i] = (double)data[i][e] ;

       /* step 1: reduced-model fit, and the residuals that get relabeled */
       if( nnuis > 0 ){
         for( i=0 ; i < nnuis ; i++ ){
           double *row = glm->pinvZ.elts[i], ss = 0.0 ;
           for( j=0 ; j < nobs ; j++ ) ss += row[j]*w.y[j] ;
           w.gz[i] = ss ;
         }
         for( i=0 ; i < nobs ; i++ ){
           double *row = glm->Z.elts[i], ss = 0.0 ;
           for( j=0 ; j < nnuis ; j++ ) ss += row[j]*w.gz[j] ;
           w.zfit[i] = ss ; w.ez[i] = w.y[i] - ss ;
         }
       } else {
         for( i=0 ; i < nobs ; i++ ){ w.zfit[i] = 0.0 ; w.ez[i] = w.y[i] ; }
       }

       pr->stat[e] = perm_glm_stat(glm,w.y,&w,&pr->effect[e]) ;
       obs = (pr->tail == PERM_TAIL_TWO) ? fabsf(pr->stat[e]) : pr->stat[e] ;

       for( p=0 ; p < nperm ; p++ ){
         int *pp = pset->perm + (size_t)p*nobs ;
         signed char *sg = pset->sign + (size_t)p*nobs ;
         float st, cmp ;
         for( i=0 ; i < nobs ; i++ ){
           w.ep[i] = sg[i] * w.ez[pp[i]] ;
           w.yp[i] = w.zfit[i] + w.ep[i] ;
         }
         st  = perm_glm_stat(glm,w.yp,&w,NULL) ;
         cmp = (pr->tail == PERM_TAIL_TWO) ? fabsf(st) : st ;
         if( cmp > my_max[p] ) my_max[p] = cmp ;
         if( cmp >= obs ) pr->cnt_unc[e]++ ;
       }
       if( pfunc != NULL ) pfunc(e,nelem,pdata) ;
     }

#pragma omp critical
     { for( p=0 ; p < nperm ; p++ )
         if( my_max[p] > pr->max_null[p] ) pr->max_null[p] = my_max[p] ; }
     free(my_max) ;
   }
   perm_work_free(&w) ;
 }
 AFNI_OMP_END ;

   /* An element-free mask would leave the null at -FLT_MAX, which would make
      every FWE p-value meaningless; fall back to a neutral value. */
   for( ip=0 ; ip < nperm ; ip++ )
      if( pr->max_null[ip] == -FLT_MAX ) pr->max_null[ip] = 0.0f ;

   THD_perm_result_finish(pr,mask) ;
   RETURN(pr) ;
}

/*--------------------------------------------------------------------------*/

PERM_result * THD_permute_generic( float **data, int nobs, int nelem,
                                   unsigned char *mask, PERM_set *pset,
                                   PERM_statfunc sfunc, void *sdata, int tail,
                                   PERM_progfunc pfunc, void *pdata )
{
   PERM_result *pr ;
   int ip, nperm ;

ENTRY("THD_permute_generic") ;

   if( data == NULL || pset == NULL || sfunc == NULL ) RETURN(NULL) ;
   if( nobs != pset->nobs ){
      ERROR_message("permutation: %d observations, but the relabeling set has %d",
                    nobs,pset->nobs) ;
      RETURN(NULL) ;
   }
   if( nelem < 1 ) RETURN(NULL) ;

   nperm = pset->nperm ;
   pr = THD_perm_result_new(nelem,nperm) ;
   if( pr == NULL ) RETURN(NULL) ;
   pr->dof  = -1 ;
   pr->tail = tail ;

   for( ip=0 ; ip < nperm ; ip++ ) pr->max_null[ip] = -FLT_MAX ;

   /* parallel over elements; per-thread scratch and private max-null, as in
      the Freedman-Lane driver.  sfunc must be thread-safe (it is called
      concurrently on distinct elements). */

 AFNI_OMP_START ;
#pragma omp parallel
 {
   float *y , *yp , *my_max ; int e, p, i ;

   y      = (float *)calloc((size_t)nobs,sizeof(float)) ;
   yp     = (float *)calloc((size_t)nobs,sizeof(float)) ;
   my_max = (float *)malloc(sizeof(float)*(size_t)nperm) ;

   if( y != NULL && yp != NULL && my_max != NULL ){
     for( p=0 ; p < nperm ; p++ ) my_max[p] = -FLT_MAX ;

#pragma omp for
     for( e=0 ; e < nelem ; e++ ){
       float obs ;
       if( mask != NULL && !mask[e] ){ if(pfunc) pfunc(e,nelem,pdata) ; continue ; }

       for( i=0 ; i < nobs ; i++ ) y[i] = data[i][e] ;
       pr->stat[e] = sfunc(y,nobs,&pr->effect[e],sdata) ;
       obs = (tail == PERM_TAIL_TWO) ? fabsf(pr->stat[e]) : pr->stat[e] ;

       for( p=0 ; p < nperm ; p++ ){
         float st, cmp ;
         THD_perm_set_apply(pset,p,y,yp) ;
         st  = sfunc(yp,nobs,NULL,sdata) ;
         cmp = (tail == PERM_TAIL_TWO) ? fabsf(st) : st ;
         if( cmp > my_max[p] ) my_max[p] = cmp ;
         if( cmp >= obs ) pr->cnt_unc[e]++ ;
       }
       if( pfunc != NULL ) pfunc(e,nelem,pdata) ;
     }

#pragma omp critical
     { for( p=0 ; p < nperm ; p++ )
         if( my_max[p] > pr->max_null[p] ) pr->max_null[p] = my_max[p] ; }
   }
   if( y != NULL ) free(y) ;
   if( yp != NULL ) free(yp) ;
   if( my_max != NULL ) free(my_max) ;
 }
 AFNI_OMP_END ;

   for( ip=0 ; ip < nperm ; ip++ )
      if( pr->max_null[ip] == -FLT_MAX ) pr->max_null[ip] = 0.0f ;

   THD_perm_result_finish(pr,mask) ;
   RETURN(pr) ;
}
