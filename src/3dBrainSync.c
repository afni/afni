#include "mrilib.h"

#undef MEMORY_CHECK
#if 0
#ifdef USING_MCW_MALLOC
# define MEMORY_CHECK(mm)                                               \
   do{ if( verb > 5 ) mcw_malloc_dump() ;                               \
       if( verb > 1 ){                                                  \
         long long nb = mcw_malloc_total() ;                            \
         if( nb > 0 ) INFO_message("Memory usage now = %s (%s): %s" ,   \
                      commaized_integer_string(nb) ,                    \
                      approximate_number_string((double)nb) , (mm) ) ;  \
         ININFO_message(" status = %s",mcw_malloc_status(NULL,0)) ;     \
       }                                                                \
   } while(0)
#endif
#endif

#ifndef MEMORY_CHECK
# define MEMORY_CHECK(mm) /*nada*/
#endif

static int   verb    = 0 ;
static char *Qprefix = NULL , *Qbase = NULL ;
static char *Pprefix = NULL , *Pbase = NULL ;
static int do_norm   = 0 ;

static int ct ;
#define TIMER nice_time_string(NI_clock_time()-ct)+1

static THD_3dim_dataset *dsetB=NULL, *dsetC=NULL, *maskset=NULL ;

static int *bperm = NULL ;
static THD_3dim_dataset *permset=NULL , *ortset=NULL ;;

#undef DO_DESPIKE

/*----------------------------------------------------------------------------*/

void BSY_help_the_pitiful_user(void)
{
  printf(
   "\n"
   "Usage:  3dBrainSync [options]\n"
   "\n"
   "This program 'synchronizes' the -inset2 dataset to match the -inset1\n"
   "dataset, as much as possible (average voxel-wise correlation), using the\n"
   "same transformation on each input time series from -inset2:\n"
   "\n"
   " ++ With the -Qprefix option, the transformation is an orthogonal matrix,\n"
   "    computed as described in Joshi's original OHBM 2017 presentations.\n"
   "\n"
   " ++ With the -Pprefix option, the transformation is simply a\n"
   "    permutation of the time order of -inset2 (a special case\n"
   "    of an orthogonal matrix).\n"
   "\n"
   " ++ The algorithms and a little discussion of the different features of\n"
   "    these two techniques are discussed in the METHODS section, infra.\n"
   "\n"
   " ++ At least one of '-Qprefix' or '-Pprefix' must be given, or\n"
   "    this program does not do anything! You can use both methods,\n"
   "    if you want to compare them.\n"
   "\n"
   " ++ 'Harmonize' might be a better name for what this program does,\n"
   "    but calling it 3dBrainHarm would probably not be good marketing\n"
   "    (except for Traumatic Brain Injury researchers?).\n"
   "\n"
   "One possible application of this program is to correlate resting state\n"
   "FMRI datasets between subjects, voxel-by-voxel, as is sometimes done\n"
   "with naturalistic stimuli (e.g., movie viewing).\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -inset1 dataset1 = Reference dataset\n"
   "\n"
   " -inset2 dataset2 = Dataset to be matched to the reference dataset,\n"
   "                    as much as possible.\n"
   "                    ++ These 2 datasets must be on the same spatial grid,\n"
   "                       and must have the same number of time points!\n"
   "                    ++ There must be at least twice as many voxels being\n"
   "                       processed as there are time points (see '-mask', below).\n"
   "                    ++ These are both MANDATORY 'options'.\n"
   "                    ++ As usual in AFNI, since the computations herein are\n"
   "                       voxel-wise, it is possible to input plain text .1D\n"
   "                       files as datasets. When doing so, remember that\n"
   "                       a ROW in the .1D file is interpreted as a time series\n"
   "                       (single voxel's data). If your .1D files are oriented\n"
   "                       so that time runs in down the COLUMNS, you will have to\n"
   "                       transpose the inputs, which can be done on the command\n"
   "                       line with the \\' operator, or externally using the\n"
   "                       1dtranspose program.\n"
   "                -->>++ These input datasets should be pre-processed first\n"
   "                       to remove undesirable components (motions, baseline,\n"
   "                       spikes, breathing, etc). Otherwise, you will be trying\n"
   "                       to match artifacts between the datasets, which is not\n"
   "                       likely to be interesting or useful. 3dTproject would be\n"
   "                       one way to do this. Even better: afni_proc.py!\n"
   "                    ++ In particular, the mean of each time series should have\n"
   "                       been removed! Otherwise, the calculations are fairly\n"
   "                       meaningless.\n"
   "\n"
   " -Qprefix qqq     = Specifies the output dataset to be used for\n"
   "                    the orthogonal matrix transformation.\n"
   "                    ++ This will be the -inset2 dataset transformed\n"
   "                       to be as correlated as possible (in time)\n"
   "                       with the -inset1 dataset, given the constraint\n"
   "                       that the transformation applied to each time\n"
   "                       series is an orthogonal matrix.\n"
   "\n"
   " -Pprefix ppp     = Specifies the output dataset to be used for\n"
   "                    the permutation transformation.\n"
   "                    ++ The output dataset is the -inset2 dataset\n"
   "                       re-ordered in time, again to make the result\n"
   "                       as correlated as possible with the -inset1\n"
   "                       dataset.\n"
   "\n"
   " -normalize       = Normalize the output dataset(s) so that each\n"
   "                    time series has sum-of-squares = 1.\n"
   "                    ++ This option is not usually needed in AFNI\n"
   "                       (e.g., 3dTcorrelate does not care).\n"
   "\n"
   " -mask mset       = Only operate on nonzero voxels in the mset dataset.\n"
   "                    ++ Voxels outside the mask will not be used in computing\n"
   "                       the transformation, but WILL be transformed for\n"
   "                       your application and/or edification later.\n"
   "                    ++ For FMRI purposes, a gray matter mask would make\n"
   "                       sense here, or at least a brain mask.\n"
   "                    ++ If no masking option is given, then all voxels\n"
   "                       will be processed in computing the transformation.\n"
   "                       This set will include all non-brain voxels (if any).\n"
   "                    ++ Any voxel which is all constant in time\n"
   "                       (in either input) will be removed from the mask.\n"
   "                    ++ This mask dataset must be on the same spatial grid\n"
   "                       as the other input datasets!\n"
   "\n"
   " -verb             = Print some progress reports and auxiliary information.\n"
   "                     ++ Use this option twice to get LOTS of progress\n"
   "                        reports; mostly useful for debugging.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* Is this program useful? Not even The Shadow knows!\n"
   "  (But do NOT call it BS.)\n"
   "\n"
   "* The output dataset is in floating point format.\n"
   "\n"
   "* Although the goal of 3dBrainSync is to make the transformed\n"
   "  -inset2 as correlated (voxel-by-voxel) as possible with -inset1,\n"
   "  it does not actually compute that correlation dataset. You can do\n"
   "  that computation with program 3dTcorrelate, as in\n"
   "    3dTcorrelate -polort -1 -prefix AB.pcor.nii \\\n"
   "                 dataset1 transformed-dataset2\n"
   "\n"
   "* Besides the transformed dataset(s), if the '-verb' option is used,\n"
   "  some other (text formatted) files are written out:\n"
   "   {Qprefix}.sval.1D = singular values from the BC' decomposition\n"
   "   {Qprefix}.qmat.1D = Q matrix\n"
   "   {Pprefix}.perm.1D = permutation indexes p(i)\n"
   "  You probably do not have any use for these files; they are mostly\n"
   "  present to diagnose any problems.\n"
   "\n"
   "--------\n"
   "METHODS:\n"
   "--------\n"
   "* Notation used in the explanations below:\n"
   "    M = Number of time points\n"
   "    N = Number of voxels > M (N = size of mask)\n"
   "    B = MxN matrix of time series from -inset1\n"
   "    C = MxN matrix of time series from -inset2\n"
   "        Both matrices will have each column normalized to\n"
   "        have sum-of-squares = 1 (L2 normalized) --\n"
   "        the program does this operation internally; you do not have\n"
   "        to ensure that the input datasets are so normalized)\n"
   "    Q = Desired orthgonal MxM matrix to transform C such that B-QC\n"
   "        is as small as possible (sum-of-squares = Frobenius norm)\n"
   "        normF(A) = sum_{ij} A_{ij}^2 = trace(AA') = trace(A'A).\n"
   "        NOTE: This norm is different from the matrix L2 norm.\n"
   "        NOTE: A' denotes the transpose of A.\n"
   "\n"
   "* The expansion below shows why the matrix BC' is crucial to the analysis:\n"
   "    normF(B-QC) = trace( [B-QC][B'-C'Q'] )\n"
   "                = trace(BB') + trace(QCC'Q') - trace(BC'Q') - trace(QCB')\n"
   "                = trace(BB') + trace(C'C) - 2 trace(BC'Q')\n"
   "  The second term collapses because trace(AA') = trace(A'A), so\n"
   "  trace([QC][QC]') = trace([QC]'[QC]) = trace(C'Q'QC) = trace(C'C)\n"
   "  because Q is orthogonal. So the first 2 terms in the expansion of\n"
   "  normF(B-QC) do not depend on Q at all. Thus, to minimize normF(B-QC),\n"
   "  we have to maximize trace(BC'Q') = trace([B][QC]') = trace([QC][B]').\n"
   "\n"
   "  Since the columns of B and C are the (normalized) time series,\n"
   "  each row represents the image at a particular time. So the (i,j)\n"
   "  element of BC' is the (spatial) dot product of the i-th TR image from\n"
   "  -inset1 with the j-th TR image from -inset2. Furthermore,\n"
   "  trace(BC') = trace(C'B) = sum of dot products (correlations)\n"
   "  of all time series. So maximizing trace(BC'Q') will maximize the\n"
   "  summed correlations of B (time series from -inset1) and QC\n"
   "  (transformed time series from -inset2).\n"
   "\n"
   "  Note again that the sum of correlations (dot products) of all the time\n"
   "  series is equal to the sum of dot products of all the spatial images.\n"
   "  So the algorithm to find the transformation Q is to maximize the sum of\n"
   "  dot products of spatial images from B with Q-transformed spatial images\n"
   "  from C -- since there are fewer time points than voxels, this is more\n"
   "  efficient and elegant than trying to maximize the sum over voxels of dot\n"
   "  products of time series.\n"
   "\n"
   "  If you use the '-verb' option, these summed correlations ('scores')\n"
   "  are printed to stderr during the analysis.\n"
   "\n"
   "* Joshi method [-Qprefix]:\n"
   "    (a) compute MxM matrix B C'\n"
   "    (b) compute SVD of B C' = U S V' (U, S, V are MxM matrices)\n"
   "    (c) Q = U V'\n"
   "        [note: if B=C, then U=V, so Q=I, as it should]\n"
   "    (d) transform each time series from -inset2 using Q\n"
   "  This matrix Q is the solution to the restricted least squares\n"
   "  problem (i.e., restricted to have Q be an orthogonal matrix).\n"
   "  NOTE: The sum of the singular values in S is equal to the sum\n"
   "        of the time series dot products (correlations) in B and QC,\n"
   "        when Q is calculated as above.\n"
   "\n"
   "  A pre-print of this method is available as:\n"
   "    AA Joshi, M Chong, RM Leahy.\n"
   "    BrainSync: An Orthogonal Transformation for Synchronization of fMRI\n"
   "    Data Across Subjects, Proc. MICCAI 2017\n"
   "  https://www.dropbox.com/s/tu4kuqqlg6r02kt/brainsync_miccai2017.pdf\n"
   "  https://www.google.com/search?q=joshi+brainsync\n"
   "  http://neuroimage.usc.edu/neuro/Resources/BrainSync\n"
   "\n"
   "* Permutation method [-Pprefix]:\n"
   "    (a) Compute B C' (as above)\n"
   "    (b) Find a permutation p(i) of the integers {0..M-1} such\n"
   "        that sum_i { (BC')[i,p(i)] } is as large as possible\n"
   "        (i.e., p() is used as a permutation of the COLUMNS of BC').\n"
   "        This permutation is equivalent to post-multiplying BC'\n"
   "        by an orthogonal matrix P representing the permutation;\n"
   "        such a P is full of 0s except for a single 1 in each row\n"
   "        and each column.\n"
   "    (c) Permute the ROWS (time direction) of the time series matrix\n"
   "        from -inset2 using p().\n"
   "  Only an approximate (greedy) algorithm is used to find this\n"
   "  permutation; that is, the best permutation is not guaranteed to be found\n"
   "  (just a 'good' permutation -- it is the best thing I could code quickly :).\n"
   "\n"
   "  Algorithm currently implemented (let D=BC' for notational simplicity):\n"
   "    1) Find the largest element D(i,j) in the matrix.\n"
   "       Then the permutation at row i is p(i)=j.\n"
   "       Strike row i and column j out of the matrix D.\n"
   "    2) Repeat, finding the largest element left, say at D(f,g).\n"
   "       Then p(f) = g. Strike row f and column g from the matrix.\n"
   "       Repeat until done.\n"
   "  (Choosing the largest possible element at each step is what makes this\n"
   "  method 'greedy'.) This permutation is not optimal but is pretty good,\n"
   "  and another step is used to improve it:\n"
   "    3) For all pairs (i,j), p(i) and p(j) are swapped and that permutation\n"
   "       is tested to see if the trace gets bigger.\n"
   "    4) This pair-wise swapping is repeated until it does not improve things\n"
   "       any more (typically, it improves the trace about 1-2%% -- not much).\n"
   "  The purpose of the pair swapping is to deal with situations where D looks\n"
   "  something like this: [  1 70 ]\n"
   "                       [ 70 99 ]\n"
   "  Step 1 would pick out 99, and Step 2 would pick out 1; that is,\n"
   "  p(2)=2 and then p(1)=1, for a total trace/score of 100. But swapping\n"
   "  1 and 2 would give a total trace/score of 140. In practice, extreme versions\n"
   "  of this situation do not seem common with real FMRI data, probably because\n"
   "  the subject's brain isn't actively conspiring against this algorithm :)\n"
   "\n"
   "  [Something called the 'Hungarian algorithm' can solve for the optimal]\n"
   "  [permutation exactly, but I've not had the inclination to program it.]\n"
   "\n"
   "  This whole permutation optimization procedure is very fast: about 1 second.\n"
   "  In the RS-FMRI data I've tried this on, the average time series correlation\n"
   "  resulting from this optimization is 50-65%% of that which comes from\n"
   "  optimizing over ALL orthogonal matrices (Joshi method). If you use '-verb',\n"
   "  the stderr output line that looks like this\n"
   "   + corr scores: original=-722.5 Q matrix=22366.0 permutation=12918.7 57.8%%\n"
   "  shows trace(BC') before any transforms, with the Q matrix transform,\n"
   "  and with the permutation transform. As explained above, trace(BC') is\n"
   "  the summed correlations of the time series (since the columns of B and C\n"
   "  are normalized prior to the optimizations); in this example, the ratio of\n"
   "  the average time series correlation between the permutation method and the\n"
   "  Joshi method is about 58%% (in a gray matter mask with 72221 voxels).\n"
   "\n"
   "* Results from the permutation method MUST be less correlated (on average)\n"
   "  with -inset1 than the Joshi method's results: the permutation can be\n"
   "  thought of as an orthogonal matrix containing only 1s and 0s, and the BEST\n"
   "  possible orthogonal matrix, from Joshi's method, has more general entries.\n"
   "  ++ However, the permutation method has an obvious interpretation\n"
   "     (re-ordering time points), while the general method linearly combines\n"
   "     different time points (perhaps far apart); the interpretation of this\n"
   "     combination in terms of synchronizing brain activity is harder to intuit\n"
   "     (at least for me).\n"
   "  ++ Another feature of a permutation-only transformation is that it cannot\n"
   "     change the sign of data, unlike a general orthgonal matrix; e.g.,\n"
   "       [ 0 -1]\n"
   "       [-1  0], which swaps 2 time points AND negates them, is a valid\n"
   "     orthogonal matrix. For rs-FMRI datasets, this consideration might not\n"
   "     be important, since correlations are generally positive, so don't often\n"
   "     need sign-flipping to make them so.\n"
   "\n"
   "* This program is NOT multi-threaded. Typically, I/O is a big part of\n"
   "  the run time (at least, for the cases I've tested). The '-verb' option\n"
   "  will give progress reports with elapsed-time stamps, making it easy to\n"
   "  see which parts of the program take the most time.\n"
   "\n"
   "* Author: RWCox, servant of the ChronoSynclastic Infundibulum - July 2017\n"
   "\n"
   "* Thanks go to Anand Joshi for his clear exposition of BrainSync at OHBM 2017,\n"
   "  and his encouragement about the development of this program.\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

#undef  A
#undef  QQ
#define A(i,j)  amat[(i)+(j)*m]
#define QQ(i,j) qmat[(i)+(j)*m]

#undef  BIGG
#define BIGG  1.111e37f

/* find the "best" permutation,
   in the sense of the largest trace of the row-permuted amat
   (or at least, an approximation of the "best" permutation :) */

static int * find_best_permutation( int m , float *qmat , double *amat )
{
   int nrand ;
   float *rvec,*qvec , rcost=0.0f, qcost=0.0f , bestcost, rval ;
   int *perm , *bestperm=NULL , ii,jj,rr,qq ;

   if( m < 2 || amat == NULL ) return NULL ; /* should not happen */

ENTRY("find_best_permutation") ;

   /* find score of the Q matrix [for comparison] */

   if( qmat != NULL ){
     for( qcost=0.0f,jj=0 ; jj < m ; jj++ ){
       for( ii=0 ; ii < m ; ii++ ) qcost += A(ii,jj)*QQ(ii,jj) ;
     }
     if( verb > 1 )
       ININFO_message("-- permutation search: qmat score = %g [%s]",qcost,TIMER) ;
   } else {
     if( verb > 1 )
       ININFO_message("-- permutation search starts [%s]",TIMER) ;
   }

   perm     = (int *)malloc(sizeof(int)*m) ;
   bestcost = -BIGG ; /* not very good */

   /* try a very simple greedy algorithm */
#undef  AA
#define AA(i,j) aamat[(i)+(j)*m]
   { int kk,pi,pj , mmm=m*m , *jdone ;
     float *aamat=(float *)malloc(sizeof(float)*m*m) , bbcost=0.0f ;
     for( kk=0 ; kk < mmm ; kk++ ) aamat[kk] = (float)amat[kk] ;
     bestperm = (int *)malloc(sizeof(int)*m) ;
     jdone    = (int *)malloc(sizeof(int)*m) ;
     for( ii=0 ; ii < m ; ii++ ){ bestperm[ii] = -1; jdone[ii] = 0; }
     for( rr=0 ; rr < m ; rr++ ){ /* pick biggest element left */
       rval = -BIGG ; pi = pj = -1 ;
       for( jj=0 ; jj < m ; jj++ ){
         if( jdone[jj] ) continue ; /* already did this column */
         for( ii=0 ; ii < m ; ii++ ){
           if( AA(ii,jj) > rval ){ rval = AA(ii,jj); pi=ii; pj=jj; }
         }
       }
       if( pi < 0 ) break ;  /* should not be possible */
       bestperm[pi] = pj ; jdone[pj] = 1 ;
       for( kk=0 ; kk < m ; kk++ ){  /* strike out this row+col */
         AA(pi,kk) = AA(kk,pj) = -BIGG;
       }
     }
     free(aamat); free(jdone);
     for( ii=0 ; ii < m && bestperm[ii] >= 0 ; ii++ ) ; /*nada*/
     if( ii < m ){
       free(bestperm); bestperm = NULL;  /* should never happen */
     } else {
       for( rcost=0.0f,ii=0 ; ii < m ; ii++ ) rcost += A(ii,bestperm[ii]) ;
       bestcost = bbcost = rcost ;
       if( verb > 1 )
         ININFO_message(" initial greedy perm score = %g",bbcost) ;
       if( verb > 2 ){
         fprintf(stderr,"  perm:") ;
         for( ii=0 ; ii < m ; ii++ )
           fprintf(stderr," A[%d,%d]=%g",ii,bestperm[ii],A(ii,bestperm[ii])) ;
         fprintf(stderr,"\n") ;
       }
     }

     if( bestperm != NULL ){  /* swap pairs, look for improvement */
       int ntry=0, ndone, ntot=0 ;
       do{
         for( ndone=ii=0 ; ii < m-1 ; ii++ ){
          for( jj=ii+1 ; jj < m ; jj++ ){
           memcpy(perm,bestperm,sizeof(int)*m) ;
           kk = perm[ii] ; perm[ii] = perm[jj] ; perm[jj] = kk ;
           for( rcost=0.0f,kk=0 ; kk < m ; kk++ ) rcost += A(kk,perm[kk]) ;
           if( rcost > bestcost ){
             free(bestperm); bestperm = perm; bestcost = rcost;
             perm = (int *)malloc(sizeof(int)*m); ndone++ ; ntot++ ;
             if( verb > 1 )
               ININFO_message(" improved perm score = %g swap [%d,%d]",rcost,ii,jj) ;
           }
         }}
       } while( ndone > 0 && ++ntry < 29 ) ; /* don't try forever! */
       if( verb > 1 ){
         if( bbcost > 0.0f ) bbcost = 100.0f*(bestcost-bbcost)/bbcost ;
         else                bbcost =   0.0f ;
         ININFO_message("-- finish after %d swaps: %.2f%% improvement",
                        ntot,bbcost) ;
       }
       if( verb > 2 ){
         fprintf(stderr,"  perm:") ;
         for( ii=0 ; ii < m ; ii++ )
           fprintf(stderr," A[%d,%d]=%g",ii,bestperm[ii],A(ii,bestperm[ii])) ;
         fprintf(stderr,"\n") ;
       }
     }
   }
#undef AA

   if( bestperm == NULL ){  /* should never happen: use old method */
     nrand = 9999*m ;               /* of random trials (not good) */
     rvec  = (float *)malloc(sizeof(float)*m) ;
     qvec  = (float *)malloc(sizeof(float)*m) ;
     for( rr=0 ; rr < nrand ; rr++ ){
       for( ii=0 ; ii < m ; ii++ ){
         perm[ii] = ii; rvec[ii] = 0.0f;
         qvec[ii] = (float)(drand48()+drand48()-1.0);
       }
       qsort_float(m,qvec) ;
       for( jj=0 ; jj < m ; jj++ ){
         rval = qvec[jj] ;
         for( ii=0 ; ii < m ; ii++ ) rvec[ii] += QQ(ii,jj)*rval ;
       }
       qsort_floatint(m,rvec,perm) ;

       for( rcost=0.0f,ii=0 ; ii < m ; ii++ ) rcost += A(ii,perm[ii]) ;
       if( rcost > bestcost ){
         if( bestperm != NULL ) free(bestperm) ;
         bestperm = perm; bestcost = rcost;
         perm = (int *)malloc(sizeof(int)*m);
         if( verb > 1 )
           ININFO_message(" new perm score = %g [%d]",rcost,rr) ;
       }
     }
     free(rvec); free(qvec);
   }

   free(perm);

   if( verb && qcost > 0.0f ){
     for( rcost=0.0f,ii=0 ; ii < m ; ii++ ) rcost += A(ii,ii) ;
     ININFO_message("corr scores: "
                    "original=%.1f Q matrix=%.1f permutation=%.1f %.1f%%",
                    rcost, qcost, bestcost , 100.0f*bestcost/qcost ) ;
   }
   RETURN(bestperm) ;
}

#undef A
#undef QQ

/*----------------------------------------------------------------------------*/
/* Given m X n matrices bmat and cmat, compute the orthogonal m X m matrix
   qmat that 'best' transforms cmat to bmat.
    1) normalize all columns of bmat and cmat
    2) compute m X m matrix amat = [bmat] * [cmat]'
    3) SVD that to get amat = [umat] * [sigma] * [vmat]'
    4) qmat = [umat] * [vmat]'
*//*--------------------------------------------------------------------------*/

static void compute_joshi_matrix( int m , int n ,
                                  float *bmat , float *cmat , float *qmat )
{
   int ii,jj,kk , kbot ;
   double *bmatn , *cmatn ;
   double *amat , *umat , *vmat , *sval ;
   register double bsum , csum ;

ENTRY("compute_joshi_matrix") ;

   /* temp matrices */

   bmatn = (double *)calloc( sizeof(double) , m*n ) ; /* normalized bmat */
   cmatn = (double *)calloc( sizeof(double) , m*n ) ; /* normalized cmat */
   amat  = (double *)calloc( sizeof(double) , m*m ) ; /* [bmatn] * [cmatn]' */

   /* macros for 2D array indexing */

#undef  B
#undef  C
#undef  A
#undef  U
#undef  V
#undef  BN
#undef  CN

#define B(i,j)  bmat[(i)+(j)*m]
#define C(i,j)  cmat[(i)+(j)*m]
#define BN(i,j) bmatn[(i)+(j)*m]
#define CN(i,j) cmatn[(i)+(j)*m]
#define A(i,j)  amat[(i)+(j)*m]
#define U(i,j)  umat[(i)+(j)*m]
#define V(i,j)  vmat[(i)+(j)*m]
#define QQ(i,j) qmat[(i)+(j)*m]

   /* copy input matrices into bmatn and cmatn, normalizing as we go */

   if( verb > 1 ) ININFO_message("Normalizing time series [%s]",TIMER) ;

   for( jj=0 ; jj < n ; jj++ ){
     bsum = csum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ){
       bsum += B(ii,jj)*B(ii,jj) ;
       csum += C(ii,jj)*C(ii,jj) ;
     }
     if( bsum > 0.0 ) bsum = 1.0 / sqrt(bsum) ;
     if( csum > 0.0 ) csum = 1.0 / sqrt(csum) ;
     for( ii=0 ; ii < m ; ii++ ){
       BN(ii,jj) = B(ii,jj)*bsum ;
       CN(ii,jj) = C(ii,jj)*csum ;
     }
   }
MEMORY_CHECK("a") ;

   /* form A matrix = BN * CN' */

   if( verb > 1 ) ININFO_message("forming BC' matrix [%s]",TIMER) ;

#if 0  /* old method for computing BC' */
   kbot = n%2 ; /* 1 if odd, 0 if even */
   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = (kbot) ? BN(ii,0)*CN(jj,0) : 0.0 ;
       for( kk=kbot ; kk < n ; kk+=2 ) /* unrolled by 2 */
         bsum += BN(ii,kk)*CN(jj,kk) + BN(ii,kk+1)*CN(jj,kk+1) ;
       A(ii,jj) = bsum ;
   }}
#else  /* new method (faster) */
   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) = 0.0 ;
   }
   for( kk=0 ; kk < n ; kk++ ){
     for( jj=0 ; jj < m ; jj++ ){
       bsum = CN(jj,kk) ;
       for( ii=0 ; ii < m ; ii++ ) A(ii,jj) += BN(ii,kk)*bsum ;
     }
   }
#endif
MEMORY_CHECK("b") ;

   free(bmatn) ; free(cmatn) ;
   umat = (double *)calloc( sizeof(double),m*m ) ; /* SVD outputs */
   vmat = (double *)calloc( sizeof(double),m*m ) ;
   sval = (double *)calloc( sizeof(double),m   ) ;

   /* compute SVD of scaled matrix */

   if( verb > 1 ) ININFO_message("SVD-ing BC' matrix %dx%d [%s]",m,m,TIMER) ;

   svd_double( m , m , amat , sval , umat , vmat ) ;

   bsum = 0.0 ;
   for( ii=0 ; ii < m ; ii++ ) bsum += sval[ii] ;
   if( verb > 1 )
     ININFO_message("first 5 singular values: "
                    " %.2f [%.2f%%] "
                    " %.2f [%.2f%%] "
                    " %.2f [%.2f%%] "
                    " %.2f [%.2f%%] "
                    " %.2f [%.2f%%] ... sum = %.1f" ,
                    sval[0], 100.0*sval[0]/bsum ,
                    sval[1], 100.0*sval[1]/bsum ,
                    sval[2], 100.0*sval[2]/bsum ,
                    sval[3], 100.0*sval[3]/bsum ,
                    sval[4], 100.0*sval[4]/bsum , bsum ) ;

   /* write the singular values to a file? [30 Jul 2017] */

   if( Qbase != NULL && verb ){
     char *fname ; MRI_IMAGE *qim ; float *qar ;
     fname = (char *)malloc(sizeof(char)*(strlen(Qbase)+32)) ;
     strcpy(fname,Qbase) ;
     strcat(fname,".sval.1D") ;
     qim = mri_new( m,1, MRI_float ) ; qar = MRI_FLOAT_PTR(qim) ;
     for( ii=0 ; ii < m ; ii++ ) qar[ii] = sval[ii] ;
     mri_write_1D( fname , qim ) ;
     ININFO_message("wrote singular values to %s",fname) ;
     mri_free(qim) ; free(fname) ;
   }

   /* compute QQ = output matrix = U V' */

   if( verb > 1 ) ININFO_message("computing Q matrix [%s]",TIMER) ;

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < m ; ii++ ){
       bsum = 0.0 ;
       for( kk=0 ; kk < m ; kk++ ) bsum += U(ii,kk)*V(jj,kk) ;
       QQ(ii,jj) = (float)bsum ;
   }}
MEMORY_CHECK("d") ;

   free(umat) ; free(vmat) ;

   /* also find permutation to approximate qmat */

   bperm = find_best_permutation( m , qmat , amat ) ;

   free(amat) ; free(sval) ;

   /* write the matrix to a file? */

   if( Qbase != NULL && verb ){
     char *fname ; MRI_IMAGE *qim ; float *qar ;
     fname = (char *)malloc(sizeof(char)*(strlen(Qbase)+32)) ;
     strcpy(fname,Qbase) ;
     strcat(fname,".qmat.1D") ;
     qim = mri_new_vol_empty( m,m,1, MRI_float ) ;
     mri_fix_data_pointer( qmat , qim ) ;
     mri_write_1D( fname , qim ) ;
     ININFO_message("wrote Q matrix to %s",fname) ;
     mri_clear_data_pointer(qim) ; mri_free(qim) ; free(fname) ;
   }

   if( Pbase != NULL && bperm != NULL && verb ){
     char *fname ; MRI_IMAGE *qim ; float *qar ;
     fname = (char *)malloc(sizeof(char)*(strlen(Pbase)+32)) ;
     strcpy(fname,Pbase) ;
     strcat(fname,".perm.1D") ;
     qim = mri_new(m,1,MRI_float) ; qar = MRI_FLOAT_PTR(qim) ;
     for( ii=0 ; ii < m ; ii++ ) qar[ii] = bperm[ii] ;
     mri_write_1D(fname,qim); mri_free(qim);
     ININFO_message("wrote permutation vector to %s",fname) ;
     free(fname) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static INLINE int is_vector_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

#ifdef DO_DESPIKE
/*------------------- 08 Oct 2010: functions for despiking ----------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}
#undef SORT2
#undef SWAP

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+8 >= num ) jj = num-9;     \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }

/*-------------------------------------------------------------------------*/
/*! Remove spikes from a time series, in a very simplistic way.
    Return value is the number of spikes that were squashed [RWCox].
*//*-----------------------------------------------------------------------*/

static int despike9( int num , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return 0 ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   free(zme) ; return nsp ;
}
#undef mead9
/*----------------------------------------------------------------------------*/
#endif /* DO_DESPIKE */

/*----------------------------------------------------------------------------*/
/* compute the output datasets */

void BSY_process_data(void)
{
   byte *vmask ;
   int nvmask , ii,jj,kk , mm , nvox , ncut=0 ;
   MRI_IMAGE *bim, *cim ;
   float *bar, *car, *bmat, *cmat, *qmat, *cvec ;
   register float csum ;

ENTRY("BSY_process_data") ;

   if( verb ) INFO_message("begin BrainSync [%s]",TIMER) ;

   /* build mask */

   nvox = DSET_NVOX(dsetB) ;
   mm   = DSET_NVALS(dsetB) ;

   if( maskset != NULL ){  /*** explicit mask ***/
     vmask = THD_makemask( maskset , 0 , 1.0f,0.0f ) ;
     DSET_unload(maskset) ;
     if( vmask == NULL )
       ERROR_exit("Can't make mask from -mask dataset '%s'",DSET_BRIKNAME(maskset)) ;
     nvmask = THD_countmask( nvox , vmask ) ;
     if( verb ) ININFO_message("%d voxels in the spatial mask",nvmask) ;
     if( nvmask == 0  )
       ERROR_exit("Mask from -mask dataset %s has 0 voxels",DSET_BRIKNAME(maskset)) ;
   } else {               /*** all voxels */
     vmask = (byte *)malloc(sizeof(byte)*(nvox+2)) ; nvmask = nvox ;
     for( jj=0 ; jj < nvox ; jj++ ) vmask[jj] = 1 ;
     if( verb )
       ININFO_message("no -mask option ==> processing all %d voxels in dataset",nvox);
   }
MEMORY_CHECK("P") ;

   /* find and eliminate any voxels with all-constant vectors */

   if( verb > 1 )
     ININFO_message("looking for all-constant time series [%s]",TIMER) ;
   bar = (float *)malloc(sizeof(float)*mm) ;
   for( kk=0 ; kk < nvox ; kk++ ){
     if( vmask[kk] ){
       THD_extract_array( kk , dsetB , 0 , bar ) ;
       if( is_vector_constant(mm,bar) ){ vmask[kk] = 0 ; ncut++ ; continue ; }
       THD_extract_array( kk , dsetC , 0 , bar ) ;
       if( is_vector_constant(mm,bar) ){ vmask[kk] = 0 ; ncut++ ; continue ; }
     }
   }
   free(bar) ;
   if( ncut > 0 ){
     nvmask = THD_countmask(nvox,vmask) ;
     if( verb )
       ININFO_message("removed %d voxel%s for being constant in time" ,
                      ncut , (ncut > 1) ? "s" : "\0" ) ;
   }
   if( nvmask <= 2*mm )
     ERROR_exit("%d is not enough voxels in mask to process :(",nvmask) ;

   /* load datasets into matrices */

   if( verb > 1 )
     ININFO_message("loading datasets into matrices [%s]",TIMER) ;

   bmat = (float *)calloc( sizeof(float) , mm*nvmask ) ;
   cmat = (float *)calloc( sizeof(float) , mm*nvox   ) ; /* extra big */
   qmat = (float *)calloc( sizeof(float) , mm*mm     ) ;

   for( ii=0 ; ii < mm ; ii++ ){
     bim = THD_extract_float_brick( ii , dsetB ) ; bar = MRI_FLOAT_PTR(bim) ;
     cim = THD_extract_float_brick( ii , dsetC ) ; car = MRI_FLOAT_PTR(cim) ;
     for( kk=jj=0 ; jj < nvox ; jj++ ){
       if( vmask[jj] ){
         bmat[ii+kk*mm] = bar[jj] ;
         cmat[ii+kk*mm] = car[jj] ; kk++ ;
       }
     }
     mri_free(bim) ; mri_free(cim) ;
   }
MEMORY_CHECK("Q") ;
   DSET_unload(dsetB) ;
MEMORY_CHECK("R") ;

#ifdef DO_DESPIKE  /*-----------------------------------------------------*/
   /* despike matrix columns */

   if( verb > 1 )
     ININFO_message("despiking time series [%s]",TIMER) ;

   for( ncut=kk=0 ; kk < nvmask ; kk++ ){
     ncut += despike9( mm , bmat+kk*mm ) ;
     ncut += despike9( mm , cmat+kk*mm ) ;
   }
   if( ncut > 0 )
     if( verb > 1 )
       ININFO_message("  squashed %d spike%s from data vectors [%s]",
                      ncut , (ncut==1)?"\0":"s" , TIMER ) ;
#endif /* DO_DESPIKE */  /*-----------------------------------------------*/

   /* compute transform matrix qmat and permutation bperm */

   compute_joshi_matrix( mm , nvmask , bmat , cmat , qmat ) ;
MEMORY_CHECK("S") ;

   free(bmat) ; /* not needed no more */

   if( Qprefix != NULL ){  /* compute ortset = Qprefix output dataset */

     /* reload input matrix C with ALL voxels, not just mask */
     if( nvmask < nvox ){
       if( verb > 1 )
         ININFO_message("reloading C matrix with all voxels [%s]",TIMER) ;
       for( ii=0 ; ii < mm ; ii++ ){
         cim = THD_extract_float_brick( ii , dsetC ) ; car = MRI_FLOAT_PTR(cim) ;
         for( jj=0 ; jj < nvox ; jj++ ) cmat[ii+jj*mm] = car[jj] ;
         mri_free(cim) ;
       }
     }

     if( verb > 1 )
       ININFO_message("Q transforming dataset [%s]",TIMER) ;

     cvec = (float *)calloc( sizeof(float) , mm ) ;

#undef  QQ
#undef  C
#define QQ(i,j) qmat[(i)+(j)*mm]
#define C(i,j)  cmat[(i)+(j)*mm]

     for( jj=0 ; jj < nvox ; jj++ ){
       for( kk=0 ; kk < mm && C(kk,jj)==0.0f ; kk++ ) ; /*nada*/
       if( kk == mm ) continue ;  /* skip all zero vector */
       for( ii=0 ; ii < mm ; ii++ ){
         for( csum=0.0f,kk=0 ; kk < mm ; kk++ ) csum += QQ(ii,kk) * C(kk,jj) ;
         cvec[ii] = csum ;
       }
       if( do_norm ){  /* -normalize column cvec */
         for( csum=0.0f,ii=0 ; ii < mm ; ii++ ) csum += cvec[ii]*cvec[ii] ;
         if( csum > 0.0f ){
           csum = 1.0f/sqrtf(csum) ;
           for( ii=0 ; ii < mm ; ii++ ) cvec[ii] *= csum ;
         }
       }
       for( ii=0 ; ii < mm ; ii++ ) C(ii,jj) = cvec[ii] ; /* overload matrix */
     }
MEMORY_CHECK("T") ;

     free(cvec) ; free(qmat) ;

     if( verb > 1 )
       ININFO_message("filling output dataset [%s]",TIMER) ;

     ortset = EDIT_empty_copy( dsetB ) ;
     EDIT_dset_items( ortset ,
                        ADN_prefix    , Qprefix ,
                        ADN_datum_all , MRI_float ,
                        ADN_brick_fac , NULL ,
                      ADN_none ) ;
     for( ii=0 ; ii < mm ; ii++ ){
       car = (float *)calloc(sizeof(float),nvox) ;
       for( jj=0 ; jj < nvox ; jj++ ) car[jj] = C(ii,jj) ;
       EDIT_substitute_brick( ortset , ii , MRI_float , car ) ;
     }
MEMORY_CHECK("U") ;

   } /* end of making ortset */

   /* do it again with the permutation */

   if( Pprefix != NULL && bperm != NULL ){
     if( verb > 1 )
       ININFO_message("reloading C matrix with all voxels [%s]",TIMER) ;
     DSET_load(dsetC) ;
     for( ii=0 ; ii < mm ; ii++ ){
       cim = THD_extract_float_brick( ii , dsetC ) ; car = MRI_FLOAT_PTR(cim) ;
       for( jj=0 ; jj < nvox ; jj++ ) cmat[ii+jj*mm] = car[jj] ;
       mri_free(cim) ;
     }
     cvec = (float *)calloc( sizeof(float) , mm ) ;
     if( verb > 1 )
       ININFO_message("permuting C matrix [%s]",TIMER) ;
     for( jj=0 ; jj < nvox ; jj++ ){
       for( kk=0 ; kk < mm && C(kk,jj)==0.0f ; kk++ ) ; /*nada*/
       if( kk == mm ) continue ;  /* skip all zero vector */
       for( ii=0 ; ii < mm ; ii++ ) cvec[ii] = C(bperm[ii],jj) ;
       if( do_norm ){  /* -normalize */
         for( csum=0.0f,ii=0 ; ii < mm ; ii++ ) csum += cvec[ii]*cvec[ii] ;
         if( csum > 0.0f ){
           csum = 1.0f/sqrtf(csum) ;
           for( ii=0 ; ii < mm ; ii++ ) cvec[ii] *= csum ;
         }
       }
       for( ii=0 ; ii < mm ; ii++ ) C(ii,jj) = cvec[ii] ;
     }
     free(cvec) ; free(bperm) ;
     if( verb > 1 )
       ININFO_message("filling permuted dataset [%s]",TIMER) ;
     permset = EDIT_empty_copy( dsetB ) ;
     EDIT_dset_items( permset ,
                        ADN_prefix    , Pprefix ,
                        ADN_datum_all , MRI_float ,
                        ADN_brick_fac , NULL ,
                      ADN_none ) ;
     for( ii=0 ; ii < mm ; ii++ ){
       car = (float *)calloc(sizeof(float),nvox) ;
       for( jj=0 ; jj < nvox ; jj++ ) car[jj] = C(ii,jj) ;
       EDIT_substitute_brick( permset , ii , MRI_float , car ) ;
     }
   }

   DSET_unload(dsetC) ; free(cmat) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , mm,nn , nbad=0 ;

   /*----------*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 )
     BSY_help_the_pitiful_user() ;

   /*----- bureaucracy -----*/

   mainENTRY("3dBrainSync"); machdep();
   AFNI_logger("3dBrainSync",argc,argv);
   PRINT_VERSION("3dBrainSync"); AUTHOR("Cox the ChronoSynclastic") ;
   ct = NI_clock_time() ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*----- scan options -----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-inset1") == 0 ){
       if( dsetB != NULL )
          ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
          ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;

       dsetB = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(dsetB,argv[iarg]);
       DSET_mallocize(dsetB) ; /* is faster than mmap */
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-inset2") == 0 ){
       if( dsetC != NULL )
          ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
          ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;

       dsetC = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(dsetC,argv[iarg]);
       DSET_mallocize(dsetC) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( maskset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg  >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       maskset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(maskset,argv[iarg]) ;
       DSET_mallocize(maskset) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-Qprefix") == 0 ){
       char *cpt ;
       if( ++iarg >= argc )
         ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       Qprefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(Qprefix) )
         ERROR_exit("-Qprefix '%s' is not acceptable :-(",Qprefix) ;
       Qbase = strdup(Qprefix) ;
       cpt = strstr(Qbase,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
       cpt = strstr(Qbase,".1D" ) ; if( cpt != NULL ) *cpt = '\0' ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-Pprefix") == 0 ){
       char *cpt ;
       if( ++iarg >= argc )
         ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       Pprefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(Pprefix) )
         ERROR_exit("-Pprefix '%s' is not acceptable :-(",Pprefix) ;
       Pbase = strdup(Pprefix) ;
       cpt = strstr(Pbase,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
       cpt = strstr(Pbase,".1D" ) ; if( cpt != NULL ) *cpt = '\0' ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncasecmp(argv[iarg],"-norm",5) == 0 ){
       do_norm = 1 ; iarg++ ; continue ;
     }

     /*--- error! ---*/

     ERROR_exit(
       "[3dBrainSync] Don't know what to do with option '%s' :(",argv[iarg]) ;

   } /* end of option scanning loop */

   /*----- error checking -----*/

   if( Qprefix == NULL && Pprefix == NULL ){
     ERROR_message("no -Qprefix or -Pprefix option? Nothing to compute!") ;
     nbad++ ;
   }

   if( Qprefix != NULL && Pprefix != NULL ){
     if( strcmp(Qprefix,Pprefix) == 0 ){
       ERROR_message("-Qprefix and -Pprefix cannot be the same!") ;
       nbad++ ;
     } else if( strcmp(Qbase,Pbase) == 0 ){
       ERROR_message("-Qprefix and -Pprefix are too similar!") ;
       nbad++ ;
     }
   }

   if( dsetB == NULL ){ ERROR_message("no -inset1 dataset?") ; nbad++ ; }
   if( dsetC == NULL ){ ERROR_message("no -inset2 dataset?") ; nbad++ ; }

   if( !EQUIV_GRIDXYZ(dsetB,dsetC) ){
     ERROR_message("-inset1 and -inset2 datasets are not on same 3D grid :(") ;
     nbad++ ;
   }

   mm = DSET_NVALS(dsetB) ;
   if( mm < 19 ){
     ERROR_message("-inset1 only has %d time points -- need at least 19 :(",mm);
     nbad++ ;
   }
   if( DSET_NVALS(dsetC) != mm ){
     ERROR_message(
           "-inset1 has %d time points; -inset2 has %d -- they should match :(",
           mm , DSET_NVALS(dsetC) ) ;
     nbad++ ;
   }

   if( maskset != NULL && !EQUIV_GRIDXYZ(dsetB,maskset) ){
     ERROR_message("-mask and -inset1 datsets are NOT on the same 3D grid :(") ;
     nbad++ ;
   }

   if( nbad > 0 ) ERROR_exit("Can't continue after above ERROR%s" ,
                             (nbad==1) ? "\0" : "s" ) ;

   /*----- Load datasets -----*/

   if( verb )
     INFO_message("start reading datasets [%s]",TIMER) ;
   DSET_load(dsetB) ; CHECK_LOAD_ERROR(dsetB) ;
   DSET_load(dsetC) ; CHECK_LOAD_ERROR(dsetC) ;
   if( maskset != NULL ){ DSET_load(maskset) ; CHECK_LOAD_ERROR(maskset) ; }

   /*----- process the data, get output datasets -----*/

   BSY_process_data() ;

   if( ortset == NULL && permset == NULL )
     ERROR_exit("Processing the data failed for some reason :(") ;

   if( verb > 1 )
     ININFO_message("Writing results [%s]" , TIMER ) ;

   if( ortset != NULL ){
     tross_Copy_History( dsetB , ortset ) ;
     tross_Make_History( "3dBrainSync" , argc,argv , ortset ) ;
     DSET_write(ortset) ;
     if( verb ) WROTE_DSET(ortset) ;
   }

   if( permset != NULL ){
     tross_Copy_History( dsetB , permset ) ;
     tross_Make_History( "3dBrainSync" , argc,argv , permset ) ;
     DSET_write(permset) ;
     if( verb ) WROTE_DSET(permset) ;
   }

   if( verb )
     INFO_message("=== 3dBrainSync clock time = %s" , TIMER ) ;

   exit(0) ;
}
