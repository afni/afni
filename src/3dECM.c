/*
afni/src/3dECM.c
*/

// Look for OpenMP macro
#ifdef USE_OMP
#include <omp.h>
#endif

// Include libraries
#include "mrilib.h"
#include <sys/mman.h>
#include <sys/types.h>

// Define constants
#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define ETA2     4

#define MAX_NUM_TAGS 32
#define MAX_TAG_LEN 256

#define SQRT_2 ((double) 1.4142135623 )

/* CC - variables for tracking memory usage stats */
static int MEM_PROF = 0;
static int MEM_STAT = 0;
static char mem_tags[MAX_NUM_TAGS][MAX_TAG_LEN];
static long mem_allocated[MAX_NUM_TAGS];
static long mem_freed[MAX_NUM_TAGS];
static long mem_num_tags = 0;
static long running_mem = 0;
static long peak_mem = 0;
static long total_mem = 0;

/* CC macro for updating mem stats */
#define INC_MEM_STATS( INC, TAG ) \
    { \
        if( MEM_PROF == 1 ) \
        { \
            int ndx = 0; \
            while( ndx < mem_num_tags ) \
            { \
                if( strncmp( mem_tags[ndx], TAG, MAX_TAG_LEN ) == 0 ) \
                { \
                    break; \
                } \
                ndx++; \
            } \
            if(( ndx >= mem_num_tags ) && (ndx < MAX_NUM_TAGS)) \
            { \
                /* adding a new tag */ \
                strncpy( mem_tags[ ndx ], TAG, (MAX_TAG_LEN-1) ); \
                mem_allocated[ ndx ] = 0; \
                mem_freed[ ndx ] = 0; \
                mem_num_tags++; \
            } \
            if( ndx < MAX_NUM_TAGS ) \
            { \
                mem_allocated[ ndx ] += (long)(INC); \
                if ((long)(INC) > 1024 ) WARNING_message("Incrementing memory for %s by %ldB\n", TAG, (INC)); \
            } \
            else WARNING_message("No room in mem profiler for %s\n", TAG ); \
        } \
        total_mem += (long)(INC); \
        running_mem += (long)(INC); \
        if (running_mem > peak_mem) peak_mem = running_mem; \
    }

#define DEC_MEM_STATS( DEC, TAG ) \
    { \
        if( MEM_PROF == 1 ) \
        { \
            int ndx = 0; \
            while( ndx < mem_num_tags ) \
            { \
                if( strncmp( mem_tags[ndx], TAG, MAX_TAG_LEN ) == 0 ) \
                { \
                    break; \
                } \
                else ndx++ ; \
            } \
            if(( ndx >= mem_num_tags ) && (ndx < MAX_NUM_TAGS)) \
            { \
                WARNING_message("Could not find tag %s in mem profiler\n", TAG ); \
            } \
            else \
            { \
                mem_freed[ ndx ] += (long)(DEC); \
                if ((long)(DEC) > 1024 ) INFO_message("Free %ldB of memory for %s\n", (DEC), TAG); \
            } \
        } \
        running_mem -= (long)(DEC); \
    }

#define PRINT_MEM_STATS( TAG ) \
        if ( MEM_STAT == 1 ) \
        { \
            INFO_message("\n======\n== Mem Stats (%s): Running %3.3fMB, Total %3.3fMB, Peak %3.3fMB\n", \
            TAG, \
            (double)(running_mem/(1024.0*1024.0)), \
            (double)(total_mem/(1024.0*1024.0)), \
            (double)(peak_mem/(1024.0*1024.0))); \
            if( MEM_PROF ==  1 ) \
            { \
                int ndx = 0; \
                INFO_message("== Memory Profile\n"); \
                for( ndx=0; ndx < mem_num_tags; ndx++ ) \
                { \
                    INFO_message("%s: %ld allocated %ld freed\n", mem_tags[ndx], \
                        mem_allocated[ndx], mem_freed[ndx] ); \
                } \
            } \
        }


/* freeing all of the allocated mem on an error can get a little messy. instead
   we can use this macro to check what has been allocated and kill it. this of 
   course requires strict discipline for initiazing all pointers to NULL and 
   resetting them to NULL when free'd. i should be able to handle that */
#define CHECK_AND_FREE_ALL_ALLOCATED_MEM \
{ \
    /* eliminate DSETS */ \
        if ( mset != NULL ) \
        { \
            DSET_unload(mset) ; \
            DSET_delete(mset) ; \
            mset = NULL ; \
        } \
\
        if ( xset != NULL ) \
        { \
            DSET_unload(xset) ; \
            DSET_delete(xset) ; \
            xset = NULL ; \
        } \
\
        if ( cset != NULL ) \
        { \
            DSET_unload(cset) ; \
            DSET_delete(cset) ; \
            cset = NULL ; \
        } \
\
     /* free the xvectim */ \
        if ( xvectim != NULL ) \
        { \
            VECTIM_destroy(xvectim) ; \
            xvectim = NULL ; \
        } \
\
    /* free allocated mems */ \
        if( mask != NULL ) \
        { \
            free(mask) ; \
            mask = NULL ; \
        } \
\
        if( eigen_vec != NULL ) \
        { \
            free(eigen_vec) ; \
            eigen_vec = NULL ; \
        } \
}

/* being good and cleaning up before erroring out can be a pain, and messy, lets
   make a variadic macro that does it for us. */
#define ERROR_EXIT_CC( ... ) \
    { \
        CHECK_AND_FREE_ALL_ALLOCATED_MEM; \
        ERROR_exit( __VA_ARGS__ ); \
    }

/*----------------------------------------------------------------------------*/
static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,",") ;
   nn++ ;
}

/*----------------------------------------------------------------*/
/**** Include these hesre for potential optimization for OpenMP ****/
/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified.
    And we know ahead of time that the time series have 0 mean
    and L2 norm 1.
*//*--------------------------------------------------------------*/

float zm_THD_pearson_corr( int n, float *x , float *y ) /* inputs are */
{                                                       /* zero mean  */
   register float xy ; register int ii ;                /* and norm=1 */
   if( n%2 == 0 ){
     xy = 0.0f ;
     for( ii=0 ; ii < n ; ii+=2 ) xy += x[ii]*y[ii] + x[ii+1]*y[ii+1] ;
   } else {
     xy = x[0]*y[0] ;
     for( ii=1 ; ii < n ; ii+=2 ) xy += x[ii]*y[ii] + x[ii+1]*y[ii+1] ;
   }
   return xy ;
}

double cc_pearson_corr( long n, float *x, float*y )
{
    int ii;
    double xy = (double)0.0;
    for(ii=0; ii<n; ii++)
    {
        xy+=x[ii]*y[ii];
    }
    return( (double)1.0/((double)(n-1))*xy );
}

double* calc_fecm_power(MRI_vectim *xvectim, double shift, double scale, double eps, long max_iter)
{
    /* CC - we need a few arrays for calculating the power method */
    double* v_prev = NULL;
    double* v_new = NULL;
    double* v_temp = NULL;
    double* xv_int = NULL;
    double  v_err = 0.0;
    long    power_it;
    float*  xsar=NULL;
    double  v_new_sum_sq = 0.0;
    double  v_new_norm = 0.0;
    double  v_prev_sum_sq = 0.0;
    double  v_prev_norm = 0.0;
    double  v_prev_sum = 0.0;
    int lout,lin,ithr,nthr,vstep,vii ;

    /* -- CC initialize memory for power iteration */
    /* -- v_new will hold the new vector as it is being calculated */
    v_new = (double*)calloc(xvectim->nvec,sizeof(double));

    if( v_new == NULL )
    {
        WARNING_message("Cannot allocate %d bytes for v_new",xvectim->nvec*sizeof(double));
        return( NULL );
    }

    /*-- CC update our memory stats to reflect v_new -- */
    INC_MEM_STATS(xvectim->nvec*sizeof(double), "v_new");
    PRINT_MEM_STATS( "v_new" );

    /* -- v_prev will hold the vector from the previous calculation */
    v_prev = (double*)calloc(xvectim->nvec,sizeof(double));

    if( v_prev == NULL )
    {
        if( v_new != NULL ) free(v_new);
        WARNING_message("Cannot allocate %d bytes for v_prev",xvectim->nvec*sizeof(double));
        return( NULL );
    }

    /*-- CC update our memory stats to reflect v_new -- */
    INC_MEM_STATS(xvectim->nvec*sizeof(double), "v_prev");
    PRINT_MEM_STATS( "v_prev" );

    /* -- xv_int is an intermediary of the A'v matrix 
          product */
    xv_int = (double*)calloc(xvectim->nvals,sizeof(double));

    if( xv_int == NULL )
    {
        if( v_new != NULL ) free(v_new);
        if( v_prev != NULL ) free(v_prev);
        WARNING_message("Cannot allocate %d bytes for xv_int",
            xvectim->nvec*sizeof(double));
        return( NULL );
    }

    /*-- CC update our memory stats to reflect v_new -- */
    INC_MEM_STATS(xvectim->nvals*sizeof(double), "xv_int");
    PRINT_MEM_STATS( "xv_int" );


    /*--- Initiatilize power method ---*/

    /*  set the initial vector to the first vector */
    for ( lout=0; lout<xvectim->nvec; lout++ )
    {
        v_prev[lout]=1.0 / sqrt((double)xvectim->nvec);
        v_prev_sum += v_prev[lout];
        v_prev_sum_sq += v_prev[lout] * v_prev[lout];
    }

    v_prev_norm = sqrt(v_prev_sum_sq);
    v_err = v_prev_norm;

    power_it = 0;

    while( (v_err > eps) && (power_it < max_iter))
    {

        ithr = 0 ; nthr = 1 ;

        /* -- reset the xv_int to zeros --*/
        bzero(xv_int,xvectim->nvals*sizeof(double));

        /* -- Calculate xv_int = X'v */
        for( lout=0 ; lout < xvectim->nvec ; lout++ )
        {  /*----- outer voxel loop -----*/

            /* get ref time series from this voxel */
            xsar = VECTIM_PTR(xvectim,lout) ;

            for( lin=0; lin<xvectim->nvals; lin++ )
            {
                xv_int[lin] += ((double)xsar[lin]*v_prev[lout]);
            }

        } /* for lout */

        /* now calculate X xv_int */
        for( lout=0 ; lout < xvectim->nvec ; lout++ )
        {  /*----- outer voxel loop -----*/

            /* get ref time series from this voxel */
            xsar = VECTIM_PTR(xvectim,lout) ;

            v_new[lout] = scale*shift*v_prev_sum ;
            
            for( lin=0; lin<xvectim->nvals; lin++ )
            {
                v_new[lout] +=  scale*xv_int[lin]*xsar[lin];
            }
        }

        /* calculate the error, norms, and sums for the next
           iteration */
        v_prev_sum = 0.0;
        v_new_norm = 0.0;
        v_new_sum_sq = 0.0;
        v_err = 0.0;

        /* calculate the norm of the new vector */
        for( lout=0 ; lout < xvectim->nvec ; lout++ )
        {  /*----- outer voxel loop -----*/
            v_new_sum_sq += v_new[lout]*v_new[lout];
        }
        v_new_norm = sqrt(v_new_sum_sq);

        /* normalize the new vector, calculate the 
           error between this vector and the previous,
           and get the sum */
        v_new_sum_sq = 0.0;
        for( lout=0; lout < xvectim->nvec; lout++ )
        {
            double vdiff = 0;
            v_new[lout] = v_new[lout] / v_new_norm;
            v_prev_sum += v_new[lout];
            v_new_sum_sq += v_new[lout]*v_new[lout];

            /* calcualte the differences */
            vdiff = v_prev[lout] - v_new[lout];
            v_err += (vdiff * vdiff);
        }

        v_err = sqrt(v_err) / v_prev_norm;
        v_prev_norm = sqrt(v_new_sum_sq);

        /* now set the new vec to the previous */
        v_temp = v_prev;
        v_prev = v_new;
        v_new = v_temp;

        /* increment iteration counter */
        power_it++;

        /* tell the user what has happened */
        INFO_message ("Finished iter %d: Verr %3.3f, Vnorm %3.3f\n",
            power_it, v_err, v_prev_norm);
    } 

    if ((v_err >= eps) && (power_it >= max_iter))
    {
        WARNING_message("Power iteration did not converge (%3.3f >= %3.3f)\n"
            "in %d iterations. You might consider increase max_iters, or\n"
            "epsilon. For now we are writing out the optained solution,\n"
            "which may or may not be good enough.\n",
            (v_err), (eps), (power_it));
    }

    /* the eigenvector that we are interested in should now be in v_prev,
       free all other power iteration temporary vectors */
    if( v_new != NULL ) 
    {
        free(v_new);
        v_new = NULL;

        /* update running memory statistics to reflect freeing the vectim */
        DEC_MEM_STATS(xvectim->nvec*sizeof(double), "v_new");
    }
   
    if( xv_int != NULL ) 
    {
        free(xv_int);
        xv_int = NULL;

        /* update running memory statistics to reflect freeing the vectim */
        DEC_MEM_STATS(xvectim->nvals*sizeof(double), "xv_int");
    }

    /* return the result */
    return(v_prev);
}

double* calc_full_power(MRI_vectim *xvectim, double thresh, double shift, double scale, double eps, long max_iter)
{
    /* CC - we need a few arrays for calculating the power method */
    double* v_prev = NULL;
    double* v_new = NULL;
    double* v_temp = NULL;
    double  v_err = 0.0;
    long    power_it;
    double  v_new_sum_sq = 0.0;
    double  v_new_norm = 0.0;
    double  v_prev_sum_sq = 0.0;
    double  v_prev_norm = 0.0;
    double  v_prev_sum = 0.0;
    long ii =0;

    /* -- CC initialize memory for power iteration */
    /* -- v_new will hold the new vector as it is being calculated */
    v_new = (double*)calloc(xvectim->nvec,sizeof(double));

    if( v_new == NULL )
    {
        WARNING_message("Cannot allocate %d bytes for v_new",xvectim->nvec*sizeof(double));
        return( NULL );
    }

    /*-- CC update our memory stats to reflect v_new -- */
    INC_MEM_STATS(xvectim->nvec*sizeof(double), "v_new");
    PRINT_MEM_STATS( "v_new" );

    /* -- v_prev will hold the vector from the previous calculation */
    v_prev = (double*)calloc(xvectim->nvec,sizeof(double));

    if( v_prev == NULL )
    {
        if( v_new != NULL ) free(v_new);
        WARNING_message("Cannot allocate %d bytes for v_prev",xvectim->nvec*sizeof(double));
        return( NULL );
    }

    /*-- CC update our memory stats to reflect v_new -- */
    INC_MEM_STATS(xvectim->nvec*sizeof(double), "v_prev");
    PRINT_MEM_STATS( "v_prev" );

    /*---------- loop over mask voxels, correlate ----------*/

    /*  set the initial vector to the first vector */
    for ( ii=0; ii<xvectim->nvec; ii++ )
    {
        v_prev[ii]=1.0 / sqrt((double)xvectim->nvec);
        v_prev_sum += v_prev[ii];
        v_prev_sum_sq += v_prev[ii] * v_prev[ii];
    }

    v_prev_norm = sqrt(v_prev_sum_sq);
    v_err = v_prev_norm;

    power_it = 0;

    while( (v_err > eps) && (power_it < max_iter))
    {

        /* zero out the new vector */
        bzero(v_new, xvectim->nvec*sizeof(double));

        AFNI_OMP_START ;
#pragma omp parallel if( xvectim->nvec > 999 )
        {

            int lout,lin,ithr,nthr,vstep,vii ;
            double car = 0.0; 
            float *xsar = NULL;
            float *ysar = NULL;

            /*-- get information about who we are --*/
#ifdef USE_OMP
            ithr = omp_get_thread_num() ;
            nthr = omp_get_num_threads() ;
            if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
#else
            ithr = 0 ; nthr = 1 ;
#endif


            vstep = (int)( xvectim->nvec / (nthr*50.0f) + 0.901f ); vii = 0;
            if((MEM_STAT==0) && (ithr==0)) fprintf(stderr,"Looping:");

#pragma omp for schedule(static, 1)
            for( lout=0 ; lout < xvectim->nvec ; lout++ )
            {  /*----- outer voxel loop -----*/

                if( ithr == 0 && vstep > 2 )
                { vii++; if( vii%vstep == vstep/2 && MEM_STAT == 0) vstep_print(); }

                /* get ref time series from this voxel */
                xsar = VECTIM_PTR(xvectim,lout) ;

                for( lin=(lout+1); lin<xvectim->nvec; lin++ )
                {
                    ysar = VECTIM_PTR(xvectim,lin) ;
                    car = (double)cc_pearson_corr(xvectim->nvals,xsar,ysar);

                    if( car >= thresh )
                    {
#pragma omp critical(dataupdate)
                        {
                            v_new[lout] += scale*(car+shift)*v_prev[lin];
                            v_new[lin] += scale*(car+shift)*v_prev[lout];
                        }
                    }
                }
            } /* for lout */
        } /* AFNI_OMP */
        AFNI_OMP_END;

        /* calculate the error, norms, and sums for the next
           iteration */
        v_prev_sum = 0.0;
        v_new_norm = 0.0;
        v_new_sum_sq = 0.0;
        v_err = 0.0;

        /* calculate the norm of the new vector */
        for( ii=0 ; ii < xvectim->nvec ; ii++ )
        {  /*----- outer voxel loop -----*/
            v_new_sum_sq += v_new[ii]*v_new[ii];
        }
        v_new_norm = sqrt(v_new_sum_sq);

        /* normalize the new vector, calculate the 
           error between this vector and the previous,
           and get the sum */
        v_new_sum_sq = 0.0;
        for( ii=0; ii < xvectim->nvec; ii++ )
        {
            double vdiff = 0;
            v_new[ii] = v_new[ii] / v_new_norm;
            v_new_sum_sq += v_new[ii]*v_new[ii];

            /* calcualte the differences */
            vdiff = v_prev[ii] - v_new[ii];
            v_err += (vdiff * vdiff);
        }

        v_err = sqrt(v_err) / v_prev_norm;
        v_prev_norm = sqrt(v_new_sum_sq);

        /* now set the new vec to the previous */
        v_temp = v_prev;
        v_prev = v_new;
        v_new = v_temp;

        /* increment iteration counter */
        power_it++;

        /* tell the user what has happened */
        INFO_message ("Finished iter %d: Verr %3.3f, Vnorm %3.3f\n",
            power_it, v_err, v_prev_norm);
    } 

    if ((v_err >= eps) && (power_it >= max_iter))
    {
        WARNING_message("Power iteration did not converge (%3.3f >= %3.3f)\n"
            "in %d iterations. You might consider increase max_iters, or\n"
            "epsilon. For now we are writing out the optained solution,\n"
            "which may or may not be good enough.\n",
            (v_err), (eps), (power_it));
    }

    /* the eigenvector that we are interested in should now be in v_prev,
       free all other power iteration temporary vectors */
    if( v_new != NULL ) 
    {
        free(v_new);
        v_new = NULL;

        /* update running memory statistics to reflect freeing the vectim */
        DEC_MEM_STATS(xvectim->nvec*sizeof(double), "v_new");
    }

    /* return the result */
    return(v_prev);
}

/* 3dECM was created from 3dAutoTCorrelate by
   R. Cameron Craddock */

/*----------------------------------------------------------------*/
/* General correlation calculation. */

#if 0
float my_THD_pearson_corr( int n, float *x , float *y )
{
   float xv,yv,xy , vv,ww , xm,ym ;
   register int ii ;

   xm = ym = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){ xm += x[ii] ; ym += y[ii] ; }
   xm /= n ; ym /= n ;
   xv = yv = xy = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     vv = x[ii]-xm ; ww = y[ii]-ym ; xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return xy/sqrtf(xv*yv) ;
}
#endif

/*----------------------------------------------------------------*/
/*! eta^2 (Cohen, NeuroImage 2008)              25 Jun 2010 [rickr]
 *
 *  eta^2 = 1 -  SUM[ (a_i - m_i)^2 + (b_i - m_i)^2 ]
 *               ------------------------------------
 *               SUM[ (a_i - M  )^2 + (b_i - M  )^2 ]
 *
 *  where  o  a_i and b_i are the vector elements
 *         o  m_i = (a_i + b_i)/2
 *         o  M = mean across both vectors
 -----------------------------------------------------------------*/

float my_THD_eta_squared( int n, float *x , float *y )
{
   float num , denom , gm , lm, vv, ww;
   register int ii ;

   gm = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){ gm += x[ii] + y[ii] ; }
   gm /= (2.0f*n) ;

   num = denom = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     lm = 0.5f * ( x[ii] + y[ii] ) ;
     vv = (x[ii]-lm); ww = (y[ii]-lm); num   += ( vv*vv + ww*ww );
     vv = (x[ii]-gm); ww = (y[ii]-gm); denom += ( vv*vv + ww*ww );
   }

   if( num < 0.0f || denom <= 0.0f || num >= denom ) return 0.0f ;
   return (1.0f - num/denom) ;
}

/*-----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
    THD_3dim_dataset *xset = NULL;
    THD_3dim_dataset *cset = NULL;
    THD_3dim_dataset *mset = NULL ;
    int nopt=1 , method=PEARSON , do_autoclip=0 ;
    int nvox , nvals , ii, polort=1 ;
    char *prefix = "ECM" ;
    byte *mask=NULL;
    int   nmask , abuc=1 ;
    char str[32] , *cpt = NULL;
    int *mask_ndx_to_vol_ndx = NULL;
    MRI_vectim *xvectim = NULL ;

    /* CC - flags that control options */
    static const char* method_strings [] = {"FECM","Full"};
    enum power_methods { FECM=0, FULL };
    enum power_methods pow_method = FECM;
    double  scale = 0.5;
    double  shift = 1.0;
    double  thresh = -1.2;

    /* CC - iteration stopping criteria */
    long max_iter = 1000;
    double eps = 0.1;

    /* CC - vector to hold the result */
    double* eigen_vec;

    /* CC - we will have one subbricks: eigenvector centrality is always weighted*/
    int nsubbriks = 1;
    int subbrik = 0;
    float * wodset;
 
   /*----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dECM [options] dset\n"
"  Computes voxelwise local functional connectivity density and\n"
"  stores the result in a new 3D bucket dataset as floats to\n"
"  preserve their values. ECM reflects the strength and\n"
"  extent of a voxel's global connectivity as well as the\n"
"  importance of the voxels tht it is directly connected to.\n\n"
"  Conceptually the process involves: \n"
"      1. Calculating the correlation between voxel time series for\n"
"         every pair of voxels in the brain (as determined by masking)\n"
"      2. Calculate the eigenvector corresponding to the largest\n"
"         eigenvalue of the similarity matrix.\n\n" 
"  Guaranteeing that this eigenvector is unique and all positive\n"
"  requires that the similarity matrix is strictly positive. This\n"
"  is enforced by either adding one to the correlations (Lohmann \n"
"  et. al. 2010), or by adding one and dividing by two (Wink et al.\n"
"  2012).\n\n" 
"  Practically the power iteration algorithm described in Wink et\n"
"  al. 2012) is used to optimize for computational time and memory\n"
"  usage.\n\n"
"  Lohmann G, Margulies DS, Horstmann A, Pleger B, Lepsien J, et al.\n"
"      (2010) Eigenvector Centrality Mapping for Analyzing\n"
"      Connectivity Patterns in fMRI Data of the Human Brain. PLoS\n"
"      ONE 5(4): e10232. doi: 10.1371/journal.pone.0010232\n\n"
"  Wink, A. M., de Munck, J. C., van der Werf, Y. D., van den Heuvel,\n"
"      O. A., & Barkhof, F. (2012). Fast Eigenvector Centrality\n"
"      Mapping of Voxel-Wise Connectivity in Functional Magnetic\n"
"      Resonance Imaging: Implementation, Validation, and\n"
"      Interpretation. Brain Connectivity, 2(5), 265–274.\n"
"      doi:10.1089/brain.2012.0087\n\n"
"\n"
"Options:\n"
"  -full       = uses the full power method (Lohmann et. al. 2010).\n"
"  -FECM       = uses a shortcut that substantially speeds up \n"
"                computation, but is less flexibile in what can be\n"
"                done the similarity matrix. i.e. does not allow \n"
"                thresholding correlation coefficients. based on \n" 
"                fast eigenvector centrality mapping (Wink et. al\n"
"                2012). [default]\n"
"  -thresh r   = exclude connections with correlation < r. only works\n"
"                for full power iteration\n"
"  -shift s    = value that should be added to correlation coeffs to\n"
"                enforce non-negativity, s >= 0 (default = 1.0).\n"
"  -scale x    = value that correlation coeffs should be multplied by\n"
"                after shifting, x >= 0 (default = 0.5) (e.g. Wink et\n"
"                al 2012).\n"
"  -eps p      = sets the stopping criterion for the power iteration\n"
"                l2|v_old - v_new| < eps*|v_old|. default = .1 (10%%)\n"
"  -max_iter i = sets the maximum number of iterations to use in\n"
"                in the power iteration. default = 1000\n"
"\n"
"  -polort m   = Remove polynomical trend of order 'm', for m=0..3.\n"
"                [default is m=1; removal is by least squares].\n"
"                Using m=0 means that just the mean is removed.\n"
"\n"
"  -autoclip   = Clip off low-intensity regions in the dataset,\n"
"  -automask   = so that the correlation is only computed between\n"
"                high-intensity (presumably brain) voxels.  The\n"
"                mask is determined the same way that 3dAutomask works.\n"
"\n"
"  -mask mmm   = Mask to define 'in-brain' voxels. Reducing the number\n"
"                the number of voxels included in the calculation will\n"
"                significantly speedup the calculation. Consider using\n"
"                a mask to constrain the calculations to the grey matter\n"
"                rather than the whole brain. This is also preferrable\n"
"                to using -autoclip or -automask.\n"
"\n"
"  -prefix p   = Save output into dataset with prefix 'p'\n"
"                [default prefix is 'ecm'].\n"
"\n"
"Notes:\n"
" * The output dataset is a bucket type of floats.\n"
" * The program prints out an estimate of its memory used\n"
"    when it ends.  It also prints out a progress 'meter'\n"
"    to keep you pacified.\n"
"\n"
"-- RWCox - 31 Jan 2002 and 16 Jul 2010\n"
"-- Cameron Craddock - 13 Nov 2015 \n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dECM",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dECM main"); machdep(); PRINT_VERSION("3dECM");
   AFNI_logger("3dECM",argc,argv);

   /* CC - set the default for ECM methods */
   pow_method = FECM;
   scale = 0.5;
   shift = 1.0;
   thresh = -1.2;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-time") == 0 ){
         abuc = 0 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         do_autoclip = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mask") == 0 ){
         /* mset is opened here, but not loaded? */
         mset = THD_open_dataset(argv[++nopt]);
         CHECK_OPEN_ERROR(mset,argv[nopt]);
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-full") == 0 ){
         pow_method = FULL; 
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-FECM") == 0 ){
         pow_method = FECM;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-max_iter") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 1 ){
            ERROR_EXIT_CC("Illegal value after -max_iter!") ;
         }
         max_iter = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-eps") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0 || val > 1 ){
            ERROR_EXIT_CC("Illegal value after -eps!") ;
         }
         eps = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-thresh") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1.1 || val > 1 ){
            ERROR_EXIT_CC("Illegal value after -thresh!") ;
         }
         thresh = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-scale") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0 ){
            ERROR_EXIT_CC("Illegal value after -scale!") ;
         }
         scale = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-shift") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0 ){
            ERROR_EXIT_CC("Illegal value after -shift!") ;
         }
         shift = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = strdup(argv[++nopt]) ;
         if( !THD_filename_ok(prefix) ){
            ERROR_EXIT_CC("Illegal value after -prefix!") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0 || val > 3 ){
            ERROR_EXIT_CC("Illegal value after -polort!") ;
         }
         polort = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mem_stat") == 0 ){
         MEM_STAT = 1 ; nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-mem_profile",8) == 0 ){
         MEM_PROF = 1 ; nopt++ ; continue ;
      }

      ERROR_EXIT_CC("Illegal option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

    if( nopt >= argc ) ERROR_EXIT_CC("Need a dataset on command line!?") ;
    xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);


   if( DSET_NVALS(xset) < 3 )
     ERROR_EXIT_CC("Input dataset %s does not have 3 or more sub-bricks!",
        argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/
   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;
   INC_MEM_STATS((nvox * nvals * sizeof(double)), "input dset");
   PRINT_MEM_STATS("inset");

   /* if a mask was specified make sure it is appropriate */
   if( mset ){

      if( DSET_NVOX(mset) != nvox )
         ERROR_EXIT_CC("Input and mask dataset differ in number of voxels!") ;
      mask  = THD_makemask(mset, 0, 1.0, 0.0) ;

      /* update running memory statistics to reflect loading the image */
      INC_MEM_STATS( mset->dblk->total_bytes, "mask dset" );
      PRINT_MEM_STATS( "mset load" );

      /* iupdate statistics to reflect creating mask array */
      nmask = THD_countmask( nvox , mask ) ;
      INC_MEM_STATS( nmask * sizeof(byte), "mask array" );
      PRINT_MEM_STATS( "mask" );

      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_EXIT_CC("Only %d voxels in -mask, exiting...",nmask);

      /* update running memory statistics to reflect loading the image */
      DEC_MEM_STATS( mset->dblk->total_bytes, "mask dset" );
      PRINT_MEM_STATS( "mset unload" );

      /* free all memory associated with the mask datast */
      DSET_unload(mset) ;
      DSET_delete(mset) ;
      mset = NULL ;
   } 
   /* if automasking is requested, handle that now */
   else if( do_autoclip ){
      mask  = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mask ) ;
      INFO_message("%d voxels survive -autoclip",nmask) ;
      if( nmask < 2 ) ERROR_EXIT_CC("Only %d voxels in -automask!",nmask);
   }
   /* otherwise we use all of the voxels in the image */
   else {
      nmask = nvox ;
      INFO_message("computing for all %d voxels",nmask) ;
   }
   
    /*-- create vectim from input dataset --*/
    INFO_message("vectim-izing input dataset") ;

    /*-- CC added in mask to reduce the size of xvectim -- */
    xvectim = THD_dset_to_vectim( xset , mask , 0 ) ;
    if( xvectim == NULL ) ERROR_EXIT_CC("Can't create vectim?!") ;

    /*-- CC update our memory stats to reflect vectim -- */
    INC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                    ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                    sizeof(MRI_vectim), "vectim");
    PRINT_MEM_STATS( "vectim" );

   /*--- CC the vectim contains a mapping between voxel index and mask index, 
         tap into that here to avoid duplicating memory usage, also create a
         mapping that goes the other way ---*/

    if( mask != NULL )
    {
        /* tap into the xvectim mapping */
        mask_ndx_to_vol_ndx = xvectim->ivec;

        /* --- CC free the mask */
        DEC_MEM_STATS( nmask*sizeof(byte), "mask array" );
        free(mask); mask=NULL;
        PRINT_MEM_STATS( "mask unload" );
    }

    /* -- CC unloading the dataset to reduce memory usage ?? -- */
    DEC_MEM_STATS((DSET_NVOX(xset) * DSET_NVALS(xset) * sizeof(double)), 
        "input dset");
    DSET_unload(xset) ;
    PRINT_MEM_STATS("inset unload");

    /* -- CC perform detrending -- */
    if( polort >= 0 )
    {
        INFO_message( "Detrending with polort = %d\n", polort );
        for( ii=0 ; ii < xvectim->nvec ; ii++ )
        {  
            /* remove polynomial trend */
            DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
        }
    }

    /* -- CC normalize input data to zero mean and unit variance
          this procedure does not change time series that 
          have zero variance -- */
    THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

    if (( pow_method == FECM ) && ( thresh > -1.2 ))
    {
        WARNING_message( "Cannot use a thresh with FECM, changing to full power iteration\n");
        pow_method = FULL;
    }

    /* update the user so that they know what we are up to */
    /* -- CC tell the user what we are up to */
    INFO_message( "Calculating ECM with %s method (thresh=%3.3f,\n"
        "  scale=%3.3f, shift=%3.3f, max_iter=%d, eps=%3.3f)\n",
        method_strings[pow_method], thresh, scale, shift, max_iter, eps );

    /* calculate the eigenvector */
    if ( pow_method == FECM )
    {
        eigen_vec=calc_fecm_power(xvectim, shift, scale, eps, max_iter);
    }
    else
    {
        eigen_vec=calc_full_power(xvectim, thresh, shift, scale, eps, max_iter);
    }
 
    /*-- create output dataset --*/
    cset = EDIT_empty_copy( xset ) ;

    /*-- configure the output dataset */
    if( abuc )
    {
        printf( "creating output bucket(%s)\n", prefix );
        EDIT_dset_items( cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /*  subbricks */
            ADN_ntt       , 0              , /* no time axis */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_BUCK_TYPE ,
            ADN_datum_all , MRI_float     ,
            ADN_none ) ;
    } 
    else 
    {
        EDIT_dset_items( cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /*  subbricks */
            ADN_ntt       , nsubbriks      ,  /* num times */
            ADN_ttdel     , 1.0            ,  /* fake TR */
            ADN_nsl       , 0              ,  /* no slice offsets */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_EPI_TYPE  ,
            ADN_datum_all , MRI_float      ,
            ADN_none ) ;
    }

    /* add history information to the hearder */
    tross_Make_History( "3dECM" , argc,argv , cset ) ;

    ININFO_message("creating output dataset in memory") ;

    /* -- Configure the subbriks: Eigenvector centrality is only weighted */
    subbrik = 0;
    EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
    /* CC this sets the subbrik scaling factor, which we will probably want
       to do again after we calculate the voxel values */
    EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */

    sprintf(str,"Weighted ECM") ;

    EDIT_BRICK_LABEL(cset,subbrik,str) ;
    EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

    /* copy measure data into the subbrik */
    wodset = DSET_ARRAY(cset,subbrik);

    /* increment memory stats */
    INC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)),
        "output dset");
    PRINT_MEM_STATS( "outset" );    

    /* set all of the voxels in the output image to zero */
    bzero(wodset, DSET_NVOX(cset)*sizeof(float));

    /* output the eigenvector, scaling it by sqrt(2) */
    for (ii = 0; ii < xvectim->nvec; ii++ )
    {
       wodset[ mask_ndx_to_vol_ndx[ ii ] ] = (float)(SQRT_2 * eigen_vec[ ii ]);
    }

    /* we have copied out v_prev, now we can kill it */
    if( eigen_vec != NULL ) 
    {
        free(eigen_vec);
        eigen_vec = NULL;

        /* update running memory statistics to reflect freeing the vectim */
        DEC_MEM_STATS(xvectim->nvec*sizeof(double), "eigen_vec");
    }
   

    /*-- tell the user what we are about to do --*/
    INFO_message("Done..\n") ;

    /* update running memory statistics to reflect freeing the vectim */
    DEC_MEM_STATS(((xvectim->nvec*sizeof(int)) +
                       ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                       sizeof(MRI_vectim)), "vectim");

    /* toss some trash */
    VECTIM_destroy(xvectim) ;
    DSET_delete(xset) ;

    PRINT_MEM_STATS( "Vectim Unload" );

    /* finito */
    INFO_message("Writing output dataset to disk [%s bytes]",
                commaized_integer_string(cset->dblk->total_bytes)) ;

    /* write the dataset */
    DSET_write(cset) ;
    WROTE_DSET(cset) ;

    /* increment our memory stats, since we are relying on the header for this
       information, we update the stats before actually freeing the memory */
    DEC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)), "output dset");

    /* free up the output dataset memory */
    DSET_unload(cset) ;
    DSET_delete(cset) ;

    PRINT_MEM_STATS( "Unload Output Dset" );

    /* force a print */
    MEM_STAT = 1;
    PRINT_MEM_STATS( "Fin" );

    exit(0) ;
}
