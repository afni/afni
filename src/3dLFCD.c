/*
afni/src/3dLFCD.c
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

static char mem_tags[MAX_NUM_TAGS][MAX_TAG_LEN];
static long mem_allocated[MAX_NUM_TAGS];
static long mem_freed[MAX_NUM_TAGS];
static long mem_num_tags = 0;
static long running_mem = 0;
static long peak_mem = 0;
static long total_mem = 0;
static int MEM_PROF = 0;
static int MEM_STAT = 0;

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


/* CC define nodes for histogram data structure
   that we will use for sparsity thresholding */
// Alias _hist_node declaration type as "hist_node" type via typedef
typedef struct _list_node list_node;

// Define list node structure
struct _list_node
{
    long idx;
    long ix;
    long jy;
    long kz;

    list_node* next;
};


/* free the list of hist_nodes */
list_node* free_list( list_node* list )
{
    list_node *pptr;
    list_node *hptr = list;

    while(hptr != NULL )
    {
        /* increment node pointers */
        pptr = hptr;
        hptr = hptr->next;

        /* delete the node */
        if(pptr != NULL)
        { 
            /* -- update running memory estimate to reflect memory allocation */ 
            DEC_MEM_STATS(( sizeof(list_node)), "list nodes");
            free(pptr);
            pptr=NULL;
        }
    }

    return(NULL);
}

/* 3dLFCD was created from 3dAutoTCorrelate by
   R. Cameron Craddock */

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

/*----------------------------------------------------------------------------*/
static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,",") ;
   nn++ ;
}

/*-----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset, *mset=NULL ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii, jj, kout, kin, polort=1 ;
   int ix1,jy1,kz1, ix2, jy2, kz2 ;
   char *prefix = "degree_centrality" ;
   byte *mask=NULL;
   int   nmask , abuc=1 ;
   int   all_source=0;        /* output all source voxels  25 Jun 2010 [rickr] */
   char str[32] , *cpt ;
   int *imap = NULL ; MRI_vectim *xvectim ;
   float (*corfun)(int,float *,float*) = NULL ;
   /* djc - add 1d file output for similarity matrix */
   FILE *fout1D=NULL;

   /* CC - we will have two subbricks: binary and weighted centrality */
   int nsubbriks = 2;
   int subbrik = 0;
   float * bodset;
   float * wodset;

   int nb_ctr = 0;

   /* CC - added flags for thresholding correlations */
   double thresh = 0.0;
   double othresh = 0.0;
   int dothresh = 0;
   double sparsity = 0.0;
   int dosparsity = 0;
  
   /* variables for calculating degree centrality */
   long * binaryDC = NULL;
   double * weightedDC = NULL;

   /* variables for histogram */
   int bottom_node_idx = 0;
   int totNumCor = 0;
   long totPosCor = 0;
   int ngoal = 0;
   int nretain = 0;
   float binwidth = 0.0;
   int nhistnodes = 50;

   /*----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dLFCD [options] dset\n"
"  Computes voxelwise local functional connectivity density and\n"
"  stores the result in a new 3D bucket dataset as floats to\n"
"  preserve their values. LFCD reflects the strength and\n"
"  extent of the correlation of a voxel with other voxels in\n"
"  its locally connected cluster.\n\n"
"  Conceptually the process involves: \n"
"      1. Calculating the correlation between voxel time series for\n"
"         every pair of voxels in the brain (as determined by masking)\n"
"      2. Applying a threshold to the resulting correlations to exclude\n"
"         those that might have arisen by chance, or to sparsify the\n"
"         connectivity graph.\n"
"      3. Find the cluster of above-threshold voxels that are spatially\n"
"         adjacent to the target voxel."
"      4. Count the number of voxels in the local cluster."
"   Practically the algorithm is ordered differently to optimize for\n"
"   computational time and memory usage.\n\n"
"\n"
"Options:\n"
"  -pearson  = Correlation is the normal Pearson (product moment)\n"
"               correlation coefficient [default].\n"
   #if 0
"  -spearman = Correlation is the Spearman (rank) correlation\n"
"               coefficient.\n"
"  -quadrant = Correlation is the quadrant correlation coefficient.\n"
   #else
"  -spearman AND -quadrant are disabled at this time :-(\n"
   #endif
"\n"
"  -thresh r = exclude correlations <= r from calculations\n"
"\n"
"  -polort m = Remove polynomical trend of order 'm', for m=-1..3.\n"
"               [default is m=1; removal is by least squares].\n"
"               Using m=-1 means no detrending; this is only useful\n"
"               for data/information that has been pre-processed.\n"
"\n"
"  -autoclip = Clip off low-intensity regions in the dataset,\n"
"  -automask =  so that the correlation is only computed between\n"
"               high-intensity (presumably brain) voxels.  The\n"
"               mask is determined the same way that 3dAutomask works.\n"
"\n"
"  -mask mmm = Mask to define 'in-brain' voxels. Reducing the number\n"
"               the number of voxels included in the calculation will\n"
"               significantly speedup the calculation. Consider using\n"
"               a mask to constrain the calculations to the grey matter\n"
"               rather than the whole brain. This is also preferrable\n"
"               to using -autoclip or -automask.\n"
"\n"
"  -prefix p = Save output into dataset with prefix 'p', this file will\n"
"               contain bricks for both 'weighted' or 'degree' centrality\n"
"               [default prefix is 'deg_centrality'].\n"
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
      PRINT_AFNI_OMP_USAGE("3dLFCD",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dLFCD main"); machdep(); PRINT_VERSION("3dLFCD");
   AFNI_logger("3dLFCD",argc,argv);

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
         mset = THD_open_dataset(argv[++nopt]);
         CHECK_OPEN_ERROR(mset,argv[nopt]);
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-pearson") == 0 ){
         method = PEARSON ; nopt++ ; continue ;
      }

#if 0
      if( strcmp(argv[nopt],"-spearman") == 0 ){
         method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-quadrant") == 0 ){
         method = QUADRANT ; nopt++ ; continue ;
      }
#endif

      if( strcmp(argv[nopt],"-eta2") == 0 ){
         method = ETA2 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = strdup(argv[++nopt]) ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("Illegal value after -prefix!") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-thresh") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val >= 1.0 || val < 0.0 ){
            ERROR_exit("Illegal value (%f) after -thresh!", val) ;
         }
         dothresh = 1;
         thresh = val ; othresh = val ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-polort") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 3 ){
            ERROR_exit("Illegal value after -polort!") ;
         }
         polort = val ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-mem_stat") == 0 ){
         MEM_STAT = 1 ; nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-mem_profile",8) == 0 ){
         MEM_PROF = 1 ; nopt++ ; continue ;
      }

      ERROR_exit("Illegal option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

   if( nopt >= argc ) ERROR_exit("Need a dataset on command line!?") ;

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);


   if( DSET_NVALS(xset) < 3 )
     ERROR_exit("Input dataset %s does not have 3 or more sub-bricks!",
        argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/
   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;
   INC_MEM_STATS((nvox * nvals * sizeof(double)), "input dset");
   PRINT_MEM_STATS("inset");

   /* if a mask was specified make sure it is appropriate */
   if( mset ){

      if( DSET_NVOX(mset) != nvox )
         ERROR_exit("Input and mask dataset differ in number of voxels!") ;
      mask  = THD_makemask(mset, 0, 1.0, 0.0) ;

      /* update running memory statistics to reflect loading the image */
      INC_MEM_STATS( mset->dblk->total_bytes, "mask dset" );
      PRINT_MEM_STATS( "mset load" );

      nmask = THD_countmask( nvox , mask ) ;
      INC_MEM_STATS( nmask * sizeof(byte), "mask array" );
      PRINT_MEM_STATS( "mask" );

      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -mask, exiting...",nmask);

      /* update running memory statistics to reflect loading the image */
      DEC_MEM_STATS( mset->dblk->total_bytes, "mask dset" );
      DSET_unload(mset) ;
      PRINT_MEM_STATS( "mset unload" );
   } 
   /* if automasking is requested, handle that now */
   else if( do_autoclip ){
      mask  = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mask ) ;
      INFO_message("%d voxels survive -autoclip",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -automask!",nmask);
   }
   /* otherwise we use all of the voxels in the image */
   else {
      nmask = nvox ;
      INFO_message("computing for all %d voxels",nmask) ;
   }
   
   if( method == ETA2 && polort >= 0 )
      WARNING_message("Polort for -eta2 should probably be -1...");

   /**  For the case of Pearson correlation, we make sure the  **/
   /**  data time series have their mean removed (polort >= 0) **/
   /**  and are normalized, so that correlation = dot product, **/
   /**  and we can use function zm_THD_pearson_corr for speed. **/

    switch( method ){
        default:
        case PEARSON: corfun = zm_THD_pearson_corr ; break ;
        case ETA2:    corfun = my_THD_eta_squared  ; break ;
   }

    /*-- create vectim from input dataset --*/
    INFO_message("vectim-izing input dataset") ;

    /*-- CC added in mask to reduce the size of xvectim -- */
    xvectim = THD_dset_to_vectim( xset , mask , 0 ) ;
    if( xvectim == NULL ) ERROR_exit("Can't create vectim?!") ;

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
        imap = xvectim->ivec;

        /* create a mapping that goes the opposite way */
        if(( ndx_mask = calloc( nvox * sizeof(long) )) == NULL)
        {
            /* CC - free allocated memory, maybe make this a macro? */
            if(mask != NULL ) free(mask);
            ERROR_message( 
                "Could not allocate %d byte array for mask index mapping\n",
                nmask*sizeof(long));
        }

        /* --- CC account for the size of the mask */
        INC_MEM_STATS( nvox*sizeof(long), "ndx mask array" );

        /* --- create the reverse mapping */
        for ( ii = 0; ii < xvectim->nvec; ii++)
        {
            ndx_mask[ imap[ ii ] ] = ii;
        }

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

    /* -- CC configure detrending --*/
    if( polort < 0 && method == PEARSON )
    {
        polort = 0; 
        WARNING_message("Pearson correlation always uses polort >= 0");
    }
    if( polort >= 0 )
    {
        for( ii=0 ; ii < xvectim->nvec ; ii++ )
        {  
            /* remove polynomial trend */
            DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
        }
    }


    /* -- this procedure does not change time series that 
          have zero variance -- */
    if( method == PEARSON ) THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

    /*-- create output dataset --*/
    cset = EDIT_empty_copy( xset ) ;

    /*-- configure the output dataset */
    if( abuc )
    {
        EDIT_dset_items( 
            cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /* 2 subbricks */
            ADN_ntt       , 0              , /* no time axis */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_BUCK_TYPE ,
            ADN_datum_all , MRI_float      ,
            ADN_none ) ;
    } 
    else 
    {
        EDIT_dset_items( cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /* 2 subbricks */
            ADN_ntt       , nsubbriks      ,  /* num times */
            ADN_ttdel     , 1.0            ,  /* fake TR */
            ADN_nsl       , 0              ,  /* no slice offsets */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_EPI_TYPE  ,
            ADN_datum_all , MRI_float      ,
            ADN_none ) ;
    }

    /* add history information to the hearder */
    tross_Make_History( "3dLFCD" , argc,argv , cset ) ;

    ININFO_message("creating output dataset in memory") ;

    /* -- Configure the subbriks: Binary Degree Centrality */
    subbrik = 0;
    EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
    /* CC this sets the subbrik scaling factor, which we will probably want
      to do again after we calculate the voxel values */
    EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */

    sprintf(str,"Binary LFCD") ;

    EDIT_BRICK_LABEL(cset,subbrik,str) ;
    EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

    /* copy measure data into the subbrik */
    bodset = DSET_ARRAY(cset,subbrik);
 
    /* -- Configure the subbriks: Weighted Degree Centrality */
    subbrik = 1;
    EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
    /* CC this sets the subbrik scaling factor, which we will probably want
       to do again after we calculate the voxel values */
    EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */

    sprintf(str,"Weighted LFCD") ;

    EDIT_BRICK_LABEL(cset,subbrik,str) ;
    EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

    /* copy measure data into the subbrik */
    wodset = DSET_ARRAY(cset,subbrik);

    /* increment memory stats */
    INC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)),
        "output dset");
    PRINT_MEM_STATS( "outset" );    

    /*-- tell the user what we are about to do --*/
    INFO_message( "Calculating LFCD with threshold = %f.\n", thresh);

    /*---------- loop over mask voxels, correlate ----------*/
    AFNI_OMP_START ;
#pragma omp parallel if( nmask > 999 )
    {
        int lii,ljj,lin,lout,ithr,nthr,vstep,vii ;
        float *xsar , *ysar ;
        list_node* current_node = NULL ;
        list_node* new_node = NULL ;
        list_node* recycled_nodes = NULL ;
        list_node* boundary_list = NULL ;
        long* seen_voxels;
        double car = 0.0 ; 

        /*-- get information about who we are --*/
#ifdef USE_OMP
        ithr = omp_get_thread_num() ;
        nthr = omp_get_num_threads() ;
        if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
#else
        ithr = 0 ; nthr = 1 ;
#endif

        /*-- For the progress tracker, we want to print out 50 numbers,
            figure out a number of loop iterations that will make this easy */
        vstep = (int)( nmask / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
        if((MEM_STAT==0) && (ithr == 0 )) fprintf(stderr,"Looping:") ;

        /* allocate a vector to track previously seen voxels */
        if((seen_voxels = (long*)calloc(xvectim->nvec*sizeof(long)))==NULL)
        {
            ERROR_message( "Could not allocate memory for seen_voxels!\n");
        }

#pragma omp for schedule(static, 1)
 
        for( lout=0 ; lout < xvectim->nvec ; lout++ )
        {  /*----- outer voxel loop -----*/

            if( ithr == 0 && vstep > 2 )
            { 
                vii++ ; 
                if( vii%vstep == vstep/2 && MEM_STAT == 0 ) vstep_print(); 
            }

            /* get ref time series from this voxel */
            xsar = VECTIM_PTR(xvectim,lout) ;

            /* initialize the boundary with the target voxel */
            if( recycled_nodes == NULL)
            {
#pragma critical(mem_alloc)
                if(( new_node = (list_node*)malloc(sizeof(list_node))) == NULL)
                {
                    WARNING_message( "Could not allocate list node\n");
                    continue;
                }
            }
            else
            {
                new_node = recycled_nodes;
                recycled_nodes = recycled_nodes->next;
            }

            /* determine the full ndx for the seed target */
            new_node->ndx = imap[ lout ];

            /* add source, dest, correlation to 1D file */
            new_node->ix = DSET_index_to_ix(cset, new_node->ndx) ;
            new_node->jy = DSET_index_to_jy(cset, new_node->ndx) ;
            new_node->kz = DSET_index_to_kz(cset, new_node->ndx) ;

            /* push the new node onto the boundary list,
               if the boundary list is empty, there is a problem */
            if (boundary_list != NULL)
            {
                WARNING_message("Boundary list not empyt!\n");
            }
            new_node->next = boundary_list;
            boundary_list = new_node;
            
            /* reset the seen_voxels map */
            bzero(seen_voxels, vectim->nvec);

            /* iterate over the boundary until it is empty */
            while( boundary_list != NULL )
            {

                /* pop a node off of the list */
                current_node = boundary_list;
                boundary_list = boundary_list->next;

                /* iterate through a box around the current voxel */
                for ( int dx = 0; dx < 3; dx++ )
                {
                    ix = ( current_node->ix + (dx-1) );
                    /* make sure that we are in bounds */
                    if (( ix < 0 ) || ( ix > DSET_NX(xset) )) 
                    {
                        continue;
                    }

                    for( int dy = 0; dy < 3; dy++ )
                    {
                        jy = ( current_node->jy + (dy-1) );
                        /* make sure that we are in bounds */                        
                        if (( jy < 0 ) || ( jy > DSET_NY(xset) ))
                        {
                            continue;
                        }

                        for ( int dz = 0; dz < 3; dz++)
                        {
                            kz = ( current_node->kz + (dz-1) );
                            /* make sure that we are in bounds */
                            if (( kz < 0 ) || (kz > DSET_NZ(xset))) 
                            {
                                continue;
                            }

                            /* get the index of this voxel */
                            lndx = 
                                ndx_mask[ DSET_ixyz_to_index(xset,ix,jy,kz) ];

                            /* if the voxel is in the mask, and hasn't been
                               seen before, evaluate it for inclusion in the
                               boundary */
                            if(( lndx != 0) && ( seen_voxels[ lndx ] != 1 ))
                            {

                                /* indicate that we have seen the voxel */
                                seen_voxels[ lndx ] = 1;

                                /* extract the time series */
                                ysar = VECTIM_PTR(xvectim,lndx) ;

                                /* calculate the correlation */
                                car = (double)(corfun(nvals,xsar,ysar)) ;

                                /* if correlation is above threshold, add
                                   it to the LFCD stats and to the boundary */
                                if ( car > thresh )
                                {

                                    if( recycled_nodes == NULL)
                                    {
#pragma critical(mem_alloc)
                                        if(( new_node = (list_node*)malloc(sizeof(list_node))) == NULL)
                                        {
                                            WARNING_message( "Could not allocate list node\n");
                                            continue;
                                        }
                                    }
                                    else
                                    {
                                        new_node = recycled_nodes;
                                        recycled_nodes = recycled_nodes->next;
                                    }

                                    /* determine the full ndx for the seed target */
                                    new_node->ndx = imap[ lout ];

                                    /* add source, dest, correlation to 1D file */
                                    new_node->ix = ix ;
                                    new_node->jy = jy ;
                                    new_node->kz = kz ;

                                    /* add the node to the boundary */
                                    new_node->next = boundary_list;
                                    boundary_list = new_node;

                                    /* now increment the LFCD measures, this is
                                       done in a critical section */
#pragma omp critical(dataupdate)
                                    {
                                        wodset[ imap[ lout ]] += car;
                                        bodset[ imap[ lout ]] += 1;
                                    }                                  
                                } /* if car > thresh */
                            } /* if vox is in mask and hasn't been seen */
                        } /* for dz */
                    } /* for dy */
                } /* for dx */

                /* we have finished processing this node, so recycle it */
                current_node->next = recycled_nodes;
                recycled_nodes = current_node;

            } /* while( boundary_list != NULL ) */
        } /* for lout */
 
        /* clean up the memory that is local to this thread */
        if (seen_voxels != NULL) free( seen_voxels );

        /* clean out the recycled list */
        recycled_nodes = free_list( recycled_nodes );

    } /* end OpenMP */
    AFNI_OMP_END ;

    /* update the user so that they know what we are up to */
    INFO_message ("AFNI_OMP finished\n");
 
    INFO_message("Done..\n") ;

    /* update running memory statistics to reflect freeing the vectim */
    DEC_MEM_STATS(((xvectim->nvec*sizeof(int)) +
                       ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                       sizeof(MRI_vectim)), "vectim");

    /* toss some trash */
    VECTIM_destroy(xvectim) ;
    DSET_delete(xset) ;
    if(fout1D!=NULL)fclose(fout1D);


    PRINT_MEM_STATS( "vectim unload" );

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

    /* force a print */
    MEM_STAT = 1;
    PRINT_MEM_STATS( "Fin" );

    exit(0) ;
}