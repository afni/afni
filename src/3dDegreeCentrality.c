/*
afni/src/3dDegreeCentrality.c
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
typedef struct _hist_node hist_node;
// Define histogram node structure
struct _hist_node
{
    long i;
    long j;
    double corr;
    hist_node* next;
};

// Alias _hist_node_head declaration as "hist_node_head" type via typedef
typedef struct _hist_node_head
{
    double bin_low;
    double bin_high;
    long nbin;
    hist_node* nodes;
    hist_node* tail;
} hist_node_head;

/* free the list of hist_nodes */
hist_node* free_hist_list( hist_node* hist_list )
{
    hist_node *pptr;
    hist_node *hptr = hist_list;

    while(hptr != NULL )
    {
        /* increment node pointers */
        pptr = hptr;
        hptr = hptr->next;

        /* delete the node */
        if(pptr != NULL)
        { 
            /* -- update running memory estimate to reflect memory allocation */ 
            DEC_MEM_STATS(( sizeof(hist_node)), "hist nodes");
            free(pptr);
            pptr=NULL;
        }
    }

    return(NULL);
}

/* function to simplify free histogram */
hist_node_head* free_histogram(hist_node_head * histogram, int nhistnodes)
{
    int kout = 0;
    hist_node *pptr;
    hist_node *hptr;

    /* only try to free the histogram if we have reason
       to beleive that it exists */
    if (histogram != NULL )
    {
        /* iterate through the histogram bins */
        for( kout = 0; kout < nhistnodes; kout++ )
        {
            /* if there is a linked list for this
               bin, iterate over it and free the list */
            if( histogram[kout].nodes != NULL )
            {
                /* free the list of hist_nodes */
                histogram[kout].nodes = free_hist_list( histogram[kout].nodes );
            }
        } 

        /* all of the linked lists should be empty,
           now free the bin array */
        if (histogram != NULL) {
            free(histogram);
        }
        DEC_MEM_STATS(( nhistnodes * sizeof(hist_node_head)), "hist bins");
    }

    return(NULL);
}

/* 3dDegreeCentrality was created from 3dAutoTCorrelate by
   R. Cameron Craddock */

/*----------------------------------------------------------------*/
/**** Include these here for potential optimization for OpenMP ****/
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
   hist_node_head* histogram=NULL;
   hist_node* hptr=NULL;
   hist_node* pptr=NULL;
   hist_node* recycled_nodes=NULL;
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
"Usage: 3dDegreeCentrality [options] dset\n"
"  Computes voxelwise weighted and binary degree centrality and\n"
"  stores the result in a new 3D bucket dataset as floats to\n"
"  preserve their values. Degree centrality reflects the strength and\n"
"  extent of the correlation of a voxel with every other voxel in\n"
"  the brain.\n\n"
"  Conceptually the process involves: \n"
"      1. Calculating the correlation between voxel time series for\n"
"         every pair of voxels in the brain (as determined by masking)\n"
"      2. Applying a threshold to the resulting correlations to exclude\n"
"         those that might have arisen by chance, or to sparsify the\n"
"         connectivity graph.\n"
"      3. At each voxel, summarizing its correlation with other voxels\n"
"         in the brain, by either counting the number of voxels correlated\n"
"         with the seed voxel (binary) or by summing the correlation \n"
"         coefficients (weighted).\n"
"   Practically the algorithm is ordered differently to optimize for\n"
"   computational time and memory usage.\n\n"
"   The threshold can be supplied as a correlation coefficient, \n"
"   or a sparsity threshold. The sparsity threshold reflects the fraction\n"
"   of connections that should be retained after the threshold has been\n"
"   applied. To minimize resource consumption, using a sparsity threshold\n"
"   involves a two-step procedure. In the first step, a correlation\n"
"   coefficient threshold is applied to substantially reduce the number\n"
"   of correlations. Next, the remaining correlations are sorted and a\n"
"   threshold is calculated so that only the specified fraction of \n"
"   possible correlations are above threshold. Due to ties between\n"
"   correlations, the fraction of correlations that pass the sparsity\n"
"   threshold might be slightly more than the number specified.\n\n"
"   Regardless of the thresholding procedure employed, negative \n"
"   correlations are excluded from the calculations.\n" 
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
"  -sparsity s = only use top s percent of correlations in calculations\n"
"                s should be an integer between 0 and 100. Uses an\n"
"                an adaptive thresholding procedure to reduce memory.\n"
"                The speed of determining the adaptive threshold can\n"
"                be improved by specifying an initial threshold with\n"
"                the -thresh flag.\n"
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
"  -out1D f = Save information about the above threshold correlations to\n"
"              1D file 'f'. Each row of this file will contain:\n"
"               Voxel1 Voxel2 i1 j1 k1 i2 j2 k2 Corr\n"
"              Where voxel1 and voxel2 are the 1D indices of the pair of\n"
"              voxels, i j k correspond to their 3D coordinates, and Corr\n"
"              is the value of the correlation between the voxel time courses.\n" 
"\n"
"Notes:\n"
" * The output dataset is a bucket type of floats.\n"
" * The program prints out an estimate of its memory used\n"
"    when it ends.  It also prints out a progress 'meter'\n"
"    to keep you pacified.\n"
"\n"
"-- RWCox - 31 Jan 2002 and 16 Jul 2010\n"
"-- Cameron Craddock - 26 Sept 2015 \n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dDegreeCentrality",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dDegreeCentrality main"); machdep(); PRINT_VERSION("3dDegreeCentrality");
   AFNI_logger("3dDegreeCentrality",argc,argv);

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
      if( strcmp(argv[nopt],"-sparsity") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val > 100 || val <= 0 ){
            ERROR_exit("Illegal value (%f) after -sparsity!", val) ;
         }
         if( val > 5.0 )
         {
             WARNING_message("Sparsity %3.2f%% is large and will require alot of memory and time, consider using a smaller value. ", val);
         }
         dosparsity = 1 ;
         sparsity = val ; nopt++ ; continue ;
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
      /* check for 1d argument */
      if ( strcmp(argv[nopt],"-out1D") == 0 ){
          if (!(fout1D = fopen(argv[++nopt], "w"))) {
             ERROR_message("Failed to open %s for writing", argv[nopt]);
             exit(1);
          }
          nopt++ ; continue ;
      }

      ERROR_exit("Illegal option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

   if( nopt >= argc ) ERROR_exit("Need a dataset on command line!?") ;

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);


   if( DSET_NVALS(xset) < 3 )
     ERROR_exit("Input dataset %s does not have 3 or more sub-bricks!",argv[nopt]) ;
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

    /* djc - 1d file out init */
    if (fout1D != NULL) {
        /* define affine matrix */
        mat44 affine_mat = xset->daxes->ijk_to_dicom;

        /* print command line statement */
        fprintf(fout1D,"#Similarity matrix from command:\n#");
        for(ii=0; ii<argc; ++ii) fprintf(fout1D,"%s ", argv[ii]);

        /* Print affine matrix */
        fprintf(fout1D,"\n");
        fprintf(fout1D,"#[ ");
        int mi, mj;
        for(mi = 0; mi < 4; mi++) {
            for(mj = 0; mj < 4; mj++) {
                fprintf(fout1D, "%.6f ", affine_mat.m[mi][mj]);
            }
        }
        fprintf(fout1D, "]\n");

        /* Print image extents*/
        THD_dataxes *xset_daxes = xset->daxes;
        fprintf(fout1D, "#Image dimensions:\n");
        fprintf(fout1D, "#[%d, %d, %d]\n",
                xset_daxes->nxx, xset_daxes->nyy, xset_daxes->nzz);

        /* Similarity matrix headers */
        fprintf(fout1D,"#Voxel1 Voxel2 i1 j1 k1 i2 j2 k2 Corr\n");
    }


   /* CC calculate the total number of possible correlations, will be 
       usefule down the road */
   totPosCor = (.5*((float)nmask))*((float)(nmask-1));

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
         tap into that here to avoid duplicating memory usage ---*/

   if( mask != NULL )
   {
       imap = xvectim->ivec;

       /* --- CC free the mask */
       DEC_MEM_STATS( nmask*sizeof(byte), "mask array" );
       free(mask); mask=NULL;
       PRINT_MEM_STATS( "mask unload" );
   }

   /* -- CC unloading the dataset to reduce memory usage ?? -- */
   DEC_MEM_STATS((DSET_NVOX(xset) * DSET_NVALS(xset) * sizeof(double)), "input dset");
   DSET_unload(xset) ;
   PRINT_MEM_STATS("inset unload");

   /* -- CC configure detrending --*/
   if( polort < 0 && method == PEARSON ){
     polort = 0; WARNING_message("Pearson correlation always uses polort >= 0");
   }
   if( polort >= 0 ){
     for( ii=0 ; ii < xvectim->nvec ; ii++ ){  /* remove polynomial trend */
       DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
     }
   }


   /* -- this procedure does not change time series that have zero variance -- */
   if( method == PEARSON ) THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

    /* -- CC create arrays to hold degree and weighted centrality while
          they are being calculated -- */
    if( dosparsity == 0 )
    {
        if( ( binaryDC = (long*)calloc( nmask, sizeof(long) )) == NULL )
        {
            ERROR_message( "Could not allocate %d byte array for binary DC calculation\n",
                nmask*sizeof(long)); 
        }

        /* -- update running memory estimate to reflect memory allocation */ 
        INC_MEM_STATS( nmask*sizeof(long), "binary DC array" );
        PRINT_MEM_STATS( "binaryDC" );

        if( ( weightedDC = (double*)calloc( nmask, sizeof(double) )) == NULL )
        {
            if (binaryDC){ free(binaryDC); binaryDC = NULL; }
            ERROR_message( "Could not allocate %d byte array for weighted DC calculation\n",
                nmask*sizeof(double)); 
        }
        /* -- update running memory estimate to reflect memory allocation */ 
        INC_MEM_STATS( nmask*sizeof(double), "weighted DC array" );
        PRINT_MEM_STATS( "weightedDC" );
    }


    /* -- CC if we are using a sparsity threshold, build a histogram to calculate the 
         threshold */
    if (dosparsity == 1)
    {
        /* make sure that there is a bin for correlation values that == 1.0 */
        binwidth = (1.005-thresh)/nhistnodes;

        /* calculate the number of correlations we wish to retain */
        ngoal = nretain = (int)(((double)totPosCor)*((double)sparsity) / 100.0);

        /* allocate memory for the histogram bins */
        if(( histogram = (hist_node_head*)malloc(nhistnodes*sizeof(hist_node_head))) == NULL )
        {
            /* if the allocation fails, free all memory and exit */
            if (binaryDC){ free(binaryDC); binaryDC = NULL; }
            if (weightedDC){ free(weightedDC); weightedDC = NULL; }
            ERROR_message( "Could not allocate %d byte array for histogram\n",
                nhistnodes*sizeof(hist_node_head)); 
        }
        else {
            /* -- update running memory estimate to reflect memory allocation */ 
            INC_MEM_STATS( nhistnodes*sizeof(hist_node_head), "hist bins" );
            PRINT_MEM_STATS( "hist1" );
        }

        /* initialize history bins */
        for( kout = 0; kout < nhistnodes; kout++ )
        {
            histogram[ kout ].bin_low = thresh+kout*binwidth;
            histogram[ kout ].bin_high = histogram[ kout ].bin_low+binwidth;
            histogram[ kout ].nbin = 0;
            histogram[ kout ].nodes = NULL; 
            histogram[ kout ].tail = NULL; 
            /*INFO_message("Hist bin %d [%3.3f, %3.3f) [%d, %p]\n",
                kout, histogram[ kout ].bin_low, histogram[ kout ].bin_high,
                histogram[ kout ].nbin, histogram[ kout ].nodes );*/
        }
    }

    /*-- tell the user what we are about to do --*/
    if (dosparsity == 0 )
    {
        INFO_message( "Calculating degree centrality with threshold = %f.\n", thresh);
    }
    else
    {
        INFO_message( "Calculating degree centrality with threshold = %f and sparsity = %3.2f%% (%d)\n",
            thresh, sparsity, nretain);
    }

    /*---------- loop over mask voxels, correlate ----------*/
    AFNI_OMP_START ;
#pragma omp parallel if( nmask > 999 )
    {
       int lii,ljj,lin,lout,ithr,nthr,vstep,vii ;
       float *xsar , *ysar ;
       hist_node* new_node = NULL ;
       hist_node* tptr = NULL ;
       hist_node* rptr = NULL ;
       int new_node_idx = 0;
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

#pragma omp for schedule(static, 1)
       for( lout=0 ; lout < xvectim->nvec ; lout++ ){  /*----- outer voxel loop -----*/

          if( ithr == 0 && vstep > 2 ) /* allow small dsets 16 Jun 2011 [rickr] */
          { vii++ ; if( vii%vstep == vstep/2 && MEM_STAT == 0 ) vstep_print(); }

          /* get ref time series from this voxel */
          xsar = VECTIM_PTR(xvectim,lout) ;

          /* try to make calculation more efficient by only calculating the unique 
             correlations */
          for( lin=(lout+1) ; lin < xvectim->nvec ; lin++ ){  /*----- inner loop over voxels -----*/

             /* extract the voxel time series */
             ysar = VECTIM_PTR(xvectim,lin) ;

             /* now correlate the time series */
             car = (double)(corfun(nvals,xsar,ysar)) ;

             if ( car <= thresh )
             {
                 continue ;
             }

/* update degree centrality values, hopefully the pragma
   will handle mutual exclusion */
#pragma omp critical(dataupdate)
             {
                 /* if the correlation is less than threshold, ignore it */
                 if ( car > thresh )
                 {
                     totNumCor += 1;
               
                     if ( dosparsity == 0 )
                     { 
                         binaryDC[lout] += 1; binaryDC[lin] += 1;
                         weightedDC[lout] += car; weightedDC[lin] += car;

                         /* print correlation out to the 1D file */
                         if ( fout1D != NULL )
                         {
                             /* determine the i,j,k coords */
                             lii = imap[lout];
                             ljj = imap[lin];
                             ix1 = DSET_index_to_ix(xset,lii) ;
                             jy1 = DSET_index_to_jy(xset,lii) ;
                             kz1 = DSET_index_to_kz(xset,lii) ;
                             ix2 = DSET_index_to_ix(xset,ljj) ;
                             jy2 = DSET_index_to_jy(xset,ljj) ;
                             kz2 = DSET_index_to_kz(xset,ljj) ;
                             /* add source, dest, correlation to 1D file */
                             fprintf(fout1D, "%d %d %d %d %d %d %d %d %.6f\n",
                                lii, ljj, ix1, jy1, kz1, ix2, jy2, kz2, car);
                        }
                    }
                    else
                    {
                        /* determine the index in the histogram to add the node */
                        new_node_idx = (int)floor((double)(car-othresh)/(double)binwidth);
                        if ((new_node_idx > nhistnodes) || (new_node_idx < bottom_node_idx))
                        {
                            /* this error should indicate a programming error and should not happen */
                            WARNING_message("Node index %d is out of range [%d,%d)!",new_node_idx,
                            bottom_node_idx, nhistnodes);
                        }
                        else
                        {
                            /* create a node to add to the histogram, try to use a 
                               recycled node to save time and memory */
                            if ( recycled_nodes == NULL )
                            {
                                new_node = (hist_node*)calloc(1,sizeof(hist_node));
                                /* -- update running memory estimate to reflect memory allocation */ 
                                INC_MEM_STATS( sizeof(hist_node), "hist nodes" );
                                if ((totNumCor % (1024*1024)) == 0) PRINT_MEM_STATS( "hist nodes" );

                            }
                            else
                            {
                                new_node = recycled_nodes;
                                new_node->next = NULL;
                                recycled_nodes = recycled_nodes->next;
                            }
                            if( new_node == NULL )
                            {
                                /* allocate memory for this node, rather than fiddling with 
                                   error handling here, lets just move on */
                                WARNING_message("Could not allocate a new node!");
                            }
                            else
                            {
                 
                                /* populate histogram node */
                                new_node->i = lout; 
                                new_node->j = lin;
                                new_node->corr = car;
                                new_node->next = NULL;

                                /* populate histogram */
                                new_node->next = histogram[new_node_idx].nodes;
                                histogram[new_node_idx].nodes = new_node;
                                if (histogram[new_node_idx].tail == NULL)
                                {
                                    histogram[new_node_idx].tail = new_node;
                                }
                                histogram[new_node_idx].nbin++; 

                                /* see if there are enough correlations in the histogram
                                   for the sparsity */
                                if ((totNumCor - histogram[bottom_node_idx].nbin) > nretain)
                                { 
                                    /* push the histogram nodes onto the list of recycled nodes */
                                    if( histogram[bottom_node_idx].tail == NULL ) printf("tail is null!\n" );
                                    histogram[bottom_node_idx].tail->next = recycled_nodes;
                                    recycled_nodes = histogram[bottom_node_idx].nodes;
                                   
                                    /* bookkeeping */ 
                                    histogram[bottom_node_idx].nodes = NULL;
                                    histogram[bottom_node_idx].tail = NULL;
                                    totNumCor -= histogram[bottom_node_idx].nbin;
                                    histogram[bottom_node_idx].nbin = 0;
 
                                    /* get the new threshold */
                                    thresh = (double)histogram[++bottom_node_idx].bin_low;
                                    if(MEM_STAT == 1) INFO_message("Increasing threshold to %3.2f (%d)\n",
                                        thresh,bottom_node_idx); 
                                }

                            } /* else, newptr != NULL */
                        } /* else, new_node_idx in range */
                    } /* else, do_sparsity == 1 */
                 } /* car > thresh */
             } /* this is the end of the critical section */
          } /* end of inner loop over voxels */
       } /* end of outer loop over ref voxels */

       if( ithr == 0 ) fprintf(stderr,".\n") ;

    } /* end OpenMP */
    AFNI_OMP_END ;

    /* update the user so that they know what we are up to */
    INFO_message ("AFNI_OMP finished\n");
    INFO_message ("Found %d (%3.2f%%) correlations above threshold (%f)\n",
       totNumCor, 100.0*((float)totNumCor)/((float)totPosCor), thresh);

   /*----------  Finish up ---------*/

   /*if( dosparsity == 1 )
   {
       for( kout = 0; kout < nhistnodes; kout++ )
       {
           INFO_message("Hist bin %d [%3.3f, %3.3f) [%d, %p]\n",
                kout, histogram[ kout ].bin_low, histogram[ kout ].bin_high,
                histogram[ kout ].nbin, histogram[ kout ].nodes );
       }
   }*/

   /*-- create output dataset --*/
   cset = EDIT_empty_copy( xset ) ;

   /*-- configure the output dataset */
   if( abuc ){
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , nsubbriks      , /* 2 subbricks, degree and weighted centrality */
                        ADN_ntt       , 0              , /* no time axis */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_BUCK_TYPE ,
                        ADN_datum_all , MRI_float      ,
                      ADN_none ) ;
   } else {
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , nsubbriks      , /* 2 subbricks, degree and weighted centrality */
                        ADN_ntt       , nsubbriks      ,  /* num times */
                        ADN_ttdel     , 1.0            ,  /* fake TR */
                        ADN_nsl       , 0              ,  /* no slice offsets */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_EPI_TYPE  ,
                        ADN_datum_all , MRI_float      ,
                      ADN_none ) ;
   }

   /* add history information to the hearder */
   tross_Make_History( "3dDegreeCentrality" , argc,argv , cset ) ;

   ININFO_message("creating output dataset in memory") ;

   /* -- Configure the subbriks: Binary Degree Centrality */
   subbrik = 0;
   EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
   /* CC this sets the subbrik scaling factor, which we will probably want
      to do again after we calculate the voxel values */
   EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */

   sprintf(str,"Binary Degree Centrality") ;

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

   sprintf(str,"Weighted Degree Centrality") ;

   EDIT_BRICK_LABEL(cset,subbrik,str) ;
   EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

   /* copy measure data into the subbrik */
   wodset = DSET_ARRAY(cset,subbrik);

   /* increment memory stats */
   INC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)), "output dset");
   PRINT_MEM_STATS( "outset" );

   /* pull the values out of the histogram */
   if( dosparsity == 0 )
   {
       for( kout = 0; kout < nmask; kout++ )
       {
          if ( imap != NULL )
          {
              ii = imap[kout] ;  /* ii= source voxel (we know that ii is in the mask) */
          }
          else
          {
              ii = kout ;
          }
   
          if( ii >= DSET_NVOX(cset) )
          {
              WARNING_message("Avoiding bodset, wodset overflow %d > %d (%s,%d)\n",
                  ii,DSET_NVOX(cset),__FILE__,__LINE__ );
          }
          else
          {
              bodset[ ii ] = (float)(binaryDC[kout]);
              wodset[ ii ] = (float)(weightedDC[kout]);
          }
       }

       /* we are done with this memory, and can kill it now*/
       if(binaryDC)
       {
           free(binaryDC);
           binaryDC=NULL;
           /* -- update running memory estimate to reflect memory allocation */ 
           DEC_MEM_STATS( nmask*sizeof(long), "binary DC array" );
           PRINT_MEM_STATS( "binaryDC" );
       }
       if(weightedDC)
       {
           free(weightedDC);
           weightedDC=NULL;
           /* -- update running memory estimate to reflect memory allocation */ 
           DEC_MEM_STATS( nmask*sizeof(double), "weighted DC array" );
           PRINT_MEM_STATS( "weightedDC" );
       }
   }
   else
   {

       /* add in the values from the histogram, this is a two stage procedure:
             at first we add in values a whole bin at the time until we get to a point
             where we need to add in a partial bin, then we create a new histogram
             to sort the values in the bin and then add those bins at a time */
       kout = nhistnodes - 1;
       while (( histogram[kout].nbin < nretain ) && ( kout >= 0 ))
       {
           hptr = pptr = histogram[kout].nodes;
           while( hptr != NULL )
           {

               /* determine the indices corresponding to this node */
               if ( imap != NULL )
               {
                   ii = imap[hptr->i] ;  /* ii= source voxel (we know that ii is in the mask) */
               }
               else 
               {
                   ii = hptr->i ;
               }
               if ( imap != NULL )
               {
                   jj = imap[hptr->j] ;  /* ii= source voxel (we know that ii is in the mask) */
               }
               else
               {
                   jj = hptr->j ;
               }

               /* add in the values */
               if(( ii >= DSET_NVOX(cset) ) || ( jj >= DSET_NVOX(cset)))
               {
                   if( ii >= DSET_NVOX(cset))
                   {
                       WARNING_message("Avoiding bodset, wodset overflow (ii) %d > %d\n (%s,%d)\n",
                           ii,DSET_NVOX(cset),__FILE__,__LINE__ );
                   }
                   if( jj >= DSET_NVOX(cset))
                   {
                       WARNING_message("Avoiding bodset, wodset overflow (jj) %d > %d\n (%s,%d)\n",
                           jj,DSET_NVOX(cset),__FILE__,__LINE__ );
                   }
               }
               else
               {
                   bodset[ ii ] += 1.0 ;
                   wodset[ ii ] += (float)(hptr->corr);
                   bodset[ jj ] += 1.0 ;
                   wodset[ jj ] += (float)(hptr->corr);
               }

               if( fout1D != NULL )
               {
                   /* add source, dest, correlation to 1D file */
                   ix1 = DSET_index_to_ix(cset,ii) ;
                   jy1 = DSET_index_to_jy(cset,ii) ;
                   kz1 = DSET_index_to_kz(cset,ii) ;
                   ix2 = DSET_index_to_ix(cset,jj) ;
                   jy2 = DSET_index_to_jy(cset,jj) ;
                   kz2 = DSET_index_to_kz(cset,jj) ;
                   fprintf(fout1D, "%d %d %d %d %d %d %d %d %.6f\n",
                           ii, jj, ix1, jy1, kz1, ix2, jy2, kz2, (float)(hptr->corr));
               }

               /* increment node pointers */
               pptr = hptr;
               hptr = hptr->next;

               /* delete the node */
               if(pptr)
               {
                   /* -- update running memory estimate to reflect memory allocation */ 
                   DEC_MEM_STATS(sizeof( hist_node ), "hist nodes" );
                   /* free the mem */
                   free(pptr);
                   pptr=NULL;
               }
           } 
           /* decrement the number of correlations we wish to retain */
           nretain -= histogram[kout].nbin;
           histogram[kout].nodes = NULL;
           histogram[kout].tail = NULL;

           /* go on to the next bin */
           kout--;
       }
       PRINT_MEM_STATS( "hist1 bins free - inc into output" );

        /* if we haven't used all of the correlations that are available, go through and 
           add a subset of the voxels from the remaining bin */
        if(( nretain > 0 ) && (kout >= 0))
        {

            hist_node_head* histogram2 = NULL; 
            hist_node_head* histogram2_save = NULL; 
            int h2nbins = 100;
            float h2binwidth = 0.0;
            int h2ndx=0;

            h2binwidth = (((1.0+binwidth/((float)h2nbins))*histogram[kout].bin_high) - histogram[kout].bin_low) /
               ((float)h2nbins);

            /* allocate the bins */
            if(( histogram2 = (hist_node_head*)malloc(h2nbins*sizeof(hist_node_head))) == NULL )
            {
                if (binaryDC){ free(binaryDC); binaryDC = NULL; }
                if (weightedDC){ free(weightedDC); weightedDC = NULL; }
                if (histogram){ histogram = free_histogram(histogram, nhistnodes); }
                ERROR_message( "Could not allocate %d byte array for histogram2\n",
                    h2nbins*sizeof(hist_node_head)); 
            }
            else {
                /* -- update running memory estimate to reflect memory allocation */ 
                histogram2_save = histogram2;
                INC_MEM_STATS(( h2nbins*sizeof(hist_node_head )), "hist bins");
                PRINT_MEM_STATS( "hist2" );
            }
   
            /* initiatize the bins */ 
            for( kin = 0; kin < h2nbins; kin++ )
            {
                histogram2[ kin ].bin_low = histogram[kout].bin_low + kin*h2binwidth;
                histogram2[ kin ].bin_high = histogram2[ kin ].bin_low + h2binwidth;
                histogram2[ kin ].nbin = 0;
                histogram2[ kin ].nodes = NULL; 
                /*INFO_message("Hist2 bin %d [%3.3f, %3.3f) [%d, %p]\n",
                    kin, histogram2[ kin ].bin_low, histogram2[ kin ].bin_high,
                    histogram2[ kin ].nbin, histogram2[ kin ].nodes );*/
            }

            /* move correlations from histogram to histgram2 */
            INFO_message ("Adding %d nodes from histogram to histogram2",histogram[kout].nbin);
            while ( histogram[kout].nodes != NULL )
            {
                hptr = histogram[kout].nodes;
                h2ndx = (int)floor((double)(hptr->corr - histogram[kout].bin_low)/(double)h2binwidth);
                if(( h2ndx < h2nbins ) && ( h2ndx >= 0 ))
                {
                    histogram[kout].nodes = hptr->next;
                    hptr->next = histogram2[h2ndx].nodes;
                    histogram2[h2ndx].nodes = hptr; 
                    histogram2[h2ndx].nbin++;
                    histogram[kout].nbin--;
                }
                else
                {
                    WARNING_message("h2ndx %d is not in range [0,%d) :: %.10f,%.10f\n",h2ndx,h2nbins,hptr->corr, histogram[kout].bin_low);
                }
               
            }

            /* free the remainder of histogram */
            {
                int nbins_rem = 0;
                for(ii = 0; ii < nhistnodes; ii++) nbins_rem+=histogram[ii].nbin;
                histogram = free_histogram(histogram, nhistnodes);
                PRINT_MEM_STATS( "free remainder of histogram1" );
                if( recycled_nodes != NULL ) recycled_nodes = free_hist_list (recycled_nodes);
                PRINT_MEM_STATS( "free recycled nodes!" );
            }

            kin = h2nbins - 1;
            while (( nretain > 0 ) && ( kin >= 0 ))
            {
                hptr = pptr = histogram2[kin].nodes;
                while( hptr != NULL )
                {
     
                    /* determine the indices corresponding to this node */
                    if ( imap != NULL )
                    {
                        ii = imap[hptr->i] ;  
                    }
                    else
                    {
                        ii = hptr->i ;
                    }
                    if ( imap != NULL )
                    {
                        jj = imap[hptr->j] ; 
                    }
                    else
                    {
                        jj = hptr->j ;
                    }

                    /* add in the values */
                    if(( ii >= DSET_NVOX(cset) ) || ( jj >= DSET_NVOX(cset)))
                    {
                        if( ii >= DSET_NVOX(cset))
                        {
                            WARNING_message("Avoiding bodset, wodset overflow (ii) %d > %d\n (%s,%d)\n",
                                ii,DSET_NVOX(cset),__FILE__,__LINE__ );
                        }
                        if( jj >= DSET_NVOX(cset))
                        {
                            WARNING_message("Avoiding bodset, wodset overflow (jj) %d > %d\n (%s,%d)\n",
                                jj,DSET_NVOX(cset),__FILE__,__LINE__ );
                        }
                    }
                    else
                    {
                        bodset[ ii ] += 1.0 ;
                        wodset[ ii ] += (float)(hptr->corr);
                        bodset[ jj ] += 1.0 ;
                        wodset[ jj ] += (float)(hptr->corr);
                    }
                    if( fout1D != NULL )
                    {
                        /* add source, dest, correlation to 1D file */
                        ix1 = DSET_index_to_ix(cset,ii) ;
                        jy1 = DSET_index_to_jy(cset,ii) ;
                        kz1 = DSET_index_to_kz(cset,ii) ;
                        ix2 = DSET_index_to_ix(cset,jj) ;
                        jy2 = DSET_index_to_jy(cset,jj) ;
                        kz2 = DSET_index_to_kz(cset,jj) ;
                        fprintf(fout1D, "%d %d %d %d %d %d %d %d %.6f\n",
                            ii, jj, ix1, jy1, kz1, ix2, jy2, kz2, (float)(hptr->corr));
                    }

                    /* increment node pointers */
                    pptr = hptr;
                    hptr = hptr->next;

                    /* delete the node */
                    if(pptr)
                    {
                        free(pptr);
                        DEC_MEM_STATS(( sizeof(hist_node) ), "hist nodes");
                        pptr=NULL;
                    }
                }
 
                /* decrement the number of correlations we wish to retain */
                nretain -= histogram2[kin].nbin;
                histogram2[kin].nodes = NULL;

                /* go on to the next bin */
                kin--;
            }
            PRINT_MEM_STATS("hist2 nodes free - incorporated into output");

            /* we are finished with histogram2 */
            {
                histogram2 = free_histogram(histogram2, h2nbins);
                /* -- update running memory estimate to reflect memory allocation */ 
                PRINT_MEM_STATS( "free hist2" );
            }

            if (nretain < 0 )
            {
                WARNING_message( "Went over sparsity goal %d by %d, with a resolution of %f",
                      ngoal, -1*nretain, h2binwidth);
            }
        }
        if (nretain > 0 )
        {
            WARNING_message( "Was not able to meet goal of %d (%3.2f%%) correlations, %d (%3.2f%%) correlations passed the threshold of %3.2f, maybe you need to change the threshold or the desired sparsity?",
                  ngoal, 100.0*((float)ngoal)/((float)totPosCor), totNumCor, 100.0*((float)totNumCor)/((float)totPosCor),  thresh);
        }
   }

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

   if (weightedDC) free(weightedDC) ; weightedDC = NULL;
   if (binaryDC) free(binaryDC) ; binaryDC = NULL;
   /* check one more time and delete recycled_nodes if they are still around */
   if( recycled_nodes != NULL ) recycled_nodes = free_hist_list (recycled_nodes);
   
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
