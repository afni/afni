#ifdef USE_OMP
#include <omp.h>
#endif

#include "mrilib.h"
#include <sys/mman.h>
#include <sys/types.h>

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define ETA2     4


/* CC define nodes for histogram data structure
   that we will use for sparsity thresholding */
typedef struct _hist_node hist_node;
struct _hist_node
{
    int i;
    int j;
    float corr;
    hist_node* next;
};

typedef struct _hist_node_head
{
    float bin_low;
    float bin_high;
    int nbin;
    hist_node* nodes;
} hist_node_head;

/* function to simplify free histogram */
hist_node_head* free_histogram(hist_node_head * histogram, int nhistnodes)
{
    int kout = 0;
    hist_node *pptr;
    hist_node *hptr;

    if (histogram != NULL )
    {
        for( kout = 0; kout < nhistnodes; kout++ )
        {
            if( histogram[kout].nodes != NULL )
            {
                pptr = hptr = histogram[kout].nodes;

                while(hptr != NULL )
                {
                    /* increment node pointers */
                    pptr = hptr;
                    hptr = hptr->next;

                    /* delete the node */
                    if(pptr){free(pptr);pptr=NULL;}
                }
                histogram[kout].nodes = NULL;
            }
        }
        free(histogram);
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
   int nvox , nvals , ii, jj, kout, kin, polort=1 , ix,jy,kz ;
   char *prefix = "degree_centrality" ;
   byte *mask=NULL;
   int   nmask , abuc=1 ;
   int   all_source=0;        /* output all source voxels  25 Jun 2010 [rickr] */
   char str[32] , *cpt ;
   int *imap ; MRI_vectim *xvectim ;
   float (*corfun)(int,float *,float*) = NULL ;
   /* djc - add 1d file output for similarity matrix */
   FILE *fout1D=NULL;

   /* CC - we will have two subbricks: binary and weighted centrality */
   int nsubbriks = 2;
   int subbrik = 0;
   float * bodset;
   float * wodset;

   /* CC - added flags for thresholding correlations */
   float thresh = 0.0;
   float sparsity = 0.0;
   int dosparsity = 0;
  
   /* CC flag to controls verbosity */
   int verbose = 0;
 
   /* variables for calculating degree centrality */
   int * binaryDC = NULL;
   float * weightedDC = NULL;

   /* variables for histogram */
   hist_node_head* histogram=NULL;
   hist_node* hptr=NULL;
   hist_node* pptr=NULL;
   int totNumCor = 0;
   long totPosCor = 0;
   int ngoal = 0;
   int nretain = 0;
   float binwidth = 0.0;
   int nhistnodes = 100;

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
"  -thresh r = exclude correlations < r from calculations\n"
"  -sparsity s = only use top s percent of correlations in calculations\n"
"                s should be an integer between 0 and 100.\n"
"                Requires that a first-pass threshold by defined\n"
"                using -thresh\n"
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
"  -mask mmm = Mask to define 'in-brain' voxels.\n"
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
         float val = (float)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val >= 1.0 || val < 0.0 ){
            ERROR_exit("Illegal value (%f) after -thresh!", val) ;
         }
         thresh = val ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-sparsity") == 0 ){
         float val = (float)strtod(argv[++nopt],&cpt) ;
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
      if( strcmp(argv[nopt],"-verbose") == 0 ){
         verbose = 1 ; nopt++ ; continue ;
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

   if(fout1D && !mset) {
      fclose(fout1D);
      ERROR_message("Must use -mask and -mask_source with -out1D");
      exit(1);
   }

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);
   if( DSET_NVALS(xset) < 3 )
     ERROR_exit("Input dataset %s does not have 3 or more sub-bricks!",argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;


   /* if a mask was specified make sure it is appropriate */
   if( mset ){
      if( DSET_NVOX(mset) != nvox )
         ERROR_exit("Input and mask dataset differ in number of voxels!") ;
      mask  = THD_makemask(mset, 0, 1.0, 0.0) ;
      nmask = THD_countmask( nvox , mask ) ;
      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -mask, exiting...",nmask);
      DSET_unload(mset) ;
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
   xvectim = THD_dset_to_vectim( xset , NULL , 0 ) ;
   if( xvectim == NULL ) ERROR_exit("Can't create vectim?!") ;
   DSET_unload(xset) ;
   if( polort < 0 && method == PEARSON ){
     polort = 0; WARNING_message("Pearson correlation always uses polort >= 0");
   }
   if( polort >= 0 ){
     for( ii=0 ; ii < nvox ; ii++ ){  /* remove polynomial trend */
       DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
     }
   }

   /* -- this procedure does not change time series that have zero variance -- */
   if( method == PEARSON ) THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

   /*--- make a mapping to simplify identifying in-mask voxels ---*/
   if( mask != NULL )
   {
       if( (imap = (int *)calloc(sizeof(int),nmask)) == NULL )
       {
           ERROR_message( "Could not allocate %d byte array for imap",
               nmask*sizeof(int)); 
       }
       for ( kout = ii = 0; ii < nvox; ii++ )
       {
           if( mask != NULL && mask[ii] == 0 ) continue;
           if ( kout < nmask ){ imap[kout] = ii; kout++;}
           else WARNING_message("Trying to write past end of imap (%d > %d)",
               kout,nmask);
       }
   }

    /* -- CC create arrays to hold degree and weighted centrality while
          they are being calculated -- */

    if( dosparsity == 0 )
    {
        if( ( binaryDC = (int*)calloc( nmask, sizeof(int) )) == NULL )
        {
            if (imap){ free(imap); imap = NULL; }
            ERROR_message( "Could not allocate %d byte array for binary DC calculation\n",
                nmask*sizeof(float)); 
        }
    
        if( ( weightedDC = (float*)calloc( nmask, sizeof(float) )) == NULL )
        {
            if (imap){ free(imap); imap = NULL; }
            if (binaryDC){ free(binaryDC); binaryDC = NULL; }
            ERROR_message( "Could not allocate %d byte array for weighted DC calculation\n",
                nmask*sizeof(float)); 
        }
    }


    /* -- CC if we are using a sparsity threshold, build a histogram to calculate the 
         threshold */
    if (dosparsity == 1)
    {
        /* make sure that there is a bin for correlation values that == 1.0 */
        binwidth = (1.005-thresh)/nhistnodes;
        ngoal = nretain = (int)(((float)totPosCor)*((float)sparsity) / 100.0);
        if(( histogram = (hist_node_head*)malloc(nhistnodes*sizeof(hist_node_head))) == NULL )
        {
            if (imap){ free(imap); imap = NULL; }
            if (binaryDC){ free(binaryDC); binaryDC = NULL; }
            if (weightedDC){ free(weightedDC); weightedDC = NULL; }
            ERROR_message( "Could not allocate %d byte array for weighted DC calculation\n",
                nmask*sizeof(float)); 
        }
    
        for( kout = 0; kout < nhistnodes; kout++ )
        {
            histogram[ kout ].bin_low = thresh+kout*binwidth;
            histogram[ kout ].bin_high = histogram[ kout ].bin_low+binwidth;
            histogram[ kout ].nbin = 0;
            histogram[ kout ].nodes = NULL; 
            /*INFO_message("Hist bin %d [%3.3f, %3.3f) [%d, %p]\n",
                kout, histogram[ kout ].bin_low, histogram[ kout ].bin_high,
                histogram[ kout ].nbin, histogram[ kout ].nodes );*/
        }
    }

   /*---------- loop over mask voxels, correlate ----------*/

    if (dosparsity == 0 )
    {
        INFO_message( "Calculating degree centrality with threshold = %f.\n", thresh);
    }
    else
    {
        INFO_message( "Calculating degree centrality with threshold = %f and sparsity = %3.2f%% (%d)\n",
            thresh, sparsity, nretain);
    }

    /* djc - 1d file out init */
    if (fout1D) {
       if (fout1D && (!mask)) {
          ERROR_message("Option -1Dout restricted to commands using"
                        "mask option.");
       } else {
          /* print command line statement */
          fprintf(fout1D,"#Text output of:\n#");
          for (ii=0; ii<argc; ++ii) fprintf(fout1D,"%s ", argv[ii]);
          fprintf(fout1D,"        ");
          fprintf(fout1D,"Voxel1, Voxel2, Corr\n");
         }
    }

    AFNI_OMP_START ;
    #pragma omp parallel if( nmask > 999 )
    {
       int lii,ljj,lin,lout,ithr,nthr,vstep,vii ;
       float *xsar , *ysar ;
       hist_node* new_node = NULL ;
       int new_node_idx = 0;
       float car = 0.0 ; 

    #ifdef USE_OMP
       ithr = omp_get_thread_num() ;
       nthr = omp_get_num_threads() ;
       if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
    #else
       ithr = 0 ; nthr = 1 ;
    #endif

       vstep = (int)( nmask / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
       if( ithr == 0 ) fprintf(stderr,"Looping:") ;

    #pragma omp for
       for( lout=0 ; lout < nmask ; lout++ ){  /*----- outer voxel loop -----*/

          if ( mask != NULL && imap != NULL )
          {
              lii = imap[lout] ;  /* lii= source voxel (we know that lii is in the mask) */
          }
          else
          {
              lii = lout ;
          }

          if( ithr == 0 && vstep > 2 ) /* allow small dsets 16 Jun 2011 [rickr] */
          { vii++ ; if( vii%vstep == vstep/2 ) vstep_print(); }

          /* get ref time series from this voxel */

          xsar = VECTIM_PTR(xvectim,lii) ;

          /* try to make calculation more efficient by only calculating the unique 
             correlations */
          for( lin=(lout+1) ; lin < nmask ; lin++ ){  /*----- inner loop over voxels -----*/

             /* if we are using a mask, map the index to an in-mask voxel */
             if ( mask != NULL && imap != NULL )
             {
                 ljj = imap[lin] ;  /* lii= source voxel (we know that lii is in the mask) */
             }
             /* otherwise just use the voxel */
             else
             {
                 ljj = lin ;
             }

             /* extract the voxel time series */
             ysar = VECTIM_PTR(xvectim,ljj) ;

             /* now correlate the time series */
             car = (float)(corfun(nvals,xsar,ysar)) ;

             /* if the correlation is less than threshold, ignore it */
             if ( car < thresh )
             {
                 continue;
             }

             if ( dosparsity == 1 )
             {
                 /* determine the index in the histogram to add the node */
                 new_node_idx = (int)floor((car-thresh)/binwidth);
                 if ((new_node_idx > nhistnodes) || (new_node_idx < 0))
                 {
                     WARNING_message("Node index %d is out of range (%d)!",new_node_idx,nhistnodes);
                     continue;
                 }

                 /* create a node to add to the histogram */
                 if(( new_node = (hist_node*)calloc(1,sizeof(hist_node))) == NULL )
                 {
                     WARNING_message("Could not allocate a new node!");
                     continue;
                 }
                 
                 /* populate histogram node */
                 new_node->i = lout; 
                 new_node->j = lin;
                 new_node->corr = car;
                 new_node->next = NULL;

             }
             /* update degree centrality values, hopefully the pragma
                will handle mutual exclusion */
             #pragma omp critical(dataupdate)
             {
                totNumCor += 1;
               
                if ( dosparsity == 0 )
                { 
                    binaryDC[lout] += 1; binaryDC[lin] += 1;
                    weightedDC[lout] += car; weightedDC[lin] += car;
                    /* add source, dest, correlation to 1D file */
                    fprintf(fout1D, "%d, %d, %.6f\n", lin, lout, car);
                }
                else
                {   
                    /* populate histogram */
                    new_node->next = histogram[new_node_idx].nodes;
                    histogram[new_node_idx].nodes = new_node;
                    histogram[new_node_idx].nbin++; 
                }
             }

          } /* end of inner loop over voxels */

       } /* end of outer loop over ref voxels */

       if( ithr == 0 ) fprintf(stderr,".\n") ;

    } /* end OpenMP */
    AFNI_OMP_END ;
   fprintf (stderr, "AFNI_OMP finished\n");
   fprintf (stderr, "Found %d (%3.2f%%) correlations above threshold (%f)\n",
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
   fprintf (stderr, "Creating output dataset\n");
   /*-- create output dataset --*/
   cset = EDIT_empty_copy( xset ) ;

   fprintf (stderr, "Copied input dataset header for output\n");

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

   fprintf (stderr, "Calculating dataset sizes\n");
   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) )
   {
       if (imap){ free(imap); imap = NULL; }
       if (binaryDC){ free(binaryDC); binaryDC = NULL; }
       if (weightedDC){ free(weightedDC); weightedDC = NULL; }
       if (histogram){ free(histogram); histogram = NULL; }
       ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(cset)) ;
   }

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

   if( dosparsity == 0 )
   {
       for( kout = 0; kout < nmask; kout++ )
       {
          if ( mask != NULL && imap != NULL )
          {
              ii = imap[kout] ;  /* ii= source voxel (we know that ii is in the mask) */
          }
          else
          {
              ii = kout ;
          }
     
          bodset[ ii ] = (float)(binaryDC[kout]);
          wodset[ ii ] = (float)(weightedDC[kout]);
       }

       /* we are done with this memory, and can kill it now*/
       if(binaryDC){free(binaryDC);binaryDC=NULL;}
       if(weightedDC){free(weightedDC);weightedDC=NULL;}
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
           /*INFO_message("Adding %d values from bin %d, total needed = %d",
               histogram[kout].nbin,kout,nretain);*/
           hptr = pptr = histogram[kout].nodes;
           while( hptr != NULL )
           {

               /* determine the indices corresponding to this node */
               if ( mask != NULL && imap != NULL )
               {
                   ii = imap[hptr->i] ;  /* ii= source voxel (we know that ii is in the mask) */
               }
               else
               {
                   ii = hptr->i ;
               }
               if ( mask != NULL && imap != NULL )
               {
                   jj = imap[hptr->j] ;  /* ii= source voxel (we know that ii is in the mask) */
               }
               else
               {
                   jj = hptr->j ;
               }

               /* add in the values */
               bodset[ ii ] += 1.0 ;
               wodset[ ii ] += (float)(hptr->corr);
               bodset[ jj ] += 1.0 ;
               wodset[ jj ] += (float)(hptr->corr);

               /* add source, dest, correlation to 1D file */
               fprintf(fout1D, "%d, %d, %.6f\n", ii, jj, (float)(hptr->corr));

               /* increment node pointers */
               pptr = hptr;
               hptr = hptr->next;

               /* delete the node */
               if(pptr){free(pptr);pptr=NULL;}
           }
 
           /* decrement the number of correlations we wish to retain */
           nretain -= histogram[kout].nbin;
           histogram[kout].nodes = NULL;

           /* go on to the next bin */
           kout--;
       }

        /* if we haven't used all of the correlations that are available, go through and 
           add a subset of the voxels from the remaining bin */
        if(( nretain > 0 ) && (kout >= 0))
        {

            hist_node_head* histogram2 = NULL; 
            int h2nbins = 100;
            float h2binwidth = 0.0;
            int h2ndx=0;

            h2binwidth = (((1.0+binwidth/((float)h2nbins))*histogram[kout].bin_high) - histogram[kout].bin_low) /
               ((float)h2nbins);

            /* allocate the bins */
            if(( histogram2 = (hist_node_head*)malloc(h2nbins*sizeof(hist_node_head))) == NULL )
            {
                if (imap){ free(imap); imap = NULL; }
                if (binaryDC){ free(binaryDC); binaryDC = NULL; }
                if (weightedDC){ free(weightedDC); weightedDC = NULL; }
                if (histogram){ histogram = free_histogram(histogram, nhistnodes); }
                ERROR_message( "Could not allocate %d byte array for histogram2\n",
                    h2nbins*sizeof(hist_node_head)); 
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
                h2ndx = (int)floor((hptr->corr - histogram[kout].bin_low)/h2binwidth);
                if( h2ndx < h2nbins )
                {
                    histogram[kout].nodes = hptr->next;
                    hptr->next = histogram2[h2ndx].nodes;
                    histogram2[h2ndx].nodes = hptr; 
                    histogram2[h2ndx].nbin++;
                    histogram[kout].nbin--;
                }
                else
                {
                    WARNING_message("h2ndx %d is greater than histogram size %d",h2ndx,h2nbins);
                }
               
                if( verbose == 1 )
                { 
                    INFO_message ("Adding %d nodes from histogram to histogram2 (%p)",
                        histogram[kout].nbin,hptr);
                }
            }

            /* free the remainder of histogram */
            histogram = free_histogram(histogram, nhistnodes);

            /* print the contents of hist2  */
            if (verbose == 1)
            { 
                for( kin = 0; kin < h2nbins; kin++ )
                {
                    INFO_message("Hist2 bin %d [%3.3f, %3.3f) [%d, %p]\n",
                        kin, histogram2[ kin ].bin_low, histogram2[ kin ].bin_high,
                        histogram2[ kin ].nbin, histogram2[ kin ].nodes );
                }
            }
 
            kin = h2nbins - 1;
            while (( nretain > 0 ) && ( kin >= 0 ))
            {
                if(verbose == 1)
                {
                    INFO_message("Adding %d values from bin %d, total needed = %d",
                        histogram2[kin].nbin,kin,nretain);
                }
                hptr = pptr = histogram2[kin].nodes;
                while( hptr != NULL )
                {
     
                    /* determine the indices corresponding to this node */
                    if ( mask != NULL && imap != NULL )
                    {
                        ii = imap[hptr->i] ;  
                    }
                    else
                    {
                        ii = hptr->i ;
                    }
                    if ( mask != NULL && imap != NULL )
                    {
                        jj = imap[hptr->j] ; 
                    }
                    else
                    {
                        jj = hptr->j ;
                    }

                    /* add in the values */
                    bodset[ ii ] += 1.0 ;
                    wodset[ ii ] += (float)(hptr->corr);
                    bodset[ jj ] += 1.0 ;
                    wodset[ jj ] += (float)(hptr->corr);

                    /* add source, dest, correlation to 1D file */
                    fprintf(fout1D, "%d, %d, %.6f\n", ii, jj, (float)(hptr->corr));

                    /* increment node pointers */
                    pptr = hptr;
                    hptr = hptr->next;

                    /* delete the node */
                    if(pptr){free(pptr);pptr=NULL;}
                }
 
                /* decrement the number of correlations we wish to retain */
                nretain -= histogram2[kin].nbin;
                histogram2[kin].nodes = NULL;

                /* go on to the next bin */
                kin--;
            }

            /* we are finished with histogram2 */
            histogram2 = free_histogram(histogram2, h2nbins);

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


   /* write mask info (if any) to output dataset header */
   if( mask != NULL ){
     char *maskstring = mask_to_b64string(nvox,mask) ;
     THD_set_string_atr( cset->dblk , "AFNI_AUTOTCORR_MASK" , maskstring ) ;
     free(maskstring) ; free(mask) ;
     if( ISVALID_DSET(mset) ){
       THD_set_string_atr( cset->dblk , "AFNI_AUTOCORR_MASK_IDCODE" ,
                                        DSET_IDCODE_STR(mset)        ) ;
       THD_set_string_atr( cset->dblk , "AFNI_AUTOCORR_MASK_NAME"   ,
                                        DSET_HEADNAME(mset)          ) ;
       DSET_delete(mset) ;
     }
   }
   
   /* toss some trash */

   VECTIM_destroy(xvectim) ;

   fprintf(stderr,"Done..\n") ;

   {

       double nb = (double)(xset->dblk->total_bytes)
                  +(double)(cset->dblk->total_bytes);

       if(sparsity == 1 )
       {
           nb = nb + totNumCor*sizeof(hist_node)
                +nhistnodes*sizeof(hist_node_head);
       }
       else
       {
           nb = nb +(double)(nmask*sizeof(int))
                  +(double)(nmask*sizeof(float));
       }

     if ( imap ) nb += (double)(sizeof(int)*nmask);

     nb /= (1024*1024) ;
     INFO_message(
       "Memory required = %.1f Mbytes",nb);
   }

   if (imap) free(imap) ; imap = NULL;
   if (weightedDC) free(weightedDC) ; weightedDC = NULL;
   if (binaryDC) free(binaryDC) ; binaryDC = NULL;
   
   /* finito */
   INFO_message("Writing output dataset to disk [%s bytes]",
                commaized_integer_string(cset->dblk->total_bytes)) ;
   /* this will forst compression to be off, I dont think we want this
   THD_set_write_compression(COMPRESS_NONE) ; AFNI_setenv("AFNI_AUTOGZIP=NO") ;*/
   DSET_write(cset) ;
   WROTE_DSET(cset) ;

   exit(0) ;
}
