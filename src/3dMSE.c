/*
afni/src/3dMSE.c
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

/* 3dMSE was created from 3dAutoTCorrelate by
   Stan Colcombe and R. Cameron Craddock */

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
double sampleEn( float* xvec, long nval, long w, double r )
{

    long A = 0;
    long B = 0;
    double dist = 0.0;

    long ii, jj, kk;

    for ( ii=0; ii<(nval-w); ii++ )
    {
        dist = 0.0;
        for ( jj=ii; jj<(nval-w); jj++ )
        {
            for ( kk=0; kk<w; kk++ )
            {
                /* lets make sure we aren't exceeding 
                   arrays */
                if ((ii+kk > nval) || (jj+kk > nval ))
                {
                    WARNING_message("Indexing exceeds array bounds (%ld, %ld, %ld) (%s,%d)\n",
                       ii+kk,jj+kk,nval,__FILE__,__LINE__ );
                }
                else
                {
                    dist = MAX( dist, fabs(xvec[ii+kk]-xvec[jj+kk]) );
                }
            }

            if( dist < r )
            {
                B = B + 1;
            }
            
            if ((ii+kk+1 > nval) || (jj+kk+1 > nval ))
            {
                WARNING_message("Indexing exceeds array bounds (%ld, %ld, %ld) (%s,%d)\n",
                   ii+kk+1,jj+kk+1,nval,__FILE__,__LINE__ );
            }
            else if( MAX(dist, fabs(xvec[ii+kk+1]-xvec[jj+kk+1])) < r )
            {
                A = A + 1;
            }
        }
    }

    if (( A > 0 ) && ( B > 0 ))
    {
        dist = -1.0 * log((double)A / (double)B );
    }
    else
    {
        dist = 0.0;
    }

    return( dist );
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset, *mset=NULL ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii, jj, kout, kin, polort=1 ;
   int ix1,jy1,kz1, ix2, jy2, kz2 ;
   char *prefix = "MSE" ;
   byte *mask=NULL;
   int   nmask , abuc=1 ;
   char str[32] , *cpt ;
   int *imap = NULL ; MRI_vectim *xvectim ;

   /* CC - number of scales to use in the calculation */
   long num_scales = 5;

   /* CC - the radius used in the SampleEn calculation */
   long ent_win = 2;
   double rthresh = 0.5;
   double sd = 0.0;

   /* CC - variables for writing out the results */
   int nsubbriks = 0;
   int subbrik = 0;
   float * odset;

   /* variables for holding results */
   double * mse_results = NULL;

   /*----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dMSE [options] dset\n"
"  Computes voxelwise multi-scale entropy."
"\n"
"Options:\n"
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
"               [default prefix is 'MSE'].\n"
"\n"
"  -scales N = The number of scales to be used in the calculation.\n"
"               [default is 5].\n"
"\n"
"  -entwin w = The window size used in the calculation.\n"
"               [default is 2].\n"
"\n"
"  -rthresh r = The radius threshold for determining if values are the\n"
"                same in the SampleEn calculation, in fractions of the\n"
"                standard deviation.\n"
"               [default is .5].\n"
"\n"
"Notes:\n"
" * The output dataset is a bucket type of floats.\n"
"\n"
"-- RWCox - 31 Jan 2002 and 16 Jul 2010\n"
"-- Cameron Craddock - 26 Sept 2015 \n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dMSE",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dMSE main"); machdep(); PRINT_VERSION("3dMSE");
   AFNI_logger("3dMSE",argc,argv);


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

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = strdup(argv[++nopt]) ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("Illegal value after -prefix!") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-scales") == 0 ){
         long val = (long)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0.0 ){
            ERROR_exit("Illegal value (%ld) after -scales!", val) ;
         }
         num_scales = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-rthresh") == 0 ){
         double val = (double)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0.0 ){
            ERROR_exit("Illegal value (%f) after -rthresh!", val) ;
         }
         rthresh = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-entwin") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < 0 ){
            ERROR_exit("Illegal value after -entwin!") ;
         }
         ent_win = val ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 3 ){
            ERROR_exit("Illegal value after -polort!") ;
         }
         polort = val ; nopt++ ; continue ;
      }

      ERROR_exit("Illegal option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

   if( nopt >= argc ) ERROR_exit("Need a dataset on command line!?") ;

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);

   if( DSET_NVALS(xset) < ent_win * num_scales + 1 )
     ERROR_exit("Input dataset %s does not have enough sub-bricks!",argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/
   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;

   /* if a mask was specified make sure it is appropriate */
   if( mset ){

      if( DSET_NVOX(mset) != nvox )
         ERROR_exit("Input and mask dataset differ in number of voxels!") ;

      /* make a mask */
      mask  = THD_makemask(mset, 0, 1.0, 0.0) ;

      /* determine number of voxels in the mask */
      nmask = THD_countmask( nvox , mask ) ;

      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -mask, exiting...",nmask);

      /* update running memory statistics to reflect loading the image */
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
   
   /*-- create vectim from input dataset --*/
   INFO_message("vectim-izing input dataset") ;

   /*-- CC added in mask to reduce the size of xvectim -- */
   xvectim = THD_dset_to_vectim( xset , mask , 0 ) ;
   if( xvectim == NULL ) ERROR_exit("Can't create vectim?!") ;

   /*--- CC the vectim contains a mapping between voxel index and mask index, 
         tap into that here to avoid duplicating memory usage ---*/

   if( mask != NULL )
   {
       imap = xvectim->ivec;

       /* --- CC free the mask */
       free(mask); mask=NULL;
   }

   /* -- CC unloading the dataset to reduce memory usage ?? -- */
   DSET_unload(xset) ;

   /* detrend the data */
    if( polort >= 0 )
    {
        for( ii=0 ; ii < xvectim->nvec ; ii++ )
        {  /* remove polynomial trend */
            DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
        }
    }

    /* -- this procedure does not change time series that have zero variance -- */
    THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

    /* after this normalization, the sd = sqrt(1/nvals) */
    sd = sqrt(1.0/(double)xvectim->nvals);

    /* -- allocate memory to hold the results as they are being calculated -- */
    if( ( mse_results = (double*)calloc( num_scales*nmask, sizeof(double) )) == NULL )
    {
        ERROR_message( "Could not allocate %d byte array for MSE calculation\n",
                nmask*sizeof(long)); 
    }

    /*-- tell the user what we are about to do --*/
    INFO_message( "Calculating multi-scale entropy for %d scales, window length = %ld, and threshold r = %f.\n",
       num_scales,ent_win,rthresh);

    /*---------- loop over mask voxels, correlate ----------*/
    AFNI_OMP_START ;
#pragma omp parallel if( nmask > 999 )
    {
       long ithr, nthr;
       long lout, lin, kin, scale;
       float *xsar;
       float *ysar = NULL;
       float *ysar_orig = NULL;
       long vstep, vii;
       long scale_nvals;

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
       vstep = (long)( nmask / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
       if(ithr == 0 ) fprintf(stderr,"Looping:") ;

       /* allocate memory */
#pragma omp critical (MALLOC)
        {
             ysar = (float*)malloc( xvectim->nvals * sizeof(float));
        }

        if( ysar  == NULL )
        {
            WARNING_message("Could not allocate scratch memory for thread %d (%s,%d)\n",
              ithr,__FILE__,__LINE__ );
        }

        ysar_orig = ysar;
#pragma omp for schedule(static, 1)
        for( lout=0 ; lout < xvectim->nvec ; lout++ )  /*----- outer voxel loop -----*/
        {
            if( ysar != NULL )
            {
                if( ithr == 0 && vstep > 2 ) /* allow small dsets 16 Jun 2011 [rickr] */
                {
                    vii++;
                    if( vii%vstep == vstep/2 ) vstep_print();
                }

                /* get ref time series from this voxel */
                xsar = VECTIM_PTR(xvectim,lout) ;
    
                /* calculate the entropy at scale = 1 */
                mse_results[ lout ] =
                    sampleEn( xsar, xvectim->nvals, ent_win, rthresh * sd );

                /* calculate the entropy at the remaining scales */
                for( scale = 2; scale <= num_scales; scale++ )
                {
                    scale_nvals = (long)floor((double)xvectim->nvals / (double)scale);
                    for(lin=0; lin < scale_nvals; lin++ )
                    {
                        if( lin > xvectim->nvals )
                        {
                            WARNING_message("Avoiding ysar overflow %d > %d (%s,%d)\n",
                                lin,xvectim->nvals,__FILE__,__LINE__ );
                        }
                        else
                        {
                            ysar[ lin ] = 0.0;
                            for(kin=0; kin<scale; kin++ )
                            {
                                ysar[ lin ] = ysar[ lin ] + xsar[ lin*scale + kin ];
                            }
                            ysar[ lin ] = ysar[ lin ] / (double)scale;
                        }
                    }
                    mse_results[ ((scale-1)*xvectim->nvec) + lout ] =
                        sampleEn( ysar, scale_nvals, ent_win, rthresh * sd );
                }
            }
        } /* end of outer loop over ref voxels */

        if( ithr == 0 ) fprintf(stderr,".\n") ;

#pragma omp critical (MALLOC)
        {
            if( ysar != NULL )
            {
                free(ysar);
                ysar = NULL;
            }
        }

    } /* end OpenMP */
    AFNI_OMP_END ;

    /* update the user so that they know what we are up to */
#ifdef USE_OMP
    INFO_message ("AFNI_OMP finished\n");
#endif

   /*----------  Finish up ---------*/

   /*-- create output dataset --*/
   cset = EDIT_empty_copy( xset ) ;

   /*-- configure the output dataset */
   if( abuc ){
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , num_scales     , /* subbricks */
                        ADN_ntt       , 0              , /* no time axis */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_BUCK_TYPE ,
                        ADN_datum_all , MRI_float      ,
                      ADN_none ) ;
   } else {
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , num_scales     ,  /* subbricks */
                        ADN_ntt       , num_scales     ,  /* num times */
                        ADN_ttdel     , 1.0            ,  /* fake TR */
                        ADN_nsl       , 0              ,  /* no slice offsets */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_EPI_TYPE  ,
                        ADN_datum_all , MRI_float      ,
                      ADN_none ) ;
   }

   /* add history information to the hearder */
   tross_Make_History( "3dMSE" , argc,argv , cset ) ;

   ININFO_message("Creating output dataset in memory") ;

   /* -- Configure the subbriks and copy in the data */
   for (subbrik=0; subbrik<num_scales; subbrik++ )
   {
       EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
       /* CC this sets the subbrik scaling factor, which we will probably want
          to do again after we calculate the voxel values */
       EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */
    
       sprintf(str,"Multi-scale entropy (%d)", subbrik) ;

       EDIT_BRICK_LABEL(cset,subbrik,str) ;
       EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

       /* copy measure data into the subbrik */
       odset = DSET_ARRAY(cset,subbrik);

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
   
          if( mse_results == NULL )
          {
              WARNING_message("MSE Results is NULL %d > %d (%s,%d)\n",
                  ii,DSET_NVOX(cset),__FILE__,__LINE__ );
          }
          else if( ii >= DSET_NVOX(cset) )
          {
              WARNING_message("Avoiding odset overflow %d > %d (%s,%d)\n",
                  ii,DSET_NVOX(cset),__FILE__,__LINE__ );
          }
          else if( (subbrik*nmask)+kout >= num_scales*nmask )
          {
              WARNING_message("Avoiding mse_results overflow %d >= %d (%s,%d)\n",
                  (subbrik*nmask)+kout,num_scales*nmask,__FILE__,__LINE__ );
          }
          else
          {
              odset[ ii ] = (double)(mse_results[(subbrik*nmask)+kout]);
          }
       }
   }

   /* we are done with this memory, and can kill it now*/
   if(mse_results)
   {
       free(mse_results);
       mse_results=NULL;
   }

   INFO_message("Done..\n") ;

   /* toss some trash */
   VECTIM_destroy(xvectim) ;
   DSET_delete(xset) ;

   /* finito */
   INFO_message("Writing output dataset to disk [%s bytes]",
                commaized_integer_string(cset->dblk->total_bytes)) ;

   /* write the dataset */
   DSET_write(cset) ;
   WROTE_DSET(cset) ;

   exit(0) ;
}
