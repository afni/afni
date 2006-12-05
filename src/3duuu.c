/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <string.h>
#include "mrilib.h"
#include <stdlib.h>
#include <ctype.h>

/*-------------------------- global data --------------------------*/

/** inputs **/

static THD_3dim_dataset * UC_dset = NULL ; /* dataset */

static char UC_prefix[THD_MAX_PREFIX] = "uuu" ;

static int UC_be_quiet = 1 ;

static byte * UC_mask = NULL ;
static int    UC_mask_nvox = 0 ;
static int    UC_mask_hits = 0 ;

static int    UC_nvec = 0 ;      /* # of vectors to use from dataset */
static int    UC_vdim = 0 ;      /* length of each vector            */

static float ** UC_vec = NULL ;  /* UC_vec[k][i] is the i-th component  */
                                 /* of the k-th vector 0 <= i < UC_vdim */
                                 /*                    0 <= k < UC_nvec */

static int    * UC_iv  = NULL ;  /* UC_vec[k] comes from voxel # UC_iv[k] */

static float UC_ptail = 0.0001 ;

void UC_syntax(char * msg) ;

#include "uuu.c"

/*-------------------------------------------------------------------
     detrend: routine to remove unwanted components from time series
---------------------------------------------------------------------*/

void detrend( int n , float vec[] )
{
   register int ii ;
   register float sum0 , sum1 , cf , lf ;
   float sum2 , det ;

   static int nold = -1 ;             /* initialization flag */
   static float cf0,cf1 , lf0,lf1 ;   /* to be initialized */

 /*** initialize coefficients for detrending ***/

   if( n != nold ){
      nold = n ; sum0 = sum1 = sum2 = 0.0 ;
      for( ii=0 ; ii < n ; ii++ ){
         sum0 += 1.0 ; sum1 += ii ; sum2 += ii*ii ;
      }
      det = sum0 * sum2 - sum1 * sum1 ;
      cf0 =  sum2 / det ;     /* constant factor for sum0 */
      cf1 = -sum1 / det ;     /* constant factor for sum1 */
      lf0 = cf1 ;             /* linear factor for sum0 */
      lf1 =  sum0 / det ;     /* linear factor for sum1 */
   }

 /*** remove mean and linear trend ***/

   sum0 = sum1 = 0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      sum0 += vec[ii] ; sum1 += vec[ii] * ii ;
   }

   cf = cf0 * sum0 + cf1 * sum1 ;
   lf = lf0 * sum0 + lf1 * sum1 ;
   for( ii=0 ; ii < n ; ii++ ) vec[ii] -= cf + ii*lf ;
}

/*----------------------------------------------------------
   normalize: routine to scale a time series to unit vector
------------------------------------------------------------*/

void normalize( int n , float vec[] )
{
   register int ii ;
   register float sqsum ;

   detrend( n , vec ) ;

   sqsum = 0.0 ;
   for( ii=0 ; ii < n ; ii++ ) sqsum += vec[ii] * vec[ii] ;

   if( sqsum < 1.e-10 ){
      for( ii=0 ; ii < n ; ii++ ) vec[ii] = 0.0 ;
   } else {
      sqsum = 1.0 / sqrt(sqsum) ;
      for( ii=0 ; ii < n ; ii++ ) vec[ii] *= sqsum ;
   }
}

/*------------------------------------------------------------------*/

void UC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  kk, nxyz, mm,nn ;
   float * vv , * bb ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -verbose ****/

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         UC_be_quiet = 0 ;
         nopt++ ; continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) UC_syntax("-prefix needs an argument!") ;
         MCW_strncpy( UC_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -mask mset ****/

      if( strncmp(argv[nopt],"-mask",5) == 0 ){
         THD_3dim_dataset * mset ; int ii,nn ;
         nopt++ ;
         if( nopt >= argc ) UC_syntax("need arguments after -mask!") ;
         mset = THD_open_dataset( argv[nopt] ) ;
         if( mset == NULL ) UC_syntax("can't open -mask dataset!") ;
         UC_mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
         UC_mask_nvox = DSET_NVOX(mset) ;
         DSET_delete(mset) ;
         if( UC_mask == NULL ) UC_syntax("can't use -mask dataset!") ;
         UC_mask_hits = THD_countmask( UC_mask_nvox , UC_mask ) ;
         if( UC_mask_hits == 0 ) UC_syntax("mask is all zeros!") ;
         if( !UC_be_quiet ) printf("--- %d voxels in mask\n",UC_mask_hits) ;

         UC_iv = (int *) malloc( sizeof(int) * UC_mask_hits ) ;
         for( nn=ii=0 ; ii < UC_mask_nvox ; ii++ )
            if( UC_mask[ii] ) UC_iv[nn++] = ii ;

         nopt++ ; continue ;
      }

      /**** -ptail p ****/

      if( strcmp(argv[nopt],"-ptail") == 0 ){
         if( ++nopt >= argc ) UC_syntax("-ptail needs an argument!") ;
         UC_ptail = strtod( argv[nopt] , NULL ) ;
         if( UC_ptail <= 0.0 || UC_ptail >= 0.499 )
            UC_syntax("value after -ptail is illegal!") ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"\n*** unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

   /*--- a simple consistency check ---*/

   /*--- last input is dataset name ---*/

   if( nopt >= argc ) UC_syntax("no input dataset name?") ;

   UC_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_3DIM_DATASET(UC_dset) ){
      fprintf(stderr,"\n*** can't open dataset file %s\n",argv[nopt]) ;
      exit(1) ;
   }

   nxyz = DSET_NVOX(UC_dset) ;
   if( UC_mask != NULL && nxyz != UC_mask_nvox )
      UC_syntax("mask and input dataset size mismatch!") ;

   /*--- load vectors ---*/

   UC_nvec = (UC_mask_hits > 0) ? UC_mask_hits : nxyz ;
   UC_vdim = DSET_NVALS(UC_dset) ;
   if( UC_vdim < 4 )
      UC_syntax("input dataset needs at least 4 sub-bricks!") ;

   vv     = (float *) malloc( sizeof(float) * UC_nvec * UC_vdim ) ;
   UC_vec = (float **) malloc( sizeof(float *) * UC_nvec ) ;
   for( kk=0 ; kk < UC_nvec ; kk++ ) UC_vec[kk] = vv + (kk*UC_vdim) ;

   if( !UC_be_quiet ) printf("--- reading input dataset\n") ;
   DSET_load(UC_dset) ; CHECK_LOAD_ERROR(UC_dset) ;

   /* copy brick data into float storage */

   if( !UC_be_quiet ) printf("--- loading vectors\n") ;

   bb = (float *) malloc( sizeof(float) * nxyz ) ;
   for( mm=0 ; mm < UC_vdim ; mm++ ){

      EDIT_coerce_type( nxyz ,
                        DSET_BRICK_TYPE(UC_dset,mm) , DSET_ARRAY(UC_dset,mm) ,
                        MRI_float , bb ) ;

      DSET_unload_one( UC_dset , mm ) ;

      if( UC_mask == NULL ){
         for( kk=0 ; kk < nxyz ; kk++ ) UC_vec[kk][mm] = bb[kk] ;
      } else {
         for( nn=kk=0 ; kk < nxyz ; kk++ )
            if( UC_mask[kk] ) UC_vec[nn++][mm] = bb[kk] ;
      }
   }
   free(bb) ; DSET_unload( UC_dset ) ;

   /* detrend and normalize vectors */

   if( !UC_be_quiet ) printf("--- normalizing vectors\n") ;

   for( kk=0 ; kk < UC_nvec ; kk++ )
      normalize( UC_vdim , UC_vec[kk] ) ;

   return ;
}

/*-----------------------------------------------------------------------
  Compute the unusuality index of a reference vector in a set of vectors
-------------------------------------------------------------------------*/

float UC_unusuality( int ndim , float * ref , int nvec , float ** vec )
{
   register int ii , kk ;
   register float psum , * vv ;

   static int     nvold=-1   ;
   static float * zval =NULL ;

   if( ndim < 4 || nvec < 4 || ref == NULL || vec == NULL ) return 0.0 ;

   /* initialize if number of vectors has changed */

   if( nvold != nvec ){
      if( zval != NULL ) free(zval) ;
      zval = (float *) malloc(sizeof(float)*nvec) ;
      nvold = nvec ;
   }

   /* compute dot products */

   for( kk=0 ; kk < nvec ; kk++ ){
      psum = 0.0 ; vv = vec[kk] ;
      for( ii=0 ; ii < ndim ; ii++ ) psum += ref[ii] * vv[ii] ;
      zval[kk] = psum ;
   }

   psum = unusuality( nvec, zval ) ;

   return psum ;
}

/*---------------------------------------------------------------------------*/

void UC_syntax(char * msg)
{
   if( msg != NULL ){ fprintf(stderr,"\n*** %s\n",msg) ; exit(1) ; }

   printf(
    "Usage: 3duuu [options] dataset ...\n"
    "\n"
    "The input dataset may have a sub-brick selector list.\n"
    "Otherwise, all sub-bricks from a dataset will be used.\n"
    "\n"
    "OPTIONS:\n"
    "  -prefix pname \n"
    "  -verbose\n"
    "  -mask mset\n"
    "  -ptail p\n"
   ) ;

   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int kk , nvox , ii ;
   THD_3dim_dataset * oset ;
   float * far ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) UC_syntax(NULL) ;

   (void) my_getenv("junk") ;

   UC_read_opts( argc , argv ) ;
   set_unusuality_tail( UC_ptail ) ;

   oset = EDIT_empty_copy( UC_dset ) ;
   EDIT_dset_items( oset ,
                       ADN_prefix      , UC_prefix ,
                       ADN_ntt         , 0 ,
                       ADN_nvals       , 1 ,
                       ADN_datum_all   , MRI_float ,
                       ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                    ADN_none ) ;

   nvox = DSET_NVOX(oset) ;
   far = (float *) malloc( sizeof(float) * nvox ) ;
   for( kk=0 ; kk < nvox ; kk++ ) far[kk] = 0.0 ;
   EDIT_substitute_brick( oset , 0 , MRI_float , far ) ;

   if( !UC_be_quiet ){ printf("--- computing u") ; fflush(stdout) ; }

   for( kk=0 ; kk < UC_nvec ; kk++ ){
      ii = (UC_iv == NULL) ? kk : UC_iv[kk] ;
      far[ii] = UC_unusuality( UC_vdim, UC_vec[kk] , UC_nvec, UC_vec ) ;
      if( !UC_be_quiet && kk%1000==999 ){ printf(".");fflush(stdout); }
   }
   if( !UC_be_quiet ) printf("\n--- writing output\n") ;

   DSET_write(oset) ;
   exit(0) ;
}
