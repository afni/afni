#include "mrilib.h"

/*==============================================================================*/
/*========== The following functions are lifted from afni_fimfunc.c ============*/
/*------------------------------------------------------------------------------
   Rank-order a float array, with ties getting the average rank.
   The output overwrites the input.
--------------------------------------------------------------------------------*/

static void rank_order_float( int n , float * a )
{
   register int ii , ns , n1 , ib ;
   static int    nb = 0 ;
   static int *   b = NULL ;  /* workspaces */
   static float * c = NULL ;
   float cs ;

   /*- handle special cases -*/

   if( a == NULL ){
      if( b != NULL ){ free(b); free(c); b=NULL ; c=NULL; nb=0; }  /* free workspaces */
      return ;
   }

   if( n < 1 ) return ;                     /* meaningless input */
   if( n == 1 ){ a[0] = 0.0 ; return ; }    /* only one point!? */

   /*- make workspaces, if needed -*/

   if( nb < n ){
      if( b != NULL ){ free(b); free(c); }
      b  = (int   *) malloc(sizeof(int  )*n) ;
      c  = (float *) malloc(sizeof(float)*n) ;
      nb = n ;
   }

   for( ii=0 ; ii < n ; ii++ ) c[ii] = b[ii] = ii ;

   /*- sort input, carrying b along -*/

   qsort_floatint( n , a , b ) ;  /* see cs_sort_fi.c */

   /* compute ranks into c[] */

   n1 = n-1 ;
   for( ii=0 ; ii < n1 ; ii++ ){
      if( a[ii] == a[ii+1] ){                  /* handle ties */
         cs = 2*ii+1 ; ns = 2 ; ib=ii ; ii++ ;
         while( ii < n1 && a[ii] == a[ii+1] ){ ii++ ; ns++ ; cs += ii ; }
         for( cs/=ns ; ib <= ii ; ib++ ) c[ib] = cs ;
      }
   }

   for( ii=0 ; ii < n ; ii++ ) a[b[ii]] = c[ii] ;

   return ;
}

/*---------------------------------------------------------------------------
   Rank orders a[], subtracts the mean rank, and returns the sum-of-squares
-----------------------------------------------------------------------------*/

static float spearman_rank_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] -= rb ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

/*------------------------------------------------------------------------------*/

static float quadrant_corr_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] = (a[ii] > rb) ? 1.0
                           : (a[ii] < rb) ? -1.0 : 0.0 ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

/*-----------------------------------------------------------------------------
    To correlate x[] with r[], do
      rv = spearman_rank_prepare(n,r) ;
    then
      corr = spearman_rank_corr(n,x,rv,r) ;
    Note that these 2 routines are destructive (r and x are replaced by ranks)
-------------------------------------------------------------------------------*/

static float spearman_rank_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

/*------------------------------------------------------------------------------*/

static float quadrant_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

/*==============================================================================*/

#define SPEARMAN 1
#define QUADRANT 2

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   int nopt=1 , method=SPEARMAN , do_range=0 , do_autoclip=0 ;
   int nvox , nvals , ii,iv,jj ;
   float *medar, *var , rv , *corr , cmed,cmad,cbot,ctop , clip_val=0.0 ;
   MRI_IMAGE *tsim , *medim ;
   int nkeep , *keep ;
   float *mkeep , *tkeep ;

   /*----*/

   mainENTRY("3dTqual main") ; machdep() ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTqual [options] dataset\n"
             "Computes a `quality index' for each sub-brick in a 3D+time dataset.\n"
             "The output is a 1D time series with the index for each sub-brick.\n"
             "The results are written to stdout.\n"
             "\n"
             "Note that small values of the index are 'good', indicating that\n"
             "the sub-brick is not very different from the norm.  The purpose\n"
             "of this program is to provide a crude way of screening FMRI\n"
             "time series for sporadic abnormal images, such as might be\n"
             "caused by large subject head motion or scanner glitches.\n"
             "\n"
             "Do not take the results of this program too literally.  It\n"
             "is intended as a GUIDE to help you find data problems, and no\n"
             "more.  It is not an assurance that the dataset is good, and\n"
             "it may indicate problems where nothing is wrong.\n"
             "\n"
             "Sub-bricks with index values much higher than others should be\n"
             "examined for problems.  How you determine what 'much higher' means\n"
             "is mostly up to you.  I suggest graphical inspection of the indexes\n"
             "(cf. Example, infra).  As a guide, the program will print (stderr)\n"
             "the median quality index and the range median-3.5*MAD .. median+3*MAD\n"
             "(MAD=Median Absolute Deviation).  Values well outside this range might\n"
             "be considered suspect; if the quality index were normally distributed,\n"
             "then values outside this range would occur only about 1%% of the time.\n"
             "\n"
             "Options:\n"
             "  -spearman = Quality index is 1 minus the Spearman (rank)\n"
             "               correlation coefficient of each sub-brick\n"
             "               with the median sub-brick.\n"
             "               [This is the default method.]\n"
             "  -quadrant = Similar to -spearman, but using 1 minus the\n"
             "               quadrant correlation coefficient as the\n"
             "               quality index.\n"
             "\n"
             "  -autoclip = Clip off low-intensity regions in the median sub-brick,\n"
             "               so that the correlation is only computed between\n"
             "               high-intensity (presumably brain) voxels.  The\n"
             "               intensity level is determined the same way that\n"
             "               3dClipLevel works.  This prevents the vast number\n"
             "               of nearly 0 voxels outside the brain from biasing\n"
             "               the correlation coefficient calculations.\n"
             "  -clip val = Clip off values below 'val' in the median sub-brick.\n"
             "\n"
             "  -range    = Print the median-3.5*MAD and median+3.5*MAD values\n"
             "               out with EACH quality index, so that they\n"
             "               can be plotted (cf. Example, infra).\n"
             "     Notes: * These values are printed to stderr in any case.\n"
             "            * This is only useful for plotting with 1dplot.\n"
             "            * The lower value median-3.5*MAD is never allowed\n"
             "                to go below 0.\n"
             "\n"
             "Example:\n"
             "   3dTqual -range -autoclip fred+orig | 1dplot -one -stdin\n"
             "will calculate the time series of quality indexes and plot them\n"
             "to an X11 window, along with the median+/-3.5*MAD bands.\n"
             "\n"
             "NOTE: also see program 3dToutcount for a similar quality check.\n"
             "\n"
             "-- RWCox - Aug 2001\n"
            ) ;
      exit(0) ;
   }

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-autoclip") == 0 ){
         do_autoclip = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-clip") == 0 ){
         do_autoclip = 0 ;
         clip_val = strtod(argv[++nopt],NULL) ;
         if( clip_val <= 0.0 ){
            fprintf(stderr,"** value after -clip is illegal!\n"); exit(1);
         }
         nopt++ ;continue ;
      }

      if( strcmp(argv[nopt],"-range") == 0 ){
         do_range = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-spearman") == 0 ){
         method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-quadrant") == 0 ){
         method = QUADRANT ; nopt++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]); exit(1);
   }

   /*-- open dataset --*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No dataset on command line!?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[nopt] ) ;
   if( dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }
   if( DSET_NUM_TIMES(dset) < 2 ){
      fprintf(stderr,"*** Input dataset is not 3D+time\n"); exit(1);
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"*** Can't read dataset bricks\n"); exit(1);
   }

   /*-- compute median brick, and order it for correlation --*/

   nvox  = DSET_NVOX(dset) ;
   nvals = DSET_NVALS(dset) ;

   medim = THD_median_brick( dset ) ; if( medim == NULL ) exit(1) ;
   medar = MRI_FLOAT_PTR(medim) ;

   if( do_autoclip ){
      clip_val = THD_cliplevel( medim , 0.5 ) ;
      fprintf(stderr,"++ Autoclip value = %g\n",clip_val) ;
   }

   if( clip_val > 0.0 ){
      nkeep = 0 ;
      for( ii=0 ; ii < nvox ; ii++ )
         if( medar[ii] >= clip_val ) nkeep++ ;
      if( nkeep < 9 ){
         fprintf(stderr,"*** %d voxels survive the clip: can't continue\n",nkeep) ;
         exit(1) ;
      } else if( nkeep == nvox ){ /* no clipping */
         fprintf(stderr,"++ All %d voxels survive the clip\n",nvox) ;
         mkeep = medar ;
      } else {                    /* nkeep < nvox */
         keep  = (int *)   malloc( sizeof(int)  *nkeep ) ;
         mkeep = (float *) malloc( sizeof(float)*nkeep ) ;
         tkeep = (float *) malloc( sizeof(float)*nkeep ) ;
         for( jj=ii=0 ; ii < nvox ; ii++ )
            if( medar[ii] >= clip_val ){
               keep[jj] = ii ; mkeep[jj] = medar[ii] ; jj++ ;
            }
         mri_free(medim) ;
         fprintf(stderr,"++ %d out of %d voxels survive the clip\n",nkeep,nvox) ;
      }
   } else {
      nkeep = nvox ; mkeep = medar ; /* no clipping */
   }

   switch( method ){
     default:
     case SPEARMAN:
       rv = spearman_rank_prepare( nkeep , mkeep ) ; break ;

     case QUADRANT:
       rv = quadrant_corr_prepare( nkeep , mkeep ) ; break ;
   }

   /*-- loop over input bricks, and correlate with median --*/

   corr = (float *) malloc( sizeof(float)*nvals ) ;

   for( iv=0 ; iv < nvals ; iv++ ){

      /*- get sub-brick -*/

      tsim = THD_extract_float_brick( iv , dset ) ;
      if( tsim == NULL ) exit(1) ;
      var = MRI_FLOAT_PTR(tsim) ;

      if( nkeep < nvox ){
         for( jj=0 ; jj < nkeep ; jj++ ) tkeep[jj] = var[keep[jj]] ;
      } else {
         tkeep = var ;
      }

      /*- compute correlation -*/

      switch( method ){
        default:
        case SPEARMAN:
           corr[iv] = 1.0-spearman_rank_corr(nkeep,tkeep,rv,mkeep) ; break ;

        case QUADRANT:
           corr[iv] = 1.0-quadrant_corr(nkeep,tkeep,rv,mkeep) ; break ;
      }

      mri_free(tsim) ; DSET_unload_one(dset,iv) ;

      if( !do_range ) printf( "%g\n" , corr[iv] ) ;

   } /* end of loop over sub-bricks */

   /*-- now compute median and MAD of corr[] --*/

   qmedmad_float( nvals,corr , &cmed,&cmad ) ;

   cbot = cmed - 3.5*cmad ;
   ctop = cmed + 3.5*cmad ; if( cbot < 0.0 ) cbot = 0.0 ;

   /*-- now print results (if not already out) --*/

   if( do_range ){
      for( iv=0 ; iv < nvals ; iv++ )
         printf( "%g  %g  %g\n", corr[iv] , cbot,ctop ) ;
   }

   fprintf(stderr,"++ Median=%g  Median-3.5*MAD=%g  Median+3.5*MAD=%g\n",
           cmed,cbot,ctop) ;
   exit(0) ;
}
