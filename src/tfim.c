/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "mrilib.h"

/*** global data (gasp, shock, horror) ***/

#define MAX_NAME    64
#define SUFFIX_DIFF "diff"
#define SUFFIX_TSPM "tspm"
#define SUFFIX_CORR "corr"

static char        TF_pname[MAX_NAME] = "tfim." ;
static MRI_IMARR * TF_set1            = NULL ;
static MRI_IMARR * TF_set2            = NULL ;
static float       TF_bval            = 0.0 ;
static int         TF_use_bval        = 0 ;
static float       TF_pthresh         = 0.0 ;
static int         TF_eqcorr          = 0 ;
static float       TF_eqval           = 0.0 ;
static int         TF_paired          = 0 ;

/*** prototypes ***/

double qginv( double ) ;            /* stat functions */
double stas4( double , double ) ;
double stinv( double , double ) ;

void TFIM_syntax( char * ) ;
void TFIM_getopts( int , char * argv[] ) ;

/*** actual program code ***/

int main( int argc , char * argv[] )
{
   int kk , ii,nx,ny,npix , num1,num2 , zerout ;
   MRI_IMAGE ** stat_ret ;
   MRI_IMAGE * avim1 , * sdim1 , * avim2 , * sdim2 ;
   float     * avar1 , * sdar1 , * avar2 , * sdar2 ;
   MRI_IMAGE * difim , * tspim , * corim , * dofim ;
   float     * difar , * tspar , * corar , * dofar ;
   char name[256] ;
   float sdmax , dofbar ;

   /*----- read inputs -----*/

   printf(
    "MCW TFIM: t-tests on sets of functional images, by RW Cox\n") ;

   machdep() ;

   if( argc < 2 ) TFIM_syntax("try tfim -help for usage") ;
   else if( strcmp(argv[1],"-help") == 0 ) TFIM_syntax(NULL) ;

   TFIM_getopts( argc , argv ) ;

#ifdef TFIM_DEBUG
   printf("prefix = %s\n",TF_pname) ;
   if( TF_use_bval == 1 ){
      printf("bval = %f\n",TF_bval) ;
   } else {
      printf("set1 has %d images\n",TF_set1->num) ;
   }
   printf("set2 has %d images\n",TF_set2->num) ;
#endif

   /*----- form averages of images -----*/

   nx = TF_set2->imarr[0]->nx ;
   ny = TF_set2->imarr[0]->ny ; npix = nx * ny ;

   if( TF_set1 != NULL ){
      num1 = TF_set1->num ;
      for( kk=0 ; kk < num1 ; kk++ ){
         (void) mri_stat_seq( TF_set1->imarr[kk] ) ;
         mri_free( TF_set1->imarr[kk] ) ;
      }
      stat_ret = mri_stat_seq( NULL ) ;
      avim1    = stat_ret[0] ; avar1 = mri_data_pointer( avim1 ) ;
      sdim1    = stat_ret[1] ; sdar1 = mri_data_pointer( sdim1 ) ;
   }

   num2 = TF_set2->num ;
   for( kk=0 ; kk < num2 ; kk++ ){
      (void) mri_stat_seq( TF_set2->imarr[kk] ) ;
      mri_free( TF_set2->imarr[kk] ) ;
   }
   stat_ret = mri_stat_seq( NULL ) ;
   avim2    = stat_ret[0] ; avar2 = mri_data_pointer( avim2 ) ;
   sdim2    = stat_ret[1] ; sdar2 = mri_data_pointer( sdim2 ) ;

   /*----- process set averages into statistics -----*/

   difim = mri_new( nx,ny , MRI_float ) ; difar = mri_data_pointer(difim) ;
   tspim = mri_new( nx,ny , MRI_float ) ; tspar = mri_data_pointer(tspim) ;
   dofim = mri_new( nx,ny , MRI_float ) ; dofar = mri_data_pointer(dofim) ;

   zerout = 0 ;  /* count of pixels zero-ed out due to no variance */

   if( TF_use_bval == 1 ){
      float scl = 1.0 / sqrt((double) num2) ;

      for( ii=0 ; ii < npix ; ii++ ){
         difar[ii] = avar2[ii] - TF_bval ;
         dofar[ii] = num2 - 1 ;
         if( sdar2[ii] > 0 ){
            tspar[ii] = difar[ii] / (scl * sdar2[ii]) ;
         } else {
            tspar[ii] = 0.0 ; zerout++ ;
         }
      }
   } else {
      float q1 , q2 ;
      float n1i =1.0/num1      , n2i =1.0/num2 ,
            n11i=1.0/(num1-1.0), n21i=1.0/(num2-1.0) ;

      for( ii=0 ; ii < npix ; ii++ ){
         difar[ii] = avar2[ii] - avar1[ii] ;
         q1        = SQR(sdar1[ii]) * n1i ;  /* qi = sigi^2 / numi */
         q2        = SQR(sdar2[ii]) * n2i ;
         if( q1 > 0 && q2 > 0 ){
            tspar[ii] = difar[ii] / sqrt(q1+q2) ;
            dofar[ii] = SQR(q1+q2) / ( q1*q1*n11i + q2*q2*n21i ) ;
         } else {
            tspar[ii] = 0.0 ; dofar[ii] = num1+num2-2 ; zerout++ ;
         }
      }
   }

   if( zerout > 0 ){
      printf("** set %d pixels to zero due to 0 variance!\n",zerout) ;
   }

   /** threshold, if desired **/

   if( TF_pthresh > 0.0 ){
      float thr ;
      if( TF_use_bval == 1 ){
         dofbar = dofar[0] ;
         thr    = stinv( TF_pthresh , dofbar ) ;
         printf("-- fixed t-threshold = %g\n",TF_pthresh,thr) ;
      } else {
         dofbar = 0.0 ;
         for( ii=0 ; ii < npix ; ii++ ) dofbar += dofar[ii] ;
         dofbar /= npix ;
         thr     = stinv( TF_pthresh , dofbar ) ;
         printf("-- variable t-threshold about %g (dof mean = %g)\n",
                thr,dofbar) ;
      }

      for( ii=0 ; ii < npix ; ii++ ){
         if( TF_use_bval == -1 ) thr = stinv( TF_pthresh , dofar[ii] ) ;
         if( fabs(tspar[ii]) < thr ) difar[ii] = 0.0 ;
      }
   }

   /*----- write output images -----*/

   sprintf( name , "%s%s" , TF_pname , SUFFIX_DIFF ) ;
   printf("-- writing difference file %s\n",name) ;
   mri_write( name , difim ) ;

   sprintf( name , "%s%s" , TF_pname , SUFFIX_TSPM ) ;
   printf("-- writing t-statistic file %s\n",name) ;
   mri_write( name , tspim ) ;

   if( TF_eqcorr ){
      float pth , thr , cth , doff ;

      corim = mri_new( nx , ny , MRI_float ) ;
      corar = mri_data_pointer( corim ) ;
      for( ii=0 ; ii < npix ; ii++ ){
         doff      = (TF_eqval > 0) ? TF_eqval : dofar[ii] ;
         corar[ii] = tspar[ii] / sqrt( doff + SQR(tspar[ii]) ) ;
      }

      sprintf( name , "%s%s" , TF_pname , SUFFIX_CORR ) ;
      printf("-- writing 'correlation' file %s\n",name) ;
      mri_write( name , corim ) ;

      pth  = (TF_pthresh > 0) ? TF_pthresh : 0.001 ;
      doff = (TF_eqval > 0) ? TF_eqval : dofbar ;
      thr  = stinv( pth , doff ) ;
      cth  = thr / sqrt( doff + SQR(thr) ) ;
      printf("-- 'correlation' threshold for p=%g is %g\n", pth,cth ) ;
   }

   exit(0) ;
}

/*---------------------------------------------------------------------*/

void TFIM_getopts( int argc , char * argv[] )
{
   int nopt = 1 , kk , nx,ny ;

   /*--- scan argument list ---*/

   while( nopt < argc ){

      /** -paired **/

      if( strncmp(argv[nopt],"-paired",5) == 0 ){
         TF_paired = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -prefix pname **/

      if( strncmp(argv[nopt],"-prefix",5) == 0 ){
         if( ++nopt >= argc ) TFIM_syntax("-prefix needs a name!") ;
         strcpy( TF_pname , argv[nopt] ) ;
         kk = strlen(TF_pname) ;
         if( TF_pname[kk-1] != '.' ){
            TF_pname[kk]   = '.' ;
            TF_pname[kk+1] = '\0' ;
         }
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -pthresh pval **/

      if( strncmp(argv[nopt],"-pthresh",5) == 0 ){
         char * ch ;

         if( ++nopt >= argc ) TFIM_syntax("-pthresh needs a value!");

         TF_pthresh = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || TF_pthresh <= 0.0 || TF_pthresh > 0.99999 )
            TFIM_syntax("value after -pthresh is illegal!") ;

         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -eqcorr dval **/

      if( strncmp(argv[nopt],"-eqcorr",5) == 0 ){
         char * ch ;

         if( ++nopt >= argc ) TFIM_syntax("-eqcorr needs a value!");

         TF_eqval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || TF_eqval < 0.0 )
            TFIM_syntax("value after -eqcorr is illegal!") ;

         TF_eqcorr = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** after this point, the options are no longer 'free floating' **/

      /** -base1 bval **/

      if( strncmp(argv[nopt],"-base1",5) == 0 ){
         char * ch ;

         if( ++nopt >= argc )    TFIM_syntax("-base1 needs a value!");
         if( TF_use_bval == -1 ) TFIM_syntax("-base1 with -set1 illegal!");

         TF_bval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' ) TFIM_syntax("value after -base1 is illegal!") ;

         TF_use_bval = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -set1 file file ... **/

      if( strncmp(argv[nopt],"-set1",5) == 0 ){
         if( TF_use_bval == 1 ) TFIM_syntax("-set1 with -base1 illegal!");
         TF_use_bval = -1 ;

         for( kk=nopt+1 ; kk < argc ; kk++ )
            if( strncmp(argv[kk],"-set2",5) == 0 ) break ;

         if( kk >= argc )     TFIM_syntax("-set1 not followed by -set2") ;
         if( kk-1-nopt <= 0 ) TFIM_syntax("-set1 has no files after it") ;

         TF_set1 = mri_read_many_files( kk-1-nopt , argv+(nopt+1) ) ;
         if( TF_set1 == NULL )
            TFIM_syntax("cannot continue without -set1 images") ;

         nopt = kk ; continue ; /* skip to arg that matched -set2 */
      }

      /** -set2 file file ... */

      if( strncmp(argv[nopt],"-set2",5) == 0 ){
         if( ++nopt >= argc ) TFIM_syntax("-set2 has not files after it") ;

         TF_set2 = mri_read_many_files( argc-nopt , argv+nopt ) ;
         if( TF_set2 == NULL )
            TFIM_syntax("cannot continue without -set2 images") ;

         break ;  /* end of possible inputs */
      }

      /** get to here is bad news! **/

      fprintf(stderr,"*** can't interpret this option: %s\n",argv[nopt]) ;
      TFIM_syntax("try tfim -help for usage details") ;
   }

   /*--- check arguments for OK-ositiness ---*/

   if( TF_use_bval == -1 &&
       ( TF_set1 == NULL || TF_set1->num < 2 ) )
      TFIM_syntax("-set1 has too few files in it!") ;

   if( TF_set2 == NULL || TF_set2->num < 2 )
      TFIM_syntax("-set2 has too few files in it!") ;

   if( TF_use_bval == 1 && TF_paired == 1 )
      TFIM_syntax("-paired and -base1 are mutually exclusive!") ;

   if( TF_paired == 1 && TF_set1 == NULL )
      TFIM_syntax("-paired requires presence of -set1!") ;

   if( TF_paired == 1 && TF_set1->num != TF_set2->num ){
      char str[256] ;
      sprintf(str,"-paired requires equal size images sets,\n"
                  "but -set1 has %d images and -set2 has %d images" ,
              TF_set1->num , TF_set2->num ) ;
      TFIM_syntax(str) ;
   }

   /* check images for consistency */

   nx = TF_set2->imarr[0]->nx ;
   ny = TF_set2->imarr[0]->ny ;

   for( kk=1 ; kk < TF_set2->num ; kk++ ){
      if( nx != TF_set2->imarr[kk]->nx || ny != TF_set2->imarr[kk]->ny ){
        fprintf(stderr,
          "*** image %d in -set2 not conformant to image 0\n",
          kk) ;
         TFIM_syntax("cannot continue with images whose sizes differ!") ;
      }
   }

   if( TF_set1 != NULL ){
      for( kk=0 ; kk < TF_set1->num ; kk++ ){
         if( nx != TF_set1->imarr[kk]->nx || ny != TF_set1->imarr[kk]->ny ){
           fprintf(stderr,
             "*** image %d in -set1 not conformant to image 0 in -set2\n",
             kk) ;
            TFIM_syntax("cannot continue with images whose sizes differ!") ;
         }
      }
   }

   /* if paired t-test, subtract set1 from set2
      to convert it into the equivalent base level test vs. 0 */

   if( TF_paired == 1 ){
      MRI_IMAGE * im1 , * im2 ;
      float     * ar1 , * ar2 ;
      int ii , npix = nx * ny ;

      for( kk=0 ; kk < TF_set1->num ; kk++ ){
         im1 = mri_to_float(TF_set1->imarr[kk]) ; mri_free(TF_set1->imarr[kk]) ;
         im2 = mri_to_float(TF_set2->imarr[kk]) ; mri_free(TF_set2->imarr[kk]) ;
         ar1 = mri_data_pointer(im1) ; ar2 = mri_data_pointer(im2) ;
         for( ii=0 ; ii < npix ; ii++ ) ar2[ii] -= ar1[ii] ;
         mri_free(im1) ; TF_set2->imarr[kk] = im2 ;
      }

      FREE_IMARR( TF_set1 ) ; TF_set1 = NULL ;
      TF_use_bval = 1 ;
      TF_bval     = 0.0 ;
   }

   return ;
}
/*---------------------------------------------------------------------*/

void TFIM_syntax( char * str )
{
   if( str != NULL ){
      fprintf(stderr,"*** %s\n",str) ;
      exit(-1) ;
   }

   printf(
   "\n"
   "Usage 1: tfim [options] -set1 image_files ... -set2 image_files ...\n"
   "Usage 2: tfim [options] -base1 bval -set2 image_files ...\n"
   "\n"
   "In usage 1, the collection of images files after '-set1' and the\n"
   "collection after '-set2' are averaged and differenced, and the\n"
   "difference is tested for significance with a 2 sample Student t-test.\n"
   "\n"
   "In usage 2, the collection of image files after '-set2' is averaged\n"
   "and then has the constant numerical value 'bval' subtracted, and the\n"
   "difference is tested for significance with a 1 sample Student t-test.\n"
   "\n"
   "N.B.: The input images can be in the usual 'short' or 'byte'\n"
   "      formats, or in the floating point 'flim' format.\n"
   "N.B.: If in either set of images, a given pixel has zero variance\n"
   "      (i.e., is constant), then the t-test is not performed.\n"
   "      In that pixel, the .tspm file will be zero.\n"
   "\n"
   "Options are:\n"
   "\n"
   " -prefix pname: 'pname' is used as the prefix for the output\n"
   "                  filenames.  The output image files are\n"
   "                   + pname.diff = average of set2 minus average of set1\n"
   "                                  (or minus 'bval')\n"
   "                   + pname.tspm = t-statistic of difference\n"
   "                  Output images are in the 'flim' (floating pt. image)\n"
   "                  format, and may be converted to 16 bit shorts using\n"
   "                  the program 'ftosh'.\n"
   "              *** The default 'pname' is 'tfim', if -prefix isn't used.\n"
   " -pthresh pval: 'pval' is a numeric value between 0 and 1, giving\n"
   "                  the significance level (per voxel) to threshold the\n"
   "                  output with; voxels with (2-sided) t-statistic\n"
   "                  less significant than 'pval' will have their diff\n"
   "                  output zeroed.\n"
   "              *** The default is no threshold, if -pthresh isn't used.\n"
   " -eqcorr dval:  If present, this option means to write out the file\n"
   "                   pname.corr = equivalent correlation statistic\n"
   "                              =  t/sqrt(dof+t^2)\n"
   "                  The number 'dval' is the value to use for 'dof' if\n"
   "                  dval is positive.  This would typically be the total\n"
   "                  number of data images used in forming the image sets,\n"
   "                  if the image sets are from sfim or fim.\n"
   "                  If dval is zero, then dof is computed from the number\n"
   "                  of images in -set1 and -set2; if these are averages\n"
   "                  from program sfim, then dof will be smallish, which in\n"
   "                  turn means that significant corr values will be higher\n"
   "                  than you may be used to from using program fim.\n"
   "              *** The default is not to write, if -eqcorr isn't used.\n"
   " -paired:       If present, this means that -set1 and -set2 should be\n"
   "                  compared using a paired sample t-test.  This option is\n"
   "                  illegal with the -base1 option.  The number of samples\n"
   "                  in the two sets of images must be equal.\n"
   "                  [This test is implemented by subtracting -set1 images\n"
   "                   from the -set2 images, then testing as in '-base1 0'.]\n"
   "              *** The default is to do an unpaired test, if -paired isn't\n"
   "                  used.  In that case, -set1 and -set2 don't need to have\n"
   "                  the same number of images.\n"
   ) ;
   exit(0) ;
}

/*----------------------------------------------------------------------
   code for inverse of central t distribution
   Inputs: p  = double sided tail probability
           nu = degrees of freedom
   Output: T such that P( |t| > T ) = p

   This version is only good for nu >= 5, since it uses the
   approximations in Abramowitz and Stegun, Eq. 26.7.5 (p. 949).
------------------------------------------------------------------------*/

double stinv( double p , double nu )
{
   double xg , t4 ;
   xg = qginv(0.5*p) ;
   t4 = stas4( xg , nu ) ;
   return t4 ;
}

double stas4( double x , double nu)  /* this code generated by Maple */
{
   double t1,t2,t8,t9,t14,t17,t26,t34,t37 ;
   t1 = x*x;
   t2 = t1*x;
   t8 = t1*t1;
   t9 = t8*x;
   t14 = nu*nu;
   t17 = t8*t2;
   t26 = t8*t8;
   t34 = t14*t14;
   t37 = x+(t2/4+x/4)/nu
        +(5.0/96.0*t9+t2/6+x/32)/t14
        +(t17/128+19.0/384.0*t9
        +17.0/384.0*t2-5.0/128.0*x)/t14/nu
        +(79.0/92160.0*t26*x+97.0/11520.0*t17+247.0/15360.0*t9
                                          -t2/48-21.0/2048.0*x)/t34;
   return t37 ;
}

/*** given p, return x such that Q(x)=p, for 0 < p < 1 ***/

double qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 0.0 ){
      dx = 13.0 ;
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

      dt = sqrt( -2.0 * log(dp) ) ;
      dx = dt
           - ((.010328e+0*dt + .802853e+0)*dt + 2.525517e+0)
           /(((.001308e+0*dt + .189269e+0)*dt + 1.432788e+0)*dt + 1.e+0) ;

#if 0
/**  Step 2:  do 3 Newton steps to improve this **/

      for( newt=0 ; newt < 3 ; newt++ ){
         dq  = 0.5e+0 * erfc( dx / 1.414213562373095e+0 ) - dp ;
         ddq = exp( -0.5e+0 * dx * dx ) / 2.506628274631000e+0 ;
         dx  = dx + dq / ddq ;
      }
#endif

      return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}
