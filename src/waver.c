#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mrilib.h"

#ifndef PI
#  define PI 3.14159265358979323846
#endif

double ztone( double x ) ;
double waveform( double t ) ;
double waveform_GAM( double t ) ;
double waveform_WAV( double t ) ;
void Process_Options( int c , char * a[] ) ;
void Syntax(void) ;

/*----------------------------------------------------------------
  Function that transitions from 0 to 1 over input x in [0,1].
------------------------------------------------------------------*/

#define ZT_FAC 0.50212657
#define ZT_ADD 0.99576486

double ztone( double x )
{
   double y , z ;

   if( x < 0.0 ) return 0.0 ;
   if( x > 1.0 ) return 1.0 ;

   y = (0.5*PI) * ( 1.6 * x - 0.8 ) ;
   z = ZT_FAC * ( tanh(tan(y)) + ZT_ADD ) ;
   return z ;
}

/*-----------------------------------------------------------------
  Given t in seconds, return the impulse response waveform
-------------------------------------------------------------------*/

#define WAV_TYPE 1
#define GAM_TYPE 2

static int    waveform_type    = WAV_TYPE ;

static double WAV_delay_time   =  2.0 ,
              WAV_rise_time    =  4.0 ,
              WAV_fall_time    =  6.0 ,
              WAV_undershoot   =  0.2 ,
              WAV_restore_time =  2.0  ;

static double WAV_rise_start   = -666.0 ,
              WAV_fall_start   = -666.0 ,
              WAV_fall_end     = -666.0 ,
              WAV_restore_end  = -666.0  ;

static double GAM_power        = 8.6 ;
static double GAM_time         = 0.547 ;
static double GAM_ampl         = 0.0 ;

double waveform( double t )
{
   switch( waveform_type ){

      default:
      case WAV_TYPE: return waveform_WAV(t) ;

      case GAM_TYPE: return waveform_GAM(t) ;
   }
   return 0.0 ;  /* unreachable */
}

double waveform_GAM( double t )
{
   if( GAM_ampl == 0.0 ){
      GAM_ampl = exp(GAM_power) / pow(GAM_power*GAM_time,GAM_power) ;
   }

   if( t <= 0.0 ) return 0.0 ;

   return GAM_ampl * pow(t,GAM_power) * exp(-t/GAM_time) ;
}

double waveform_WAV( double t )
{
   if( WAV_rise_start < 0.0 ){
      WAV_rise_start   = WAV_delay_time ;
      WAV_fall_start   = WAV_rise_start + WAV_rise_time ;
      WAV_fall_end     = WAV_fall_start + WAV_fall_time ;
      WAV_restore_end  = WAV_fall_end   + WAV_restore_time ;
   }

   if( t < WAV_rise_start )
      return 0.0 ;

   if( t < WAV_fall_start )
      return ztone( (t-WAV_rise_start)/WAV_rise_time ) ;

   if( t < WAV_fall_end )
      return (1.0+WAV_undershoot) * ztone( (WAV_fall_end-t)/WAV_fall_time )
             - WAV_undershoot ;

   if( t < WAV_restore_end )
      return -WAV_undershoot * ztone( (WAV_restore_end-t)/WAV_restore_time ) ;

   return 0.0 ;
}

static double WAV_peak = 100.0 ;
static double WAV_dt   =   0.1 ;

static int      IN_npts = -666 ;
static double * IN_ts   = NULL ;

static int      WAV_duration = -666.0 ;
static int      WAV_npts     = -666 ;
static double * WAV_ts       = NULL ;

static int      OUT_xy   = 0 ;
static int      OUT_npts = -666 ;
static double * OUT_ts   = NULL ;

int main( int argc , char * argv[] )
{
   int ii , jj ;
   double val ;

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) Syntax() ;

   Process_Options( argc , argv ) ;

   /*---- compute duration of sampled waveform ----*/

   switch( waveform_type ){

      /* simple for the Cox function */

      default:
      case WAV_TYPE:
         WAV_duration = WAV_delay_time + WAV_rise_time + WAV_fall_time
                       + ( (WAV_undershoot != 0.0) ? WAV_restore_time : 0.0 ) ;
      break ;

      /* slightly complicated for the gamma variate */

      case GAM_TYPE:{
         double bal = 5.5/GAM_power + 1.0 ;
         double al  = bal ;
         double lal = log(al) ;

         while( al < bal + lal ){ al = bal + 1.1*lal ; lal = log(al) ; }

         WAV_duration = al * GAM_power * GAM_time ;
      }
      break ;
   }

   /*---- compute sampled waveform ----*/

   WAV_npts = (int)(1 + ceil(WAV_duration/WAV_dt)) ;
   WAV_ts   = (double *) malloc( sizeof(double) * WAV_npts ) ;

   for( ii=0 ; ii < WAV_npts ; ii++ )
      WAV_ts[ii] = WAV_peak * waveform( WAV_dt * ii ) ;

   /*---- if no input timeseries, just output waveform ----*/

   if( IN_npts < 1 ){
      if( OUT_xy ){
         for( ii=0 ; ii < WAV_npts ; ii++ )
            printf( "%g %g\n" , WAV_dt * ii , WAV_ts[ii] ) ;
      } else {
         for( ii=0 ; ii < WAV_npts ; ii++ )
            printf( "%g\n" , WAV_ts[ii] ) ;
      }
      exit(0) ;
   }

   /*---- must convolve input with waveform ----*/

   OUT_npts = IN_npts + WAV_npts ;
   OUT_ts   = (double *) malloc( sizeof(double) * OUT_npts ) ;

   for( ii=0 ; ii < OUT_npts ; ii++ ) OUT_ts[ii] = 0.0 ;

   for( jj=0 ; jj < IN_npts ; jj++ ){
      val = IN_ts[jj] ;
      if( val == 0.0 || fabs(val) >= 33333.0 ) continue ;
      for( ii=0 ; ii < WAV_npts ; ii++ )
         OUT_ts[ii+jj] += val * WAV_ts[ii] ;
   }

   for( jj=0 ; jj < IN_npts ; jj++ ){
      val = IN_ts[jj] ;
      if( fabs(val) >= 33333.0 ) OUT_ts[jj] = 99999.0 ;
   }

   if( OUT_xy ){
      for( ii=0 ; ii < OUT_npts ; ii++ )
            printf( "%g %g\n" , WAV_dt * ii , OUT_ts[ii] ) ;
   } else {
      for( ii=0 ; ii < OUT_npts ; ii++ )
            printf( "%g\n" , OUT_ts[ii] ) ;
   }

   exit(0) ;
}

void Syntax(void)
{
   printf(
    "Usage: waver [options] > output_filename\n"
    "Creates an ideal waveform timeseries file.\n"
    "The output goes to stdout, and normally would be redirected to a file.\n"
    "\n"
    "Options: (# refers to a number; [xx] is the default value)\n"
    "  -WAV           = Sets waveform to Cox special          [default]\n"
    "  -GAM           = Sets waveform to form t^b * exp(-t/c)\n"
    "                   (waveforms will also be chosen if\n"
    "                    one of the options below is used)\n"
    "These options set parameters for the -WAV waveform.\n"
    "  -delaytime #   = Sets delay time to # seconds                [2]\n"
    "  -risetime #    = Sets rise time to # seconds                 [4]\n"
    "  -falltime #    = Sets fall time to # seconds                 [6]\n"
    "  -undershoot #  = Sets undershoot to # times the peak         [0.2]\n"
    "                     (this should be a nonnegative factor)\n"
    "  -restoretime # = Sets time to restore from undershoot        [2]\n"
    "These options set parameters for the -GAM waveform:\n"
    "  -gamb #        = Sets the parameter 'b' to #                 [8.6]\n"
    "  -gamc #        = Sets the parameter 'c' to #                 [0.547]\n"
    "These options apply to any waveform type:\n"
    "  -peak #        = Sets peak value to #                        [100]\n"
    "  -dt #          = Sets time step of output AND input          [0.1]\n"
    "\n"
    "The default is just to output the waveform defined by the parameters\n"
    "above.  If an input file is specified by one the options below, then\n"
    "the timeseries defined by that file will be convolved with the ideal\n"
    "waveform defined above -- that is, each nonzero point in the input\n"
    "timeseries will generate a copy of the waveform starting at that point\n"
    "in time, with the amplitude scaled by the input timeseries value.\n"
    "\n"
    "  -xyout         = Output data in 2 columns:\n"
    "                     1=time 2=waveform (useful for graphing)\n"
    "                     [default is 1 column=waveform]\n"
    "\n"
    "  -input infile  = Read timeseries from 'infile';\n"
    "                     convolve with waveform to produce output\n"
    "\n"
    "  -inline DATA   = Read timeseries from command line DATA;\n"
    "                     convolve with waveform to produce output\n"
    "                     DATA is in the form of numbers and\n"
    "                     count@value, as in\n"
    "                     -inline 20@0.0 5@1.0 30@0.0 1.0 20@0.0 2.0\n"
    "     which means a timeseries with 20 zeros, then 5 ones, then 30 zeros,\n"
    "     a single 1, 20 more zeros, and a final 2.\n"
    "     [The '@' character may actually be any of: '@', '*', 'x', 'X'.\n"
    "      Note that * must be typed as \\* to prevent the shell from\n"
    "      trying to interpret it as a filename wildcard.]\n"
    "\n"
    "At least one option is required, or the program will just print this message\n"
    "to stdout.  Only one of the timeseries input options above can be used.\n"
    "\n"
    "If you have the 'xmgr' graphing program, then a useful way to preview the\n"
    "results of this program is through a command pipe like\n"
    "   waver -dt 0.25 -xyout -inline 16@1 40@0 16@1 40@0 | xmgr -source stdin\n"
    "\n"
    "If a square wave is desired, see the 'sqwave' program.\n"
   ) ;
   exit(0) ;
}

#define ERROR \
 do{fprintf(stderr,"Illegal %s option\n",argv[nopt]);Syntax();}while(0)

void Process_Options( int argc , char * argv[] )
{
   int nopt = 1 ;

   while( nopt < argc ){

      if( strncmp(argv[nopt],"-GAM",4) == 0 ){
         waveform_type = GAM_TYPE ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-WAV",4) == 0 ){
         waveform_type = WAV_TYPE ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-gamb",5) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         GAM_power = strtod(argv[nopt+1],NULL) ;
         if( GAM_power <= 0.0 ) ERROR ;
         waveform_type = GAM_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-gamc",5) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         GAM_time = strtod(argv[nopt+1],NULL) ;
         if( GAM_time <= 0.0 ) ERROR ;
         waveform_type = GAM_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-del",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_delay_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_delay_time < 0.0 ) ERROR ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-ris",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_rise_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_rise_time <= 0.0 ) ERROR ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-fal",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_fall_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_fall_time <= 0.0 ) ERROR ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-und",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_undershoot = strtod(argv[nopt+1],NULL) ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-pea",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_peak = strtod(argv[nopt+1],NULL) ;
         if( WAV_peak == 0.0 ) ERROR ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-res",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_restore_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_restore_time <= 0.0 ) ERROR ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-dt",4) == 0 ){
         if( nopt+1 >= argc ) ERROR ;
         WAV_dt = strtod(argv[nopt+1],NULL) ;
         if( WAV_dt <= 0.0 ) ERROR ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-xyo",4) == 0 ){
         OUT_xy = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-inp",4) == 0 ){
         MRI_IMAGE * tsim ;
         float * tsar ;
         int ii ;

         if( IN_npts > 0 ){
            fprintf(stderr,"Cannot input two timeseries!\n") ;
            exit(1) ;
         }

         if( nopt+1 >= argc ) ERROR ;
         tsim = mri_read_ascii( argv[nopt+1] ) ;
         if( tsim == NULL ) ERROR ;

         IN_npts = tsim->ny ;
         IN_ts   = (double *) malloc( sizeof(double) * IN_npts ) ;
         tsar    = MRI_FLOAT_PTR(tsim) ;
         for( ii=0 ; ii < IN_npts ; ii++ ) IN_ts[ii] = tsar[ii] ;
         mri_free(tsim) ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-inl",4) == 0 ){
         int iopt , count , nnn ;
         float value ;
         char sep ;

         if( IN_npts > 0 ){
            fprintf(stderr,"Cannot input two timeseries!\n") ;
            exit(1) ;
         }

         if( nopt+1 >= argc ) ERROR ;
         iopt    = nopt+1 ;
         IN_npts = 0 ;
         IN_ts   = (double *) malloc( sizeof(double) ) ;
         while( iopt < argc && argv[iopt][0] != '-' ){

            if( strstr(argv[iopt],"@") != NULL ||    /* if has one of the    */
                strstr(argv[iopt],"x") != NULL ||    /* allowed separator    */
                strstr(argv[iopt],"X") != NULL ||    /* characters, then     */
                strstr(argv[iopt],"*") != NULL   ){  /* scan for count@value */

               nnn = sscanf( argv[iopt] , "%d%c%f" , &count , &sep , &value ) ;
               if( nnn != 3 || count < 1 ){
                  fprintf(stderr,"Illegal value after -inline: %s\n",argv[iopt]) ;
                  exit(1) ;
               }

            } else {                                 /* just scan for value */
               count = 1 ;
               nnn   = sscanf( argv[iopt] , "%f" , &value ) ;
               if( nnn != 1 ){
                  fprintf(stderr,"Illegal value after -inline: %s\n",argv[iopt]) ;
                  exit(1) ;
               }
            }

            IN_ts = (double *) realloc( IN_ts , sizeof(double) * (IN_npts+count) ) ;
            for( nnn=0 ; nnn < count ; nnn++ )
               IN_ts[nnn+IN_npts] = value ;

            IN_npts += count ; iopt++ ;
         }
         nopt = iopt ; continue ;
      }

      ERROR ;
   }

   return ;
}
