/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "parser.h"
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

double waveform_EXPR( double t ) ;  /* 01 Aug 2001 */

/*----------------------------------------------------------------
  Function that transitions from 0 to 1 over input x in [0,1].
------------------------------------------------------------------*/

#define ZT_FAC 0.50212657
#define ZT_ADD 0.99576486

double ztone( double x )
{
   double y , z ;

   if( x <= 0.0 ) return 0.0 ;
   if( x >= 1.0 ) return 1.0 ;

   y = (0.5*PI) * ( 1.6 * x - 0.8 ) ;
   z = ZT_FAC * ( tanh(tan(y)) + ZT_ADD ) ;
   return z ;
}

/*-----------------------------------------------------------------
  Given t in seconds, return the impulse response waveform
-------------------------------------------------------------------*/

#define WAV_TYPE  1
#define GAM_TYPE  2
#define EXPR_TYPE 3  /* 01 Aug 2001 */

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

static PARSER_code * EXPR_pcode = NULL ;  /* 01 Aug 2001 */
static double        EXPR_fac   = 1.0  ;

double waveform( double t )
{
   switch( waveform_type ){

      default:
      case WAV_TYPE:  return waveform_WAV(t) ;

      case GAM_TYPE:  return waveform_GAM(t) ;

      case EXPR_TYPE: return waveform_EXPR(t);  /* 01 Aug 2001 */
   }
   return 0.0 ;  /* unreachable */
}

#define TT 19
double waveform_EXPR( double t )  /* 01 Aug 2001 */
{
   static int first=1 ;
   static double atoz[26] ;

   if( t < 0.0 ) return 0.0 ;     /* 02 Aug 2001: oops */

   if( first ){
      int ii ;
      for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = 0.0 ;
      first =0 ;
   }
   atoz[TT] = t ;
   return (EXPR_fac * PARSER_evaluate_one(EXPR_pcode,atoz) ) ;
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

static int      OUT_numout = -666 ;    /* 08 Apr 2002 */

static int      IN_num_tstim = -666 ;  /* 16 May 2001 = #8 */
static double * IN_tstim     = NULL ;
static double   IN_top_tstim = 0.0  ;

int main( int argc , char * argv[] )
{
   int ii , jj ;
   double val ;

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) Syntax() ;

   machdep(); if(argc > 1) AFNI_logger("waver",argc,argv) ;

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

      /* 01 Aug 2001: yet more complicated for the EXPR type */

#define FPASS 500
#define SPASS 10000
#define STHR  5
      case EXPR_TYPE:{
         double val , vtop=0.0 , vthr ;
         int itop=-1 , icount ;
         for( ii=0 ; ii < FPASS ; ii++ ){
            val = waveform_EXPR( WAV_dt * ii ) ; val = fabs(val) ;
            if( val > vtop ){ vtop = val ; itop = ii ; }
         }
         if( itop < 0 ){
            fprintf(stderr,"** -EXPR is 0 for 1st %d points!\n",FPASS);
            exit(1) ;
         }
         vthr = 0.01 * vtop ;
         for( icount=0,ii=itop+1 ; ii < SPASS && icount < STHR ; ii++ ){
            val = waveform_EXPR( WAV_dt * ii ) ; val = fabs(val) ;
            if( val <= vthr ) icount++ ;
            else              icount=0 ;
         }
         if( ii == SPASS && icount < STHR ){
            fprintf(stderr,"** -EXPR doesn't decay away in %d points!\n",SPASS);
            exit(1) ;
         }
         WAV_duration = WAV_dt * ii ;

         if( WAV_peak != 0.0 ) EXPR_fac = WAV_peak / vtop ;
         WAV_peak = 1.0 ;
      }
      break ;
   }

   /*---- compute sampled waveform ----*/

   WAV_npts = (int)(1 + ceil(WAV_duration/WAV_dt)) ;
   WAV_ts   = (double *) malloc( sizeof(double) * WAV_npts ) ;

   for( ii=0 ; ii < WAV_npts ; ii++ )
      WAV_ts[ii] = WAV_peak * waveform( WAV_dt * ii ) ;

   /*---- if no input timeseries, just output waveform ----*/

   if( IN_npts < 1 && IN_num_tstim < 1 ){
      int top = WAV_npts ;
      if( OUT_numout > 0 ) top = OUT_numout ;
      if( OUT_xy ){
         for( ii=0 ; ii < top ; ii++ )
            printf( "%g %g\n" , WAV_dt * ii , WAV_ts[ii] ) ;
      } else {
         for( ii=0 ; ii < top ; ii++ )
            printf( "%g\n" , WAV_ts[ii] ) ;
      }
      exit(0) ;
   }

   /*---- must convolve input with waveform ----*/

   if( IN_npts > 0 ){
      OUT_npts = IN_npts + WAV_npts ;
      if( OUT_numout > 0 ) OUT_npts = OUT_numout ;
      OUT_ts   = (double *) malloc( sizeof(double) * OUT_npts ) ;
      for( ii=0 ; ii < OUT_npts ; ii++ ) OUT_ts[ii] = 0.0 ;

      for( jj=0 ; jj < IN_npts ; jj++ ){
         val = IN_ts[jj] ;
         if( val == 0.0 || fabs(val) >= 33333.0 ) continue ;
         for( ii=0 ; ii < WAV_npts && ii+jj < OUT_npts ; ii++ )
            OUT_ts[ii+jj] += val * WAV_ts[ii] ;
      }

      for( jj=0 ; jj < IN_npts ; jj++ ){
         val = IN_ts[jj] ;
         if( fabs(val) >= 33333.0 ) OUT_ts[jj] = 99999.0 ;
      }

   } else if( IN_num_tstim > 0 ){  /* 16 May 2001 */
      int ibot,itop ;

      OUT_npts = ceil(IN_top_tstim/WAV_dt) + WAV_npts ;
      if( OUT_numout > 0 ) OUT_npts = OUT_numout ;
      OUT_ts   = (double *) malloc( sizeof(double) * OUT_npts ) ;
      for( ii=0 ; ii < OUT_npts ; ii++ ) OUT_ts[ii] = 0.0 ;

      for( jj=0 ; jj < IN_num_tstim ; jj++ ){
        ibot = (int) (IN_tstim[jj]/WAV_dt) ;    /* may be 1 too early */
        itop = ibot + WAV_npts ;
        if( itop > OUT_npts ) itop = OUT_npts ;
        for( ii=ibot ; ii < itop ; ii++ ){
           val = WAV_peak * waveform( WAV_dt * ii - IN_tstim[jj] ) ;
           OUT_ts[ii] += val ;
        }
      }
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
    "  -WAV = Sets waveform to Cox special                    [default]\n"
    "           (cf. AFNI FAQ list for formulas)\n"
    "  -GAM = Sets waveform to form t^b * exp(-t/c)\n"
    "           (cf. Mark Cohen)\n"
    "\n"
    "  -EXPR \"expression\" = Sets waveform to the expression given,\n"
    "                         which should depend on the variable 't'.\n"
    "     e.g.: -EXPR \"step(t-2)*step(12-t)*(t-2)*(12-t)\"\n"
    "     N.B.: The peak value of the expression on the '-dt' grid will\n"
    "           be scaled to the value given by '-peak'; if this is not\n"
    "           desired, set '-peak 0', and the 'natural' peak value of\n"
    "           the expression will be used.\n"
    "\n"
    "These options set parameters for the -WAV waveform.\n"
    "  -delaytime #   = Sets delay time to # seconds                [2]\n"
    "  -risetime #    = Sets rise time to # seconds                 [4]\n"
    "  -falltime #    = Sets fall time to # seconds                 [6]\n"
    "  -undershoot #  = Sets undershoot to # times the peak         [0.2]\n"
    "                     (this should be a nonnegative factor)\n"
    "  -restoretime # = Sets time to restore from undershoot        [2]\n"
    "\n"
    "These options set parameters for the -GAM waveform:\n"
    "  -gamb #        = Sets the parameter 'b' to #                 [8.6]\n"
    "  -gamc #        = Sets the parameter 'c' to #                 [0.547]\n"
    "\n"
    "These options apply to all waveform types:\n"
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
    "  -input infile  = Read timeseries from *.1D formatted 'infile';\n"
    "                     convolve with waveform to produce output\n"
    "              N.B.: you can use a sub-vector selector to choose\n"
    "                    a particular column of infile, as in\n"
    "                      -input 'fred.1D[3]'\n"
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
    "  -tstim DATA    = Read discrete stimulation times from the command line\n"
    "                     and convolve the waveform with delta-functions at\n"
    "                     those times.  In this input format, the times do\n"
    "                     NOT have to be at intervals of '-dt'.  For example\n"
    "                       -dt 2.0 -tstim 5.6 9.3 13.7 16.4\n"
    "                     specifies a TR of 2 s and stimuli at 4 times\n"
    "                     (5.6 s, etc.) that do not correspond to integer\n"
    "                     multiples of TR.  DATA values cannot be negative.\n"
    "                   If the DATA is stored in a file, you can read it\n"
    "                     onto the command line using something like\n"
    "                       -tstim `cat filename`\n"
    "                     where using the backward-single-quote operator\n"
    "                     of the usual Unix shells.\n"
    "\n"
    "  -when DATA     = Read time blocks when stimulus is 'on' (=1) from the\n"
    "                     command line and convolve the waveform with with\n"
    "                     a zero-one input.  For example:\n"
    "                       -when 20..40 60..80\n"
    "                     means that the stimulus function is 1.0 for time\n"
    "                     steps number 20 to 40, and 60 to 80 (inclusive),\n"
    "                     and zero otherwise.  (The first time step is\n"
    "                     numbered 0.)\n"
    "\n"
    "  -numout NN     = Output a timeseries with NN points; if this option\n"
    "                     is not given, then enough points are output to\n"
    "                     let the result tail back down to zero.\n"
    "\n"
    "At least one option is required, or the program will just print this message\n"
    "to stdout.  Only one of the 3 timeseries input options above can be used.\n"
    "\n"
    "If you have the 'xmgr' graphing program, then a useful way to preview the\n"
    "results of this program is through a command pipe like\n"
    "   waver -dt 0.25 -xyout -inline 16@1 40@0 16@1 40@0 | xmgr -source stdin\n"
    "Using the cruder AFNI package program 1dplot, you can do something like:\n"
    "   waver -GAM -tstim 0 7.7 | 1dplot -stdin\n"
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

      if( strncmp(argv[nopt],"-EXPR",4) == 0 ){  /* 01 Aug 2001 */
         waveform_type = EXPR_TYPE ;
         if( EXPR_pcode != NULL ){
            fprintf(stderr,"** Can't have 2 -EXPR options!\n") ;
            exit(1) ;
         }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -EXPR needs an argument!\n") ; exit(1) ;
         }
         EXPR_pcode = PARSER_generate_code( argv[nopt] ) ;  /* compile */
         if( EXPR_pcode == NULL ){
            fprintf(stderr,"** Illegal -EXPR expression!\n") ;
            exit(1) ;
         }
         if( !PARSER_has_symbol("T",EXPR_pcode) ){
            fprintf(stderr,"** -EXPR expression doesn't use variable 't'!\n");
            exit(1) ;
         }
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

         if( IN_npts > 0 || IN_num_tstim > 0 ){
            fprintf(stderr,"Cannot input two timeseries!\n") ;
            exit(1) ;
         }

         if( nopt+1 >= argc ) ERROR ;
         tsim = mri_read_1D( argv[nopt+1] ) ;
         if( tsim == NULL ) ERROR ;

         IN_npts = tsim->nx ;
         IN_ts   = (double *) malloc( sizeof(double) * IN_npts ) ;
         tsar    = MRI_FLOAT_PTR(tsim) ;
         for( ii=0 ; ii < IN_npts ; ii++ ) IN_ts[ii] = tsar[ii] ;
         mri_free(tsim) ;
         nopt++ ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-tstim") == 0 ){  /* 16 May 2001 */
         int iopt , nnn ;
         float value ;

         if( IN_num_tstim > 0 || IN_npts > 0 ){
            fprintf(stderr,"Cannot input two timeseries!\n") ;
            exit(1) ;
         }
         if( nopt+1 >= argc ) ERROR ;

         iopt         = nopt+1 ;
         IN_num_tstim = 0 ;
         IN_tstim     = (double *) malloc( sizeof(double) ) ;
         while( iopt < argc && argv[iopt][0] != '-' ){

            nnn = sscanf( argv[iopt] , "%f" , &value ) ;
            if( nnn != 1 || value < 0.0 ){
               fprintf(stderr,"Illegal value after -tstim: %s\n",argv[iopt]) ;
               exit(1) ;
            }

            IN_tstim = (double *)realloc(IN_tstim,sizeof(double)*(IN_num_tstim+1));
            IN_tstim[IN_num_tstim++] = value ;
            if( value > IN_top_tstim ) IN_top_tstim = value ;
            iopt++ ;
         }
         nopt = iopt ; continue ;
      }

      if( strncmp(argv[nopt],"-inl",4) == 0 ){
         int iopt , count , nnn ;
         float value ;
         char sep ;

         if( IN_npts > 0 || IN_num_tstim > 0 ){
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

      if( strcmp(argv[nopt],"-when") == 0 ){   /* 08 Apr 2002 */
         int iopt , bot,top , nn , nbt,*bt , count=0 , ii,kk ;
         float value ;
         char sep ;

         if( IN_npts > 0 || IN_num_tstim > 0 ){
            fprintf(stderr,"Cannot input two timeseries!\n") ;
            exit(1) ;
         }

         if( nopt+1 >= argc ) ERROR ;
         iopt = nopt+1 ;
         nbt  = 0 ;
         bt   = (int *) malloc(sizeof(int)) ;
         while( iopt < argc && argv[iopt][0] != '-' ){

            /* scan for value..value */

            bot = top = -1 ;
            nn = sscanf( argv[iopt],"%d..%d",&bot,&top) ;
            if( nn < 1 || bot < 0 ){
              fprintf(stderr,"Illegal value after -when: %s\n",argv[iopt]) ;
              exit(1) ;
            }
            if( nn == 1 ){
              top = bot ;
            } else if( top < bot ){
              fprintf(stderr,"Illegal value after -when: %s\n",argv[iopt]) ;
              exit(1) ;
            }

            /* save (bot,top) pairs in bt */

            bt = (int *) realloc( bt , sizeof(int)*(nbt+1)*2 ) ;
            bt[2*nbt  ] = bot ;
            bt[2*nbt+1] = top ; nbt++ ;
            if( count < top ) count = top ;
            iopt++ ;
         }

         if( nbt < 1 ){
            fprintf(stderr,"No ranges after -when?\n") ; exit(1) ;
         }

         IN_npts = count+1 ;
         IN_ts   = (double *) malloc( sizeof(double) * IN_npts ) ;
         for( ii=0 ; ii < IN_npts ; ii++ ) IN_ts[ii] = 0.0 ;

         for( kk=0 ; kk < nbt ; kk++ ){
            bot = bt[2*kk] ; top = bt[2*kk+1] ;
            for( ii=bot ; ii <= top ; ii++ ) IN_ts[ii] = 1.0 ;
         }

         free(bt) ; nopt = iopt ; continue ;
      }

      if( strcmp(argv[nopt],"-numout") == 0 ){   /* 08 Apr 2002 */
         int val = -1 ;
         if( nopt+1 >= argc ) ERROR ;
         sscanf(argv[nopt+1],"%d",&val) ;
         if( val <= 1 ){
           fprintf(stderr,"Illegal value after -numout: %s\n",argv[nopt]);
           exit(1);
         }
         OUT_numout = val ;
         nopt++ ; nopt++ ; continue ;
      }

      ERROR ;
   }

   if( WAV_peak == 0.0 && waveform_type != EXPR_TYPE ){
      fprintf(stderr,"** Illegal -peak 0 for non-EXPR waveform type!\n") ;
      exit(1) ;
   }

   return ;
}
