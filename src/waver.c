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
double waveform_FILE( double t ) ;  /* 23 Aug 2005 */

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

#undef  FILE_TYPE
#define FILE_TYPE 4  /* 23 Aug 2005 */

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
static double GAM_delay_time   = 0.0 ;

static PARSER_code * EXPR_pcode = NULL ;  /* 01 Aug 2001 */
static double        EXPR_fac   = 1.0  ;

static double FILE_dt ;                   /* 23 Aug 2005 */
static int    FILE_nval ;
static float *FILE_val = NULL ;

/*----------------------------------------------------------------*/

double waveform( double t )
{
   switch( waveform_type ){

      default:
      case WAV_TYPE:  return waveform_WAV(t) ;

      case GAM_TYPE:  return waveform_GAM(t) ;

      case EXPR_TYPE: return waveform_EXPR(t);  /* 01 Aug 2001 */

      case FILE_TYPE: return waveform_FILE(t);  /* 23 Aug 2005 */
   }
   return 0.0 ;  /* unreachable */
}

/*----------------------------------------------------------------*/

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

/*----------------------------------------------------------------*/

double waveform_GAM( double t )
{
   if( GAM_ampl == 0.0 ){
      GAM_ampl = exp(GAM_power) / pow(GAM_power*GAM_time,GAM_power) ;
   }

   if( t-GAM_delay_time <= 0.0 ) return 0.0 ;

   return GAM_ampl * pow((t-GAM_delay_time),GAM_power) * exp(-(t-GAM_delay_time)/GAM_time) ;
}

/*----------------------------------------------------------------*/

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

/*----------------------------------------------------------------*/

double waveform_FILE( double t )   /* 23 Aug 2005 */
{
   int nn ; double tf ;

   if( t < 0.0 ) return 0.0 ;

   tf = t / FILE_dt ;
   nn = (int) tf ;
   tf = tf - (double)nn ;
   if( nn < 0 || nn > FILE_nval ) return 0.0 ;
   if( nn == FILE_nval )
     return (tf < 0.0001) ? (double)FILE_val[nn-1] : 0.0 ;

   return ( (1.0-tf)*FILE_val[nn] + tf*FILE_val[nn+1] ) ;
}

/*----------------------------------------------------------------*/

static double WAV_peak = 100.0 ;
static double WAV_dt   =   0.1 ;

static int      IN_npts = -666 ;
static double * IN_ts   = NULL ;

static double   WAV_duration = -666.0 ;
static int      WAV_npts     = -666 ;
static double * WAV_ts       = NULL ;

static int      OUT_xy   = 0 ;
static int      OUT_npts = -666 ;
static double * OUT_ts   = NULL ;

static int      OUT_numout = -666 ;    /* 08 Apr 2002 */

static int      IN_num_tstim = -666 ;  /* 16 May 2001 = #8 */
static double   IN_top_tstim = 0.0  ;
static double * IN_tstim_a   = NULL ;
static double * IN_tstim_b   = NULL ;  /* 12 May 2003 */
static double * IN_tstim_c   = NULL ;  /* 13 May 2005 (Friday the 13th) */

/*----------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ii , jj ;
   double val ;

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) Syntax() ;

   /* this writes to stdout (default), so no version    06 Jan 2006 [rickr] */
   /* PRINT_VERSION("waver"); */
   machdep(); AFNI_logger("waver",argc,argv);

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
        if( itop < 0 )
          ERROR_exit("-EXPR is 0 for 1st %d points!",FPASS);
        vthr = 0.01 * vtop ;
        for( icount=0,ii=itop+1 ; ii < SPASS && icount < STHR ; ii++ ){
          val = waveform_EXPR( WAV_dt * ii ) ; val = fabs(val) ;
          if( val <= vthr ) icount++ ;
          else              icount=0 ;
        }
        if( ii == SPASS && icount < STHR )
          ERROR_exit("-EXPR doesn't decay away in %d points!",SPASS);
        WAV_duration = WAV_dt * ii ;

        if( WAV_peak != 0.0 ) EXPR_fac = WAV_peak / vtop ;
        WAV_peak = 1.0 ;
      }
      break ;

      /* 23 Aug 2005: not too hard for the FILE type */

      case FILE_TYPE:
        WAV_duration = (FILE_nval+1) * FILE_dt ;
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
#undef  TSTEP
#define TSTEP 10              /* # expansion steps per WAV_dt */
      int ibot,itop , kk ;
      int nts ;
      double dts = WAV_dt/TSTEP , dur , aa ;
      double *tst , *ast ;

      /* setup the output array */

      OUT_npts = ceil(IN_top_tstim/WAV_dt) + WAV_npts ;
      if( OUT_numout > 0 ) OUT_npts = OUT_numout ;
      OUT_ts   = (double *) malloc( sizeof(double) * OUT_npts ) ;
      for( ii=0 ; ii < OUT_npts ; ii++ ) OUT_ts[ii] = 0.0 ;

      /* 12 May 2003: compute how many steps to expand to */

      nts = 0 ;
      for( jj=0 ; jj < IN_num_tstim ; jj++ ){
        dur = IN_tstim_b[jj] - IN_tstim_a[jj] ; dur = MAX(dur,0.0) ;
        ii  = ((int)ceil(dur/dts)) + 1 ;
        nts += ii ;
      }

      /* 12 May 2003: create expansion arrays */

      tst = (double *) malloc( sizeof(double) * nts ) ;
      ast = (double *) malloc( sizeof(double) * nts ) ;
      nts = 0 ;
      for( jj=0 ; jj < IN_num_tstim ; jj++ ){
        dur = IN_tstim_b[jj] - IN_tstim_a[jj] ; dur = MAX(dur,0.0) ;
        ii  = ((int)ceil(dur/dts)) + 1 ;
        if( ii == 1 ){              /* instantaneous impulse */
          tst[nts] = IN_tstim_a[jj] ; ast[nts] = IN_tstim_c[jj] ;
        } else {
          aa  = dur/(WAV_dt*ii) * IN_tstim_c[jj]; /* amplitude of each impulse */
          dur = dur / (ii-1) ;                    /* interval between impulses */
          for( kk=0 ; kk < ii ; kk++ ){
            tst[nts+kk] = IN_tstim_a[jj]+kk*dur ;
            ast[nts+kk] = aa ;
          }
        }
        nts += ii ;
      }

      /* Plop down copies of the waveform at each tst[] time */

      for( jj=0 ; jj < nts ; jj++ ){
        ibot = (int) (tst[jj]/WAV_dt) ;    /* may be 1 too early */
        itop = ibot + WAV_npts ;
        if( itop > OUT_npts ) itop = OUT_npts ;
        for( ii=ibot ; ii < itop ; ii++ ){
           val = WAV_peak * ast[jj] * waveform( WAV_dt * ii - tst[jj] ) ;
           OUT_ts[ii] += val ;
        }
      }
      free(ast); free(tst);
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

/*----------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Usage: waver [options] > output_filename\n"
    "Creates an ideal waveform timeseries file.\n"
    "The output goes to stdout, and normally would be redirected to a file.\n"
    "\n"
    "Options: (# refers to a number; [xx] is the default value)\n"
    "  -WAV = Sets waveform to Cox special                    [default]\n"
    "           cf. AFNI FAQ list for formulas:\n"
    "           http://afni.nimh.nih.gov/afni/doc/faq/17\n"
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
    "  -FILE dt wname = Sets waveform to the values read from the file\n"
    "                   'wname', which should be a single column .1D file\n"
    "                   (i.e., 1 ASCII number per line).  The 'dt value\n"
    "                   is the time step (in seconds) between lines\n"
    "                   in 'wname'; the first value will be at t=0, the\n"
    "                   second at t='dt', etc.  Intermediate time values\n"
    "                   will be linearly interpolated.  Times past the\n"
    "                   the end of the 'wname' file length will have\n"
    "                   the waveform value set to zero.\n"
    "               *** N.B.: If the -peak option is used AFTER -FILE,\n"
    "                         its value will be multiplied into the result.\n"
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
    "  -gamd #        = Sets the delay time to # seconds            [0.0]\n"
    "\n"
    "These options apply to all waveform types:\n"
    "  -peak #        = Sets peak value to #                        [100]\n"
    "  -dt #          = Sets time step of output AND input          [0.1]\n"
    "  -TR #          = '-TR' is equivalent to '-dt'\n"
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
    "   ** 12 May 2003: The times after '-tstim' can now also be specified\n"
    "                     in the format 'a:b', indicating a continuous ON\n"
    "                     period from time 'a' to time 'b'.  For example,\n"
    "                       -dt 2.0 -tstim 13.2:15.7 20.3:25.3\n"
    "                     The amplitude of a response of duration equal to\n"
    "                     'dt' is equal the the amplitude of a single impulse\n"
    "                     response (which is the special case a=b).  N.B.: This\n"
    "                     means that something like '5:5.01' is very different\n"
    "                     from '5' (='5:5').  The former will have a small amplitude\n"
    "                     because of the small duration, but the latter will have\n"
    "                     a large amplitude because the case of an instantaneous\n"
    "                     input is special.  It is probably best NOT to mix the\n"
    "                     two types of input to '-tstim' for this reason.\n"
    "                     Compare the graphs from the 2 commands below:\n"
    "                       waver -dt 1.0 -tstim 5:5.1 | 1dplot -stdin\n"
    "                       waver -dt 1.0 -tstim 5     | 1dplot -stdin\n"
    "                     If you prefer, you can use the form 'a%%c' to indicate\n"
    "                     an ON interval from time=a to time=a+c.\n"
    "   ** 13 May 2005: You can now add an amplitude to each response individually.\n"
    "                     For example\n"
    "                       waver -dt 1.0 -peak 1.0 -tstim 3.2 17.9x2.0 23.1x-0.5\n"
    "                     puts the default response amplitude at time 3.2,\n"
    "                     2.0 times the default at time 17.9, and -0.5 times\n"
    "                     the default at time 23.1.\n"
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
    "  -ver           = Output version information and exit.\n"
    "\n"
    "* Only one of the 3 timeseries input options above can be used at a time.\n"
    "* Using the AFNI program 1dplot, you can do something like the following,\n"
    "  to check if the results make sense:\n"
    "    waver -GAM -tstim 0 7.7 | 1dplot -stdin\n"
    "* Note that program 3dDeconvolve can now generate many different\n"
    "  waveforms internally, markedly reducing the need for this program.\n"
    "* If a square wave is desired, see the 'sqwave' program.\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------*/

void Process_Options( int argc , char * argv[] )
{
   int nopt = 1 ;

   while( nopt < argc ){

      /*-----*/

      if( strncmp(argv[nopt],"-FILE",4) == 0 ){  /* 23 Aug 2005 */
        MRI_IMAGE *fim ;
        waveform_type = FILE_TYPE ;
        if( nopt+2 >= argc ) ERROR_exit("need 2 arguments after -FILE!") ;
        FILE_dt = strtod( argv[++nopt] , NULL ) ;
        if( FILE_dt <= 0.0 ) ERROR_exit("-FILE '%s' is illegal 'dt' value",argv[nopt]);
        fim = mri_read_1D( argv[++nopt] ) ;
        if( fim == NULL ) ERROR_exit("Can't read file '%s' for -FILE",argv[nopt]) ;
        FILE_nval = fim->nx ;
        if( FILE_nval < 2 ) ERROR_exit("File '%s' too short for -FILE",argv[nopt]) ;
        FILE_val = MRI_FLOAT_PTR(fim) ;
        WAV_peak = 1.0 ;
        nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-GAM",4) == 0 ){
         waveform_type = GAM_TYPE ;
         nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-WAV",4) == 0 ){
         waveform_type = WAV_TYPE ;
         nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-EXPR",4) == 0 ){  /* 01 Aug 2001 */
         waveform_type = EXPR_TYPE ;
         if( EXPR_pcode != NULL )
           ERROR_exit("Can't have 2 -EXPR options!") ;
         nopt++ ;
         if( nopt >= argc )
           ERROR_exit("-EXPR needs an argument!") ;
         EXPR_pcode = PARSER_generate_code( argv[nopt] ) ;  /* compile */
         if( EXPR_pcode == NULL )
           ERROR_exit("Illegal -EXPR expression!") ;
         if( !PARSER_has_symbol("T",EXPR_pcode) )
           ERROR_exit("-EXPR expression doesn't use variable 't'!");
         nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-gamb",5) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after -gamb") ;
         GAM_power = strtod(argv[nopt+1],NULL) ;
         if( GAM_power <= 0.0 ) ERROR_exit("non-positive value after -gamb") ;
         waveform_type = GAM_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-gamc",5) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         GAM_time = strtod(argv[nopt+1],NULL) ;
         if( GAM_time <= 0.0 ) ERROR_exit("non-positive value after -gamc") ;
         waveform_type = GAM_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-gamd",5) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         GAM_delay_time = strtod(argv[nopt+1],NULL) ;
         /*if( GAM_time <= 0.0 ) ERROR_exit("non-positive value after -gamd") ;*/
         waveform_type = GAM_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-del",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_delay_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_delay_time < 0.0 ) ERROR_exit("negative value after -del") ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-ris",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_rise_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_rise_time <= 0.0 ) ERROR_exit("non-positive value after -ris") ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-fal",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_fall_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_fall_time <= 0.0 ) ERROR_exit("non-positive value after -fal") ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-und",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_undershoot = strtod(argv[nopt+1],NULL) ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-pea",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_peak = strtod(argv[nopt+1],NULL) ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-res",4) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_restore_time = strtod(argv[nopt+1],NULL) ;
         if( WAV_restore_time <= 0.0 ) ERROR_exit("non-positive value after -res") ;
         waveform_type = WAV_TYPE ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-dt",3) == 0 || strncmp(argv[nopt],"-TR",3) == 0 ){
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         WAV_dt = strtod(argv[nopt+1],NULL) ;
         if( WAV_dt <= 0.0 ) ERROR_exit("non-positive value after -dt") ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-xyo",4) == 0 ){
         OUT_xy = 1 ;
         nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-inp",4) == 0 ){
         MRI_IMAGE * tsim ;
         float * tsar ;
         int ii ;

         if( IN_npts > 0 || IN_num_tstim > 0 )
           ERROR_exit("Cannot input two timeseries!") ;

         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         tsim = mri_read_1D( argv[nopt+1] ) ;
         if( tsim == NULL ) ERROR_exit("can't read -input file '%s'",argv[nopt+1]) ;

         IN_npts = tsim->nx ;
         IN_ts   = (double *) malloc( sizeof(double) * IN_npts ) ;
         tsar    = MRI_FLOAT_PTR(tsim) ;
         for( ii=0 ; ii < IN_npts ; ii++ ) IN_ts[ii] = tsar[ii] ;
         mri_free(tsim) ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[nopt],"-tstim") == 0 ){  /* 16 May 2001 */
        int iopt , nnn , zero_valc=0 ;
        float value , valb , valc ;
        char *cpt , *dpt ;

        if( IN_num_tstim > 0 || IN_npts > 0 )
          ERROR_exit("Cannot input two timeseries!") ;
        if( nopt+1 >= argc )
          ERROR_exit("need argument after %s",argv[nopt]) ;

        iopt         = nopt+1 ;
        IN_num_tstim = 0 ;
        IN_tstim_a   = (double *) malloc( sizeof(double) ) ;
        IN_tstim_b   = (double *) malloc( sizeof(double) ) ;
        IN_tstim_c   = (double *) malloc( sizeof(double) ) ;

        /* loop over argv until get a '-' or get to end of args */

        while( iopt < argc && argv[iopt][0] != '-' ){

          if( isspace(argv[iopt][0]) ){   /* skip if starts with blank */
            ERROR_message(                /* (usually from Microsoft!) */
                    "Skipping -tstim value #%d that starts with whitespace!",
                    IN_num_tstim ) ;
            iopt++; continue;
          }

          /* formats:  start            ==> instantaneous impulse
                       start%duration   ==> extended duration
                       start:end        ==> extended duration
             can also have "/amplitude" afterwards to scale result */

          valb = 0.0 ; valc = 1.0 ;
          if( strchr(argv[iopt],'%') != NULL ){                       /* 12 May 2003 */
            nnn = sscanf( argv[iopt] , "%f%%%f" , &value , &valb ) ;
            if( nnn == 2 ) valb += value ;
          } else if( strchr(argv[iopt],':') != NULL ){
            nnn = sscanf( argv[iopt] , "%f:%f"  , &value , &valb ) ;
          } else {
            nnn = sscanf( argv[iopt] , "%f"     , &value ) ;
          }
          if( nnn < 1 || value < 0.0 ){
            fprintf(stderr,"** Weird value after -tstim: argv='%s'\n",argv[iopt]  ) ;
            fprintf(stderr,"**                  previous argv='%s'\n",argv[iopt-1]) ;
            fprintf(stderr,"** ==> Skipping this value!\n") ;
            iopt++; continue;
          }
          if( nnn == 1 || valb < value ) valb = value ;  /* 12 May 2003 */

          /* 13 May 2005: check for amplitude that follows a 'x' */

          cpt = strchr(argv[iopt],'x') ;
          if( cpt != NULL ){
            cpt++ ; valc = strtod( cpt , &dpt ) ;
            if( valc == 0.0 && dpt == cpt ) valc = 1.0 ;
            if( valc == 0.0 ) zero_valc++ ;
          }

          IN_tstim_a = (double *)realloc(IN_tstim_a,sizeof(double)*(IN_num_tstim+1));
          IN_tstim_b = (double *)realloc(IN_tstim_b,sizeof(double)*(IN_num_tstim+1));
          IN_tstim_c = (double *)realloc(IN_tstim_c,sizeof(double)*(IN_num_tstim+1));
          IN_tstim_a[IN_num_tstim] = value ;   /* start time */
          IN_tstim_b[IN_num_tstim] = valb  ;   /* end time */
          IN_tstim_c[IN_num_tstim] = valc  ;   /* amplitude */
          IN_num_tstim++ ;
          if( valb > IN_top_tstim ) IN_top_tstim = valb ;
          iopt++ ;
        }
        if( zero_valc == IN_num_tstim )
          WARNING_message("all '/' amplitudes in 'waver -tstim' are zero!") ;

        nopt = iopt ; continue ;  /* end of -tstim */
      }

      /*-----*/

      if( strncmp(argv[nopt],"-inl",4) == 0 ){
         int iopt , count , nnn ;
         float value ;
         char sep ;

         if( IN_npts > 0 || IN_num_tstim > 0 )
           ERROR_exit("Cannot input two timeseries!") ;
         if( nopt+1 >= argc )
           ERROR_exit("need argument after %s",argv[nopt]) ;
         iopt    = nopt+1 ;
         IN_npts = 0 ;
         IN_ts   = (double *) malloc( sizeof(double) ) ;
         while( iopt < argc && argv[iopt][0] != '-' ){

            if( strstr(argv[iopt],"@") != NULL ||    /* if has one of the    */
                strstr(argv[iopt],"x") != NULL ||    /* allowed separator    */
                strstr(argv[iopt],"X") != NULL ||    /* characters, then     */
                strstr(argv[iopt],"*") != NULL   ){  /* scan for count@value */

               nnn = sscanf( argv[iopt] , "%d%c%f" , &count , &sep , &value ) ;
               if( nnn != 3 || count < 1 )
                 ERROR_exit("Illegal value after -inline: '%s'",argv[iopt]) ;

            } else {                                 /* just scan for value */
               count = 1 ;
               nnn   = sscanf( argv[iopt] , "%f" , &value ) ;
               if( nnn != 1 )
                 ERROR_exit("Illegal value after -inline: '%s'",argv[iopt]) ;
            }

            IN_ts = (double *) realloc( IN_ts , sizeof(double) * (IN_npts+count) ) ;
            for( nnn=0 ; nnn < count ; nnn++ )
               IN_ts[nnn+IN_npts] = value ;

            IN_npts += count ; iopt++ ;
         }
         nopt = iopt ; continue ;
      }

      /*-----*/

      if( strcmp(argv[nopt],"-when") == 0 ){   /* 08 Apr 2002 */
         int iopt , bot,top , nn , nbt,*bt , count=0 , ii,kk ;

         if( IN_npts > 0 || IN_num_tstim > 0 )
           ERROR_exit("Cannot input two timeseries!") ;

         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         iopt = nopt+1 ;
         nbt  = 0 ;
         bt   = (int *) malloc(sizeof(int)) ;
         while( iopt < argc && argv[iopt][0] != '-' ){

            /* scan for value..value */

            bot = top = -1 ;
            nn = sscanf( argv[iopt],"%d..%d",&bot,&top) ;
            if( nn < 1 || bot < 0 )
              ERROR_exit("Illegal value after -when: '%s'",argv[iopt]) ;
            if( nn == 1 )
              top = bot ;
            else if( top < bot )
              ERROR_exit("Illegal value after -when: '%s'",argv[iopt]) ;

            /* save (bot,top) pairs in bt */

            bt = (int *) realloc( bt , sizeof(int)*(nbt+1)*2 ) ;
            bt[2*nbt  ] = bot ;
            bt[2*nbt+1] = top ; nbt++ ;
            if( count < top ) count = top ;
            iopt++ ;
         }

         if( nbt < 1 ) ERROR_exit("No ranges after -when?") ;

         IN_npts = count+1 ;
         IN_ts   = (double *) malloc( sizeof(double) * IN_npts ) ;
         for( ii=0 ; ii < IN_npts ; ii++ ) IN_ts[ii] = 0.0 ;

         for( kk=0 ; kk < nbt ; kk++ ){
            bot = bt[2*kk] ; top = bt[2*kk+1] ;
            for( ii=bot ; ii <= top ; ii++ ) IN_ts[ii] = 1.0 ;
         }

         free(bt) ; nopt = iopt ; continue ;
      }

      /*-----*/

      if( strcmp(argv[nopt],"-numout") == 0 ){   /* 08 Apr 2002 */
         int val = -1 ;
         if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
         sscanf(argv[nopt+1],"%d",&val) ;
         if( val <= 1 )
           ERROR_exit("Illegal value after -numout: '%s'",argv[nopt]);
         OUT_numout = val ;
         nopt++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-ver",4) == 0 ){  /* 06 Jan 2006 [rickr] */
         PRINT_VERSION("waver");
         exit(0) ;
      }

      /*-----*/

      ERROR_exit("Unknown option '%s'",argv[nopt]) ;
   }

   if( WAV_peak == 0.0 && waveform_type != EXPR_TYPE )
     ERROR_exit("Illegal -peak 0 for non-EXPR waveform type!") ;

   return ;
}
