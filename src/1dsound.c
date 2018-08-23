#include "mrilib.h"

#include "cs_playsound.c"

void usage_1dsound(int detail)
{
   printf(
     "Usage: 1dsound [options] tsfile\n"
     "\n"
     "-------\n"
     "OPTIONS\n"
     "-------\n"
     " -prefix ppp  = Output filename will be ppp.au\n"
     "                [Sun audio format]\n"
     " -8PCM        = Output in 8-bit linear PCM encoding\n"
     "                [default is 8-bit mu-law encoding]\n"
     " -tper X      = X seconds of sound per time point in 'tsfile'\n"
     "                [default is 0.5 s]\n"
     "\n"
     "-----\n"
     "NOTES\n"
     "-----\n"
     "* At this time, program is very limited in what it does:\n"
     "  + frequency modulated between 110 and 1760 Hz\n"
     "    [min to max in tsfile]\n"
     "* File can be played with the 'sox' audio library command\n"
     "    play ppp.au gain -5\n"
     "  Here 'gain -5' turns the volume down.\n"
     "  + sox is not provided with AFNI :(\n"
     "  + to see if sox is on your system, type the command 'which sox'\n"
     "* Creation of the file does not depend on sox, so if you have\n"
     "  another way to play .au files, you can use that.\n"
     "  * Mac OS X: Quicktime (GUI) or afplay (command line)\n"
     "  * Linux:    sox is probably the simplest\n"
     "\n"
     "--- Quick hack for experimentation - RWCox - Aug 2018 ---\n"
     "\n"
   ) ;

   return;
}

/*---------------------------------------------------------------------------*/
/* This program is a very elaborate wrapper for the plot_ts.c functions. */
/*---------------------------------------------------------------------------*/

#define SRATE 48000

int main( int argc , char *argv[] )
{
   int iarg ;
   char *prefix = "sound.au" ;
   MRI_IMAGE *inim , *phim ;
   float *far ;
   int do_8PCM=0 ;
   float tper=0.5f ; int nsper ;

   /*---------- startup bureaucracy ----------*/

#if 0
   mainENTRY("1dsound main"); machdep();
   PRINT_VERSION("1dsound"); AUTHOR("RWC et al.");
#endif

   /*------------ scan arguments that X11 didn't eat ------------*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-- help? --*/

     if(strcmp(argv[iarg],"-help") == 0 ||
        strcmp(argv[iarg],"-h")    == 0   ){
       usage_1dsound(strlen(argv[iarg])>3?2:1);
       exit(0) ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = strdup(argv[++iarg]) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-8PCM") == 0 ){
       do_8PCM = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-tper") == 0 || strcmp(argv[iarg],"-dt") == 0 ){
       tper = (float)strtod(argv[++iarg],NULL) ;
       if( tper < 0.01f || tper > 1.00f )
         ERROR_exit("1dsound: %s %s is out of range 0.01..1.0",
                    argv[iarg-1],argv[iarg]) ;
       nsper = (int)rintf( SRATE * tper ) ;
       iarg++ ; continue ;
     }

     /*--- symplectically stoopid user ---*/

     ERROR_message("Unknown option: %s\n",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);

   } /*--------- end of scan over command line args ----------*/

   if( argc < 2 ){ usage_1dsound(0); exit(0) ; }

   if( iarg >= argc )
      ERROR_exit("No time series file on command line!\n") ;

   inim = mri_read_1D( argv[iarg] ) ;
   if( inim == NULL )
     ERROR_exit("Can't read input file '%s' iarg=%d\n",argv[iarg],iarg) ;

   phim = mri_sound_1D_to_FM( inim ,
                              0.0f , 0.0f , SRATE , nsper ) ;

   if( phim == NULL )
     ERROR_exit("mri_sound_1D_to_FM fails") ;

   if( phim != NULL ){
     char fname[1024] ;
     sprintf(fname,"%s.au",prefix) ;
     if( do_8PCM ){
       sound_write_au_8PCM( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f ) ;
     } else {
       sound_write_au_ulaw( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f ) ;
     }
   }

   exit(0) ;
}
