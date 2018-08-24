#include "mrilib.h"

#include "cs_playsound.c"

static char *pprog = NULL ;

void usage_1dsound(int detail)
{
   printf(
     "\n"
     "Usage: 1dsound [options] tsfile\n"
     "\n"
     "Program to create a sound file from a 1D file (column of numbers).\n"
     "\n"
     "Is this program useful? Probably not, but it can be fun.\n"
     "\n"
     "-------\n"
     "OPTIONS\n"
     "-------\n"
     "\n"
     " ===== output filename =====\n"
     "\n"
     " -prefix ppp  = Output filename will be ppp.au\n"
     "                [old Sun audio format]\n"
     "                If you don't use '-prefix', the output is file 'sound.au'.\n"
     "\n"
     " ===== encoding details =====\n"
     "\n"
     " -8PCM        = Output in 8-bit linear PCM encoding\n"
     "                [default is 8-bit mu-law encoding]\n"
     "\n"
     " -tper X      = X seconds of sound per time point in 'tsfile'.\n"
     " -TR X          Allowed range for 'X' is 0.01 to 1.0 (inclusive).\n"
     " -dt X          [default time step is 0.2 s]\n"
     "                You can use '-tper', '-dt', or '-TR', as you like.\n"
     "\n"
     " ===== how the sound is produced from the data =====\n"
     "\n"
     " -FM          = Output sound is frequency modulated between 110 and 1760 Hz\n"
     "                from min to max in the input 1D file.\n"
     "\n"
     " -notes       = Output sound is a sequence of notes, low to high pitch\n"
     "                based on min to max in the input 1D file.\n"
     "\n"
     " -notewave W  = Selects the shape of the notes used. 'W' is one of these:\n"
     "                  sine     = pure sine wave\n"
     "                  h2sine   = sine wave with some second harmonic\n"
     "                  square   = square wave\n"
     "                  triangle = triangle wave\n"
     "\n"
     " ** At this time, the default production method is '-notes',\n"
     " **               using the triangle waveform (I like this best).\n"
     "\n"
     " ===== hear the sound right away! =====\n"
     "\n"
     " -play        = Plays the sound file after it is written.\n"
     "                On this computer: %s %s\n"
    , (pprog != NULL)
        ? "uses program"
        : "can't find any sound playing program"
    ,
      (pprog != NULL) ? pprog : " :("
   ) ;
   printf(
     "\n"
     "--------\n"
     "EXAMPLES\n"
     "--------\n"
     "The first 2 examples are purely synthetic, using 'data' files created\n"
     "on the command line. The third example uses a data file that was written\n"
     "out of an AFNI graph viewer using the 'w' keystroke.\n"
     "\n"
     " 1dsound -prefix A1 '1D: 0 1 2 1 0 1 2 0 1 2'\n"
     "\n"
     " 1deval -num 100 -expr 'sin(x+0.01*x*x)' | 1dsound -tper 0.1 -prefix A2 1D:stdin\n"
     "\n"
     " 1dsound -prefix -tper 0.1 A3 028_044_003.1D\n"
     "\n"
     "-----\n"
     "NOTES\n"
     "-----\n"
     "* At this time, program is very limited in what it does:\n"
     "  + frequency modulated between 110 and 1760 Hz\n"
     "    [min to max in tsfile]\n"
     "  + Is this useful? Probably not.\n"
     "\n"
     "* File can be played with the 'sox' audio library command\n"
     "    play A1.au gain -5\n"
     "  + Here 'gain -5' turns the volume down :)\n"
     "  + sox is not provided with AFNI :(\n"
     "  + To see if sox is on your system, type the command 'which sox'\n"
     "\n"
     "* Creation of the file does not depend on sox, so if you have\n"
     "  another way to play .au files, you can use that.\n"
     "  * Mac OS X: Quicktime (GUI) or afplay (command line) programs.\n"
     "              + sox can be installed by first installing 'brew'\n"
     "                -- see https://brew.sh/ -- and then using command\n"
     "                'brew install sox'.\n"
     "  * Linux:    Getting sox is probably the simplest thing to do.\n"
     "              + Or install the mplayer package (which also does videos).\n"
     "              + Another possibility is the aplay program.\n"
     "\n"
     "* The audio output file is sampled at 16K bytes per second.\n"
     "  For example, a 30 second file will be 480K bytes in size.\n"
     "\n"
     "* The -FM auditory effect varies significantly with the '-tper'\n"
     "  parameter X; '-tper 0.02' is very different than '-tper 0.4'.\n"
     "\n"
     "--- Quick hack for experimentation and fun - RWCox - Aug 2018 ---\n"
     "\n"
   ) ;

   return;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

#define SRATE 16000  /* sampling rate of output audio file */

#define CODE_FM    1
#define CODE_NOTES 2

int main( int argc , char *argv[] )
{
   int iarg ;
   char *prefix = "sound.au" ;
   char fname[1024] ;
   MRI_IMAGE *inim , *phim ;
   float *far ;
   int do_8PCM=0 ; int do_play=0 ;
   float tper=0.2f ; int nsper ;
   int opcode = CODE_NOTES ;

   /*---------- find a sound playing program ----------*/

     pprog = THD_find_executable("play") ;
   if( pprog == NULL )
     pprog = THD_find_executable("afplay") ;
   if( pprog == NULL )
     pprog = THD_find_executable("mplayer") ;
   if( pprog == NULL )
     pprog = THD_find_executable("aplay") ;

   /*----- immediate help and quit? -----*/

   if( argc < 2 ){ usage_1dsound(1) ; exit(0) ; }

   /*---------- startup bureaucracy ----------*/

   mainENTRY("1dsound main"); machdep();

   /*------------ scan arguments that X11 didn't eat ------------*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-- help? --*/

     if( strcasecmp(argv[iarg],"-help") == 0 ||
         strcasecmp(argv[iarg],"-h")    == 0   ){
       usage_1dsound(strlen(argv[iarg])>3?2:1);
       exit(0) ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( iarg >= argc-1 )
         ERROR_exit("need arg after %s",argv[iarg]) ;
       prefix = strdup(argv[++iarg]) ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-8PCM") == 0 ){
       do_8PCM = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-play") == 0 ){
       if( pprog == NULL ){
         WARNING_message("no program available for playing sound :(") ;
       } else {
         do_play = 1 ;
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-FM") == 0 ){
       opcode = CODE_FM ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-NOTES") == 0 ){
       opcode = CODE_NOTES ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-notewave") == 0 ){
       if( iarg >= argc-1 )
         ERROR_exit("need arg after %s",argv[iarg]) ;

       iarg++ ;
            if( strncasecmp(argv[iarg],"sine",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_SINE) ;
       else if( strncasecmp(argv[iarg],"h2sine",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_H2SINE) ;
       else if( strncasecmp(argv[iarg],"square",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_SQUARE) ;
       else if( strncasecmp(argv[iarg],"boxcar",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_SQUARE) ;
       else if( strncasecmp(argv[iarg],"triangle",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_TRIANGLE) ;
       else 
             WARNING_message("unknown note waveform '%s'",argv[iarg]) ;

       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-tper")   == 0 ||
         strcasecmp(argv[iarg],"-dt")     == 0 ||
         strcasecmp(argv[iarg],"-TR") == 0   ){
       if( iarg >= argc-1 )
         ERROR_exit("need arg after %s",argv[iarg]) ;
       tper = (float)strtod(argv[++iarg],NULL) ;
       if( tper < 0.01f || tper > 1.00f )
         ERROR_exit("1dsound: %s %s is out of range 0.01..1.0",
                    argv[iarg-1],argv[iarg]) ;
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

   /*----- read input data file -----*/

   inim = mri_read_1D( argv[iarg] ) ;
   if( inim == NULL )
     ERROR_exit("Can't read input file '%s' iarg=%d\n",argv[iarg],iarg) ;

   /*-- samples per time point --*/

   nsper = (int)rintf( SRATE * tper ) ;

   /*-- create float time series of sound (cs_playsound.c) --*/

   switch( opcode ){

     case CODE_FM:
       phim = mri_sound_1D_to_FM( inim ,
                                  0.0f , 0.0f , SRATE , nsper ) ;
       if( phim == NULL )
         ERROR_exit("mri_sound_1D_to_FM fails") ;
     break ;

     default:
     case CODE_NOTES:
       phim = mri_sound_1D_to_notes( inim , SRATE , nsper ) ;
       if( phim == NULL )
         ERROR_exit("mri_sound_1D_to_notes fails") ;
     break ;

   }

   /*-- create filename from prefix --*/

   if( STRING_HAS_SUFFIX(prefix,".au") ) strcpy(fname,prefix) ;
   else                                  sprintf(fname,"%s.au",prefix) ;

   /*-- write .au file out (cs_playsound.c) --*/

   if( do_8PCM ){
     sound_write_au_8PCM( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f );
   } else {
     sound_write_au_ulaw( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f );
   }
   INFO_message  ("output sound file %s",fname) ;
   ININFO_message(" %.1f s of audio" , phim->nx/(float)SRATE ) ;

   mri_free(phim) ;

   /*----- play the sound as well? -----*/

   if( pprog != NULL && do_play ){
     char cmd[2048] ;
     sprintf(cmd,"%s %s >& /dev/null &",pprog,fname) ;
     ININFO_message(" running command %s",cmd) ;
     system(cmd) ;
   }

   exit(0) ;
}
