#include "mrilib.h"

#include "cs_playsound.c"
#include "despike_inc.c"

#define SRATE 16000  /* sampling rate of output audio file */

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
     "                [Sun audio format https://en.wikipedia.org/wiki/Au_file_format]\n"
     "                + If you don't use '-prefix', the output is file 'sound.au'.\n"
     "                + If 'ppp' ends in '.au', this program won't add another '.au.\n"
     "\n"
     " ===== encoding details =====\n"
     "\n"
     " -16PCM       = Output in 16-bit linear PCM encoding (uncompressed)\n"
     "                + Less quantization noise (audible hiss)            :)\n"
     "                + Takes twice as much disk space for output as 8-bit output :(\n"
     "              +++ This is the default method now!\n"
     "                + https://en.wikipedia.org/wiki/Pulse-code_modulation\n"
     "\n"
     " -8PCM        = Output in 8-bit linear PCM encoding\n"
     "                + There is no good reason to use this option.\n"
     "\n"
     " -8ulaw       = Output in 8-bit mu-law encoding.\n"
     "                + Provides a little better quality than -8PCM,\n"
     "                  but still has audible quantization noise hiss.\n"
     "                +  https://en.wikipedia.org/wiki/M-law_algorithm\n"
     "\n"
     " -tper X      = X seconds of sound per time point in 'tsfile'.\n"
     " -TR X          Allowed range for 'X' is 0.01 to 1.0 (inclusive).\n"
     " -dt X          [default time step is 0.2 s]\n"
     "                You can use '-tper', '-dt', or '-TR', as you like.\n"
     "\n"
     " ===== how the sound timeseries is produced from the data timeseries =====\n"
     "\n"
     " -FM          = Output sound is frequency modulated between 110 and 1760 Hz\n"
     "                from min to max in the input 1D file.\n"
     "                + Usually 'sounds terrible'.\n"
     "                + The only reason this is here is that it was the first method\n"
     "                  I implemented, and I kept it for the sake of nostalgia.\n"
     "\n"
     " -notes       = Output sound is a sequence of notes, low to high pitch\n"
     "                based on min to max in the input 1D file.\n"
     "              +++ This is the default method of operation.\n"
     "                + A pentatonic scale is used, which usually 'sounds nice':\n"
     "                  https://en.wikipedia.org/wiki/Pentatonic_scale\n"
     "\n"
     " -notewave W  = Selects the shape of the notes used. 'W' is one of these:\n"
     " -waveform W      sine     = pure sine wave (sounds simplistic)\n"
#if 0
     "                  h2sine   = sine wave with some second harmonic\n"
#endif
     "                  sqsine   = square root of sine wave (a little harsh and loud)\n"
     "                  square   = square wave              (a lot harsh and loud)\n"
     "                  triangle = triangle wave            [the default waveform]\n"
#if 0  /** hidden - doesn't do much **/
     "\n"
     " -noADSR      = turn off the note 'envelope' to make sound more continuous.\n"
     "                + The envelope is used to ramp each note's sound up and\n"
     "                  then back down over the '-tper' interval, making the\n"
     "                  notes sound somewhat discrete.\n"
     "                + ADSR stands for Attack, Decay, Sustain, Release, which\n"
     "                  are the components of the envelope shape that modulates\n"
     "                  a note's pure waveform.\n"
     "                + At this time, you cannot set the ADSR parameters;\n"
     "                  you can only turn the ADSR envelope off.\n"
#endif
     "\n"
     " -despike     = apply a simple despiking algorithm, to avoid the artifact\n"
     "                of one very large or small value making all the other notes\n"
     "                end up being the same.\n"
     "\n"
     " ===== Notes about notes =====\n"
     "\n"
     " ** At this time, the default production method is '-notes',      **\n"
     " **               using the triangle waveform (I like this best). **\n"
     "\n"
     " ** With '-notes', up to 6 columns of the input file will be used **\n"
     " ** to produce a polyphonic sound (in a single channel).          **\n"
     " ** (Any columns past the 6th in the input 'tsfile' are ignored.) **\n"
     "\n"
     " ===== hear the sound right away! =====\n"
     "\n"
     " -play        = Plays the sound file after it is written.\n"
     "                On this computer: %s %s\n"
     "            ===>> Playing sound on a remote computer is\n"
     "                  annoying, pointless, and likely to get you punched.\n"
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
     "* File can be played with the 'sox' audio package command\n"
     "    play A1.au gain -5\n"
     "  + Here 'gain -5' turns the volume down :)\n"
     "  + sox is not provided with AFNI :(\n"
     "  + To see if sox is on your system, type the command 'which sox'\n"
     "  + If you have sox, you can add 'reverb 99' at the end of the\n"
     "    'play' command line, and have some extra fun.\n"
     "  + Many other effects are available with sox 'play',\n"
     "    and they can also be used to produce edited sound files:\n"
     "    http://sox.sourceforge.net/sox.html#EFFECTS\n"
     "  + You can convert the .au file produced from here to other\n"
     "    formats using sox; for example:\n"
     "      sox Bob.au Cox.au BobCox.aiff\n"
     "    combines the 2 .au input files to a 2-channel (stereo)\n"
     "    Apple .aiff output file. See this for more information:\n"
     "    http://sox.sourceforge.net/soxformat.html\n"
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
     "  For example, a 30 second file will be 960K bytes in size,\n"
     "  at 16 bits per sample.\n"
     "\n"
     "* The auditory effect varies significantly with the '-tper'\n"
     "  parameter X; '-tper 0.02' is very different than '-tper 0.4'.\n"
     "\n"
     "--- Quick hack for experimentation and fun - RWCox - Aug 2018 ---\n"
     "\n"
   ) ;

   return;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

#define CODE_FM    1
#define CODE_NOTES 2

#define ENCODE_8ULAW 1
#define ENCODE_8PCM  2
#define ENCODE_16PCM 3

int main( int argc , char *argv[] )
{
   int iarg ;
   char *prefix = "sound.au" ;
   char fname[1024] ;
   MRI_IMAGE *inim , *phim ;
   float *far ;
   int encoding=ENCODE_16PCM ;
   int do_play=0 ;
   float tper=0.2f ; int nsper ;
   int opcode = CODE_NOTES ;
   int do_despike = 0 ;

   /*---------- find a sound playing program ----------*/

   pprog = get_sound_player() ;

   /*----- immediate help and quit? -----*/

   if( argc < 2 ){ usage_1dsound(1) ; exit(0) ; }

   /*---------- startup bureaucracy ----------*/

   mainENTRY("1dsound main"); machdep();
   sound_set_note_ADSR(1) ;
   sound_set_note_waveform(SOUND_WAVEFORM_TRIANGLE) ;

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
       encoding = ENCODE_8PCM ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-16PCM") == 0 ){
       encoding = ENCODE_16PCM ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-8ulaw") == 0 ){
       encoding = ENCODE_8ULAW ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-play") == 0 ){
       if( pprog == NULL ){
         WARNING_message("No external program available for playing sound :(") ;
       } else if( getenv("SSH_CLIENT") != NULL ){
         WARNING_message("You are logged in remotely: -play is not allowed!") ;
       } else {
         do_play = 1 ;
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-despike") == 0 ){
       do_despike = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-FM") == 0 ){
       opcode = CODE_FM ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-NOTES") == 0 ){
       opcode = CODE_NOTES ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-noADSR") == 0 ||     /* hidden option */
         strcasecmp(argv[iarg],"-noENV" ) == 0   ){
       sound_set_note_ADSR(0) ; iarg++ ; continue ;
     }

     if( strcasecmp (argv[iarg],"-notewave")   == 0 ||
         strncasecmp(argv[iarg],"-waveform",5) == 0   ){
       if( iarg >= argc-1 )
         ERROR_exit("need arg after %s",argv[iarg]) ;

       iarg++ ;
            if( strncasecmp(argv[iarg],"sine",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_SINE) ;
       else if( strncasecmp(argv[iarg],"h2sine",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_H2SINE) ;
       else if( strncasecmp(argv[iarg],"sqsine",3) == 0 )
             sound_set_note_waveform(SOUND_WAVEFORM_SQSINE) ;
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

   if( do_despike ){
     int nx = inim->nx, ny = inim->ny, jj , nspike=0 ;
     float *iar = MRI_FLOAT_PTR(inim), *far ;
     if( ny > 6 ) ny = 6 ;
     for( jj=0 ; jj < ny ; jj++ ){
       nspike += DES_despike25( nx , iar+jj*nx , NULL ) ;
     }
     INFO_message( "%d spike%s squashed from %d input column%s" ,
                   nspike , (nspike!=1)?"s were":" was" ,
                   ny     , (ny    !=1)?"s"     :"\n"    ) ;
   }

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
       phim = mri_sound_1D_to_notes( inim , SRATE , nsper , 6,0,0 ) ;
       if( phim == NULL )
         ERROR_exit("mri_sound_1D_to_notes fails") ;
     break ;

   }

   /*-- create filename from prefix --*/

   if( STRING_HAS_SUFFIX(prefix,".au") ) strcpy(fname,prefix) ;
   else                                  sprintf(fname,"%s.au",prefix) ;

   /*-- write .au file out (cs_playsound.c) --*/

   switch( encoding ){
     default:
     case ENCODE_8ULAW:
       sound_write_au_ulaw( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f );
     break ;

     case ENCODE_8PCM:
       sound_write_au_8PCM( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f );
     break ;

     case ENCODE_16PCM:
       sound_write_au_16PCM( fname, phim->nx, MRI_FLOAT_PTR(phim), SRATE, 0.2f );
     break ;
   }

   INFO_message  ("output sound file %s = %s bytes",
                   fname , commaized_integer_string(THD_filesize(fname)) ) ;
   ININFO_message(" %.1f s of audio" , phim->nx/(float)SRATE ) ;

   mri_free(phim) ;

   /*----- play the sound as well? -----*/

   if( pprog != NULL && do_play ){
     char cmd[2048] ;
     sprintf(cmd,"%s %s >& /dev/null &",pprog,fname) ;
     ININFO_message(" running command %s",cmd) ;
     ININFO_message(" to stop it early: killall %s",pprog_name) ;
     system(cmd) ;
   }

   exit(0) ;
}
