#include "mrilib.h"

/*----- Play music, using the sox program (must be installed) -----*/

#define NUM_NOTE 48
#define DUR_NOTE 0.3

static int have_sox = -1 ;

/*--------------------------------------------------------------------------*/
static char note_type[32] = "pluck" ;
static int  gain_value    = -27 ;
static int  two_tone      = 0 ;

#define NFORKLIST 4
static int  nfork = 0 ;
static int  forklist[NFORKLIST] ;

void set_sound_note_type( char *typ )
{
   if( typ == NULL || typ[0] == '\0' ){ strcpy(note_type,"pluck"); return; }

   if( strncmp(typ,"sine",3)      == 0 ||
       strncmp(typ,"square",3)    == 0 ||
       strncmp(typ,"triangle",3)  == 0 ||
       strncmp(typ,"sawtooth",3)  == 0 ||
       strncmp(typ,"trapezium",3) == 0 ||
       strncmp(typ,"pluck",3)     == 0   ){

     strcpy( note_type , typ ) ;
   } else {
     WARNING_message("unknown sox note type '%s'",typ) ;
   }

   return ;
}

/*-----*/

void set_sound_gain_value( int ggg )
{
   if( ggg < 0 ) gain_value = ggg ;
   return ;
}

/*-----*/

void set_sound_twotone( int ggg )
{
   two_tone = ggg ; return ;
}

/*-----*/

static int psk_set = 0 ;

void play_sound_killer(void){
  int ii ;
  for( ii=0 ; ii < NFORKLIST ; ii++ ){
    if( forklist[ii] > 0 ){
      killpg(forklist[ii],SIGKILL) ;
    }
  }
  return ;
}

/*--------------------------------------------------------------------------*/

void play_sound_1D( int nn , float *xx )
{
   float xbot , xtop , fac , shf , duration ;
   int ii ;
   char *pre , fname[64] , cmd[256] ;
   static int first_big_call=1 ;

   if( nn < 2 || xx == NULL ) return ;

   if( have_sox < 0 ) have_sox = ( THD_find_executable("sox") != NULL ) ;

   if( have_sox == 0 ) return ;

   /*--- fork a sub-process to do the work and play the sound ---*/

   if( nn > 199 && first_big_call ){
     INFO_message("long sound timeseries ==> several seconds for setup") ;
     first_big_call = 0 ;
   }

   if( !psk_set ){ atexit(play_sound_killer) ; psk_set = 1 ; }

   ii = (int)fork() ;
   if( ii != 0 ){  /*-- return to parent process --*/
     int jj = getpgid(ii) ;
     if( nfork == 0 || (nfork > 0 && jj != forklist[nfork-1]) ){
       forklist[nfork] = getpgid(ii) ;
       nfork           = (nfork+1)%NFORKLIST ;
     }
     return ;
   }

   /*--- from here on, am in sub-process, which quits when done ---*/

   xbot = xtop = xx[0] ;
   for( ii=1 ; ii < nn ; ii++ ){
           if( xx[ii] < xbot ) xbot = xx[ii] ;
      else if( xx[ii] > xtop ) xtop = xx[ii] ;
   }
   if( xbot == xtop ) _exit(0) ;

   fac      = (NUM_NOTE+0.5f) / (xtop-xbot) ;
   shf      = 0.6f * NUM_NOTE ;
   duration = (nn < 66) ?  DUR_NOTE : (DUR_NOTE/1.5f) ;

   pre = UNIQ_idcode_11() ;  /* make up name for sound file */
   sprintf(fname,"AFNI_SOUND_TEMP.%s.ul",pre) ;
   unlink(fname) ;           /* remove sound file, in case it already exists */

   if( two_tone ){                  /* doesn't work well for some reason */
     for( ii=0 ; ii < nn ; ii++ ){
       sprintf( cmd ,
                "sox -e mu-law -r 48000 -n -t raw - synth %.2f %s %%%d %s %%%d gain -h %d >> %s" ,
                duration ,
                note_type , (int)rintf( fac*(xx[ii]-xbot)-shf )-2 ,
                note_type , (int)rintf( fac*(xx[ii]-xbot)-shf )+2 ,
                gain_value , fname ) ;
       system(cmd) ;
     }
   } else {                         /* single tone [default] */
     for( ii=0 ; ii < nn ; ii++ ){
       sprintf( cmd ,
                "sox -e mu-law -r 48000 -n -t raw - synth %.2f %s %%%d gain -h %d >> %s" ,
                duration ,
                note_type , (int)rintf( fac*(xx[ii]-xbot)-shf ) ,
                gain_value , fname ) ;
       system(cmd) ;
     }
   }

   /* play the raw (-r) single change (-c 1) file */

   sprintf( cmd , "sox -r 48000 -c 1 %s -d &> /dev/null" , fname ) ;
   system(cmd) ;
   unlink(fname) ; /* and delete the file */
   _exit(0) ;
}
