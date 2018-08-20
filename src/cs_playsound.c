#include "mrilib.h"

/*----- Play music, using the sox program (must be installed) -----*/

#define NUM_NOTE 48
#define DUR_NOTE 0.3

static int have_sox = -1 ;

/*--------------------------------------------------------------------------*/
static char note_type[32] = "pluck" ;
static int  gain_value    = -27 ;

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

/*--------------------------------------------------------------------------*/

void play_sound_1D( int nn , float *xx )
{
   float xbot , xtop , fac , shf , durn ;
   int ii ;
   char *pre , fname[32] , cmd[128] ;

   if( nn < 2 || xx == NULL ) return ;

   if( have_sox < 0 ) have_sox = ( THD_find_executable("sox") != NULL ) ;

   if( have_sox == 0 ) return ;

   /*--- fork a sub-process to do the work and play the sound ---*/

   ii = (int)fork() ;
   if( ii != 0 ) return ; /* return to parent process */

   /*--- from here on, am in sub-process, which quits when done ---*/

   xbot = xtop = xx[0] ;
   for( ii=1 ; ii < nn ; ii++ ){
           if( xx[ii] < xbot ) xbot = xx[ii] ;
      else if( xx[ii] > xtop ) xtop = xx[ii] ;
   }
   if( xbot == xtop ) _exit(0) ;

   fac  = (NUM_NOTE+0.5f) / (xtop-xbot) ;
   shf  = 0.6f * NUM_NOTE ;
   durn = (nn < 50) ?  DUR_NOTE : (DUR_NOTE/2.0f) ;

   pre = UNIQ_idcode_11() ;  /* make up name for sound file */
   sprintf(fname,"%s.ul",pre) ;
   unlink(fname) ;           /* remove sound file, in case it already exists */

   for( ii=0 ; ii < nn ; ii++ ){
     sprintf( cmd ,
              "sox -e mu-law -r 48000 -n -t raw - synth %.2f %s %%%d gain -h %d >> %s" ,
              durn , note_type ,
              (int)rintf( fac*(xx[ii]-xbot)-shf ) ,
              gain_value , fname ) ;
     system(cmd) ;
   }

   sprintf( cmd , "sox -r 48000 -c 1 %s -d &> /dev/null" , fname ) ;
   system(cmd) ;
   unlink(fname) ;
   _exit(0) ;
}
