#include "mrilib.h"

/*----- Play music, using the sox program (must be installed) -----*/

#define NUM_NOTE 48
#define DUR_NOTE 0.3

static int have_sox = -1 ;

static byte mulaw( float x ) ; /* prototype */

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

/*---------------------------------------------------------------------------*/
/*-------- convert value in range -1..1 into 8 bit code via 'mu-law' --------*/

static INLINE byte mulaw( float x )
{
   short sx ; byte bx ;
   int   sr , im ;
   short is , imag, iesp, ofst, ofst1 , sp ;

        if( x < -1.0f ) x = -1.0f ;
   else if( x >  1.0f ) x =  1.0f ;

   sx = (short)rintf(8158.0f * x) ;

   is = sx >> 15 ;
   sr = (sx & 65535) ;
   im = (is==0) ? sr : (65536-sr)&32767 ;

   imag = im ; if( imag > 8158 ) imag = 8158 ;
   ++imag;
   iesp = 0;
   ofst = 31;

   if( imag > ofst ){
     for( iesp = 1 ; iesp <= 8 ; ++iesp){
       ofst1 = ofst ;
       ofst += (1 << (iesp + 5)) ;
       if (imag <= ofst) break ;
     }
     imag -= ofst1 + 1 ;
   }

   imag /= (1 << (iesp + 1)) ;

   sp = (is == 0) ? (imag + (iesp << 4)) : (imag + (iesp << 4) + 128) ;

   sp ^= 128 ; sp ^= 127 ;

   bx = (byte)sp ; return bx ;
}

/*---------------------------------------------------------------------------*/

static int little_endian = -66 ;

static INLINE void set_little_endian(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   if( little_endian < 0 ){
     fred.bb[0] = 1 ; fred.bb[1] = 0 ;
     little_endian = (fred.ss == 1) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d ; } fourby ;

static uint32_t INLINE swap_fourby( uint32_t ii )
{
   union { uint32_t qq ; fourby ff ; } qf ;
   unsigned char tt ;

   qf.qq   = ii ;
   tt      = qf.ff.a ;
   qf.ff.a = qf.ff.d ;
   qf.ff.d = tt ;
   tt      = qf.ff.b ;
   qf.ff.b = qf.ff.c ;
   qf.ff.c = tt ;
   return qf.qq ;
}

/*---------------------------------------------------------------------------*/
/* samples in aa[] are between -1 and 1.
   sample rate (per second) is in srate.
   scl is a scale down factor (0 < scl <= 1).
*//*-------------------------------------------------------------------------*/

void sound_write_au_ulaw( char *fname, int nn, float *aa, int srate, float scl )
{
   FILE *fp ;
   uint32_t ival , jval ;
   byte *bb ;
   int ii ;

   /* check inputs */

   if( fname == NULL || nn < 2 || aa == NULL ){
     ERROR_message("Illegal inputs to sound_write_au :(") ;
     return ;
   }

   if( srate < 100 ) srate = 8000 ;
   if( scl   < 0.0f || scl > 1.0f ) scl = 1.0f ;

   /* open output file */

   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
     ERROR_message("Can't open audio output file '%s'",fname) ;
     return ;
   }

   set_little_endian() ;  /* do we need to swap bytes in header output? */

   /* write header */

   fwrite( ".snd" , 1 , 4, fp ) ;                 /*----- magic 'number' -----*/

   ival = 24    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;                /*----- offset to data -----*/

   ival = nn    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;          /*----- number of data bytes -----*/

   ival =  1    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;          /*----- encoding = 1 = u-law -----*/

   ival = srate ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;            /*----- samples per second -----*/

   ival =  1    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;            /*----- number of channels -----*/

   /* convert and write data */

   bb = (byte *)malloc(sizeof(byte)*nn) ;
   for( ii=0 ; ii < nn ; ii++ ) bb[ii] = mulaw(scl*aa[ii]) ;
   fwrite( bb , 1 , nn , fp ) ;

   /* done done done */

   fclose(fp) ;
   free(bb) ;
   return ;
}

/*---------------------------------------------------------------------------*/

#undef  A4
#define A4 440.0  /* Hz */

MRI_IMAGE * mri_sound_1D_to_FM( MRI_IMAGE *imin ,
                                float fbot , float ftop ,
                                int srate , int nsper    )
{
   float *aa,*bb ;
   float abot,atop , lfbot,lftop,dlf,dfac,ai,ap,di,dp ;
   float afac,dph,ph ;
   int nout , ii,jj,kk,ip , nn ;
   MRI_IMAGE *imout , *qim ;

   if( imin == NULL ) return NULL ;

   nn = imin->nx ;
   if( nn < 2 ){
     ERROR_message("mri_sound_1D_to_FM: nn = %d",nn) ;
     return NULL ;
   }

   if( imin->kind != MRI_float ){
     qim = mri_to_float(imin) ;
   } else {
     qim = imin ;
   }
   aa = MRI_FLOAT_PTR(qim) ;

   if( fbot <= 0.0f || fbot >= ftop ){
     fbot = 0.1f * A4 ; ftop = 10.0f * A4 ;
   }
   if( ftop > 10000.0f ) ftop = 10000.0f ;
   if( fbot <    30.0f ) fbot =    30.0f ;

   if( srate < 8000 ) srate = 8000 ;
   if( nsper <= 0   ) nsper = 1 ;

   /* will scale so abot..atop = log(fbot)..log(ftop) */

   abot = atop = aa[0] ;
   for( ii=1 ; ii < nn ; ii++ ){
           if( aa[ii] < abot ) abot = aa[ii] ;
      else if( aa[ii] > atop ) atop = aa[ii] ;
   }

   if( abot >= atop ){
     if( qim != imin ) mri_free(qim) ;
     ERROR_message("mri_sound_1D_to_FM: abot=%g atop=%g",abot,atop) ;
     return NULL ;
   }

   nout  = nsper * nn ;
   imout = mri_new( nout , 1 , MRI_float ) ;
   bb    = MRI_FLOAT_PTR(imout) ;

   lfbot = log2(fbot) ; lftop = log2(ftop) ; dlf = lftop-lfbot ;

   dfac = 1.0f / nsper ;
   afac = dlf  / (atop-abot) ;

#undef  FRMOD
#define FRMOD(x) exp2f( lfbot + afac*((x)-abot) )

   ph  = 0.0f ;
   dph = 2.0f * PI / srate ;
   for( jj=ii=0 ; ii < nn ; ii++ ){
     ph += FRMOD(aa[ii]) * dph ; bb[jj++] = sin(ph) ;
     if( nsper > 1 ){
       ip = ii+1 ; if( ip >= nn ) ip = nn-1 ;
       ai = aa[ii] ; ap = aa[ip] ;
       di = 1.0f   ; dp = 0.0f   ;
       for( kk=1 ; kk < nsper ; kk++ ){
         di -= dfac ; dp += dfac ;
         ph += FRMOD( di*ai+dp*ap ) * dp ; bb[jj++] = sin(ph) ;
       }
     }
   }

#undef FRMOD

   if( qim != imin ) mri_free(qim) ;
   return imout ;
}
