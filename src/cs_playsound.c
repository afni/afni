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

void sound_write_au_header( FILE *fp , int nn , int srate , int code )
{
   uint32_t ival ;

   set_little_endian() ;  /* do we need to swap bytes in header output? */

   fwrite( ".snd" , 1 , 4, fp ) ;                 /*----- magic 'number' -----*/

   ival = 32    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;                /*----- offset to data -----*/

   ival = nn    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;          /*----- number of data bytes -----*/

   ival =  code ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;                      /*----- encoding -----*/

   ival = srate ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;            /*----- samples per second -----*/

   ival =  1    ; if( little_endian ) ival = swap_fourby(ival) ;
   fwrite( &ival  , 1 , 4 , fp ) ;            /*----- number of channels -----*/

   fwrite( "AFNI.au"  , 1 , 8 , fp ) ;     /*----- 8 bytes of annotation -----*/

   return ;
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

   if( srate < 8000 ) srate = 8000 ;
   if( scl   < 0.0f || scl > 1.0f ) scl = 1.0f ;

   /* open output file */

   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
     ERROR_message("Can't open audio output file '%s'",fname) ;
     return ;
   }

   /* write .au file header */

   sound_write_au_header( fp , nn , srate , 1 ) ;

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
/* samples in aa[] are between -1 and 1.
   sample rate (per second) is in srate.
   scl is a scale down factor (0 < scl <= 1).
*//*-------------------------------------------------------------------------*/

void sound_write_au_8PCM( char *fname, int nn, float *aa, int srate, float scl )
{
   FILE *fp ;
   uint32_t ival , jval ;
   int8_t *bb ;
   int ii ; float fac , val ;

   /* check inputs */

   if( fname == NULL || nn < 2 || aa == NULL ){
     ERROR_message("Illegal inputs to sound_write_au :(") ;
     return ;
   }

   if( srate < 8000 ) srate = 8000 ;
   if( scl   < 0.0f || scl > 1.0f ) scl = 1.0f ;

   /* open output file */

   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
     ERROR_message("Can't open audio output file '%s'",fname) ;
     return ;
   }

   /* write .au file header */

   sound_write_au_header( fp , nn , srate , 2 ) ;

   /* convert and write data */

   bb  = (int8_t *)malloc(sizeof(int8_t)*nn) ;
   fac = 127.444f * scl ;
   for( ii=0 ; ii < nn ; ii++ ){
     val = fac*aa[ii] ;
     bb[ii] = ( val < -127.0f ) ? -127 :
              ( val >  127.0f ) ?  127 : (int8_t)rintf(val) ;
   }
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
   float afac,dph,ph , val ;
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
     fbot = 0.25f * A4 ; ftop = 4.0f * A4 ;
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

   nout  = nsper * nn ;
   imout = mri_new( nout , 1 , MRI_float ) ;
   bb    = MRI_FLOAT_PTR(imout) ;

   if( abot >= atop ){
     float fmid = sqrtf(fbot*ftop) ;
     INFO_message("mri_sound_1D_to_FM: only 1 value == sine wave output f=%.1g",fmid) ;
     ph  = 0.0f ;
     dph = 2.0f * PI * fmid / srate ;
     for( ii=0 ; ii < nout ; ii++ ){
       bb[ii] = sin(ph) ; ph += dph ;
     }
     if( qim != imin ) mri_free(qim) ;
     return imout ;
   }

   lfbot = log2(fbot) ; lftop = log2(ftop) ; dlf = lftop-lfbot ;

   dfac = 1.0f / nsper ;
   afac = dlf  / (atop-abot) ;

#undef  FRMOD
#define FRMOD(x) exp2f( lfbot + afac*((x)-abot) )

   ph  = 0.0f ;
   dph = 2.0f * PI / srate ;
   for( jj=ii=0 ; ii < nn ; ii++ ){
     ph += FRMOD(aa[ii]) * dph ; bb[jj++] = sin(ph) ;
#if 0
val = FRMOD(aa[ii]) ;
ININFO_message(" %g  %g",val,bb[jj-1]) ;
#endif
     if( nsper > 1 ){
       ip = ii+1 ; if( ip >= nn ) ip = nn-1 ;
       ai = aa[ii] ; ap = aa[ip] ;
       di = 1.0f   ; dp = 0.0f   ;
       for( kk=1 ; kk < nsper ; kk++ ){
         di -= dfac ; dp += dfac ;
         ph += FRMOD( di*ai+dp*ap ) * dph ; bb[jj++] = sin(ph) ;
#if 0
val = FRMOD( di*ai+dp*ap ) ;
ININFO_message(" %g  %g",val,bb[jj-1]) ;
#endif
       }
     }
   }

#undef FRMOD

   if( qim != imin ) mri_free(qim) ;
   return imout ;
}

#undef A4

/*---------------------------------------------------------------------------*/

#define PA 54.0f  /* 5 base notes (Hz) */
#define PC 64.0f
#define PD 72.0f
#define PE 81.0f
#define PG 96.0f

#define POCTA 4   /* number of octaves */

static int   npenta = 0 ;     /* will be 5*POCTA+1 */
static float *penta = NULL ;  /* will hold all the notes */

void sound_setup_penta( int octstart )
{
   int ii , jj ; float fac ;

   if( npenta > 0 ) return ;
   if( octstart < 0 || octstart >= POCTA ) octstart = 0 ;

   npenta = 5*POCTA+1 ;
    penta = (float *)malloc(sizeof(float)*npenta) ;
   fac    = 1.0f ;

   for( jj=ii=0 ; ii < POCTA ; ii++ ){
     if( ii >= octstart ){
       penta[jj++] = fac * PA ;
       penta[jj++] = fac * PC ;
       penta[jj++] = fac * PD ;
       penta[jj++] = fac * PE ;
       penta[jj++] = fac * PG ;
     }
     fac *= 2.0f ;
   }
   penta[jj++] = fac * PA ; /* complete last octave */
   npenta      = jj ;

   return ;
}

/*---------------------------------------------------------------------------*/

static int note_waveform = SOUND_WAVEFORM_TRIANGLE ;

void sound_set_note_waveform( int nn ){ note_waveform = nn; }

#undef  twoPI
#define twoPI   6.283185f
#undef  fourPI
#define fourPI 12.566371f
#undef  sixPI
#define sixPI  18.849556f

/* wav_xxx functions have period 1, amplitude 1, and assume t > 0 */

static INLINE float wav_sine(float t){ return ( 0.999f*sinf(twoPI*t) ) ; }

static INLINE float wav_h2sine(float t){
  return ( 0.953f*sinf(twoPI*t)+0.3f*cosf(fourPI*t) ) ;
}

static INLINE float wav_square(float t){
  float dt = fmodf(t,1.0f) ;
  return ( (dt < 0.45f) ? 0.999f
          :(dt < 0.55f) ? 0.999f-19.98f*(dt-0.45f)
          : -0.999f ) ;
}

#undef  TW
#define TW(x) (0.999f-3.996f*fabsf(x))

static INLINE float wav_triangle(float t){
  float dt = fmodf(t,1.0f) ;
  return ( (dt < 0.5f) ? TW(dt-0.25f) : -TW(dt-0.75f) ) ;
}

#undef  envA
#undef  envD
#undef  envS
#define envA 0.0666f
#define envD 0.1666f
#define envS 0.9333f

/* Envelope for note, with 0 <= t <= 1 */

static INLINE float ADSR_env(float t){
       if( t <= envA ) return (0.07f+0.93f*t/envA) ;
  else if( t <= envD ) return (1.0f-0.1f*(t-envD)/(envD-envA)) ;
  else if( t <= envS ) return (0.9f) ;
                       return (0.9f-0.83f*(t-envS)/(1.0f-envS)) ;
}
  
/*---------------------------------------------------------------------------*/
/* Note that if frq >= 0.5*srate, bad things will happen due to Mr Nyquist!! */
/*---------------------------------------------------------------------------*/

void sound_make_note( float frq, int waveform, int srate, int nsam, float *sam )
{
   int ii ; float dt,tt ;

   if( frq <= 0.0f || nsam < 9 || sam == NULL ) return ;

   if( srate < 8000 ) srate = 8000 ;

   dt = frq / srate ; tt = 0.0f ;

   /* form periodic waveform */

   switch( waveform ){

     default:
     case SOUND_WAVEFORM_SINE:
       for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] = wav_sine(tt); tt += dt; }
     break ;

     case SOUND_WAVEFORM_SQUARE:
       for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] = wav_square(tt); tt += dt; }
     break ;

     case SOUND_WAVEFORM_TRIANGLE:
       for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] = wav_triangle(tt); tt += dt; }
     break ;

     case SOUND_WAVEFORM_H2SINE:
       for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] = wav_h2sine(tt); tt += dt; }
     break ;

   }

   /* apply envelope (if note has enough samples) */

   if( nsam > 63 ){
     dt = 1.0f/nsam ; tt= 0.0f ;
     for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] *= ADSR_env(tt); tt += dt; }
   }

   return ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_sound_1D_to_notes( MRI_IMAGE *imin, int srate, int nsper )
{
   float *aa,*bb ;
   float abot,atop , fac ;
   int nout , ii,jj , nn ;
   MRI_IMAGE *imout , *qim ;

   /*--- deal with inputs ---*/

   if( imin == NULL ) return NULL ;

   nn = imin->nx ;
   if( nn < 2 ){
     ERROR_message("mri_sound_1D_to_notes: nn = %d",nn) ;
     return NULL ;
   }

   if( imin->kind != MRI_float ){
     qim = mri_to_float(imin) ;
   } else {
     qim = imin ;
   }
   aa = MRI_FLOAT_PTR(qim) ;

   if( srate < 8000 ) srate = 8000 ;
   if( nsper <= 9   ) nsper = srate/2 ;

   abot = atop = aa[0] ;
   for( ii=1 ; ii < nn ; ii++ ){
           if( aa[ii] < abot ) abot = aa[ii] ;
      else if( aa[ii] > atop ) atop = aa[ii] ;
   }

   /*-- create output ---*/

   nout  = nsper * nn ;
   imout = mri_new( nout , 1 , MRI_float ) ;
   bb    = MRI_FLOAT_PTR(imout) ;

   sound_setup_penta(1) ; /* skip octave 0 -- it's too low */

   if( abot >= atop ){    /* no data range??? */

     INFO_message("mri_sound_1D_to_notes: only 1 data value == something random") ;
     for( ii=0 ; ii < nn ; ii++ ){
       jj = lrand48() % npenta ;
       sound_make_note( penta[jj], note_waveform, srate, nsper, bb+(ii*nsper) ) ;
     }

   } else {

     fac = (npenta-1.0f) / (atop-abot) ;
     for( ii=0 ; ii < nn ; ii++ ){
       jj = rintf( fac*(aa[ii]-abot) ) ;
       sound_make_note( penta[jj], note_waveform, srate, nsper, bb+(ii*nsper) ) ;
     }
   }

   if( qim != imin ) mri_free(qim) ;
   return imout ;
}
