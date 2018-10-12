#include "mrilib.h"

static byte mulaw( float x ) ; /* prototype for mu-law conversion */

#undef  DEFAULT_SRATE
#define DEFAULT_SRATE 16000

                               /* get specific sound recordings */
#include "cs_sounds.h"         /* stored in Audio subdirectory */

/*--------------------------------------------------------------------------*/
/*---------- find a sound playing program ----------*/

static int found_pprog = -1 ;
static char *pprog      = NULL ;
static char *pprog_name = NULL ;

char * get_sound_player(void)
{
   if( found_pprog < 0 ){
     char *eee ;
     eee = getenv("AFNI_SOUND_PLAYER") ;
     if( eee != NULL || THD_is_executable(eee) ){
       pprog = strdup(eee) ;
     } else {
         pprog = THD_find_executable("play") ;      /* sox */
       if( pprog == NULL )
         pprog = THD_find_executable("afplay") ;    /* OS X */
       if( pprog == NULL )
         pprog = THD_find_executable("mplayer") ;   /* Linux or Mac */
       if( pprog == NULL )
         pprog = THD_find_executable("aplay") ;     /* Linux */
     }

     found_pprog = (pprog != NULL) ;
     if( found_pprog ) pprog_name = THD_trailname(pprog,0) ;
   }

   return pprog ;
}

/*--------------------------------------------------------------------------*/
static char note_type[32] = "pluck" ;
static int  gain_value    = -27 ;
static int  two_tone      = 0 ;

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

/*-----*/

#if 0
#define NFORKLIST 4
static int  nfork_pg = 0 ;
static int  forklist_pg[NFORKLIST] ;

void play_sound_killpg(void){
  int ii ;
  for( ii=0 ; ii < NFORKLIST ; ii++ ){
    if( forklist_pg[ii] > 0 ){
      killpg(forklist_pg[ii],SIGKILL) ;
    }
  }
  return ;
}
#endif

/*-----*/

#if 0
static int  nfork_id = 0 ;
static int  forklist_id[NFORKLIST] ;

void play_sound_killid(void)
{
  int ii ;
INFO_message("play_sound_killid()") ;
  for( ii=0 ; ii < NFORKLIST ; ii++ ){
    if( forklist_id[ii] > 0 ){
ININFO_message("try to kill pid %d",forklist_id[ii]) ;
      kill(forklist_id[ii],SIGKILL) ;
      forklist_id[ii] = 0 ;
    }
  }
  nfork_id = 0 ;
  return ;
}
#endif

/*-----*/

void kill_sound_players(void)
{
  char cmd[1024] ;
  if( pprog_name != NULL ){
    sprintf(cmd,"killall %s >& /dev/null",pprog_name) ;
    system(cmd) ;
  }
  return ;
}

/*---------------------------------------------------------------------------*/
/* Play sound from a 1D image (up to 4 columns) */

void mri_play_sound( MRI_IMAGE *imin , int ignore )
{
   MRI_IMAGE *qim ;
   char *pre , fname[256] , cmd[2048] , extras[1024] ;
   int ii , nsper ; float dt ;

   (void)get_sound_player() ;
   if( imin == NULL || imin->nx < 2 || pprog == NULL ) return ;

   kill_sound_players() ;

   if( !psk_set ){ atexit(kill_sound_players) ; psk_set = 1 ; }

   ii = (int)fork() ;
   if( ii != 0 ){  /*-- return to parent process --*/
#if 0
     int jj = getpgid(ii) ;
     if( nfork_pg == 0 || (nfork_pg > 0 && jj != forklist_pg[nfork_pg-1]) ){
       forklist_pg[nfork_pg] = getpgid(ii) ;
       nfork_pg              = (nfork_pg+1)%NFORKLIST ;
     }
#endif
#if 0
     jj = ii ;
     if( nfork_id == 0 || (nfork_id > 0 && jj != forklist_id[nfork_id-1]) ){
       forklist_id[nfork_id] = jj ;
       nfork_id              = (nfork_id+1)%NFORKLIST ;
     }
#endif
     return ;
   }

   dt = (imin->nx <= 100) ? 0.20f
       :(imin->nx <= 200) ? 0.15f
       :                    0.10f ;
   nsper = (int)rintf(DEFAULT_SRATE*dt) ;

   qim = mri_sound_1D_to_notes( imin , DEFAULT_SRATE , nsper , 4 , ignore , 0 ) ;
   if( qim == NULL ) _exit(0) ;

   pre = UNIQ_idcode_11() ;  /* make up name for sound file */
   sprintf(fname,"AFNI_SOUND_TEMP.%s.au",pre) ;
   unlink(fname) ;           /* remove sound file, in case it already exists */
   sound_write_au_16PCM( fname, qim->nx, MRI_FLOAT_PTR(qim), DEFAULT_SRATE, 0.1f ) ;
   extras[0] = '\0' ;
   if( strcmp(pprog_name,"play") == 0 )
     strcat(extras," reverb 33") ;
   sprintf(cmd,"%s %s %s >& /dev/null",pprog,fname,extras) ;
   system(cmd) ;
   unlink(fname) ;
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

typedef struct { unsigned char a,b ; } twoby ;

static void swap_twobyar( int n , void *ar )
{
   int ii ;
   twoby *tb = (twoby *)ar ;
   unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt       = tb[ii].a ;
     tb[ii].a = tb[ii].b ;
     tb[ii].b = tt ;
   }
   return ;
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

   if( srate < 8000 ) srate = DEFAULT_SRATE ;
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

   if( srate < 8000 ) srate = DEFAULT_SRATE ;
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
/* samples in aa[] are between -1 and 1.
   sample rate (per second) is in srate.
   scl is a scale down factor (0 < scl <= 1).
*//*-------------------------------------------------------------------------*/

void sound_write_au_16PCM( char *fname, int nn, float *aa, int srate, float scl )
{
   FILE *fp ;
   uint32_t ival , jval ;
   int16_t *bb ;
   int ii ; float fac , val ;

   /* check inputs */

   if( fname == NULL || nn < 2 || aa == NULL ){
     ERROR_message("Illegal inputs to sound_write_au :(") ;
     return ;
   }

   if( srate < 8000 ) srate = DEFAULT_SRATE ;
   if( scl   < 0.0f || scl > 1.0f ) scl = 1.0f ;

   /* open output file */

   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
     ERROR_message("Can't open audio output file '%s'",fname) ;
     return ;
   }

   /* write .au file header */

   sound_write_au_header( fp , 2*nn , srate , 3 ) ;

   /* convert and write data */

   bb  = (int16_t *)malloc(sizeof(int16_t)*nn) ;
   fac = 32767.444f * scl ;
   for( ii=0 ; ii < nn ; ii++ ){
     val = fac*aa[ii] ;
     bb[ii] = ( val < -32767.0f ) ? -32767 :
              ( val >  32767.0f ) ?  32767 : (int16_t)rintf(val) ;
   }
   if( little_endian ) swap_twobyar( nn , bb ) ;

   fwrite( bb , 2 , nn , fp ) ;

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

   if( srate < 8000 ) srate = DEFAULT_SRATE ;
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

static INLINE float wav_sqsine(float t){
  float val = sinf(twoPI*t) , aval ;
#if 1
  aval = sqrtf(fabsf(val)) ;
#else
  aval = powf( fabsf(val) , 0.6f ) ;
#endif
  return ( (val >= 0.0f ) ?  aval : -aval ) ;
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

static int use_ADSR = 1 ;

void sound_set_note_ADSR(int qq){ use_ADSR = qq ; }

/*---------------------------------------------------------------------------*/
/* Note that if frq >= 0.5*srate, bad things will happen due to Mr Nyquist!! */
/*---------------------------------------------------------------------------*/

static float ttn = 0.0f ;  /* continuous time between notes */

static void reset_note_ttn(void){ ttn = 0.0f ; }

void sound_make_note( float frq, int waveform, int srate, int nsam, float *sam )
{
   int ii ; float dt,tt ;

   if( frq <= 0.0f || nsam < 9 || sam == NULL ) return ;

   if( srate < 8000 ) srate = DEFAULT_SRATE ;

   dt = frq / srate ; tt = ttn ;

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

     case SOUND_WAVEFORM_SQSINE:
       for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] = wav_sqsine(tt); tt += dt; }
     break ;

   }

   ttn = tt ;  /* continuous time between notes */

   /* apply envelope (if note has enough samples) */

   if( nsam > 127 && use_ADSR ){
     dt = 1.0f/nsam ; tt= 0.0f ;
     for( ii=0 ; ii < nsam ; ii++ ){ sam[ii] *= ADSR_env(tt); tt += dt; }
   } else if( nsam > 24 ){
#if 0
     float fac ;
     for( ii=0 ; ii < 9 ; ii++ ){
       fac = (ii+1)*0.1f ; sam[ii] *= fac ; sam[nsam-1-ii] *= fac ;
     }
#endif
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#define VAL_TO_CODE(v) ((int)rintf((v)-SOUND_WAVECODE_BASE))

MRI_IMAGE * mri_sound_1D_to_notes( MRI_IMAGE *imin, int srate, int nsper,
                                   int ny, int ignore, int use_wavecodes )
{
   float *aa,*bb,*qa ;
   float abot,atop , fac , *valn ;
   int nout , ii,jj , nn , qq ;
   MRI_IMAGE *imout , *qim ;
   int max_wavecode=0 ;
   int ncode=0 , nsamcode=0 ;

   /*--- deal with inputs ---*/

   if( imin == NULL ) return NULL ;

   nn = imin->nx ;
   if( nn < 2 ){
     ERROR_message("mri_sound_1D_to_notes: nn = %d",nn) ;
     return NULL ;
   }

   if( ignore < 0 || nn-ignore < 2 ) ignore = 0 ;

   if( imin->kind != MRI_float ){
     qim = mri_to_float(imin) ;
   } else {
     qim = imin ;
   }
   aa = MRI_FLOAT_PTR(qim) ;

        if( ny <= 0       ) ny = 1 ;
   else if( ny >  qim->ny ) ny = qim->ny ;

   if( srate < 8000 ) srate = DEFAULT_SRATE ;
   if( nsper <= 9   ) nsper = srate/2 ;

   /*-- check for wavecodes embedded in the data --*/

   if( use_wavecodes ){
     max_wavecode  = get_num_sound_waveforms() ;
     use_wavecodes = max_wavecode > 0 ;
   }

   if( use_wavecodes ){
   }

   /*-- create output ---*/

   nout  = nsper * (nn-ignore) ;
   imout = mri_new( nout , 1 , MRI_float ) ;
   bb    = MRI_FLOAT_PTR(imout) ;  /* is full of zeros */

   sound_setup_penta(1) ; /* skip octave 0 -- it's too low */

   valn = (float *)malloc(sizeof(float)*nsper) ; /* notes */

   for( qq=0 ; qq < ny ; qq++ ){  /* process first ny columns */
     qa = aa + (nn*qq) ;          /* add their sounds all up */
     abot = atop = qa[ignore] ;
     reset_note_ttn() ;           /* re-start time at 0 */
     for( ii=ignore+1 ; ii < nn ; ii++ ){
             if( qa[ii] < abot ) abot = qa[ii] ;
        else if( qa[ii] > atop ) atop = qa[ii] ;
     }

     if( abot < atop ){ /* some range to the numbers */
       fac = (npenta-1.0f) / (atop-abot) ;
       for( ii=ignore ; ii < nn ; ii++ ){
         jj = rintf( fac*(qa[ii]-abot) ) ;
         sound_make_note( penta[jj], note_waveform, srate, nsper, valn ) ;
         for( jj=0 ; jj < nsper ; jj++ ) bb[((ii-ignore)*nsper)+jj] += valn[jj];
       }
     }
   }

   abot = mri_maxabs(imout) ;
   if( abot == 0.0f ){  /* nothing computed? do something random! */
     for( ii=ignore ; ii < nn ; ii++ ){
       jj = lrand48() % npenta ;
       sound_make_note( penta[jj], note_waveform,
                        srate, nsper, bb+((ii-ignore)*nsper) ) ;
     }
   }

   THD_normmax( nout , bb ) ;  /* max abs value = 1 */

   free(valn) ;
   if( qim != imin ) mri_free(qim) ;

   return imout ;
}
