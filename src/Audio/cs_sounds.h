
/*** this file is to be #include-d into cs_playsound.c ***/

/*----------------------------------------------------------------------------*/

typedef struct {
  int    nsam ;
  char  *name ;
  short *sam  ;
} sound_waveform ;

static int         num_waveform = 0 ;
static sound_waveform *waveform = NULL ;

#define ADD_sound_waveform(nn,nam,sss)                                        \
  do{ waveform = (sound_waveform *)realloc(waveform,                          \
                                    sizeof(sound_waveform)*(num_waveform+1)); \
      waveform[num_waveform].nsam = (nn) ;                                    \
      waveform[num_waveform].name = (nam) ;                                   \
      waveform[num_waveform].sam  = (sss) ;                                   \
      num_waveform++ ;                                                        \
  } while(0)

/*----------------------------------------------------------------------------*/

#include "smb3_coin.h"
#include "smb3_fireball.h"

/*----------------------------------------------------------------------------*/

void setup_sound_waveforms(void)
{
   if( num_waveform > 0 ) return ;

   ADD_sound_waveform( smb3_coin_nsam, smb3_coin_name, smb3_coin_data ) ;

   ADD_sound_waveform( smb3_fireball_nsam, smb3_fireball_name, smb3_fireball_data ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

int get_num_sound_waveforms()
{
   setup_sound_waveforms() ;
   return num_waveform ;
}

/*----------------------------------------------------------------------------*/

char * get_name_sound_waveform(int ii)
{
   if( ii < 0 || ii >= num_waveform ) return NULL ;
   return waveform[ii].name ;
}

/*----------------------------------------------------------------------------*/

int get_nsam_sound_waveform(int ii)
{
   if( ii < 0 || ii >= num_waveform ) return 0 ;
   return waveform[ii].nsam ;
}

/*----------------------------------------------------------------------------*/

short * get_sam_sound_waveform(int ii)
{
   if( ii < 0 || ii >= num_waveform ) return NULL ;
   return waveform[ii].sam ;
}
