
/*** Contains sound recordings of specific waveforms;  ***/
/*** This file is to be #include-d into cs_playsound.c ***/

/*----------------------------------------------------------------------------*/

typedef struct {  /* struct for one sound waveform */
  int    nsam ;   /* number of points */
  char  *name ;   /* name (for menu) */
  short *sam  ;   /* 16-bit samples */
} sound_waveform ;

static int         num_waveform = 0 ;    /* number of waveforms */
static int    nsam_max_waveform = 0 ;    /* max number of samples */
static sound_waveform *waveform = NULL ; /* array of waveforms */

/* macro to add one struct to the library */

#define ADD_sound_waveform(nn,nam,sss)                                        \
  do{ waveform = (sound_waveform *)realloc(waveform,                          \
                                    sizeof(sound_waveform)*(num_waveform+1)); \
      waveform[num_waveform].nsam = (nn) ;                                    \
      waveform[num_waveform].name = (nam) ;                                   \
      waveform[num_waveform].sam  = (sss) ;                                   \
      if( (nn) > nsam_max_waveform ) nsam_max_waveform = (nn) ;               \
      num_waveform++ ;                                                        \
  } while(0)

/*----------------------------------------------------------------------------*/
/* load header arrays of sound waveforms, from @sound_to_header.csh */

#include "smb3_coin.h"     /* one ADD_sound_waveform() needed */
#include "smb3_fireball.h" /* below for each included array */

/*----------------------------------------------------------------------------*/
/* create the sound waveform library, as loaded from header files above */

void setup_sound_waveforms(void)
{
   if( num_waveform > 0 ) return ;

   ADD_sound_waveform( smb3_coin_nsam, smb3_coin_name, smb3_coin_data ) ;

   ADD_sound_waveform( smb3_fireball_nsam, smb3_fireball_name, smb3_fireball_data ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

int get_num_sound_waveforms()  /* return number of sound waveforms */
{
   setup_sound_waveforms() ;
   return num_waveform ;
}

/*----------------------------------------------------------------------------*/

int get_nsam_max_waveform()  /* return length of longest waveform */
{  return nsam_max_waveform ; }

/*----------------------------------------------------------------------------*/

char * get_name_sound_waveform(int ii) /* return the name of the ii-th sound */
{
   if( ii < 0 || ii >= num_waveform ) return NULL ;
   return waveform[ii].name ;       /* just the pointer - do not free this! */
}

/*----------------------------------------------------------------------------*/

int get_nsam_sound_waveform(int ii) /* return samples count of ii-th sound */
{
   if( ii < 0 || ii >= num_waveform ) return 0 ;
   return waveform[ii].nsam ;
}

/*----------------------------------------------------------------------------*/

short * get_sam_sound_waveform(int ii) /* return pointer to sample array */
{
   if( ii < 0 || ii >= num_waveform ) return NULL ;
   return waveform[ii].sam ;          /* just the pointer - do not free this! */
}
