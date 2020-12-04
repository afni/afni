#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Input is a dataset axes struct and an orientation code.
    Output is an int saying which dataset axis is this code.
     -  +1 => dataset +x-axis
     -  -1 => dataset -x-axis, etc.
     -   0 => bad inputs
     - RWCox - 19 Mar 2003
-----------------------------------------------------------------------------*/

int THD_get_axis_direction( THD_dataxes * daxes, int orient_code )
{
   if( daxes == NULL ) return 0;

   if(                 daxes->xxorient  == orient_code ) return  1 ;
   if( ORIENT_OPPOSITE(daxes->xxorient) == orient_code ) return -1 ;
   if(                 daxes->yyorient  == orient_code ) return  2 ;
   if( ORIENT_OPPOSITE(daxes->yyorient) == orient_code ) return -2 ;
   if(                 daxes->zzorient  == orient_code ) return  3 ;
   if( ORIENT_OPPOSITE(daxes->zzorient) == orient_code ) return -3 ;
   return 0 ;
}

/* fill ostr with the 3 letter orient code (e.g. LPI) given integer codes
 *    - null terminate ostr (so there must be space for 4 bytes)
 *    - return 0 on success, 1 on error
 * (ostr is just char *, the 4 is a reminder)   23 Jan 2013 [rickr] */
int THD_fill_orient_str_3( THD_dataxes * daxes, char ostr[4] )
{
   if ( ! daxes || ! ostr ) return 1;

   ostr[0] = ORIENT_first[daxes->xxorient];
   ostr[1] = ORIENT_first[daxes->yyorient];
   ostr[2] = ORIENT_first[daxes->zzorient];
   ostr[3] = '\0';

   return 0;
}
/*
  Fill oint with 3 int orient code in RLPAIS encoding (e.g., 'LPI'
  leads to [1, 2, 4]
*/
int THD_fill_orient_int_3_rlpais( THD_dataxes * daxes, int oint[3] )
{
   if ( ! daxes || ! oint ) return 1;

   oint[0] = daxes->xxorient;
   oint[1] = daxes->yyorient;
   oint[2] = daxes->zzorient;

   return 0;
}
/* fill ostr with the 6 letter orient code (e.g. LRPAIS) given integer codes
 *    - null terminate ostr (so there must be space for 7 bytes)
 *    - the ^1 is to toggle between 0/1, 2/3, 4/5
 *    - return 0 on success, 1 on error
 * (ostr is just char *, the 7 is a reminder)   23 Jan 2013 [rickr] */
int THD_fill_orient_str_6( THD_dataxes * daxes, char ostr[7] )
{
   if ( ! daxes || ! ostr ) return 1;

   ostr[0] = ORIENT_first[daxes->xxorient];
   ostr[1] = ORIENT_first[daxes->xxorient^1];
   ostr[2] = ORIENT_first[daxes->yyorient];
   ostr[3] = ORIENT_first[daxes->yyorient^1];
   ostr[4] = ORIENT_first[daxes->zzorient];
   ostr[5] = ORIENT_first[daxes->zzorient^1];
   ostr[6] = '\0';

   return 0;
}
