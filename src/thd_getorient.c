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
