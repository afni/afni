#include "coxplot.h"

/*-----------------------------------------------------------------
  Writes a character string at subjective location (x,y).
  isiz  = size of characters (in units of 0.001 * pagewidth)
  ior   = orientation in integer degrees CCW from horizontal
  icent = centering option:
          +1 => (x,y) is center of right edge of last character
           0 => (x,y) is center of entire string
          -1 => (x,y) is center of left edge of first character
          -2 => (x,y) is is lower left corner of string
  The characters are written with lines defined by the original
  NCAR font.
-------------------------------------------------------------------*/
void plotpak_pwrit( float x , float y , char * ch , int isiz , int ior , int icent )
{
   real xx=x , yy=y ;
   integer nch=strlen(ch) , iisiz=isiz , iior=ior , iicent=icent ;
   ftnlen ch_len = nch ;
   pwrit_( &xx , &yy , ch , &nch , &iisiz , &iior , &iicent , ch_len ) ;
}

