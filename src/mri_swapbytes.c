#include "mrilib.h"

/*** 7D SAFE ***/

#define SWAB16(x) ( ( ((x)&0x00ffU)<<8 ) | ( ((x)&0xff00U)>>8 ) )

void mri_swapbytes( MRI_IMAGE *im )
{
   register int ii , npix ;

WHOAMI ; IMHEADER(im) ;

   if( im->kind != MRI_short ){
      fprintf( stderr , "mri_swapbytes called with non-short image kind\n" ) ;
      MRI_FATAL_ERROR ;
   }

   npix = im->nvox ;

   for( ii=0 ; ii < npix ; ii++ )
      im->im.short_data[ii] = SWAB16( im->im.short_data[ii] ) ;

   return ;
}
