#include "coxplot.h"

/*----------------------------------------------------------------------------
  Same as pwrit, but with a fancier font (that uses more lines per character)
------------------------------------------------------------------------------*/
void plotpak_pwritf( float x , float y , char * ch , int isiz , int ior , int icent )
{
   real xx=x , yy=y ;
   integer nch=strlen(ch) , iisiz=isiz , iior=ior , iicent=icent ;
   ftnlen ch_len = nch ;
   pwritf_( &xx , &yy , ch , &nch , &iisiz , &iior , &iicent , ch_len ) ;
}

void plotpak_pwritf_phys( float x , float y , char * ch , int isiz , int ior , int icent )
{
   real xx=x , yy=y ;
   integer nch=strlen(ch) , iisiz=isiz , iior=ior , iicent=icent ;
   ftnlen ch_len = nch ;
   nch = -nch ;
   pwritf_( &xx , &yy , ch , &nch , &iisiz , &iior , &iicent , ch_len ) ;
}
