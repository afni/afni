#include "coxplot.h"

/*---------------------------------------------------------------------
   Draws a perimeter around the subject coordinates (see plotpak_set).
   mbx = number of x-axis major divisions (large ticks)
   mlx = number of x-axis minor divisions (small ticks)
   mby , mly = ditto for y-axis
-----------------------------------------------------------------------*/
void plotpak_perim( int mbx , int mlx , int mby , int mly )
{
   integer mmbx=mbx , mmlx=mlx , mmby=mby , mmly=mly ;
   perim_( &mmbx,&mmlx,&mmby,&mmly ) ;
}

/*---------------------------------------------------------------
  Same as plotpak_perim, but puts labels at the major divisions.
-----------------------------------------------------------------*/
void plotpak_periml( int mbx , int mlx , int mby , int mly )
{
   integer mmbx=mbx , mmlx=mlx , mmby=mby , mmly=mly ;
   periml_( &mmbx,&mmlx,&mmby,&mmly ) ;
}
void plotpak_perimm( int mbx , int mlx , int mby , int mly , int ilab )
{
   integer mmbx=mbx , mmlx=mlx , mmby=mby , mmly=mly , ill=ilab ;
   perimm_( &mmbx,&mmlx,&mmby,&mmly,&ill ) ;
}

void plotpak_tick4( int mx, int lx , int my , int ly )
{
   integer lmajx=mx , lminx=lx , lmajy=my , lminy=ly ;
   tick4_(&lmajx, &lminx, &lmajy, &lminy);
}

void plotpak_labmod( int jsizx , int jsizy )
{
   integer zero = 0 , jx = jsizx , jy = jsizy ;
   labmod_( &zero , &zero , &zero , &zero , &jx , &jy , &zero , &zero , &zero ) ;
}
