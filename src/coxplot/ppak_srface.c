#include "coxplot.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*----------------------------------------------------
  Make a surface plot from a data grid:
    x[0..nx-1] = x-axis grid
    y[0..ny-1] = y-axis grid
    z[i+j*nx]  = data grid: i=0..nx-1, j=0..ny-1
    nx , ny    = dimensions
    theta, phi = viewpoint angles in degrees
                 (theta = down from z-axis,
                  phi   = rotation from x-towards-y)
------------------------------------------------------*/

void plotpak_srface( float * x , float * y , float * z ,
                     int nx , int ny ,
                     float theta , float phi )
{
   integer * m ;
   float * xx , * yy ;
   integer  mx , nnx , nny ;
   float s[6] , stereo=0.0 , zbot,ztop , rad , cth,sth,cph,sph ;
   int ii , nxy=nx*ny ;

   if( nx <= 1 || ny <= 1 || z == NULL ) return ;

   mx = nnx = (integer) nx ; nny = (integer) ny ;

   zbot = ztop = z[0] ;
   for( ii=1 ; ii < nxy ; ii++ ){
           if( z[ii] < zbot ) zbot = z[ii] ;
      else if( z[ii] > ztop ) ztop = z[ii] ;
   }

   xx = x ;
   if( xx == NULL ){
      xx = (float *) malloc( sizeof(float) * nx ) ;
      for( ii=0 ; ii < nx ; ii++ ) xx[ii] = ii ;
   }

   yy = y ;
   if( yy == NULL ){
      yy = (float *) malloc( sizeof(float) * ny ) ;
      for( ii=0 ; ii < ny ; ii++ ) yy[ii] = ii ;
   }

   s[3] = 0.5 * (xx[0] + xx[nx-1]) ;
   s[4] = 0.5 * (yy[0] + yy[ny-1]) ;
   s[5] = 0.5 * (zbot  + ztop    ) ;

   rad = 100.0 * ( fabs(xx[nx-1]-xx[0]) + fabs(yy[ny-1]-yy[0]) + (ztop-zbot) ) ;
   cth = cos( theta * 3.1416/180.0 ) ; sth = sin( theta * 3.1416/180.0 ) ;
   cph = cos( phi   * 3.1416/180.0 ) ; sph = sin( phi   * 3.1416/180.0 ) ;

   s[0] = s[3] + rad * sth * cph ;
   s[1] = s[4] + rad * sth * sph ;
   s[2] = s[5] + rad * cth ;

   m = (integer *) malloc( sizeof(integer) * 2*nx*ny ) ;  /* workspace */

   srface_( xx , yy , z , m , &mx , &nnx , &nny , s , &stereo ) ;

   free(m) ;
   if( yy != y ) free(yy) ;
   if( xx != x ) free(xx) ;
   return ;
}
