/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef PI
#  define PI 3.1415926536
#endif

/*
   mode  : -1 forward, 1 inverse.
   idim  : FFT dimension, max 1024.
   xr    : real array.
   xi    : imaginary part of the array.
*/

/* --------------------------------- */
   void cfft(mode,idim,xr,xi)
   int   mode,idim;
   float *xr,*xi;
/* --------------------------------- */
{
#define IDMAX  1024
#define LIDMAX 10
   static int idold = -999 ;
   register int i0,i1,i2,i4,i5,m0, id = idim;
   float    f1,f3,f4,f5,al,co,si,md = mode;
   static   int    n,m[LIDMAX];
   static   float  f2,c[IDMAX/2],s[IDMAX/2];

   /**** preparations if id has changed since last call ****/

   if (idold != id) {
     idold = id ;
     /* check for power of 2 */
     for( i4=4 ; i4 <= IDMAX ; i4 *= 2 ){
        if( id == i4 )break ;
     }
     if( id != i4 ){
       fprintf(stderr,"\n In cfft : illegal idim=%d\n",idim);
       EXIT(1) ;
     }
     f2     = id;
     n      = log(f2)/log(2.) + .5;
     m[n-1] = 1;
     al     = 2.*PI/f2;
     co     = cos(al);
     si     = sin(al);
     c[0]   = 1.;
     s[0]   = 0.;
     for(i4 = 1; i4 < 512; i4++) {
       c[i4] = c[i4-1]*co - s[i4-1]*si;
       s[i4] = s[i4-1]*co + c[i4-1]*si;
     }
     for(i4 = n-1; i4 >= 1; i4--) m[i4-1] = m[i4]*2;
   }

   /**** Main loop starts here ****/

   for(i0 = 0; i0 < n; i0++) {
     i1 = 0;
     m0 = m[i0];
     for(i2 = 0; i2 < m[n-i0-1]; i2++) {
       f4 = c[i1];
       f5 = s[i1]*md;
         for(i4 = 2*m0*i2; i4 < m0*(2*i2+1); i4++) {
           f3 = xr[i4+m0]*f4 - xi[i4+m0]*f5;
           f1 = xi[i4+m0]*f4 + xr[i4+m0]*f5;
           xr[i4+m0] = xr[i4] - f3;
           xr[i4] += f3;
           xi[i4+m0] = xi[i4] - f1;
           xi[i4] += f1;
         }
        for(i4 = 1; i4 < n; i4++) {
           i5 = i4;
           if (i1 < m[i4]) goto i1_plus1;
           i1 -= m[i4];
         }
i1_plus1: i1 += m[i5];
     }
   }
   i1 = 0;
   for(i4 = 0; i4 < id; i4++) {
     if (i1 > i4) {
       f3 = xr[i4];
       f1 = xi[i4];
       xr[i4] = xr[i1];
       xi[i4] = xi[i1];
       xr[i1] = f3;
       xi[i1] = f1;
     }
     for(i2 = 0; i2 < n; i2++) {
       i5 = i2;
       if (i1 < m[i2]) goto i1_plus2;
       i1 -= m[i2];
     }
i1_plus2:   i1 += m[i5];
   }

   if (md > 0.) {
     f1 = 1./f2;
     for(i4 = 0; i4 < id; i4++) {
       xr[i4] *= f1;
       xi[i4] *= f1;
     }
   }
   return;
}

/*********************************************************************/

/*----------------------------------*/
void cfft2d( mode , nx,ny , xr,xi )
  int mode , nx,ny ;
  float *xr , *xi ;
/*----------------------------------*/
{
   float *rbuf , *ibuf ;
   register int ii , jj , jbase ;

   rbuf = (float *)malloc( ny * sizeof(float) ) ;
   ibuf = (float *)malloc( ny * sizeof(float) ) ;
   if( rbuf == NULL || ibuf == NULL ){
      fprintf(stderr,"malloc error in cfft2d\n") ;
      EXIT(1) ;
   }

   for( jj=0 ; jj < ny ; jj++ ){
      jbase = nx * jj ;
      cfft( mode , nx , &xr[jbase] , &xi[jbase] ) ;
   }

   for( ii=0 ; ii < nx ; ii++ ){
      for( jj=0 ; jj < ny ; jj++ ){
         rbuf[jj] = xr[ii + jj*nx] ;
         ibuf[jj] = xi[ii + jj*nx] ;
      }
      cfft( mode , ny , rbuf , ibuf ) ;
      for( jj=0 ; jj < ny ; jj++ ){
         xr[ii+jj*nx] = rbuf[jj] ;
         xi[ii+jj*nx] = ibuf[jj] ;
      }
   }

   free(rbuf) ; free(ibuf) ;
   return ;
}
