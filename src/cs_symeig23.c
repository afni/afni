#include "mrilib.h"

void symeig_3( double *a , double *e )
{
   double aa,bb,cc,dd,ee,ff ;
   double a1,a2,a3 , qq,rr, qs,th ;

   if( a == NULL || e == NULL ) return ;

   aa = a[0] ; bb = a[1] ; cc = a[2] ;
   dd = a[4] ; ee = a[5] ; ff = a[8] ;

   a1 = -(aa+dd+ff) ;
   a2 =  (aa*ff+aa*dd+dd*ff - bb*bb-cc*cc-ee*ee) ;
   a3 =  ( aa*(ee*ee-dd*ff) + bb*(bb*ff-cc*ee) + cc*(cc*dd-bb*ee) ) ;

   qq = (a1*a1 - 3.0*a2) / 9.0 ;
   rr = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3) / 54.0 ;

   qs = sqrt(qq) ; th = acos( rr / (qs*qq) ) ;

   lam1 = -2.0 * qs * cos(  th        /3.0 ) - a1 / 3.0 ;
   lam2 = -2.0 * qs * cos( (th+2.0*PI)/3.0 ) - a1 / 3.0 ;
   lam3 = -2.0 * qs * cos( (th+4.0*PI)/3.0 ) - a1 / 3.0 ;
