#include "coxplot.h"
#include "stdio.h"
#include "math.h"

int main( int argc , char * argv[] )
{
   integer ii,jj,kk,ll ;
   float a,b,c,d,e,f,g,h ;

   ii = create_memplot( "Elvis" , 0 ) ;
   if( ii != 0 ){ fprintf(stderr,"Can't create Elvis!\n") ; exit(1) ; }

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;

   plotpak_set( 0.1,1.2 , 0.1,0.9 , 0.0,10.0 , 0.0,5.0 , 1 ) ;
   plotpak_periml( 5,10 , 5,10 ) ;

   set_color_memplot( 0.0 , 0.0 , 0.9 ) ;
   set_thick_memplot( 0.002 ) ;
   plotpak_frstpt( 0.0 , 0.0 ) ;
   for( ii=0 ; ii < 1000 ; ii++ ){
      a = 0.01 * ii ; b = sin(a) ; b = 5.0 * b * b / (1.0+0.1*a) ;
      plotpak_vector( a , b ) ;
   }

   set_color_memplot( 0.9 , 0.0 , 0.0 ) ;
   plotpak_frstpt( 0.0 , 5.0 ) ;
   for( ii=0 ; ii < 1000 ; ii++ ){
      a = 0.01 * ii ; b = cos(a) ; b = 5.0 * b * b / (1.0+0.1*a) ;
      plotpak_vector( a , b ) ;
   }

   memplot_to_ps( "Elvis.ps" ) ;
   printf("-- Wrote file Elvis.ps\n") ;
   exit(0) ;
}
