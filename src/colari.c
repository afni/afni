#include <stdio.h>
#include <stdlib.h>

int main( int argc , char * argv[] )
{
   int ii , iarg ;
   int xx,yy,zz ;
   float fx,fy,fz ;
   float sx,ax , sy,ay, sz,az ;
   int truncate = 0 ;

   if( argc < 7 ){
      fprintf(stderr,
              "Usage: colari [-int] sx ax sy ay sz ax < input > output\n"
              "where output x = sx * (input x) + ax, etc\n"
              " -int --> truncate results to integers\n" ) ;
      exit(0) ;
   }

   iarg = 1 ;
   if( strncmp(argv[iarg],"-int",4) == 0 ){
      truncate = 1 ;
      iarg++ ;
   }

   if( iarg+6 > argc ){
      fprintf(stderr,"Not enough arguments!  Try colari -help.\n") ;
      exit(-1) ;
   }

   sx = strtod( argv[iarg++] , NULL ) ;
   ax = strtod( argv[iarg++] , NULL ) ;
   sy = strtod( argv[iarg++] , NULL ) ;
   ay = strtod( argv[iarg++] , NULL ) ;
   sz = strtod( argv[iarg++] , NULL ) ;
   az = strtod( argv[iarg++] , NULL ) ;

   if( sx==0.0 || sy==0.0 || sz==0.0 ){
      fprintf(stderr,"Illegal command line!\n") ;
      exit(-1) ;
   } else {
      fprintf(stderr,"sx=%g ax=%g  sy=%g ay=%g  sz=%g az=%g\n",
              sx,ax,sy,ay,sz,az ) ;
   }

   do {

      ii = fscanf( stdin , " %f %f %f" , &fx,&fy,&fz ) ;
      if( ii != 3 ) break ;

      fx = sx*fx + ax ;
      fy = sy*fy + ay ;
      fz = sz*fz + az ;

      if( truncate ){
         xx = fx ; yy = fy ; zz = fz ;
         fprintf( stdout , "%d %d %d\n" , xx,yy,zz ) ;
      } else {
         fprintf( stdout , "%g %g %g\n" , fx,fy,fz ) ;
      }
   } while(1) ;
   exit(0) ;
}
