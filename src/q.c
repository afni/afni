#include "mrilib.h"

int main( int argc , char ** argv )
{
   int npt , ii , nx ;
   float *pol , *azi , *xyz , *wt , wtsum=0.0 ;
   MRI_IMAGE *imxyz ;

   imxyz = mri_read_ascii( argv[1] ) ;
   if( imxyz == NULL ) exit(1) ;
   npt = imxyz->ny ; nx = imxyz->nx ;
   xyz = MRI_FLOAT_PTR(imxyz) ;

   if( nx == 2 ){
      pol = (float *)malloc(sizeof(float)*npt) ;
      azi = (float *)malloc(sizeof(float)*npt) ;
      for( ii=0 ; ii < npt ; ii++ ){
         pol[ii] = xyz[2*ii  ] ;
         azi[ii] = xyz[2*ii+1] ;
      }
      ii = sphere_voronoi_angles( npt , pol,azi , &wt ) ;
      free(pol) ; free(azi) ;
   } else {
      ii = sphere_voronoi_vectors( npt , xyz , &wt ) ;
   }
   if( ii == 0 ) exit(1) ;

   for( ii=0 ; ii < npt ; ii++ ){
      wtsum += wt[ii] ;
      printf("point %2d = %g\n",ii,wt[ii]) ;
   }
   printf("wtsum = %g\n",wtsum) ;
   exit(0) ;
}
