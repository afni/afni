/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define MAXCOL 255
byte rgb[3*MAXCOL] ;
int  ncol = 0 ;

#define ASP(x,y) ( (x)[0]=(y)[0]   , (x)[1]=(y)[1]   , (x)[2]=(y)[2]  )
#define EQP(x,y) ( (x)[0]==(y)[0] && (x)[1]==(y)[1] && (x)[2]==(y)[2] ) 

int main( int argc , char * argv[] )
{
   int makemap , iarg , ii,jj ;
   char * mapfile = NULL ;
   MRI_IMAGE * mapim = NULL , * ppmim = NULL , * qqim = NULL ;
   byte * ppmar , * mapar , * qqar ;

   if( argc < 2 || strncmp(argv[1],"-help",5)==0 ){
      printf("Usage 1: Make a map of the colors in a ppm raw file\n"
             "  ppmtocmap -make ppmfile\n"
             "\n"
             "Usage 2: Make a color mapped pgm file from a map and a raw ppm file\n"
             "  ppmtocmap -map mapfile ppmfile\n"
             "\n"
             "Both usages write to stdout.\n"
            ) ;
      exit(0) ;
   }

   iarg = 1 ;
   if( strncmp(argv[1],"-make",5) == 0 ){
      makemap = 1 ; iarg++ ;
   } else if( strncmp(argv[1],"-map",4) == 0 ){
      makemap = 0 ; iarg++ ;
      if( iarg >= argc ){ fprintf(stderr,"No file after -map\n") ; exit(1) ; }
      mapfile = argv[iarg++] ;
      mapim = mri_read_ppm( mapfile ) ;
      if( mapim == NULL ){fprintf(stderr,"Can't read -map file %s\n",mapfile);exit(1);}
      mapar = MRI_RGB_PTR(ppmim) ;
   } else {
      fprintf(stderr,"Illegal first switch: %s\n",argv[1]) ;
      exit(1) ;
   }

   if( iarg >= argc ){ fprintf(stderr,"No input ppmfile?\n") ; exit(1) ; }

   ppmim = mri_read_ppm( argv[iarg] ) ;
   if( ppmim == NULL ){fprintf(stderr,"Can't read ppmfile %s\n",argv[iarg]);exit(1);}
   ppmar = MRI_RGB_PTR(ppmim) ;

   qqim = (makemap) ? ppmim : mapim ;
   qqar = MRI_RGB_PTR(qqim) ;

   ASP( rgb , qqar ) ; ncol = 1 ;
   for( ii=1 ; ii < qqim->nvox  ; ii++ ){
      for( jj=0 ; jj < ncol ; jj++ )
         if( EQP(rgb+3*jj,qqar+3*ii) ) break ;

      if( jj == ncol ){
         ASP( rgb+3*ncol , qqar+3*ii ) ;
         ncol++ ;
         if( ncol > MAXCOL ){
            fprintf(stderr,"More than %d colors at pixel %d\n",MAXCOL,ii) ;
            exit(1) ;
         }
      }
   }
   fprintf(stderr,"found %d colors\n",ncol) ;

   if( makemap ){
      printf("P6 %d 1 255\n" , ncol ) ;
      fwrite( rgb , 1 , 3*ncol , stdout ) ;
      exit(0) ;
   }

   qqim = mri_new( ppmim->nx , ppmim->ny , MRI_byte ) ;
   qqar = MRI_BYTE_PTR(qqim) ;

   for( ii=0 ; ii < ppmim->nvox ; ii++ ){
      for( jj=0 ; jj < ncol ; jj++ )
         if( EQP(rgb+3*jj,ppmar+3*ii) ) break ;

      if( jj < ncol ) qqar[ii] = jj ;
      else { fprintf(stderr,"Unmatched color at pixel %d\n",ii) ; exit(1) ; }
   }

   printf("P5 %d %d 255\n" , ppmim->nx , ppmim->ny ) ;
   fwrite( qqar , 1 , ppmim->nx * ppmim->ny , stdout ) ;
   exit(0) ;
}
