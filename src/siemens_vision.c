#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <math.h>
#include "siemens_vision.h"

static void byteswap8 (double *x) {
  char *p1,*p2; double d; int i;
  p1=(char *)x; p2=(char *)&d;
  for(i=0;i<8;i++) p2[7-i]=p1[i];
  (*x)=d;
}

static void byteswap4( u_int * x ) {
  char *p1,*p2 ; u_int d ; int i ;
  p1=(char *)x; p2=(char *)&d;
  for(i=0;i<4;i++) p2[3-i]=p1[i];
  (*x)=d;
}

/*----------------------------------------------------------------------------
 12 Mar 2001: Program siemens_vision.c, to print info from Siemens .ima header
              Adapted from program thor.cpp by CELS (?)
              Compile this with
                 cc -o siemens_vision siemens_vision.c -I. -lm
              or
                 make siemens_vision
              (if you are getting this as part of the AFNI package) -- RWCox
------------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   struct Siemens_vision_header head ;
   FILE * fp ;
   char orients[7] ;
   int i,j,xx,yy , matrix , swap , slices ;
   double dd , qq ;
   struct stat file_stat ;
   int imagesize=64 ;
   short *imar=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: siemens_vision filename\n"
             "Prints out information from the Siemens .ima file header.\n") ;
      exit(0) ;
   }

   /*--- check file size ---*/

   i = stat( argv[1] , &file_stat ) ;
   if( i < 0 ){
      fprintf(stderr,"** Can't access file %s\n",argv[1]) ; exit(1) ;
   }

   /*--- read header data ---*/

   fp = fopen( argv[1] , "r" ) ;
   if( fp == NULL ) exit(1) ;
   fread( &head , sizeof(struct Siemens_vision_header) , 1 , fp ) ;

   /*-- check some integer in header to determine if we need to byteswap --*/

   swap = ( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 12 ) ;
   if( swap ){
      byteswap4( &(head.SiemensStudyDateMM) ) ;
      if( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 12 ){
         printf("** Can't determine byte swap status of file!\n") ;
         byteswap4( &(head.SiemensStudyDateMM) ) ;
         swap = 0 ;
      }
   }

   /*-- find image size from header --*/

#if 1
   if( swap ) byteswap4( &(head.DisplayMatrixSize) ) ;
   imagesize = head.DisplayMatrixSize ;
#endif

   /*-- determine number of sub-images in file --*/

   if( file_stat.st_size==(imagesize * imagesize * 64 * 2 + SIEMENS_HEADERSIZE) ){
     matrix = 8 ;
   } else if( file_stat.st_size==(imagesize * imagesize * 16 * 2 + SIEMENS_HEADERSIZE) ){
     matrix = 4;
   } else {
     matrix = 0 ;
     printf("Don't recognize file size. Is this a Siemens Magnetom Vision .ima file?\n") ;
   }

   /*-- read image data from file (but don't byteswap it) --*/

   if( matrix > 0 ){
      imar = (short *) calloc(sizeof(short),matrix*matrix*imagesize*imagesize) ;
      fseek( fp , SIEMENS_HEADERSIZE , SEEK_SET ) ;
      fread( imar , sizeof(short) , matrix*matrix*imagesize*imagesize , fp ) ;
   }

   fclose(fp) ;

   /*--- print random info ---*/

   printf("Manufact = %s\n",head.Manufacturer) ;
   printf("Institut = %s\n",head.InstitutionName) ;

   printf("Date     = %s\n"
          "Sequence = %s\n"
          "Subject  = %s\n" ,
          head.TextDate , head.SequenceType , head.PatientName ) ;
   printf("SubjectID= %s\n",head.PatientID) ;
   printf("RF Coil  = %s\n",head.ReceivingCoil) ;

   orients[0]= head.OrientationSet1Left[0]; orients[1]=head.OrientationSet2Right[0];
   orients[2]= head.OrientationSet1Top[0] ; orients[3]=head.OrientationSet2Down[0] ;
   orients[4]= head.OrientationSet1Back[0]; orients[5]=head.OrientationSet2Front[0];
   for (i=0; i<6; i++) {
      if (orients[i]=='H') orients[i]='S';
      if (orients[i]=='F') orients[i]='I';
   }
   orients[6] = '\0' ;
   printf("Orient   = %s\n",orients) ;

   if (swap) {
      byteswap8(&(head.FOVRow));
      byteswap8(&(head.FOVColumn));
      byteswap8(&(head.SliceThickness));
      byteswap8(&(head.RepetitionTime)) ;
      byteswap8(&(head.EchoTime)) ;
      byteswap8(&(head.CenterPointX)) ;
      byteswap8(&(head.CenterPointY)) ;
      byteswap8(&(head.CenterPointZ)) ;
      byteswap8(&(head.NormalVectorX));
      byteswap8(&(head.NormalVectorY));
      byteswap8(&(head.NormalVectorZ));
      byteswap8(&(head.PixelSizeRow)) ;
      byteswap8(&(head.PixelSizeColumn)) ;
      byteswap8(&(head.RowVectorX)) ;
      byteswap8(&(head.RowVectorY)) ;
      byteswap8(&(head.RowVectorZ)) ;
      byteswap8(&(head.ColumnVectorX)) ;
      byteswap8(&(head.ColumnVectorY)) ;
      byteswap8(&(head.ColumnVectorZ)) ;
      byteswap8(&(head.DistanceFromIsocenter)) ;
   }

   printf("FOV Row  = %g\n"
          "FOV Col  = %g\n"
          "SliceThk = %g\n" , head.FOVRow , head.FOVColumn , head.SliceThickness ) ;

   printf("TR       = %g\n",head.RepetitionTime) ;
   printf("TE       = %g\n",head.EchoTime) ;
   printf("Cen X    = %g\n",head.CenterPointX) ;
   printf("Cen Y    = %g\n",head.CenterPointY) ;
   printf("Cen Z    = %g\n",head.CenterPointZ) ;
   printf("Delta X  = %g\n",head.PixelSizeRow) ;
   printf("Delta Y  = %g\n",head.PixelSizeColumn) ;
   printf("RowVec X = %g\n",head.RowVectorX) ;
   printf("RowVec Y = %g\n",head.RowVectorY) ;
   printf("RowVec Z = %g\n",head.RowVectorZ) ;
   printf("ColVec X = %g\n",head.ColumnVectorX) ;
   printf("ColVec Y = %g\n",head.ColumnVectorY) ;
   printf("ColVec Z = %g\n",head.ColumnVectorZ) ;
   printf("Normal X = %g\n",head.NormalVectorX);
   printf("Normal Y = %g\n",head.NormalVectorY);
   printf("Normal Z = %g\n",head.NormalVectorZ);

#if 0
   dd = head.RowVectorX * head.ColumnVectorX    /* vector dot products */
       +head.RowVectorY * head.ColumnVectorY
       +head.RowVectorZ * head.ColumnVectorZ ;
   printf("Row*Col  = %g\n",dd) ;

   dd = head.RowVectorX * head.NormalVectorX
       +head.RowVectorY * head.NormalVectorY
       +head.RowVectorZ * head.NormalVectorZ ;
   printf("Row*Nor  = %g\n",dd) ;

   dd = head.ColumnVectorX * head.NormalVectorX
       +head.ColumnVectorY * head.NormalVectorY
       +head.ColumnVectorZ * head.NormalVectorZ ;
   printf("Col*Nor  = %g\n",dd) ;
#endif

   printf("D-IsoCen = %g\n",head.DistanceFromIsocenter) ;
   printf("SlicePos = %s\n",head.TextSlicePosition) ;

   /*-- scan images for being all zero (blank) [doesn't depend on byteswap] --*/

   slices = matrix*matrix ;

   if( imar != NULL ){
      int nxx = matrix*imagesize , blank ;
      printf("Recognized %dx%d mosaic of %dx%d images:\n",matrix,matrix,imagesize,imagesize) ;
      for( yy=0 ; yy < matrix ; yy++ ){
         printf("  ") ;
         for( xx=0 ; xx < matrix ; xx++ ){
            blank = 1 ;
            for( j=0 ; j < imagesize ; j++ ){
               for( i=0 ; i < imagesize ; i++ ){
                  if( imar[i+xx*imagesize+(j+yy*imagesize)*nxx] ) blank = 0 ;
               }
            }
            printf(" %s" , (blank) ? "blank" : "full ") ;
            if( !blank ) slices = 1 + xx + yy*matrix ;
         }
         printf("\n") ;
      }
   }

   dd = fabs(strtod(head.TextSlicePosition,NULL)) ;
   qq = dd - (slices-1)*head.SliceThickness ;
   if( qq > 0.0 ){
      orients[5] = orients[4] ;
   } else {
       qq = -qq ;
   }

   printf("\nto3d -epan"
          " -time:zt %d %d %0.3fs alt+z"
          " -xFOV %0.2f%c-%c"
          " -yFOV %0.2f%c-%c"
          " -zSLAB %0.2f%c-%0.2f%c \n" ,

        slices , argc-1 , 0.001*head.RepetitionTime ,
        0.5*head.FOVRow    , orients[0] , orients[1] ,
        0.5*head.FOVColumn , orients[2] , orients[3] ,
        dd , orients[4] , qq , orients[5] ) ;

   exit(0) ;
}
