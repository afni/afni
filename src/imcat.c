/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

#define XYNUM    0
#define YXNUM    1
#define XDOTYNUM 2
#define YDOTXNUM 3

int main( int argc , char * argv[] )
{
   MRI_IMARR * imar, *inimar;
   MRI_IMAGE * im ;
   char prefix[240] = "cat" , fnam[256] ;
   char suffix[50] = "\0";
   int iarg , ii,jj , nx,ny , nxim,nyim ;
   int nmode = XYNUM ;

   if( argc < 4 ){
      printf("Usage: imcat [options] fname1 fname2 etc.\n"
             "Puts images of the same type and size into a montage.\n"
             " of nx by ny images, a la AFNI montage.\n"
             " 3x4 images would be assembled in this order:\n"
             "  0  1  2\n"
             "  3  4  5\n"
             "  6  7  8\n"
             "  9  10 11\n"
             "OPTIONS:\n"
             "  -prefix ppp = Prefix the output files with string 'ppp'\n"
             "  -nx NX: Number of images in each row (3 for example above)\n"
             "  -ny NY: Number of images in each column (4 for example above)\n"
             "  -matrix NX NY: Specify both numbers at the same time.\n"
             "        The program will try to guess if neither NX nor NY are\n"
             "        specified.\n"
             "\n"
             "You can look at the output image file with\n"
             "  afni -im ppp.ppm  [then open the Sagittal image window]\n"
             "\n"
            ) ;
      exit(0) ;
    }

   machdep() ;

    iarg = 1 ;
      nx = -1; ny = -1;
    while( iarg < argc && argv[iarg][0] == '-' ){

       if( strcmp(argv[iarg],"-matrix") == 0 ){
         if (iarg+2 > argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -matrix\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
         ny = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-nx") == 0 ){
         if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -nx\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-ny") == 0 ){
         if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -ny\n");
            exit(1);
         }
         ny = (int) strtod( argv[++iarg] , NULL ) ; 
          iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-yxnum") == 0 ){
          nmode = YXNUM ; iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-prefix") == 0 ){
          MCW_strncpy( prefix , argv[++iarg] , 240 ) ;
          iarg++ ; continue ;
       }

       fprintf(stderr,"*** ERROR: illegal option %s\n",argv[iarg]) ;
       exit(1) ;
    }



   /* read all images */
   inimar = mri_read_many_files( argc-iarg , argv+iarg ) ;
   if( inimar == NULL ){
      fprintf(stderr,"*** no input images read!\a\n") ;
      exit(1) ;
   } else if( inimar->num < 2 ){
      fprintf(stderr,"*** less than 2 input images read!\a\n") ;
      exit(1) ;
   }
   
   /* figure out the count */
   if (nx < 0 && ny < 0) {
      nx = ny = (int)sqrt((double)inimar->num+0.00001);
      if (nx*ny != inimar->num) {
         fprintf(stderr,"*** Can't guess at matrix layout for %d images.\n"
                        "    Use -nx, -ny or -matrix\n", inimar->num);
         exit(1);
      }
   } else if (nx < 0) {
      if (inimar->num % ny) {
         fprintf(stderr,"*** Have %d images, not divisible by %d!\n", inimar->num, ny);
         exit(1);
      }
      nx = inimar->num / ny;
   } else if (ny < 0) {
      if (inimar->num % nx) {
         fprintf(stderr,"*** Have %d images, not divisible by %d!\n", inimar->num, nx);
         exit(1);
      }
      ny = inimar->num / nx;
   }
   
   if( nx < 1 || ny < 1 ){
    fprintf(stderr,"*** ERROR: illegal values nx=%d ny=%d\n",nx,ny);
    exit(1) ;
   }
   if (nx*ny != inimar->num) {
      fprintf(stderr,"*** Have %d images, does not fit: %dx%d\n", inimar->num, nx, ny);
      exit(1);
   }
   fprintf(stdout, "\nArranging %d images into a %dx%d matrix.\n", inimar->num, nx, ny);
   
   if (!(im = mri_cat2D( nx , ny , 0 , NULL , inimar ))) {
      fprintf(stderr,"*** ERROR: Failed to create image!\n");
      exit(1) ;
   }
   
   sprintf(fnam,"%s.ppm",prefix);
   mri_write(fnam,im) ;
   
   mri_free(im); im = NULL;
   fprintf(stdout, "You can view image %s with:\n afni -im %s &\n in the saggittal view.\n", fnam, fnam);
    exit(0) ;
}
