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
   MRI_IMARR * imar ;
   MRI_IMAGE * im ;
   char prefix[240] = "cutup." , fnam[256] ;
   int iarg , ii,jj , nx,ny , nxim,nyim ;
   int nmode = XYNUM ;

   if( argc < 4 ){
      printf("Usage: imcutup [options] nx ny fname1\n"
             "Breaks up larger images into smaller image files of size\n"
             "nx by ny pixels.  Intended as an aid to using image files\n"
             "which have been catenated to make one big 2D image.\n"
             "OPTIONS:\n"
             "  -prefix ppp = Prefix the output files with string 'ppp'\n"
             "  -xynum      = Number the output images in x-first, then y [default]\n"
             "  -yxnum      = Number the output images in y-first, then x\n"
             "  -x.ynum     = 2D numbering, x.y format\n"
             "  -y.xnum     = 2D numbering, y.x format\n"
             "For example:\n"
             "  imcutup -prefix Fred 64 64 3D:-1:0:256:128:1:zork.im\n"
             "will break up the big 256 by 128 image in file zork.im\n"
             "into 8 images, each 64 by 64.  The output filenames would be\n"
             "  -xynum  => Fred.001 Fred.002 Fred.003 Fred.004\n"
             "             Fred.005 Fred.006 Fred.007 Fred.008\n"
             "\n"
             "  -yxnum  => Fred.001 Fred.003 Fred.005 Fred.007\n"
             "             Fred.002 Fred.004 Fred.006 Fred.008\n"
             "\n"
             "  -x.ynum => Fred.001.001 Fred.002.001 Fred.003.001 Fred.004.001\n"
             "             Fred.001.002 Fred.002.002 Fred.003.002 Fred.004.002\n"
             "\n"
             "  -y.xnum => Fred.001.001 Fred.001.002 Fred.001.003 Fred.001.004\n"
             "             Fred.002.001 Fred.002.002 Fred.002.003 Fred.002.004\n"
             "\n"
             "You may want to look at the input image file with\n"
             "  afni -im fname  [then open the Sagittal image window]\n"
             "before deciding on what to do with the image file.\n"
             "\n"
             "N.B.: the file specification 'fname' must result in a single\n"
             "      input 2D image - multiple images can't be cut up in one\n"
             "      call to this program.\n"
            ) ;
      exit(0) ;
    }

    iarg = 1 ;

    while( iarg < argc && argv[iarg][0] == '-' ){

       if( strcmp(argv[iarg],"-xynum") == 0 ){
          nmode = XYNUM ; iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-yxnum") == 0 ){
          nmode = YXNUM ; iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-x.ynum") == 0 ){
          nmode = XDOTYNUM ; iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-y.xnum") == 0 ){
          nmode = YDOTXNUM ; iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-prefix") == 0 ){
          MCW_strncpy( prefix , argv[++iarg] , 240 ) ;
          ii = strlen(prefix) ;
          if( prefix[ii-1] != '.' ) strcat(prefix,".") ;
          iarg++ ; continue ;
       }

       fprintf(stderr,"*** ERROR: illegal option %s\n",argv[iarg]) ;
       exit(1) ;
    }

    if( iarg+3 > argc ){
       fprintf(stderr,"*** ERROR: not enough arguments!\n"); exit(1);
    }

    nx = (int) strtod( argv[iarg++] , NULL ) ;
    ny = (int) strtod( argv[iarg++] , NULL ) ;
    if( nx < 1 || ny < 1 ){
       fprintf(stderr,"*** ERROR: illegal values nx=%d ny=%d\n",nx,ny);
       exit(1) ;
    }

    im = mri_read_just_one( argv[iarg] ) ;
    if( im == NULL ){
       fprintf(stderr,
               "*** ERROR: file %s doesn't have exactly 1 image!\n",
               argv[iarg]) ;
       exit(1) ;
    }

    nxim = im->nx / nx ;
    nyim = im->ny / ny ;
    if( nxim < 1 || nyim < 1 ){
       fprintf(stderr,"*** ERROR: image is too small for nx=%d ny=%d\n",
               nx,ny) ;
       exit(1) ;
    }

    imar = mri_uncat2D( nx,ny , im ) ;
    if( imar == NULL || IMARR_COUNT(imar) < nxim*nyim ){
       fprintf(stderr,"*** ERROR: unknown error in mri_uncat2D()\n");
       exit(1) ;
    }

    mri_free(im) ;

    for( jj=0 ; jj < nyim ; jj++ ){
       for( ii=0 ; ii < nxim ; ii++ ){
          im = IMARR_SUBIMAGE(imar,ii+jj*nxim) ;
          switch( nmode ){
             default:
             case XYNUM:
                sprintf(fnam,"%s%03d",prefix,ii+jj*nxim+1) ; break ;

             case YXNUM:
                sprintf(fnam,"%s%03d",prefix,jj+ii*nyim+1) ; break ;

             case XDOTYNUM:
                sprintf(fnam,"%s%03d.%03d",prefix,ii+1,jj+1) ; break ;

             case YDOTXNUM:
                sprintf(fnam,"%s%03d.%03d",prefix,jj+1,ii+1) ; break ;
          }
          mri_write(fnam,im) ;
       }
    }

    exit(0) ;
}
