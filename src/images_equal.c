#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *aim, *bim ; MRI_IMARR *aar, *bar ;
   int iopt=1, do_all=0, nima,nimb, ii ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: images_equal [-all] fileA fileB\n"
       "\n"
       "* Simple program to test if 2 2D images are identical.\n"
       "* Exit status is 1 if they are equal, and 0 if they are not.\n"
       "* If either image cannot be read, then exit status also 0.\n"
       "* If the '-all' option is used, then all the images in the files\n"
       "  are compared, and all must be equal for the exit status to be 1.\n"
       "* If '-all' is NOT given, only the first image in each file is\n"
       "  compared.\n"
       "* This program is meant for use in scripts that deal with DICOM\n"
       "  servers that sometimes deal out multiple copies of the same\n"
       "  image in different filenames :-(\n"
       "* Also see program uniq_images, which works on multiple inputs.\n"
       "* Author: Zhark the Comparator, October 2015.\n"
     ) ;
     exit(0) ;
   }

   mainENTRY("images_equal") ; machdep() ;

   if( strcasecmp(argv[iopt],"-all") == 0 ){
     do_all = 1 ; iopt++ ;
   }

   if( iopt+2 > argc ){
     ERROR_message("images_equal: Need 2 filenames, but don't have them!") ;
     exit(0) ;
   }

   if( strcmp(argv[iopt],argv[iopt+1]) == 0 ) return 1 ; /* equal strings! */

   /* read input images */

   aar = mri_read_file(argv[iopt++]) ; if( aar == NULL ) exit(0) ;
   aim = IMARR_SUBIM(aar,0) ;

   bar = mri_read_file(argv[iopt++]) ; if( bar == NULL ) exit(0) ;
   bim = IMARR_SUBIM(bar,0) ;

   nima = (do_all) ? IMARR_COUNT(aar) : 1 ;
   nimb = (do_all) ? IMARR_COUNT(bar) : 1 ;

   if( nima != nimb ) exit(0) ;

   for( ii=0 ; ii < nima ; ii++ ){
     if( mri_equal( IMARR_SUBIM(aar,ii) , IMARR_SUBIM(bar,ii) ) == 0 ) exit(0);
   }

   exit(1) ;
}
