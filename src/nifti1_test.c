#include "nifti1_io.c"   /* directly include I/O library functions */

/*-----------------------------------------------*/
/*    cc -o nifti1_test -O2 nifti1_test.c -lm    */
/*-----------------------------------------------*/

/****************************************************************************/

int main( int argc , char *argv[] )
{
   nifti_image *nim ;
   int iarg=1 , outmode=1 , ll ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nifti1_test [-n2|-n1|-na|-a2] infile [prefix]\n"
            "\n"
            " If prefix is given, then the options mean:\n"
            "  -a2 ==> write an ANALYZE 7.5 file pair: prefix.hdr/prefix.img\n"
            "  -n2 ==> write a NIFTI-1 file pair: prefix.hdr/prefix.img\n"
            "  -n1 ==> write a NIFTI-1 single file: prefix.nii\n"
            "  -na ==> write a NIFTI-1 ASCII+binary file: prefix.nia\n"
            " The default is '-n1'.\n"
            "\n"
            " If prefix is not given, then the header info from infile\n"
            " file is printed to stdout.\n"
            "\n"
            " Please note that the '.nia' format is NOT part of the\n"
            " NIFTI-1 specification, but is provided mostly for ease\n"
            " of visualization (e.g., you can edit a .nia file and\n"
            " change some header fields, then rewrite it as .nii)\n"
           ) ;
     printf("\nsizeof(nifti_1_header)=%u\n",(unsigned int)sizeof(nifti_1_header)) ;
     exit(0) ;
   }

   if( argv[1][0] == '-' ){
     if( argv[1][1] == 'a' ){
       outmode = 0 ;
     } else if( argv[1][1] == 'n' ){
       switch( argv[1][2] ){
         case '1': outmode = 1 ; break ;
         default:  outmode = 2 ; break ;
         case 'a': outmode = 3 ; break ;
       }
     }
     iarg++ ;
   }

   if( iarg >= argc ){
     fprintf(stderr,"** ERROR: no input file on command line!?\n"); exit(1);
   }

   nim = nifti_image_read( argv[iarg++] , 1 ) ;
   if( nim == NULL ) exit(1) ;

   if( iarg >= argc ){ nifti_image_infodump(nim); exit(0); }

   nim->nifti_type = outmode ;
   if( nim->fname != NULL ) free(nim->fname) ;
   if( nim->iname != NULL ) free(nim->iname) ;

   ll = strlen(argv[iarg]) ;
   nim->fname = (char *)calloc(1,ll+6) ; strcpy(nim->fname,argv[iarg]) ;
   nim->iname = (char *)calloc(1,ll+6) ; strcpy(nim->iname,argv[iarg]) ;
   if( nim->nifti_type == 1 ){
     strcat(nim->fname,".nii") ;
     strcat(nim->iname,".nii") ;
   } else if ( nim->nifti_type == 3 ){
     strcat(nim->fname,".nia") ;
     strcat(nim->iname,".nia") ;
   } else {
     strcat(nim->fname,".hdr") ;
     strcat(nim->iname,".img") ;
   }
   nifti_image_write( nim ) ;
   exit(0) ;
}
