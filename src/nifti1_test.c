#include "nifti1.c"

/* cc -o nifti1_test -O2 nifti1_test.c -lm */

/****************************************************************************/

int main( int argc , char *argv[] )
{
   nifti_image *nim ;
   int iarg=1 , outmode=-1 , ll ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nifti1_test [-n2|-n1|-a2] infile [outfile]\n") ;
     printf("sizeof(nifti_1_header)=%d\n",sizeof(nifti_1_header)) ;
     exit(0) ;
   }

   if( argv[1][0] == '-' ){
     if( argv[1][1] == 'a' ){
       outmode = 0 ;
     } else if( argv[1][1] == 'n' ){
       outmode = (argv[1][2] == '1') ? 1 : 2 ;
     }
     iarg++ ;
   }

   nim = nifti_image_read( argv[iarg++] , 1 ) ;
   if( nim == NULL ) exit(1) ;
   nifti_image_infodump( nim ) ;
   if( iarg >= argc ) exit(0) ;

   if( outmode >= 0 ) nim->nifti_type = outmode ;
   if( nim->fname != NULL ) free(nim->fname) ;
   if( nim->iname != NULL ) free(nim->iname) ;

   ll = strlen(argv[iarg]) ;
   nim->fname = calloc(1,ll+6) ;
   nim->iname = calloc(1,ll+6) ;
   strcpy(nim->fname,argv[iarg]) ;
   strcpy(nim->iname,argv[iarg]) ;
   if( nim->nifti_type == 1 ){
     strcat(nim->fname,".nii") ;
     strcat(nim->iname,".nii") ;
   } else {
     strcat(nim->fname,".hdr") ;
     strcat(nim->iname,".img") ;
   }
   nifti_image_write( nim ) ;
   exit(0) ;
}
