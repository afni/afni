#include <stdio.h>
#include <string.h>

extern char *mri_dicom_header( char * ) ;  /* cf. mri_dicom_hdr.[ch] */

int main(int argc, char **argv)
{
   char *ppp ;

   if( argc < 2 ){
     printf("Usage: dicom_hdr fname\n"
            "Prints information from the DICOM file 'fname' to stdout.\n"
	   );
     exit(0);
   }

   ppp = mri_dicom_header( argv[1] ) ;
   if( ppp != NULL ) printf("%s",ppp) ;
   exit(0) ;
}
