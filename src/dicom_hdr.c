#include <stdio.h>
#include <string.h>

extern char *MRI_dicom_header( char * ) ;

int main(int argc, char **argv)
{
   char *ppp ;

   if( argc < 2 ){
     printf("Usage: mri_dicom fname\n"); exit(0);
   }

   ppp = MRI_dicom_header( argv[1] ) ;
   if( ppp != NULL ) printf("%s",ppp) ;
   exit(0) ;
}
