#include <stdio.h>
#include <string.h>

extern char *mri_dicom_header( char * ) ;  /* cf. mri_dicom_hdr.[ch] */

int main(int argc, char **argv)
{
   char *ppp ; int ii ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: dicom_hdr fname [...]\n"
            "Prints information from the DICOM file 'fname' to stdout.\n"
            "\n"
            "Based on program dcm_dump_file from the RSNA, developed at\n"
            "the Mallinckrodt Institute of Radiology.  See the source\n"
            "code file mri_dicom_hdr.c for their Copyright and license.\n"
           );
     exit(0);
   }

   for( ii=1 ; ii < argc ; ii++ ){
     if( ii > 1 )
       printf("---------------------------------------------------------------\n");

     ppp = mri_dicom_header( argv[ii] ) ;
     if( ppp != NULL ){
       printf("%s",ppp) ; free(ppp) ;
     } else {
       printf("***\n*** ERROR: can't open %s as a DICOM file!\n***\n",argv[ii]) ;
     }
   }
   exit(0) ;
}
