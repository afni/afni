#include "mrilib.h"

int main(int argc, char **argv)
{
   char *ppp ; int ii, iarg=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: dicom_hdr [options] fname [...]\n"
            "Prints information from the DICOM file 'fname' to stdout.\n"
            "\n"
            "OPTIONS:\n"
            " -hex    = include hexadecimal printout for integer values\n"
            " -noname = don't include element names in the printout\n"
            "\n"
            "Based on program dcm_dump_file from the RSNA, developed at\n"
            "the Mallinckrodt Institute of Radiology.  See the source\n"
            "code file mri_dicom_hdr.c for their Copyright and license.\n"
           );
     exit(0);
   }

   mri_dicom_nohex( 1 ) ;

   while( argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-hex") == 0 ){
       mri_dicom_nohex(0) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-noname") == 0 ){
       mri_dicom_noname(1) ; iarg++ ; continue ;
     }

     fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]) ; iarg++ ;
   }

   for( ii=iarg ; ii < argc ; ii++ ){
     if( ii > iarg )
       printf("---------------------------------------------------------------\n");

     ppp = mri_dicom_header( argv[ii] ) ;
     if( ppp != NULL ){
       off_t poff ; unsigned int plen ;
       printf("%s",ppp) ; free(ppp) ;
       mri_dicom_pxlarr( &poff , &plen ) ;
       if( plen > 0 )
         printf("Pixel array offset = %u (bytes)\n"
                "Pixel array length = %u (bytes)\n" ,
                (unsigned int)poff , plen ) ;
     } else {
       printf("***\n*** ERROR: can't open %s as a DICOM file!\n***\n",argv[ii]) ;
     }
   }
   exit(0) ;
}
