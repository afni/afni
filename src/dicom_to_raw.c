#include "mrilib.h"

int main( int argc , char *argv[] )
{
  MRI_IMARR *imar ;
  int kk,ii , nn ;
  char iname[1024] ;

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
    printf("Usage: dicom_to_raw fname ...\n"
           "Reads images from DICOM file 'fname' and writes them to raw\n"
           "file(s) 'fname.raw.0001' etc.\n"
          ) ;
    exit(0) ;
  }
           
  for( kk=1 ; kk < argc ; kk++ ){
    imar = mri_read_dicom( argv[kk] ) ;
    if( imar == NULL ){
      fprintf(stderr,"++ Can't read from file %s\n",argv[kk]) ; continue ;
    }
    nn = IMARR_COUNT(imar) ;
    for( ii=0 ; ii < nn ; ii++ ){
      sprintf(iname,"%s.raw.%04d",argv[kk],ii+1) ;
      mri_write( iname, IMARR_SUBIM(imar,ii) ) ;
    }
    DESTROY_IMARR(imar) ;
  }

  exit(0) ;
}
