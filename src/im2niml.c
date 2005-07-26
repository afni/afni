#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMARR *iar ;
   MRI_IMAGE *im ;
   NI_stream ns=NULL ;
   NI_element *nel ;
   int ii , iarg=1 ;
   int nb=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: im2niml imagefile [imagefile ...]\n"
            "Converts the input image(s) to a text-based NIML element\n"
            "and writes the result to stdout.  Sample usage:\n"
            " aiv -p 4444 &\n"
            " im2niml zork.jpg | nicat tcp:localhost:4444\n"
            "-- Author: RW Cox.\n"
           ) ;
     exit(0) ;
   }

   ns = NI_stream_open( "stdout:" , "w" ) ;
   if( ns == NULL ) ERROR_exit("Can't open stdout?!") ;
   for( ; iarg < argc ; iarg++ ){
     iar = mri_read_file( argv[iarg] ) ;
     if( iar == NULL ){
       ERROR_message("Can't read file %s",argv[iarg]); continue;
     }
     for( ii=0 ; ii < IMARR_COUNT(iar) ; ii++ ){
       im = IMARR_SUBIM(iar,ii) ; nel = mri_to_niml(im) ; mri_free(im) ;
       if( nel == NULL ){
         ERROR_message("Can't process %s[%d]",argv[iarg],ii); continue;
       }
       nb += NI_write_element( ns , nel , NI_TEXT_MODE ) ;
       NI_free_element(nel) ;
     }
     FREE_IMARR(iar) ;
   }
   NI_stream_closenow(ns) ;
   INFO_message("Wrote %d bytes to stdout",nb) ;
   exit(0) ;
}
