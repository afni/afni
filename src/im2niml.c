#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im ;
   NI_stream ns ;
   NI_element *nel ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: im2niml imagefile\n"
            "Converts the input image to a text-based NIML element\n"
            "and writes the result to stdout.  Sample usage:\n"
            "\n"
            " aiv -p 4444 &\n"
            " im2niml zork.jpg | nicat tcp:localhost:444\n"
           ) ;
     exit(0) ;
   }

   im = mri_read_just_one(argv[1]) ;
   if( im  == NULL ) ERROR_exit("Can't read file %s",argv[1]) ;
   nel = mri_to_niml(im) ;
   if( nel == NULL ) ERROR_exit("Can't convert to NIML?!") ;
   mri_free(im) ;
   ns = NI_stream_open( "stdout:" , "w" ) ;
   if( ns == NULL ) ERROR_exit("Can't open stdout?!") ;
   NI_write_element( ns , nel , NI_TEXT_MODE ) ;
   NI_stream_closenow(ns) ;
   exit(0) ;
}
