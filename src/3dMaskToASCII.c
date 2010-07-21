#include "mrilib.h"

int main( int argc , char *argv[] )
{
   bytevec *bvec ;
   int nmask ; char *mask , *str ; byte *mbin ;
   int iarg=1 ; int tob64=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dMaskToASCII [-tobin] dataset > outputfile\n"
      "\n"
      "This program reads as input a byte-valued 0/1 dataset, such as\n"
      "produced by 3dAutomask, and turns it into an ASCII string.\n"
      "This string can be used to specify a mask in a few places\n"
      "in AFNI, and will be allowed in more as time goes on.\n"
      "\n"
      "the only OPTION:\n"
      "----------------\n"
      " -tobin = read 'dataset' as an ASCII string mask, expand it,\n"
      "          and write the byte-valued mask to stdout.  This file\n"
      "          corresponds to the .BRIK file of an AFNI dataset.\n"
      "          The information needed to create a .HEAD file isn't\n"
      "          stored in the ASCII string.\n"
      "\n"
      "* Jul 2010: -STATmask options in 3dREMLfit and 3dDeconvolve\n"
      "            accept a dataset mask or an ASCII string mask.\n"
      "\n"
      "SAMPLE OUTPUT:\n"
      "--------------\n"
      "eNrlmU+u0zAQh21cySxQzZIFwld4S9gQjsJBEM7RepQeIcssrATp5WfHHnucRIBoSjefXtr8\n"
      "ef5mxvZEiAf+vAe/LujnhXdwAEe30OPvKVK+cp41oUrZr3z9/W2laNPhsbqMIhLPNbn8OQfw\n"
      "Bvb4vfgi/u/PT4xL9CzheeEIenD1K4lHDU+BhqFebrOcl1Aut51xe0cYj1/Ad8t57orzs/v3\n"
      "hDEOJ9CD4f+LcQGKz0/q28CzI/nMeJ6iZ0nyVaXjntDAF0e93C5SgRLE4zjC+QKaGsN1B+Z5\n"
      "Qvz1oKAM8TCgToXxEYEv59beB+8dV7+zvBalb5nmaZKvinjUy2WXca1Qp5xw3oTrJQzfmxq5\n"
      "61fiwqRxsBkPHv7HWAdJHLw9mXcN7xbeQd/l8yTyrjIfU99ZnQ756sGKR0WomeP0e0to9nAr\n"
      "DgYmDpJ5Q2XrmZGsf+L8ENYPHx7b/80Q7+Bks3VTX663uDyXqe/Ee8YZdXvlTKlAA9qdNCn3\n"
      "+m/Ega76n4n/UAeKeaE7iX9DvNts/Ry831cqpr7TfCXeOf8Ze/jr4bU/4N8y9cEejANN/Gf7\n"
      "kTgPeuK/2D88jX9ZW5dT/56v27Kd/4V/y/jvNrjl3+I57RH/Sd4z/t05/Q9mb92v1nsu//1K\n"
      "WasDE+t/3sr/Xf636oFfydWBbL9Q8Z/3NYL/UP9vZ/Ef1n1hvdft9Z9xLONAtub/hn8J6iQO\n"
      "WvW+O7gOsDv3BXrX/B/Wx97l+6fgv3/0+g//Q3do3X9n4mEk5P1nngtfyXFF2PRcOV+n+wZP\n"
      "9p+N/SDtV+k0H4o+Yhi3gfgX9sH3fzaP26G97z+w/+PmA0X291l+VjxKhtw+T9fof9P/2id0\n"
      "9byn3sO4nqUfEONgZ99vu/+jyDpBk/5es++TxIeszRt+5QXHr63r+LKv2PRe+ndv6t7dufJ9\n"
      "8/Pxj/T7G/1fTeLBMP1eSuqdsMs4Ri7exvK+XB94n/c73d9fn+w9wDdwAot4yPsfZTwoEg/V\n"
      "+bQSH4qpH+T9T/4eYIDvLd4Jb9x7Qm5dJz6do6/31z7fwR+0TpB4IOMX9knzXF1X9mW80Dqi\n"
      "auvOtR/lmn55z13e/wz9EKH/3RD/AmrpJfk====65536\n"
      "\n"
      "[The input binary mask is compressed (like 'gzip -9'),  then the result]\n"
      "[is encoded in Base64, and the number of voxels is appended at the end.]\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-----*/

   if( argv[iarg][0] == '-' ){
     if( strcmp(argv[iarg],"-tobin") == 0 ) tob64 = 0 ;
     iarg++ ;
   }

   /*-----*/

   bvec = THD_create_mask_from_string(argv[iarg]) ;
   if( bvec == NULL || bvec->nar < 1 ) ERROR_exit("Can't read mask :-(") ;

   if( tob64 ){   /* convert to string (default operation) */

     str = mask_to_b64string( bvec->nar , bvec->ar ) ;
     if( str == NULL ) ERROR_exit("Can't create ASCII string :-(") ;
     printf("%s\n",str) ;

   } else {       /* write byte mask directly out */

     if( isatty(fileno(stdout)) )
       ERROR_exit("Can't write byte mask to terminal directly") ;
     fwrite(bvec->ar,sizeof(byte),bvec->nar,stdout) ;
     INFO_message("Wrote %d bytes to re-directed stdout",bvec->nar) ;

   }

   exit(0) ;
}
