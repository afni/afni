/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994-1996 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Copyright 1994-6 Medical College of Wisconsin\n\n"
    "Assigns a new ID code to a dataset; this is useful when making\n"
    "a copy of a dataset, so that the internal ID codes remain unique.\n\n"
    "Usage: 3dnewid dataset [dataset ...]\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ) continue ;
      dset->idcode = MCW_new_idcode() ;
      THD_write_3dim_dataset( NULL , NULL , dset , False ) ;
      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
