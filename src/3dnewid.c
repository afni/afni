/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Assigns a new ID code to a dataset; this is useful when making\n"
    "a copy of a dataset, so that the internal ID codes remain unique.\n\n"
    "Usage: 3dnewid dataset [dataset ...]\n"
    " or\n"
    "       3dnewid -fun\n"
    "       to see what a couple of randomly generated ID codes look like.\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg ;
   MCW_idcode idc ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;

   mainENTRY("3dnewid main"); machdep(); AFNI_logger("3dnewid",argc,argv);

   if( strcmp(argv[1],"-fun") == 0 ){         /* 22 May 2000: for fun */
      MCW_idcode idc2 ;
      idc  = MCW_new_idcode() ;
      idc2 = MCW_new_idcode() ;
      printf("%s %s\n",idc.str,idc.date) ;
      printf("%s %s\n",idc2.str,idc2.date) ;
      exit(0) ;
   }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ) continue ;
      dset->idcode = MCW_new_idcode() ;
      THD_write_3dim_dataset( NULL , NULL , dset , False ) ;
      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
