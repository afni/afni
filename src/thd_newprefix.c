#include "mrilib.h"

/*--------------------------------------------------------------------
  Attach a suffix to a dataset prefix to make a new one.
  Result is malloc()-ed and can be free()-ed if you don't
  want it anymore.  -- 16 Feb 2001 -- RWCox
----------------------------------------------------------------------*/

char * THD_newprefix( THD_3dim_dataset *dset , char *suffix )
{
   char *np ;

ENTRY("THD_newprefix") ;

   if( !ISVALID_DSET(dset) ){
      np = (suffix == NULL || suffix[0] == '\0') ? strdup("none")
                                                 : strdup(suffix) ;
   } else {
      np = (char *) malloc( strlen(DSET_PREFIX(dset)) + strlen(suffix) + 1 ) ;
      strcpy(np,DSET_PREFIX(dset)) ; strcat(np,suffix) ;
   }

   RETURN(np) ;
}
