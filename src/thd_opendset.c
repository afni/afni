#include "3ddata.h"
#include "thd.h"


/*----------------------------------------------------------------
   simply given a pathname, try to open it as a dataset
   [allow for .HEAD, .BRIK, or just prefix+viewcode filenames]
------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_one_dataset( char * pathname )
{
   int ii , plen ;
   char dirname[THD_MAX_NAME] , fullname[THD_MAX_NAME] ;
   THD_3dim_dataset * dset ;
   THD_datablock    * dblk ;
   char * sub ;

   /*-- sanity check --*/

   if( pathname == NULL              ||
       (plen=strlen(pathname)) == 0  ||
       pathname[plen-1]        == '/'  ) return NULL ;

   /*-- find directory and last names in the pathname --*/

   for( ii=plen-1 ; ii >= 0 ; ii-- ) if( pathname[ii] == '/' ) break ;

   if( ii < 0 ){
      strcpy( dirname , "./" ) ;      /* fake directory name */
   } else {
      strcpy( dirname , pathname ) ;
      dirname[ii+1] = '\0' ;
   }

   /*-- perform surgery on the name to make it a valid .HEAD --*/

   strcpy( fullname , pathname ) ;

   sub = strstr( fullname , DATASET_HEADER_SUFFIX ) ;   /* .HEAD ? */

   if( sub == NULL ){                                   /* no! */
      sub = strstr( fullname , DATASET_BRICK_SUFFIX ) ; /* .BRIK ? */

      if( sub == NULL ){                               /* no! */
         ii = strlen(fullname) ;
         if( fullname[ii-1] != '.' ) strcat( fullname , "." ) ; /* tack .HEAD */
         strcat( fullname , DATASET_HEADER_SUFFIX ) ;           /* onto end */

      } else {                                     /* yes! */
         strcpy( sub , DATASET_HEADER_SUFFIX ) ;   /* replace .BRIK with .HEAD */
      }
   }

   /*-- open it up? --*/

   dblk = THD_init_one_datablock( dirname , fullname ) ;
   if( dblk == NULL ) return NULL ;

   dset = THD_3dim_from_block( dblk ) ;
   return dset ;
}
