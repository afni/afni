#include "mrilib.h"

/*-------------------------------------------------------------------------*/
/*! Create an empty dataset with geometry given by a string. Examples:
     - "RAI:101,-50,2,91,-45,2,81,-40,2"
     - "TTatlas"
*//*-----------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_geometry_constructor( char *gstr )
{
   THD_3dim_dataset *new_dset = NULL ;

ENTRY("EDIT_empty_constructor") ;
   if( gstr == NULL || *gstr == '\0' ) RETURN(NULL) ;

   new_dset = EDIT_empty_copy(NULL) ;
}
