#include "mrilib.h"

/*-----------------------------------------------------------------------------*/
/*! Produce a 1D (float) image from a string of the form "20@1,10@0,5@1".
-------------------------------------------------------------------------------*/

MRI_IMAGE * mri_1D_fromstring( char *str )
{
   int ii,nnn,count , ntot=0 ;
   float *far , value ;
   NI_str_array *sar ;
   char sep ;
   MRI_IMAGE *flim ;

   sar = NI_decode_string_list( str , ",;" ) ;
   if( sar == NULL ) return NULL ;
   if( sar->num == 0 ){ NI_delete_str_array(sar); return NULL; }

   far = (float *) malloc(sizeof(float)) ;
   for( ii=0 ; ii < sar->num ; ii++ ){

     if( strstr(sar->str[ii],"@") != NULL ||    /* if has one of the    */
         strstr(sar->str[ii],"x") != NULL ||    /* allowed separator    */
         strstr(sar->str[ii],"X") != NULL ||    /* characters, then     */
         strstr(sar->str[ii],"*") != NULL   ){  /* scan for count@value */

        nnn = sscanf( sar->str[ii] , "%d%c%f" , &count , &sep , &value ) ;
        if( nnn != 3 || count < 1 ){ free(far); return NULL; }

     } else {                                 /* just scan for value */
        count = 1 ;
        nnn   = sscanf( sar->str[ii] , "%f" , &value ) ;
        if( nnn != 1 ){ free(far); return NULL; }
     }

     far = (float *) realloc( far , sizeof(float)*(ntot+count) ) ;
     for( nnn=0 ; nnn < count ; nnn++ ) far[nnn+ntot] = value ;
     ntot += count ;

   }

   NI_delete_str_array(sar) ;
   flim = mri_new_vol_empty( ntot,1,1 , MRI_float ) ;
   mri_fix_data_pointer( far , flim ) ;
   return flim ;
}
