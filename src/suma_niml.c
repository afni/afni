#include "niml/niml.h"
#include "suma_types.h"

/*------------------------------------------------------------------*/
/*! Make a NIML data element from an array of SUMA_ixyz structs.
    Return value is NULL if you input stupid values.  Otherwise,
    will return a populated element. Example:
     - int    num_ixyz=999 ;
     - SUMA_ixyz *ixyz=malloc(sizeof(SUMA_ixyz)*num_ixyz) ;
     - NI_element *nel ;
     - NI_stream *ns ;
     - ** fill ixyz[0..998].stuff somehow **
     - nel = SUMA_ixyz_to_NIML( num_ixyz , ixyz ) ;
     - NI_write_element( ns , nel , NI_TEXT_MODE ) ;
     - NI_free_element( nel ) ;
--------------------------------------------------------------------*/

NI_element * SUMA_ixyz_to_NIML( int num_ixyz , SUMA_ixyz *ixyz )
{
   NI_element *nel ;
   int ii , *ic ;
   float *xc,*yc,*zc ;

   /* check inputs for sanity */

   if( num_ixyz < 1 || ixyz == NULL ) return NULL ;  /* stupid caller */

   /* make a new data element, to be filled by columns */

   nel = NI_new_data_element( "SUMA_ixyz" , num_ixyz ) ;

   /* make the temp columns to be put in the element */

   ic = NI_malloc(int, sizeof(int)   * num_ixyz ) ;
   xc = NI_malloc(float, sizeof(float) * num_ixyz ) ;
   yc = NI_malloc(float, sizeof(float) * num_ixyz ) ;
   zc = NI_malloc(float, sizeof(float) * num_ixyz ) ;

   /* load these columns from the struct array */

   for( ii=0 ; ii < num_ixyz ; ii++ ){
      ic[ii] = ixyz[ii].id ;
      xc[ii] = ixyz[ii].x ;
      yc[ii] = ixyz[ii].y ;
      zc[ii] = ixyz[ii].z ;
   }

   /* copy columns into NI_element, freeing them when done */

   NI_add_column( nel , NI_INT   , ic ) ; free(ic) ;
   NI_add_column( nel , NI_FLOAT , xc ) ; free(xc) ;
   NI_add_column( nel , NI_FLOAT , yc ) ; free(yc) ;
   NI_add_column( nel , NI_FLOAT , zc ) ; free(zc) ;

   return nel ;
}

/*------------------------------------------------------------------*/
/*! Unload a <SUMA_ixyz> NI_element into an array of newly
    malloc()-ed SUMA_ixyz structs.
    Return value is number of structs (will be 0 inputs are bad).
    *ixyz will point to the output array if the number of structs is
    positive.  This array will be newly malloc()-ed.  Example:
     - SUMA_ixyz *ixyz ;
     - int    num_ixyz ;
     - NI_element *nel ;  ** get this from somewhere **
     - num_ixyz = NIML_to_SUMA_ixyz( nel , &ixyz ) ;
     - if( num_ixyz == 0 ){ ** error error error ** }
     - else               { ** good good good ** }
     - NI_free_element(nel) ;
--------------------------------------------------------------------*/

int NIML_to_SUMA_ixyz( NI_element *nel , SUMA_ixyz **ixyz )
{
   int   num_ixyz ;       /* return value */
   SUMA_ixyz *myixyz ;    /* output array of structs */
   int   *ic , ii ;
   float *xc, *yc, *zc ;

   /* check element for correctness */

   if( nel             == 0        ||   /* no data element?          */
       nel->vec_len    <  1        ||   /* empty element?             */
       nel->vec_filled <  1        ||   /* no data was filled in?      */
       nel->vec_num    <  4        ||   /* less than 4 columns?         */
       nel->vec_typ[0] != NI_INT   ||   /* must be int,float,float,float */
       nel->vec_typ[1] != NI_FLOAT ||
       nel->vec_typ[2] != NI_FLOAT ||
       nel->vec_typ[3] != NI_FLOAT   ) return 0 ;  /* bad bad bad */

   /* number of structs is number of completely filled rows */

   num_ixyz = nel->vec_filled ;

   /* make space for the new structs */

   myixyz = NI_malloc(SUMA_ixyz, sizeof(SUMA_ixyz) * num_ixyz ) ;

   /* pointers to the data columns in the NI_element */

   ic = (int *)   nel->vec[0] ;
   xc = (float *) nel->vec[1] ;
   yc = (float *) nel->vec[2] ;
   zc = (float *) nel->vec[3] ;

   /* load the values from the element */

   for( ii=0 ; ii < num_ixyz ; ii++ ){
      myixyz[ii].id = ic[ii] ;
      myixyz[ii].x  = xc[ii] ;
      myixyz[ii].y  = yc[ii] ;
      myixyz[ii].z  = zc[ii] ;
   }

   /* we is done */

   *ixyz = myixyz ; return num_ixyz ;
}
