/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"

/***********************************************************************
  The first set of routines is concerned with "attributes".
  These are values stored in the header file of a dataset,
  and can be conceived of being in the form
    name = value value value ...
  where "name" is an identifying string, and "value value value ..."
  is an array of values.  These attributes are read in, and later
  interrogated to form the actual data structure of a THD_3dim_dataset.
************************************************************************/

/*-----------------------------------------------------------------------
   given the rudiments of a datablock, read all the attributes into it
-------------------------------------------------------------------------*/

void THD_read_all_atr( char * headername , THD_datablock * blk )
{
   ATR_any * next_atr ;
   int code , ii ;
   FILE * header_file ;

   if( ! ISVALID_DATABLOCK(blk) )
      THD_FATAL_ERROR( "Illegal datablock type in THD_read_all_atr" ) ;

   blk->natr       = 0 ;     /* initialize to no attributes */
   blk->natr_alloc = 0 ;
   blk->atr        = NULL ;

   if( STRING_HAS_SUFFIX(headername,".mnc") ) return ;

   header_file = fopen( headername , "r" ) ;
   if( header_file == NULL ){
       return ;
   }

   /* read attributes from the header file */

   do{
      char aname[THD_MAX_NAME] , atypestr[THD_MAX_NAME] ;
      int  atype , acount ;

      atypestr[0] = aname[0] = '\0' ; acount = 0 ;
      code = fscanf( header_file ,
                     " type = %s name = %s count = %d" ,
                     atypestr , aname , &acount ) ;

      code = (code != 3 || acount < 1) ? FAIL : SUCCESS ;
      if( code == FAIL ) break ;  /* bad read */

      for( atype=FIRST_ATR_TYPE ; atype <= LAST_ATR_TYPE ; atype++ )
         if( strcmp(atypestr,ATR_typestr[atype]) == 0 ) break ;

      if( atype > LAST_ATR_TYPE ){ /* bad read */
         code = FAIL ;
         break ;
      }

      if( blk->natr == blk->natr_alloc ){  /* make new space */
         blk->natr_alloc  += ATR_ALLINC ;
         blk->atr          = (ATR_any *)
                             XtRealloc( (char *)blk->atr,
                                        sizeof(ATR_any) * blk->natr_alloc );
      }
      next_atr = &(blk->atr[blk->natr]) ;
      (blk->natr)++ ;

      switch( atype ){

         case ATR_FLOAT_TYPE:{
            ATR_float * new_atr = (ATR_float *) next_atr ;
            char bbb[256] ;

            new_atr->type = ATR_FLOAT_TYPE ;
            new_atr->name = XtNewString( aname ) ;
            new_atr->nfl  = acount ;
            new_atr->fl   = (float *) XtMalloc( sizeof(float) * acount ) ;

            code = 0 ;
            for( ii=0 ; ii < acount ; ii++ ){
#if 0
               code += fscanf( header_file , "%f" , &(new_atr->fl[ii]) ) ;
#else
               bbb[0] = '\0' ; fscanf( header_file , "%255s" , bbb ) ;
               if( bbb[0] != '\0' ){
                  new_atr->fl[ii] = strtod( bbb , NULL ) ;
                  code++ ;
               }
#endif
            }
            code = (code != acount) ? FAIL : SUCCESS ;

            ADDTO_KILL( blk->kl , new_atr->name ) ;
            ADDTO_KILL( blk->kl , new_atr->fl ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int * new_atr = (ATR_int *) next_atr ;

            new_atr->type = ATR_INT_TYPE ;
            new_atr->name = XtNewString( aname ) ;
            new_atr->nin  = acount ;
            new_atr->in   = (int *) XtMalloc( sizeof(int) * acount ) ;

            code = 0 ;
            for( ii=0 ; ii < acount ; ii++ ){
               code += fscanf( header_file , "%d" , &(new_atr->in[ii]) ) ;
            }
            code = (code != acount) ? FAIL : SUCCESS ;

            ADDTO_KILL( blk->kl , new_atr->name ) ;
            ADDTO_KILL( blk->kl , new_atr->in ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string * new_atr = (ATR_string *) next_atr ;

            new_atr->type = ATR_STRING_TYPE ;
            new_atr->name = XtNewString( aname ) ;
            new_atr->nch  = acount ;
            new_atr->ch   = (char *) XtMalloc( sizeof(char) * acount ) ;

            fscanf( header_file , " '" ) ;

            code = 0 ;
            for( ii=0 ; ii < acount ; ii++ ){
               code += fscanf( header_file , "%c" , &(new_atr->ch[ii]) ) ;
            }
            code = (code != acount) ? FAIL : SUCCESS ;

            THD_unzblock( acount , new_atr->ch ) ;

            ADDTO_KILL( blk->kl , new_atr->name ) ;
            ADDTO_KILL( blk->kl , new_atr->ch ) ;
         }
         break ;
      }  /* end of switch */

      if( code == FAIL ) break ;  /* exit if an error! */
   } while(1) ; /* end of for loop over all attributes */

   fclose( header_file ) ;
}

/*-----------------------------------------------------------------------
  29 April 1998: erase all attributes from a datablock
-------------------------------------------------------------------------*/

void THD_erase_all_atr( THD_datablock * blk )
{
   int ia ;
   ATR_any * next_atr ;

   if( !ISVALID_DATABLOCK(blk) || blk->natr == 0 || blk->atr == NULL ) return ;

   for( ia=0 ; ia < blk->natr ; ia++ ){
      next_atr = blk->atr + ia ;

      switch( next_atr->type ){
         case ATR_FLOAT_TYPE:{
            ATR_float * aa = (ATR_float *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->fl ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string * aa = (ATR_string *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->ch ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int * aa = (ATR_int *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->in ) ;
         }
         break ;
      }

      next_atr->type = ILLEGAL_TYPE ;
   }

   blk->natr = 0 ;
   return ;
}

/*-----------------------------------------------------------------------
   29 April 1998: erase a single attribute, given by name
-------------------------------------------------------------------------*/

void THD_erase_one_atr( THD_datablock * blk , char * name )
{
   ATR_any * next_atr ;

   if( ! ISVALID_DATABLOCK(blk) || name     == NULL ||
       blk->natr == 0           || blk->atr == NULL   ) return ;

   next_atr = THD_find_atr( blk , name ) ;

   if( next_atr == NULL ) return ;

   switch( next_atr->type ){
      case ATR_FLOAT_TYPE:{
         ATR_float * aa = (ATR_float *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->fl ) ;
      }
      break ;

      case ATR_STRING_TYPE:{
         ATR_string * aa = (ATR_string *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->ch ) ;
      }
      break ;

      case ATR_INT_TYPE:{
         ATR_int * aa = (ATR_int *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->in ) ;
      }
      break ;
   }

   next_atr->type = ILLEGAL_TYPE ;
   return ;
}

/*-----------------------------------------------------------------------
  given a datablock and an attribute name, return the pointer to the
  attribute structure that matches (if none, return NULL)
-------------------------------------------------------------------------*/

ATR_any * THD_find_atr( THD_datablock * blk , char * name )
{
   int ia ;

   if( ! ISVALID_DATABLOCK(blk) )
      THD_FATAL_ERROR( "Illegal block type in THD_find_atr" ) ;

   if( blk->natr == 0 || blk->atr == NULL ) return NULL ;

   /* loop over attributes and check names */

   for( ia=0 ; ia < blk->natr ; ia++ ){
      char * aname ;
      ATR_any * next_atr = &(blk->atr[ia]) ;  /* pointer to this atr */

      /* extract pointer to name from next_atr */

      switch( next_atr->type ){

         default: aname = NULL ; break ;

         case ATR_FLOAT_TYPE:{
            ATR_float * aa = (ATR_float *) next_atr ;
            aname = aa->name ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string * aa = (ATR_string *) next_atr ;
            aname = aa->name ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int * aa = (ATR_int *) next_atr ;
            aname = aa->name ;
         }
         break ;
      }

      /* check if names match; if so, return the result */

      if( aname != NULL && strcmp(aname,name) == 0 ) return next_atr ;

   } /* end of loop over attributes */

   return NULL ;  /* none matched */
}

ATR_float * THD_find_float_atr( THD_datablock * blk , char * name )
{
   ATR_any * aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_FLOAT_TYPE ) return NULL ;
   else                                           return (ATR_float *) aa ;
}

ATR_int * THD_find_int_atr( THD_datablock * blk , char * name )
{
   ATR_any * aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_INT_TYPE ) return NULL ;
   else                                         return (ATR_int *) aa ;
}

ATR_string * THD_find_string_atr( THD_datablock * blk , char * name )
{
   ATR_any * aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_STRING_TYPE ) return NULL ;
   else                                            return (ATR_string *)aa;
}

/*-----------------------------------------------------------------------
  given a datablock, set an attribute
  (if name is same as existing attribute, will overwrite)
-------------------------------------------------------------------------*/

void THD_set_atr( THD_datablock * blk , char * aname ,
                  int atype , int acount , void * ar )
{
   ATR_any * old_atr , * atr ;

   if( ! ISVALID_DATABLOCK(blk) )
      THD_FATAL_ERROR( "Illegal block type in THD_set_atr" ) ;

   if( acount < 0 || ar == NULL || aname == NULL )
      THD_FATAL_ERROR( "Illegal input data in THD_set_atr" ) ;

   old_atr = THD_find_atr( blk , aname ) ;  /* find matching name */

   if( old_atr != NULL ){  /* if an attribute with this name already is */

      atr = old_atr ;

      switch( old_atr->type ){  /* free data in old attribute */

         default: break ;  /* something unpleasant */

         case ATR_FLOAT_TYPE:{
            ATR_float * aa = (ATR_float *) old_atr ;

            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->fl   ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int * aa = (ATR_int *) old_atr ;

            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->in   ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string * aa = (ATR_string *) old_atr ;

            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->ch   ) ;
         }
         break ;
      }  /* end of switch */

   } else {  /* this is a new attribute name for this datablock */

      int ia ;

      for( ia=0 ; ia < blk->natr ; ia++ )     /* 29 April 1998: look for an */
         if( blk->atr[ia].type < 0 ) break ;  /* unused one before the end  */

      if( ia == blk->natr_alloc ){            /* need to extend array */
         blk->natr_alloc  += ATR_ALLINC ;
         blk->atr          = (ATR_any *)
                             XtRealloc( (char *)blk->atr,
                                        sizeof(ATR_any) * blk->natr_alloc );
      }
      atr = &(blk->atr[ia]) ;
      if( ia == blk->natr ) (blk->natr)++ ;
   }

   /* at this point, atr points to the location to store the data;
      now, allocate space for the actual data and store it */

   switch( atype ){

      case ATR_FLOAT_TYPE:{
         ATR_float * new_atr = (ATR_float *) atr ;

         new_atr->type = ATR_FLOAT_TYPE ;
         new_atr->name = XtNewString( aname ) ;
         new_atr->nfl  = acount ;
         new_atr->fl   = (float *) XtMalloc( sizeof(float) * acount ) ;
         memcpy( new_atr->fl , ar , sizeof(float)*acount ) ;

         ADDTO_KILL( blk->kl , new_atr->name ) ;
         ADDTO_KILL( blk->kl , new_atr->fl ) ;
      }
      break ;

      case ATR_INT_TYPE:{
         ATR_int * new_atr = (ATR_int *) atr ;

         new_atr->type = ATR_INT_TYPE ;
         new_atr->name = XtNewString( aname ) ;
         new_atr->nin  = acount ;
         new_atr->in   = (int *) XtMalloc( sizeof(int) * acount ) ;
         memcpy( new_atr->in , ar , sizeof(int)*acount ) ;

         ADDTO_KILL( blk->kl , new_atr->name ) ;
         ADDTO_KILL( blk->kl , new_atr->in ) ;
      }
      break ;

      case ATR_STRING_TYPE:{
         ATR_string * new_atr = (ATR_string *) atr ;

         new_atr->type = ATR_STRING_TYPE ;
         new_atr->name = XtNewString( aname ) ;
         new_atr->nch  = acount ;
         new_atr->ch   = (char *) XtMalloc( sizeof(char) * acount ) ;
         memcpy( new_atr->ch , ar , sizeof(char)*acount ) ;
         new_atr->ch[acount-1] = '\0' ;

         ADDTO_KILL( blk->kl , new_atr->name ) ;
         ADDTO_KILL( blk->kl , new_atr->ch ) ;
      }
      break ;
   }  /* end of switch */
}

void THD_set_float_atr( THD_datablock * blk ,
                        char * name , int n , float * fl )
{
   THD_set_atr( blk , name , ATR_FLOAT_TYPE , n , fl ) ;
}

void THD_set_int_atr( THD_datablock * blk ,
                      char * name , int n , int * in )
{
   THD_set_atr( blk , name , ATR_INT_TYPE , n , in ) ;
}

void THD_set_char_atr( THD_datablock * blk ,
                       char * name , int n , char * str )
{
   THD_set_atr( blk , name , ATR_STRING_TYPE , n , str ) ;
}
