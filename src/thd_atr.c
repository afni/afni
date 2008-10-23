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

void atr_print( ATR_any *atr, char *ssep, char *spsep, char quote, int do_name)
{
   int ii ;
   char ssep_def[]={"~"};
   int neword = 1;

   if (!ssep) ssep = ssep_def;

   switch( atr->type ){

      default:
         ERROR_message("Illegal attribute type found: %d",atr->type);
      exit(1) ;

      case ATR_FLOAT_TYPE:{
         ATR_float *aa = (ATR_float *) atr ;
         if( do_name ) printf("%s = ",aa->name) ;
         for( ii=0 ; ii < aa->nfl ; ii++ )
            printf("%s ",MV_format_fval(aa->fl[ii])) ;
         printf("\n") ;
      }
      return ;

      case ATR_INT_TYPE:{
         ATR_int *aa = (ATR_int *) atr ;
         if( do_name ) printf("%s = ",aa->name) ;
         for( ii=0 ; ii < aa->nin ; ii++ )
            printf("%d ",aa->in[ii]) ;
         printf("\n") ;
      }
      return ;

      case ATR_STRING_TYPE:{
         ATR_string *aa = (ATR_string *) atr ;
         char *str = (char *)malloc(sizeof(char)*(aa->nch+1)) ;
         char *eee ;
         memcpy(str,aa->ch,aa->nch) ; str[aa->nch] = '\0' ;

#if 0
         eee = tross_Expand_String(str) ;
#else
         eee = NULL ;
#endif
         if( do_name ) printf("%s = ",aa->name) ;
         if( eee != NULL ){
            printf("%s\n",eee) ; free(eee) ;
         } else if( str[0] != '\0' ){
            int isb = 0;
            neword = 1;
            for (ii=0; ii<aa->nch;++ii) {
               if (str[ii] == '\0') {
                  ++isb;
                  if (quote != '\0') printf("%c", quote);
                  if (strcmp(ssep,"NUM") == 0) {
                     /* handled below*/
                  } else {
                     printf("%s",ssep);
                  }
                  neword = 1;
               } else {
                  if (neword) {
                     if (strcmp(ssep,"NUM") == 0) {
                        printf(" %d ",isb);
                     }
                     if (quote != '\0') printf("%c", quote);
                  }
                  if (spsep && str[ii] == ' ') printf("%s", spsep);
                  else printf("%c", str[ii]);
                  neword = 0;
               }
            }
            printf("\n") ;
         } else {
            printf("(null)\n") ;
         }
         free(str) ;
      }
      return ;
   }
}

/*-----------------------------------------------------------------------*/
/*!  Given the rudiments of a datablock, read all the attributes into it
-------------------------------------------------------------------------*/

void THD_read_all_atr( char *headername , THD_datablock *blk )
{
   ATR_any *next_atr ;
   int code , ii ;
   FILE *header_file ;

ENTRY("THD_read_all_atr") ;

   if( ! ISVALID_DATABLOCK(blk) )
      THD_FATAL_ERROR( "Illegal datablock type in THD_read_all_atr" ) ;

   blk->natr       = 0 ;     /* initialize to no attributes */
   blk->natr_alloc = 0 ;
   blk->atr        = NULL ;

   /*--- certain types of filenames are verboten ---*/

   if( STRING_HAS_SUFFIX(headername,".mnc")    ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".nii")    ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".nii.gz") ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".mri")    ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".ctf")    ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".hdr")    ) EXRETURN ;
   if( STRING_HAS_SUFFIX(headername,".mpg")    ) EXRETURN ;

   /*--- open file; if unable to do so, exeunt ---*/

   header_file = fopen( headername , "r" ) ;
   if( header_file == NULL ) EXRETURN ;

   /*--- 01 Jun 2005: check if this is a NIML-style header file ---*/

   { char buf[1024] , *cpt ; int nbuf ;
     nbuf = fread( buf , 1 , 1023 , header_file ) ;    /* read first 1K */
     if( nbuf > 0 ){                                  /* got something? */
       buf[nbuf] = '\0' ;
       cpt = strstr( buf , "<AFNI_" ) ;
       if( cpt != NULL ){                        /*** NIML Dataset!!! ***/
         fclose( header_file ) ;                 /* is reopened by NIML */
         THD_read_niml_atr( headername , blk ) ; /** read the new way! **/
         EXRETURN ;
       }
     }
     rewind( header_file ) ; /*** old style dataset ==> read it below ***/
   }

   /*----- read attributes from the header file -----*/

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
            ATR_float *new_atr = (ATR_float *) next_atr ;
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
            ATR_int *new_atr = (ATR_int *) next_atr ;

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
            ATR_string *new_atr = (ATR_string *) next_atr ;

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

   fclose( header_file ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Read NIML-formatted attributes from the header file. [01 Jun 2005]
-------------------------------------------------------------------------*/

void THD_read_niml_atr( char *headername , THD_datablock *blk )
{
   NI_stream ns ;
   void *nini ;
   NI_group *ngr ;
   char sname[2048] ;
   long fsize ;

ENTRY("THD_read_niml_atr") ;

   /** open NIML stream to read from the file **/

   if( headername == NULL || *headername == '\0' || blk == NULL ) EXRETURN ;
   fsize = NI_filesize(headername) ; if( fsize <= 10 ) EXRETURN ;
   sprintf(sname,"file:%s",headername) ; STATUS(sname) ;
   ns = NI_stream_open( sname , "r" ) ;
   if( ns == (NI_stream)NULL ) EXRETURN ;
   if( fsize > NI_BUFSIZE ){
     fsize = MIN( 4*NI_BUFSIZE , fsize ) ;
     NI_stream_setbufsize( ns , fsize ) ;
   }

   /** read one group element from it (e.g., skipping the XML prolog) **/

   while(1){
     nini = NI_read_element( ns , 9 ) ;
     if( nini == NULL ){ NI_stream_close(ns); EXRETURN; }      /* bad! */
     if( NI_element_type(nini) == NI_GROUP_TYPE ) break ;      /* good */
     NI_free_element(nini) ;                       /* not what we want */
   }
   NI_stream_close( ns ) ;
   ngr = (NI_group *)nini ;
   if( strncmp(ngr->name,"AFNI_",5) != 0 ){ NI_free_element(ngr); EXRETURN; }

   /** actually process element, then exit stage right **/

   THD_dblkatr_from_niml( ngr , blk ) ;  /* cf. thd_nimlatr.c */
   NI_free_element( ngr ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  29 April 1998: erase all attributes from a datablock
-------------------------------------------------------------------------*/

void THD_erase_all_atr( THD_datablock *blk )
{
   int ia ;
   ATR_any *next_atr ;

ENTRY("THD_erase_all_atr") ;

   if( !ISVALID_DATABLOCK(blk) || blk->natr == 0 || blk->atr == NULL ) EXRETURN ;

   for( ia=0 ; ia < blk->natr ; ia++ ){
      next_atr = blk->atr + ia ;

      switch( next_atr->type ){
         case ATR_FLOAT_TYPE:{
            ATR_float *aa = (ATR_float *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->fl ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string *aa = (ATR_string *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->ch ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int *aa = (ATR_int *) next_atr ;
            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->in ) ;
         }
         break ;
      }

      next_atr->type = ILLEGAL_TYPE ;
   }

   blk->natr = 0 ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   29 April 1998: erase a single attribute, given by name
-------------------------------------------------------------------------*/

void THD_erase_one_atr( THD_datablock *blk , char *name )
{
   ATR_any *next_atr ;

ENTRY("THD_erase_one_atr") ;

   if( ! ISVALID_DATABLOCK(blk) || name     == NULL ||
       blk->natr == 0           || blk->atr == NULL   ) EXRETURN ;

   next_atr = THD_find_atr( blk , name ) ;

   if( next_atr == NULL ) EXRETURN ;

   switch( next_atr->type ){
      case ATR_FLOAT_TYPE:{
         ATR_float *aa = (ATR_float *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->fl ) ;
      }
      break ;

      case ATR_STRING_TYPE:{
         ATR_string *aa = (ATR_string *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->ch ) ;
      }
      break ;

      case ATR_INT_TYPE:{
         ATR_int *aa = (ATR_int *) next_atr ;
         SINGLE_KILL( blk->kl , aa->name ) ;
         SINGLE_KILL( blk->kl , aa->in ) ;
      }
      break ;
   }

   next_atr->type = ILLEGAL_TYPE ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  given a datablock and an attribute name, return the pointer to the
  attribute structure that matches (if none, return NULL)
-------------------------------------------------------------------------*/

ATR_any * THD_find_atr( THD_datablock *blk , char *name )
{
   int ia ;

ENTRY("THD_find_atr") ;

   if( ! ISVALID_DATABLOCK(blk) )
      THD_FATAL_ERROR( "Illegal block type in THD_find_atr" ) ;

   if( blk->natr == 0 || blk->atr == NULL ) RETURN(NULL) ;

   /* loop over attributes and check names */

   for( ia=0 ; ia < blk->natr ; ia++ ){
      char *aname ;
      ATR_any *next_atr = &(blk->atr[ia]) ;  /* pointer to this atr */

      /* extract pointer to name from next_atr */

      switch( next_atr->type ){

         default: aname = NULL ; break ;

         case ATR_FLOAT_TYPE:{
            ATR_float *aa = (ATR_float *) next_atr ;
            aname = aa->name ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string *aa = (ATR_string *) next_atr ;
            aname = aa->name ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int *aa = (ATR_int *) next_atr ;
            aname = aa->name ;
         }
         break ;
      }

      /* check if names match; if so, return the result */

      if( aname != NULL && strcmp(aname,name) == 0 ) RETURN(next_atr) ;

   } /* end of loop over attributes */

   RETURN(NULL) ;  /* none matched */
}

/*-----------------------------------------------------------------------*/

ATR_float * THD_find_float_atr( THD_datablock *blk , char *name )
{
   ATR_any *aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_FLOAT_TYPE ) return NULL ;
   else                                           return (ATR_float *) aa ;
}

/*-----------------------------------------------------------------------*/

ATR_int * THD_find_int_atr( THD_datablock *blk , char *name )
{
   ATR_any *aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_INT_TYPE ) return NULL ;
   else                                         return (ATR_int *) aa ;
}

/*-----------------------------------------------------------------------*/

ATR_string * THD_find_string_atr( THD_datablock *blk , char *name )
{
   ATR_any *aa ;
   aa = THD_find_atr( blk , name ) ;

   if( aa == NULL || aa->type != ATR_STRING_TYPE ) return NULL ;
   else                                            return (ATR_string *)aa;
}

/*-----------------------------------------------------------------------
  given a datablock, set an attribute
  (if name is same as existing attribute, will overwrite)
-------------------------------------------------------------------------*/

void THD_set_atr( THD_datablock *blk , char *aname ,
                  int atype , int acount , void *ar )
{
   ATR_any *old_atr , *atr ;

ENTRY("THD_set_atr") ;

   if( ! ISVALID_DATABLOCK(blk) )
     THD_FATAL_ERROR( "Illegal block type in THD_set_atr" ) ;

   if( acount < 0 || ar == NULL || aname == NULL )
     THD_FATAL_ERROR( "Illegal input data in THD_set_atr" ) ;

   STATUS(aname) ;

   old_atr = THD_find_atr( blk , aname ) ;  /* find matching name */

   if( old_atr != NULL ){  /* if an attribute with this name already is */

      atr = old_atr ;

      switch( old_atr->type ){  /* free data in old attribute */

         default: break ;  /* something unpleasant */

         case ATR_FLOAT_TYPE:{
            ATR_float *aa = (ATR_float *) old_atr ;

            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->fl   ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int *aa = (ATR_int *) old_atr ;

            SINGLE_KILL( blk->kl , aa->name ) ;
            SINGLE_KILL( blk->kl , aa->in   ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string *aa = (ATR_string *) old_atr ;

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
         ATR_float *new_atr = (ATR_float *) atr ;

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
         ATR_int *new_atr = (ATR_int *) atr ;

         new_atr->type = ATR_INT_TYPE ;
         new_atr->name = XtNewString( aname ) ;
         new_atr->nin  = acount ;
         new_atr->in   = (int *) XtMalloc( sizeof(int) * acount ) ;
         memcpy( new_atr->in , ar , sizeof(int)*acount ) ;

         ADDTO_KILL( blk->kl , new_atr->name ) ;
         ADDTO_KILL( blk->kl , new_atr->in ) ;

#if 0
if(PRINT_TRACING){
  char str[256] ; int ii ;
  sprintf(str,"INT atr: name=%s nin=%d vals::",new_atr->name,new_atr->nin) ;
  STATUS(str) ;
  for( ii=0 ; ii < acount ; ii++ ) printf(" %d",new_atr->in[ii]) ;
  printf("\n") ;
}
#endif
      }
      break ;

      case ATR_STRING_TYPE:{
         ATR_string *new_atr = (ATR_string *) atr ;

         new_atr->type = ATR_STRING_TYPE ;
         new_atr->name = XtNewString( aname ) ;
         new_atr->nch  = acount ;
         new_atr->ch   = (char *) XtMalloc( sizeof(char) * acount ) ;
         memcpy( new_atr->ch , ar , sizeof(char)*acount ) ;
         new_atr->ch[acount-1] = '\0' ;
         /*** fprintf(stderr,"Have %d chars\n", acount);
              atr_print( (ATR_any *)new_atr, NULL , NULL, '\0', 1) ; ***/

         ADDTO_KILL( blk->kl , new_atr->name ) ;
         ADDTO_KILL( blk->kl , new_atr->ch ) ;
      }
      break ;
   }  /* end of switch */

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void THD_set_float_atr( THD_datablock *blk ,
                        char *name , int n , float *fl )
{
   THD_set_atr( blk , name , ATR_FLOAT_TYPE , n , fl ) ;
}

/*-----------------------------------------------------------------------*/

void THD_set_int_atr( THD_datablock *blk ,
                      char *name , int n , int *in )
{
   THD_set_atr( blk , name , ATR_INT_TYPE , n , in ) ;
}

/*-----------------------------------------------------------------------*/

void THD_set_char_atr( THD_datablock *blk ,
                       char *name , int n , char *str )
{
   THD_set_atr( blk , name , ATR_STRING_TYPE , n , str ) ;
}

/*-----------------------------------------------------------------------*/
/*! Remove attributes from a dataset that might contain identifying
    strings.  Can't help against sneaky users who put them into
    other places, but might be good enough for non-programmers.
-------------------------------------------------------------------------*/

void THD_anonymize_dset( THD_3dim_dataset *dset ) /* 08 Jul 2005 */
{
   THD_datablock *blk ;
   int ia ;

ENTRY("THD_anonymize_dset") ;

   if( !ISVALID_DSET(dset) ) EXRETURN ;
   blk = dset->dblk ;
   if( !ISVALID_DATABLOCK(blk) || blk->natr <= 0 ) EXRETURN ;

   for( ia=0 ; ia < blk->natr ; ia++ ){
     char *aname ;
     ATR_any *next_atr = &(blk->atr[ia]) ;  /* pointer to this atr */

     switch( next_atr->type ){

       default: aname = NULL ; break ;

       case ATR_FLOAT_TYPE:{
         ATR_float *aa = (ATR_float *) next_atr ;
         aname = aa->name ;
       }
       break ;

       case ATR_STRING_TYPE:{
         ATR_string *aa = (ATR_string *) next_atr ;
         aname = aa->name ;
       }
       break ;

       case ATR_INT_TYPE:{
         ATR_int *aa = (ATR_int *) next_atr ;
         aname = aa->name ;
       }
       break ;
     }

     if( aname == NULL || *aname == '\0' ) continue ;

     if( strstr(aname,"NOTE") != NULL || strstr(aname,"_NAME") != NULL )
       THD_erase_one_atr( blk , aname ) ;
   }

   THD_set_string_atr( blk , ATRNAME_LABEL1         , "none" ) ;
   THD_set_string_atr( blk , ATRNAME_LABEL2         , "none" ) ;
   THD_set_string_atr( blk , ATRNAME_DATANAME       , "none" ) ;
   THD_erase_one_atr ( blk , ATRNAME_BRICK_KEYWORDS          ) ;
   THD_erase_one_atr ( blk , ATRNAME_KEYWORDS                ) ;

   EXRETURN ;
}

/*------------------------------------------------------------------*/

ATR_any * THD_copy_atr( ATR_any *atr )  /* 03 Aug 2005 */
{
   ATR_any *atr_out=NULL ;

ENTRY("THD_copy_atr") ;

   if( atr == NULL ) RETURN(NULL) ;

   switch( atr->type ){

     case ATR_FLOAT_TYPE:{
       ATR_float *aa = (ATR_float *)atr , *qq ;
       qq = (ATR_float *)XtMalloc(sizeof(ATR_float)) ;
       qq->type = ATR_FLOAT_TYPE ;
       qq->name = XtNewString( aa->name ) ;
       qq->nfl  = aa->nfl ;
       qq->fl   = (float *) XtMalloc( sizeof(float) * aa->nfl ) ;
       memcpy( qq->fl , aa->fl , sizeof(float) * aa->nfl ) ;
       atr_out = (ATR_any *)qq ;
     }
     break ;

     case ATR_STRING_TYPE:{
       ATR_string *aa = (ATR_string *)atr , *qq ;
       qq = (ATR_string *)XtMalloc(sizeof(ATR_string)) ;
       qq->type = ATR_STRING_TYPE ;
       qq->name = XtNewString( aa->name ) ;
       qq->nch  = aa->nch ;
       qq->ch   = (char *) XtMalloc( sizeof(char) * aa->nch ) ;
       memcpy( qq->ch , aa->ch , sizeof(char) * aa->nch ) ;
       atr_out = (ATR_any *)qq ;
     }
     break ;

     case ATR_INT_TYPE:{
       ATR_int *aa = (ATR_int *)atr , *qq ;
       qq = (ATR_int *)XtMalloc(sizeof(ATR_int)) ;
       qq->type = ATR_INT_TYPE ;
       qq->name = XtNewString( aa->name ) ;
       qq->nin  = aa->nin ;
       qq->in   = (int *) XtMalloc( sizeof(int) * aa->nin ) ;
       memcpy( qq->in , aa->in , sizeof(int) * aa->nin ) ;
       atr_out = (ATR_any *)qq ;
     }
     break ;
   }

   RETURN(atr_out) ;
}

/*------------------------------------------------------------------*/

void THD_insert_atr( THD_datablock *blk , ATR_any *atr )  /* 03 Aug 2005 */
{
ENTRY("THD_insert_atr") ;

   if( ! ISVALID_DATABLOCK(blk) || atr == NULL ) EXRETURN ;

   switch( atr->type ){

     case ATR_FLOAT_TYPE:{
       ATR_float *aa = (ATR_float *)atr ;
       THD_set_atr( blk , aa->name , ATR_FLOAT_TYPE , aa->nfl , aa->fl ) ;
     }
     break ;

     case ATR_STRING_TYPE:{
       ATR_string *aa = (ATR_string *)atr ;
       THD_set_atr( blk , aa->name , ATR_STRING_TYPE , aa->nch , aa->ch ) ;
     }
     break ;

     case ATR_INT_TYPE:{
       ATR_int *aa = (ATR_int *)atr ;
       THD_set_atr( blk , aa->name , ATR_INT_TYPE , aa->nin , aa->in ) ;
     }
     break ;
   }

   EXRETURN ;
}
