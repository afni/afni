/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

static int allow_negative = 0 ;

/*! Allow negative indexes in MCW_get_intlist() */

void MCW_intlist_allow_negative( int iii )   /* 22 Nov 1999 */
{
   allow_negative = iii ; return ;
}

/*! Stopping criterion for MCW_get_intlist() */

#define ISEND(c) ( (c)==']' || (c)=='}' || (c)=='\0' )

/*-----------------------------------------------------------------*/
/*! Get an integer list in the range 0..(nvals-1), from the
   character string str.  If we call the output pointer fred,
   then fred[0] = number of integers in the list (> 0), and
        fred[i] = i-th integer in the list for i=1..fred[0].
   If on return, fred == NULL or fred[0] == 0, then something is
   wrong, and the caller must deal with that.

   Syntax of input string:
     - initial '{' or '[' is skipped, if present
     - ends when '}' or ']' or end of string is found
     - contains entries separated by commas
     - entries have one of these forms:
       - a single number
       - a dollar sign '$', which means nvals-1
       - a sequence of consecutive numbers in the form "a..b" or
         "a-b", where "a" and "b" are single numbers (or '$')
       - a sequence of evenly spaced numbers in the form
         "a..b(c)" or "a-b(c)", where "c" encodes the step
     - Example:  "[2,7..4,3..9(2)]" decodes to the list
         2 7 6 5 4 3 5 7 9
     - entries should be in the range 0..nvals-1
-------------------------------------------------------------------*/

int * MCW_get_intlist( int nvals , char *str )
{
   int *subv = NULL ;
   int ii , ipos , nout , slen ;
   int ibot,itop,istep , nused ;
   char *cpt ;

   /* Meaningless input? */

   if( nvals < 1 ) return NULL ;

   /* No selection list? */

   if( str == NULL || str[0] == '\0' ) return NULL ;

   /* skip initial '[' or '{' */

   subv    = (int *) malloc( sizeof(int) * 2 ) ;
   subv[0] = nout = 0 ;

   ipos = 0 ;
   if( str[ipos] == '[' || str[ipos] == '{' ) ipos++ ;

   /*** loop through each sub-selector until end of input ***/

   slen = strlen(str) ;
   while( ipos < slen && !ISEND(str[ipos]) ){

      while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
      if( ISEND(str[ipos]) ) break ;         /* done */

      /** get starting value **/

      if( str[ipos] == '$' ){  /* special case */
         ibot = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         ibot = strtol( str+ipos , &cpt , 10 ) ;
         if( ibot < 0 && !allow_negative ){
           fprintf(stderr,"** ERROR: sub-brick index %d is out of range 0..%d\n",
                   ibot,nvals-1) ;
           free(subv) ; return NULL ;
         }
         if( ibot >= nvals ){
           fprintf(stderr,"** ERROR: sub-brick index %d is out of range 0..%d\n",
                   ibot,nvals-1) ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         if( ibot == 0 && nused == 0 ){
           fprintf(stderr,"** ERROR: sub-brick syntax error '%s'\n",str+ipos) ;
           free(subv) ; return NULL ;
         }
         ipos += nused ;
      }

      while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */

      /** if that's it for this sub-selector, add one value to list **/

      if( str[ipos] == ',' || ISEND(str[ipos]) ){
         nout++ ;
         subv = (int *) realloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ibot ;
         if( ISEND(str[ipos]) ) break ; /* done */
         ipos++ ; continue ;            /* re-start loop at next sub-selector */
      }

      /** otherwise, must have '..' or '-' as next inputs **/

      if( str[ipos] == '-' ){
         ipos++ ;
      } else if( str[ipos] == '.' && str[ipos+1] == '.' ){
         ipos++ ; ipos++ ;
      } else {
         fprintf(stderr,"** ERROR: sub-brick selector syntax is bad: '%s'\n",
                 str+ipos) ;
         free(subv) ; return NULL ;
      }

      /** get ending value for loop now **/

      if( str[ipos] == '$' ){  /* special case */
         itop = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         itop = strtol( str+ipos , &cpt , 10 ) ;
         if( itop < 0 && !allow_negative ){
           fprintf(stderr,"** ERROR: sub-brick index %d is out of range 0..%d\n",
                   itop,nvals-1) ;
           free(subv) ; return NULL ;
         }
         if( itop >= nvals ){
           fprintf(stderr,"** ERROR: sub-brick index %d is out of range 0..%d\n",
                   itop,nvals-1) ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         if( itop == 0 && nused == 0 ){
           fprintf(stderr,"** ERROR: sub-brick syntax error '%s'\n",str+ipos) ;
           free(subv) ; return NULL ;
         }
         ipos += nused ;
      }

      /** set default loop step **/

      istep = (ibot <= itop) ? 1 : -1 ;

      while( isspace(str[ipos]) ) ipos++ ;                  /* skip blanks */

      /** check if we have a non-default loop step **/

      if( str[ipos] == '(' ){  /* decode an integer */
         ipos++ ;
         istep = strtol( str+ipos , &cpt , 10 ) ;
         if( istep == 0 ){
           fprintf(stderr,"** ERROR: sub-brick loop step is 0!\n") ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         ipos += nused ;
         if( str[ipos] == ')' ) ipos++ ;
         if( (ibot-itop)*istep > 0 ){
           fprintf(stderr,"** WARNING: sub-brick count '%d..%d(%d)' means nothing!\n",
                   ibot,itop,istep ) ;
         }
      }

      /** add values to output **/

      for( ii=ibot ; (ii-itop)*istep <= 0 ; ii += istep ){
         nout++ ;
         subv = (int *) realloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ii ;
      }

      /** check if we have a comma to skip over **/

      while( isspace(str[ipos]) ) ipos++ ;                  /* skip blanks */
      if( str[ipos] == ',' ) ipos++ ;                       /* skip commas */

   }  /* end of loop through selector string */

   if( subv[0] == 0 ){ free(subv); subv = NULL; }
   return subv ;
}
