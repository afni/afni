/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

extern int *z_rand_order(int bot, int top, long int seed);

static int allow_negative = 0 ;

/*! Allow negative indexes in MCW_get_intlist() */

void MCW_intlist_allow_negative( int iii )   /* 22 Nov 1999 */
{
   allow_negative = iii ; return ;
}

/*! Stopping criterion for MCW_get_intlist()          ZSS, Aug 06: Added '#' to ISEND */

#define ISEND(c) ( (c)==']' || (c)=='}' || (c)=='#' || (c)=='\0' )

int * get_count_intlist ( char *str , int *nret)
{
   int *subv = NULL, *ret = NULL ;
   int ii , ipos , nout , slen, shuffle, step, itmp;
   int ibot,itop,istep , nused , nuni;
   long int seed=0;
   char *cpt ;
   
   *nret = -1;
   if (!str || !strstr(str,"count ") || strlen (str) < 8) {
      fprintf(stderr, "NULL input or string does not have 'count '"
                      " or at least 2 values are not present after 'count '\n");
      return (NULL);
   }
   
   /* move past count */
   slen = strlen(str) ;
   ipos = strlen("count ");

   /* see if you have seed */
   if (strstr(str, "-seed ")) {
      ipos = (strstr(str, "-seed ")-str)+strlen("-seed ");
      seed = strtol( str+ipos , &cpt , 10 ) ;
      nused = (cpt-(str+ipos)) ;
      ipos += nused ;
   }
   
   /* get first value */
   while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
   if( ISEND(str[ipos]) ) return NULL ;         /* bad */
   ibot = strtol( str+ipos , &cpt , 10 ) ;
   if( ibot < 0 && !allow_negative ){
     fprintf(stderr,"** ERROR: selector index %d cannot be < 0\n",
             ibot) ;
   }
   nused = (cpt-(str+ipos)) ;
   if( ibot == 0 && nused == 0 ){
     fprintf(stderr,"** ERROR: selector syntax error '%s'\n",str+ipos) ;
     return NULL ;
   }
   ipos += nused ;

   while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
   if( ISEND(str[ipos]) ) return NULL  ;         /* Bad */
   itop = strtol( str+ipos , &cpt , 10 ) ;
   if( itop < 0 && !allow_negative ){
     fprintf(stderr,"** ERROR: selector index %d cannot be < 0\n",
             itop) ;
     return NULL ;
   }
   if( itop == 0 && nused == 0 ){
     fprintf(stderr,"** ERROR: selector syntax error '%s'\n",str+ipos) ;
     return NULL ;
   }
   nused = (cpt-(str+ipos)) ;
   ipos += nused ;
   
   shuffle = 0;
   step = 0;
   while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
   if( !ISEND(str[ipos]) ) { /* have step */
      if (  (str[ipos] >= 'A' && str[ipos]<='Z') ||
            (str[ipos] >= 'a' && str[ipos]<='z') ) {
         /* only S is acceptable here */
         if (str[ipos] == 'S' || str[ipos] == 's') {
            shuffle = 1;
            ++ipos;
         }else {
            fprintf(stderr,
                     "** No qualifiers allowed for step, other than 'S'. "
                     "Have %c.\n", str[ipos]);
            return NULL ;
         }       
      }
      if( !ISEND(str[ipos]) ) {
         step = strtol( str+ipos , &cpt , 10 ) ;   
      }
   }
   nused = (cpt-(str+ipos)) ;
   ipos += nused ;
   
   if (step < 0) {
      fprintf(stderr,"** step must be > 0. Have %d.\n", step);
      return NULL ;
   }
   
   /* 
      fprintf(stderr,"Have count parameters: %d to %d with 
      step %d and shuffle = %d; seed = %ld\n", ibot, itop, step, shuffle, seed);
   */
      
   if (itop < ibot) {nuni = ibot - itop + 1;}
   else { nuni = itop - ibot + 1; } 
   
   if (shuffle) {
      subv = z_rand_order(ibot, itop, seed);
      if (step > 0) { 
         *nret = step;
      } else {
         *nret = nuni;
      }
   } else {
      *nret = nuni;
      subv = (int *)malloc(sizeof(int)*(*nret));
      if (!step) {
         if (itop < ibot) step = -1;
         else step = 1;
      } else {
         if (itop < ibot) step *= -1;
         else step *= 1;
      }
      itmp = 0;
      if (itop < ibot) 
         for (ii=ibot; ii>=itop;ii=ii+step) { subv[itmp] = ii; ++itmp; }
      else for (ii=ibot; ii<=itop;ii=ii+step) { subv[itmp] = ii; ++itmp; }
      *nret = itmp;
   }
   
   /* fprintf(stderr,"Have %d ints: %d to %d with step %d 
      and shuffle = %d\n", *nret, ibot, itop, step, shuffle); */
   ret = (int *)malloc(sizeof(int)*(*nret+1));
   ret[0] = *nret;
   for (ii=1; ii<=ret[0]; ++ii) ret[ii] = subv[(ii-1)%(nuni)];
   
   free(subv); subv = ret;
   
   /* for (ii=0; ii<=ret[0]; ++ii) fprintf(stderr,"%d: %d\n", ii, subv[ii]); */
   
   return(subv);
}

/*-----------------------------------------------------------------*/
/*! Get an integer list in the range 0..(nvals-1), from the
   character string str.  If we call the output pointer fred,
   then fred[0] = number of integers in the list (> 0), and
        fred[i] = i-th integer in the list for i=1..fred[0].
   If on return, fred == NULL or fred[0] == 0, then something is
   wrong, and the caller must deal with that.

   Syntax of input string:
     - initial '{' or '['  or '#' is skipped, if present
     - ends when '}' or ']' or '#' or end of string is found
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

   /* skip initial '[' or '{' or '#'*/

   subv    = (int *) malloc( sizeof(int) * 2 ) ;
   subv[0] = nout = 0 ;

   ipos = 0 ;
   if( str[ipos] == '[' || str[ipos] == '{' || str[ipos] == '#') ipos++ ;

   /* do we have a count string in there ZSS ? */
   if (strstr(str,"count ")) {
      return(get_count_intlist ( str, &ii));
   }
     
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
           fprintf(stderr,
                   "** ERROR: selector index %d is out of range 0..%d\n",
                   ibot,nvals-1) ;
           free(subv) ; return NULL ;
         }
         if( ibot >= nvals ){
           fprintf(stderr,
                   "** ERROR: selector index %d is out of range 0..%d\n",
                   ibot,nvals-1) ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         if( ibot == 0 && nused == 0 ){
           fprintf(stderr,
                   "** ERROR: selector syntax error '%s'\n",str+ipos) ;
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
         fprintf(stderr,"** ERROR: selector selector syntax is bad: '%s'\n",
                 str+ipos) ;
         free(subv) ; return NULL ;
      }

      /** get ending value for loop now **/

      if( str[ipos] == '$' ){  /* special case */
         itop = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         itop = strtol( str+ipos , &cpt , 10 ) ;
         if( itop < 0 && !allow_negative ){
           fprintf(stderr,"** ERROR: selector index %d is out of range 0..%d\n",
                   itop,nvals-1) ;
           free(subv) ; return NULL ;
         }
         if( itop >= nvals ){
           fprintf(stderr,"** ERROR: selector index %d is out of range 0..%d\n",
                   itop,nvals-1) ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         if( itop == 0 && nused == 0 ){
           fprintf(stderr,"** ERROR: selector syntax error '%s'\n",str+ipos) ;
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
           fprintf(stderr,"** ERROR: selector loop step is 0!\n") ;
           free(subv) ; return NULL ;
         }
         nused = (cpt-(str+ipos)) ;
         ipos += nused ;
         if( str[ipos] == ')' ) ipos++ ;
         if( (ibot-itop)*istep > 0 ){
           fprintf(  stderr,
                     "** WARNING: selector count '%d..%d(%d)' means nothing!\n",
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
