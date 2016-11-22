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

/*! Stopping criterion for MCW_get_intlist()  ZSS, Aug 06: Added '#' to ISEND */

#define ISEND(c) ( (c)==']' || (c)=='}' || (c)=='#' || (c)=='\0' )

int * get_count_intlist ( char *str , int *nret, int maxval )
{
   get_count_intlist_eng( str, nret, maxval, allow_negative );
}

int * get_count_intlist_eng ( char *str, int *nret, int maxval, int ok_neg )
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
   if( ibot < 0 && !ok_neg ){
     fprintf(stderr,"** ERROR: bot selector index %d cannot be < 0\n",
             ibot) ;
     return NULL ;  /* added return     4 Jan 2016 [rickr] */
   } else if( maxval >= 0 && ibot > maxval ){       /* 4 Jan 2016 [rickr] */
     fprintf(stderr,"** ERROR: count selector index %d exceeds max %d\n",
             ibot, maxval) ;
     return NULL ;
   }
   nused = (cpt-(str+ipos)) ;
   if( ibot == 0 && nused == 0 ){
     fprintf(stderr,"** ERROR: selector syntax error 1 '%s'\n",str+ipos) ;
     return NULL ;
   }
   ipos += nused ;

   while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
   if( ISEND(str[ipos]) ) return NULL  ;         /* Bad */
   itop = strtol( str+ipos , &cpt , 10 ) ;
   if( itop < 0 && !ok_neg ){
     fprintf(stderr,"** ERROR: top selector index %d cannot be < 0\n",
             itop) ;
     return NULL ;
   } else if( maxval >= 0 && itop > maxval ){       /* 4 Jan 2016 [rickr] */
     fprintf(stderr,"** ERROR: count top selector index %d exceeds max %d\n",
             itop, maxval) ;
     return NULL ;
   }
   if( itop == 0 && nused == 0 ){
     fprintf(stderr,"** ERROR: selector syntax error 2 '%s'\n",str+ipos) ;
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

int * get_1dcat_intlist ( char *sin , int *nret, int maxval)
{
   return get_1dcat_intlist_eng(sin, nret, maxval, allow_negative);
}

/* if maxval >= 0, values may not exceed it */
int * get_1dcat_intlist_eng ( char *sin , int *nret, int maxval, int ok_neg )
{
   int ipos , slen, *ret=NULL, ii=0;
   MRI_IMAGE *aim = NULL;
   float *far=NULL;
   char *str = NULL;
   int op = 0;
   
   *nret = -1;
   if (!sin || !strstr(sin,"1dcat ") || strlen (sin) < 8) {
      fprintf(stderr, "NULL input or string does not have '1dcat '"
                      " or a 1D filename not present after '1dcat '\n");
      return (NULL);
   }
   
   str = strdup(sin);
   
   /* move past count */
   slen = strlen(str) ;
   ipos = strlen("1dcat ");
   /* find ending square */
   for (ii=ipos; ii<slen; ++ii) {
      if (str[ii]=='[') ++op;
      if (str[ii]==']') { --op; }
      if (op < 0) { str[ii] = '\0'; break; }
   }
   /* read the filename */
   deblank_name(str+ipos);
   if (!(aim = mri_read_1D(str+ipos))) {
      ERROR_message("Can't read 1D file '%s'", str+ipos) ;
      free(str); str=NULL;
      return(NULL);
   } 

   /* return the indices */
   far = MRI_FLOAT_PTR(aim);
   *nret = aim->nx*aim->ny;
   ret = (int *)malloc(sizeof(int)*(*nret+1));
   
   ret[0] = *nret;
   for (ii=0; ii<*nret; ++ii) {
      ret[ii+1] = (int)far[ii];
      /* was #if 0: leave error handling for elsewhere, 4 Jan 2016 [rickr] */
      if ( (!ok_neg && ret[ii+1]<0) ||
           (maxval >= 0 && ret[ii+1] > maxval) ) {
         ERROR_message( "Bad 1dcat brick selection value in 1D file '%s'\n"
                        "   value %d is %g (max=%d)\n",
                        str+ipos, ii, far[ii], maxval);
         mri_free(aim); aim = NULL; far=NULL;
         free(str); str=NULL;
         free(ret); ret=NULL;
         return(NULL); 
      }
   }
   
   mri_free(aim); aim = NULL; far=NULL;
   
   #if 0
      fprintf(stderr,"ZSS: Selecting %d values from '%s':\n", *nret, str+ipos);
      for (ii=1; ii<=*nret; ++ii) { 
         fprintf(stderr,"%d,",ret[ii]);
      }
      fprintf(stderr,"\n");
   #endif
   
   free(str); str=NULL;
   return(ret);
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
     
     See also MCW_get_thd_intlist, or MCW_get_labels_intlist,
     which allow the use of sub-brick labels for selection.
     
     Do not update this function anymore, update MCW_get_labels_intlist
     instead.       
                                                      ZSS Dec 09
-------------------------------------------------------------------*/

int * MCW_get_intlist( int nvals , char *str )
{
   int *subv = NULL ;
   int ii , ipos , nout , slen ;
   int ibot,itop,istep , nused ;
   char *cpt ;

   /* Best to call new function to avoid redundancy  ZSS Dec 09 */
   return(MCW_get_labels_intlist( NULL, nvals, str ));
   
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
      return(get_count_intlist ( str, &ii, nvals-1));
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
                   "** ERROR: selector syntax error 3 '%s'\n",str+ipos) ;
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

      if( str[ipos] == '-' || str[ipos] == ':' ){
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
           fprintf(stderr,"** ERROR: selector syntax error 4 '%s'\n",str+ipos) ;
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

/*
   Check if lbl is one of the labels .
   
   lbl : Selector string. To specify labels with space
         characters ' ' in the name, replace space with 
         the '_' character.
         On the first pass, the function will match the
         string as is, but if nothing is found, a second
         pass is attempted, replacing ' ' in DSET_LABEL with
         '_' and matching again.
         lbl should match one and only one label.
   dset: dataset
   isb : to be set to the index of the matching sub-brick
         -1 if no match
   returns nused, the number of characters used up in lbl
           0 if no labels were found.
           
            ZSS Dec 09         
*/
int is_in_labels(char *lbl, char **labels, int N_labels, int *isb)
{
   char *lbln=NULL, sbuf2[500], *slbl=NULL;
   int nused = 0;
   int ii, repeat = 0, check=0, jj=0;
   int max_used = 0, max_ind = -1;    /* find max match  31 Dec 2012 [rickr] */

   *isb = -1;
   if (!labels || N_labels < 1) return(0);
   
   sbuf2[499] = '\0';
      
   /*
      fprintf(stderr,"ZSS: lbln >%s<\n"
                     "      lbl >%s<\n"
                     "     nused %d\n"
                     , lbln, lbl, nused);
   */
      
   repeat = 0;
   do {
      for (ii=0; ii<N_labels; ++ii) {
         check = 0;
         if (repeat) {
            strncpy(sbuf2, labels[ii], 498);
            slbl = sbuf2;
            /* second pass, check for space characters*/
            for (jj=0; jj<strlen(sbuf2); ++jj) {
               if (sbuf2[jj] == ' ') {
                  sbuf2[jj]='_'; check = 1; /* RickR tip */
               }
            }   
         } else {
            check = 1;
            slbl = labels[ii];
         }
         nused = strlen(slbl);
         /*
            fprintf(stderr,"ZSS: (rep %d) Checking against >%s< \n", 
                           repeat, slbl);
         */
         /* note match and keep looking for longer one (tokens might be easier
            than strings with unknown terminators)      31 Dec 2012 [rickr] */
         if (check && !(strncmp(lbl, slbl, nused))) {
            if( nused > max_used ){
               max_used = nused;
               max_ind = ii;
            }
         }
      }

      if( max_used > 0 ) {
         *isb = max_ind;
         return(max_used);
      }

      repeat = !repeat;
   } while (repeat);
   
   return(0);
}

/*
   Return the sub-brick indices referenced in str
   This is a new version of MCW_get_intlist, which allows
   the use of sub-brick labels 
   
   See is_in_labels for details.
   
      ZSS Dec 09
*/
int * MCW_get_thd_intlist( THD_3dim_dataset *dset , char *str )
{
   /* test for brick_lab is not needed, and breaks sub-brick selection
      of NIfTI datasets                             6 Jan 2009 [rickr]

      return( (dset && dset->dblk && dset->dblk->brick_lab) ? 
               MCW_get_labels_intlist (dset->dblk->brick_lab, 
                                       DSET_NVALS(dset), str) : NULL );
   */

   if( !dset || !dset->dblk ) return NULL;

   return MCW_get_labels_intlist(dset->dblk->brick_lab, DSET_NVALS(dset), str);
}

/*
   A slightly modified version of MCW_get_intlist, which 
   can scan for label matching instead of just numbers.
   If labels == NULL, then it functions just like 
   MCW_get_intlist
   
                              ZSS Dec 09
*/  
int * MCW_get_labels_intlist (char **labels, int nvals, char *str)
{   
   int *subv = NULL ;
   int ii , ipos , nout , slen ;
   int ibot,itop,istep , nused ;
   char *cpt ;
   static int show_labs = -1;
      
   /* Meaningless input? */
   if( nvals < 1 ) return NULL ;

   /* No selection list? */

   if( str == NULL || str[0] == '\0' ) return NULL ;

   if( show_labs == -1 ) show_labs = AFNI_yesenv("AFNI_SHOW_LABEL_TO_INDEX");

   /* skip initial '[' or '{' or '#'*/

   subv    = (int *) malloc( sizeof(int) * 2 ) ;
   subv[0] = nout = 0 ;

   ipos = 0 ;
   if( str[ipos] == '[' || str[ipos] == '{' || str[ipos] == '#') ipos++ ;

   /* do we have a 1dcat string in there ZSS ? */
   if (strstr(str,"1dcat ")) {
      return(get_1dcat_intlist ( str, &ii, nvals-1 ));
   }
   /* do we have a count string in there ZSS ? */
   if (strstr(str,"count ")) {
      return(get_count_intlist ( str, &ii, nvals-1 ));
   }
     
   /*** loop through each sub-selector until end of input ***/
   slen = strlen(str) ;
   while( ipos < slen && !ISEND(str[ipos]) ){
      while( isspace(str[ipos]) ) ipos++ ;   /* skip blanks */
      if( ISEND(str[ipos]) ) break ;         /* done */

      /** get starting value **/

      if( str[ipos] == '$' ){  /* special case */
         ibot = nvals-1 ; ipos++ ;
      } else if ( !labels || 
                  !(nused = is_in_labels(str+ipos, labels, nvals, &ibot))){
                                                      /* decode integer */
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
                   "** ERROR: selector syntax error 5 '%s'\n",str+ipos) ;
           free(subv) ; return NULL ;
         }
         ipos += nused ;
      } else {
         if( show_labs )
            fprintf(stderr,"-- label select: sub-brick %d is from label %s\n",
                        ibot, labels[ibot]);
         ipos+=nused;
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

      if( str[ipos] == '-' || str[ipos] == ':' ){
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
      } else if ( !labels ||
                  !(nused = is_in_labels(str+ipos,labels, nvals, &itop))){  
                                                      /* decode integer */
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
           fprintf(stderr,"** ERROR: selector syntax error 6 '%s'\n",str+ipos) ;
           free(subv) ; return NULL ;
         }
         ipos += nused ;
      } else {
         /* have a label */
         if( show_labs )
            fprintf(stderr,"-- label select: sub-brick %d is from label %s\n",
                        itop, labels[itop]);
         ipos+=nused;
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


/* ----------------------------------------------------------------------
 * Return a float range (bottom and top vals) for strings of the form
 * <a..b> or <a> (or without the <> chars).
 *
 * Labels:
 *    For now, allow <label>.  Think about doing it as a range or int list.
 *
 * return 0 on success, 1 on error                   17 Apr 2012 [rickr]
 * ----------------------------------------------------------------------
 */
int MCW_get_angle_range(THD_3dim_dataset * dset, char * instr, float * bot,
                        float * top)
{
   char * rstr;         /* copied range string */
   char * rptr;         /* current pointer within range string */
   char * tptr;         /* temp pointer */
   int    rlen, scount;
   float  fbot, ftop;   /* read values */
   double dval;         

   /* missing input */
   if ( !dset || !instr || !bot || !top ) {
      fprintf(stderr,"** MCW_get_angle_range: missing inputs\n");
      return 1;
   }

   /* maybe there is nothing here */
   if( strlen(instr) == 0 ) { *bot = 1.0;  *top = 0.0;  return 0; }

   /* duplicate input, skip any leaning '<', nuke any trailing '>' */
   rstr = nifti_strdup(instr);
   rlen = strlen(rstr);

   rptr = rstr; /* rptr moves, rstr is location of copied string */
   if( *rptr == '<' ) rptr++;
   tptr = strchr(rptr, '>');
   if( tptr ) *tptr = '\0';     /* clear any trailing '>' */

   /* first try the old method with just floats */
   tptr = strstr(rptr,"..") ;
   if( tptr ) {
      *tptr = '\0';
      scount  = sscanf(rptr,   "%f", &fbot);
      scount += sscanf(tptr+2, "%f", &ftop);
      if( scount == 2 && fbot <= ftop ) {
         *bot = fbot;  *top = ftop;  return 0;  /* success */
      } else { /* go after labels */
         if( AFNI_get_dset_label_val(dset, &dval, rptr) ) {
            *bot = 1.0;  *top = 0.0;  return 1; /* failure */
         }
         fbot = dval;
         if( AFNI_get_dset_label_val(dset, &dval, tptr+2) ) {
            *bot = 1.0;  *top = 0.0;  return 1; /* failure */
         }
         *bot = fbot;  *top = dval;  return 0;  /* success */
      }
   } else { /* ZSS: Why not allow for <val> ? */
      scount = sscanf(rptr,   "%f", &fbot);
      if( scount == 1 ) {
         *bot = fbot;  *top = fbot;  return 0;  /* success */
      } else { /* go after label */
         if( AFNI_get_dset_label_val(dset, &dval, rptr) ) {
            *bot = 1.0;  *top = 0.0;  return 1; /* failure */
         }
         *bot = dval;  *top = dval;  return 0;  /* success */
      }
   }

   /* should not get here */
   return 1;
}


/* ----------------------------------------------------------------------
 * Prepare to apply mastery <> selector to dataset by setting
 * master_bot/top (for <a> or <a..b>) or
 * master_int_list (for <a,b,c,...,d>).
 *
 * Comma-delimited values must be ints.  Labels are applied as ints.
 *
 * Labels:
 *    For now, allow <label>.  Think about doing it as a range or int list.
 *
 * return 0 on success, 1 on error                   17 Apr 2012 [rickr]
 * ----------------------------------------------------------------------
 */
int thd_check_angle_selector(THD_3dim_dataset * dset, char * instr)
{
   char * rstr;         /* copied range string */
   char * rptr;         /* current pointer within range string */
   char * dptr, * cptr; /* dot dot (..) and comma pointers */
   int    rlen, scount;
   float  fbot, ftop;   /* read values */
   double dval;         

   /* missing input */
   if ( !dset || !instr ) {
      fprintf(stderr,"** thd_check_angle_selector: missing inputs\n");
      return 1;
   }

   /* init to defaults */
   dset->dblk->master_bot = 1.0;
   dset->dblk->master_top = 0.0;

   /* maybe there is nothing here */
   if( strlen(instr) == 0 ) return 0;

   /* duplicate input, skip any leaning '<', nuke any trailing '>' */
   rstr = nifti_strdup(instr);
   rlen = strlen(rstr);

   rptr = rstr; /* rptr moves, rstr is location of copied string */
   if( *rptr == '<' ) rptr++;
   dptr = strchr(rptr, '>');
   if( dptr ) *dptr = '\0';     /* clear any trailing '>' */

   /* set dot and comma pointers */
   dptr = strstr(rptr,"..") ;
   cptr = strstr(rptr,",") ;

   /* having both is an error */
   if( dptr && cptr ) {
      fprintf(stderr,"** TSAR: cannot have both , and .. in <> selector\n");
      return 1;
   }

   /* handle .. range applies to bot and top */
   if( dptr ) {
      if( strstr(dptr+2,"..") ) {
         fprintf(stderr,"** TSAR: cannot handle multiple .. ranges in <>\n");
         return 1;
      }

      *dptr = '\0';
      scount  = sscanf(rptr,   "%f", &fbot);
      scount += sscanf(dptr+2, "%f", &ftop);
      if( scount == 2 && fbot <= ftop ) {
         dset->dblk->master_bot = fbot;
         dset->dblk->master_top = ftop;
         return 0;  /* success */
      } else { /* go after labels */
         if( AFNI_get_dset_label_val(dset, &dval, rptr) ) 
            return 1; /* failure */
         fbot = dval;
         if( AFNI_get_dset_label_val(dset, &dval, dptr+2) )
            return 1; /* failure */
         dset->dblk->master_bot = fbot;
         dset->dblk->master_top = dval;
         return 0;  /* success */
      }
   /* handle ,-delimited list of integers/labels */
   } else if ( cptr ) {
      int iii;
      if( thd_get_labeltable_intlist(dset, rptr, &dset->dblk->master_csv,
                                                 &dset->dblk->master_ncsv) )
         return 1;
fprintf(stderr,"== rcr have %d ints\n", dset->dblk->master_ncsv);
for(iii=0; iii<dset->dblk->master_ncsv;iii++)
  fprintf(stderr,"   val = %d\n", dset->dblk->master_csv[iii]);
      return 0;
   /* handle single value/label */
   } else { /* ZSS: Why not allow for <val> ? */
      scount = sscanf(rptr,   "%f", &fbot);
      if( scount == 1 ) {
         dset->dblk->master_bot = fbot;
         dset->dblk->master_top = fbot;
         return 0;  /* success */
      } else { /* go after label */
         if( AFNI_get_dset_label_val(dset, &dval, rptr) )
            return 1; /* failure */
         dset->dblk->master_bot = dval;
         dset->dblk->master_top = dval;
         return 0;  /* success */
      }
   }

   /* should not get here */
   return 1;
}


/* based on MCW_get_intlist
 *
 * dset is only needed if going after labeltable
 *
 * return 0 on success, else error
 */
int thd_get_labeltable_intlist(THD_3dim_dataset * dset, char *str,
                               int ** list, int * nvals)
{   
   static int   show_labs = -1;
   int_list     ilist, llist;    /* maintain list in int_list struct */
   char       * workstr, * next, * cpt;
   int        * tmplist;
   int          ival, tind, err;

   ENTRY("thd_get_labeltable_intlist");

   /* if cannot return length, fail */
   if( !nvals || !list ) RETURN(1);

   /* call empty selection list okay */
   *nvals = 0;
   *list = NULL;
   if( !str || str[0] == '\0' ) RETURN(0);

   if( show_labs == -1 ) show_labs = AFNI_yesenv("AFNI_SHOW_LABEL_TO_INDEX");

   /* ---------------------------------------------------------------------- */
   /* check if using 1dcat or count */
   tmplist = NULL;

   if (strstr(str,"1dcat ")) {
      tmplist = get_1dcat_intlist_eng(str, nvals, 0, 1);
      if( ! tmplist || *nvals < 1 ) RETURN(1);
   }
   if (strstr(str,"count ")) {
      tmplist = get_count_intlist_eng(str, nvals, 0, 1);
      if( ! tmplist || *nvals < 1 ) RETURN(1);
   }

   /* if success, get rid of initial length value in tmplist */
   if ( tmplist ) {
      *list = malloc(*nvals * sizeof(int));
      memcpy(*list, tmplist+1, *nvals * sizeof(int));
      free(tmplist);
      RETURN(0);
   }

   /* ---------------------------------------------------------------------- */
   /* else, process as comma-separated list                                  */

   /* create empty int lists */
   init_int_list(&ilist, 0);
   init_int_list(&llist, 0);

   /* use strtok to process comma-separated list */
   workstr = nifti_strdup(str);
   if( !workstr ) { ERROR_message("TGLI: failed to cp <> string"); RETURN(1); }
   next = strtok(workstr, ",");
   err = 0;
   for( tind=0;  next && ! err;  next=strtok(NULL, ", "), tind++ ) {
      if( *next == '>' ) break;

      if ( isdigit(*next) ) { /* then decode integer */
         ival = strtol( next , &cpt , 10 ) ;
         if( ival == 0 && cpt == next ){
            err = 1;
            break;
         }

         if( add_to_int_list(&ilist, ival, 16) <= 0 ) {
            ERROR_message(
               "<> select: failed to extend int_list to len %d for %s\n",
               ilist.nall, next);
            err = 1;
            break;
         }
      } else if ( isalpha(*next) ) {
         if ( thd_LT_label_to_int_list(dset, &llist, next) ) {
            ERROR_message( "<> select: invalid label %s\n", next);
            err = 1;
            break;
         }
         extend_int_list(&ilist, &llist);
      } else {
         ERROR_message("<> select: unexpected char at posn %d in '%s'",
                       (int)(next-workstr), str);
         err = 1;
      }
   }  /* end of loop through selector string */

   free(workstr);
   free_int_list(&llist);

   if( err ) {
      ERROR_message("failed to process <> selector, '%s'", str);
      free_int_list(&ilist);
      RETURN(1);
   }

   /* success!  steal integer list and flee */
   *list = ilist.list;
   *nvals = ilist.num;

if(ilist.num > 0) {
int c;
fprintf(stderr,"== have csv list:\n");
for(c = 0; c < ilist.num; c++)
  fprintf(stderr,"   %d\n", ilist.list[c]);
}

   RETURN(0);
}

