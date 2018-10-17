#include "niml_private.h"

/****************************************************************************/
/************* Debugging function *******************************************/
/****************************************************************************/

FILE *dfp = NULL ;                  /* debug file pointer */

void NI_dpr( char *fmt , ... )      /* print debug stuff */
{
  va_list vararg_ptr ;
  char *nms ;
  if( dfp == NULL ) return ;        /* printing turned off? */
  va_start( vararg_ptr , fmt ) ;
  vfprintf(dfp,fmt,vararg_ptr) ;    /* do printing */

  nms = NI_malloc_status() ;        /* 18 Nov 2002 */
  if( nms != NULL ) fprintf(dfp,"     NI_malloc status: %s\n",nms) ;

  fflush(dfp); va_end(vararg_ptr);  /* cleanup */
}

/****************************************************************************/
/*********************** NIML Utility functions *****************************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Return the file length (-1 if file not found).
----------------------------------------------------------------------------*/

long NI_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

/*-------------------------------------------------------------------*/
/*!  Sleep a given # of milliseconds (uses the Unix select routine).
---------------------------------------------------------------------*/

void NI_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;             /* can't wait into the past */
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*---------------------------------------------------------------*/
/*! Return time elapsed since first call to this routine (msec).

    Note this will overflow an int after 24+ days.  You probably
    don't want to use this if the program will be running
    continuously for such a long time.
-----------------------------------------------------------------*/

int NI_clock_time(void)
{
   struct timeval  new_tval ;
   struct timezone tzone ;
   static struct timeval old_tval ;
   static int first = 1 ;

   gettimeofday( &new_tval , &tzone ) ;

   if( first ){
      old_tval = new_tval ;
      first    = 0 ;
      return 0 ;
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){
      new_tval.tv_usec += 1000000 ;
      new_tval.tv_sec -- ;
   }

   return (int)( (new_tval.tv_sec  - old_tval.tv_sec )*1000.0
                +(new_tval.tv_usec - old_tval.tv_usec)*0.001 + 0.5 ) ;
}

/*---------------------------------------------------------------------------*/
/*! Replacement for mktemp(). */

char * NI_mktemp( char *templ )
{
   int nt ; char *xx,*uu ; struct stat buf ;

   if( templ == NULL || templ[0] == '\0' ) return NULL ;

   nt = strlen(templ) ;
   if( nt < 6 ){ templ[0] = '\0'; return NULL; }
   xx = templ+(nt-6) ;
   if( strcmp(xx,"XXXXXX") != 0 ){ templ[0] = '\0'; return NULL; }

   while(1){
     uu = UUID_idcode() ;
     memcpy( xx , uu , 6 ) ;
     nt = stat( templ , &buf ) ;
     if( nt != 0 ) return templ ;
   }
}

/*************************************************************************/
/****************** NIML string utilities ********************************/
/*************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Like strncpy, but better (result always ends in NUL char).

    If dest is NULL, does nothing.  If src is NULL, put a NUL char
    in dest[0].
----------------------------------------------------------------------------*/

char * NI_strncpy( char *dest , const char *src , size_t n )
{
   if( dest == NULL || n == 0 ) return NULL ;
   if( src  == NULL || n == 1 ){ dest[0] = '\0' ; return dest ; }
   strncpy( dest , src , n-1 ) ;
   dest[n-1] = '\0' ; return dest ;
}

/*------------------------------------------------------------------------*/
/*! Like strlen, but better (input=NULL ==> output=0).
--------------------------------------------------------------------------*/

int NI_strlen( char *str )
{
   if( str == NULL ) return 0 ;
   return strlen(str) ;
}

/*------------------------------------------------------------------------*/
/*! Like strdup, but better (input=NULL ==> output=NULL).
--------------------------------------------------------------------------*/

char * NI_strdup( char *str )
{
   int nn ; char *dup ;
   if( str == NULL ) return NULL ;
   nn = NI_strlen(str); dup = NI_malloc(char, nn+1); strcpy(dup,str); return dup;
}

/*------------------------------------------------------------------------*/
/*! Like NI_strdup, but relies on length, not nul      21 Jun 2006 [rickr]
--------------------------------------------------------------------------*/

char * NI_strdup_len( char *str, int len )
{
   char *dup ;
   if( str == NULL || len < 0 ) return NULL ;
   dup = NI_malloc(char, len+1) ; strncpy(dup,str,len) ; dup[len] = '\0' ;
   return dup;
}

/*------------------------------------------------------------------------*/
/*! Find a string in an array of strings; return index (-1 if not found).
--------------------------------------------------------------------------*/

int string_index( char *targ, int nstr, char *str[] )
{
   int ii ;

   if( nstr < 1 || str == NULL || targ == NULL ) return -1 ;

   for( ii=0 ; ii < nstr ; ii++ )
     if( str[ii] != NULL && strcmp(str[ii],targ) == 0 ) return ii ;

   return -1 ;
}

/*------------------------------------------------------------------------*/
/*! Un-escape a C string inplace.  (This can be done since the replacement
    is always smaller than the input.)  Escapes recognized are:
      -  &lt;   ->  <
      -  &gt;   ->  >
      -  &quot; ->  "
      -  &apos; ->  '
      -  &amp;  ->  &
    Also replace CR LF pair (Microsoft), or CR alone (Macintosh) with
    LF (Unix), per the XML standard.
    Return value is number of replacements made.
--------------------------------------------------------------------------*/

#undef  CR
#undef  LF
#define CR 0x0D
#define LF 0x0A

int unescape_inplace( char *str )
{
   int ii,jj , nn,ll ;

   if( str == NULL ) return 0 ;                /* no string? */
   ll = NI_strlen(str) ;

   /* scan for escapes: &something; */

   for( ii=jj=nn=0 ; ii<ll ; ii++,jj++ ){ /* scan at ii; put results in at jj */

      if( str[ii] == '&' ){  /* start of escape? */

              if( ii+3 < ll        &&   /* &lt; */
                  str[ii+1] == 'l' &&
                  str[ii+2] == 't' &&
                  str[ii+3] == ';'   ){ str[jj] = '<' ; ii += 3 ; nn++ ; }

         else if( ii+3 < ll        &&   /* &gt; */
                  str[ii+1] == 'g' &&
                  str[ii+2] == 't' &&
                  str[ii+3] == ';'   ){ str[jj] = '>' ; ii += 3 ; nn++ ; }

         else if( ii+5 < ll        &&   /* &quot; */
                  str[ii+1] == 'q' &&
                  str[ii+2] == 'u' &&
                  str[ii+3] == 'o' &&
                  str[ii+4] == 't' &&
                  str[ii+5] == ';'   ){ str[jj] = '"' ; ii += 5 ; nn++ ; }

         else if( ii+5 < ll        &&   /* &apos; */
                  str[ii+1] == 'a' &&
                  str[ii+2] == 'p' &&
                  str[ii+3] == 'o' &&
                  str[ii+4] == 's' &&
                  str[ii+5] == ';'   ){ str[jj] = '\'' ; ii += 5 ; nn++ ; }

         else if( ii+4 < ll        &&  /* &amp; */
                  str[ii+1] == 'a' &&
                  str[ii+2] == 'm' &&
                  str[ii+3] == 'p' &&
                  str[ii+4] == ';'   ){ str[jj] = '&' ; ii += 4 ; nn++ ; }

         /* although the comments above don't mention it,
            we also look for XML style numeric escapes
            of the forms &#32; (decimal) and &#xfd; (hex) */

         else if( ii+3 < ll        &&
                  str[ii+1] == '#' &&
                  isdigit(str[ii+2]) ){   /* &#dec; */

            unsigned int val='?' ; int kk=ii+3 ;
            while( kk < ll && str[kk] != ';' ) kk++ ;
            sscanf( str+ii+2 , "%u" , &val ) ;
            str[jj] = (char) val ; ii = kk ; nn++ ;
         }

         else if( ii+4 < ll        &&
                  str[ii+1] == '#' &&
                  str[ii+2] == 'x' &&
                  isxdigit(str[ii+3]) ){   /* &#hex; */

            unsigned int val='?' ; int kk=ii+4 ;
            while( kk < ll && str[kk] != ';' ) kk++ ;
            sscanf( str+ii+3 , "%x" , &val ) ;
            str[jj] = (char) val ; ii = kk ; nn++ ;
         }

         /* didn't start a recognized escape, so just copy as normal */

         else if( jj < ii ){ str[jj] = str[ii] ; }

      } else if( str[ii] == CR ) {  /* is a carriage return */

         if( str[ii+1] == LF ){ str[jj] = LF ; ii++ ; nn++ ; }  /* CR LF */
         else                 { str[jj] = LF ;      ; nn++ ; }  /* CR only */

      } else { /* is a normal character, just copy to output */

              if( jj < ii ){ str[jj] = str[ii] ; }
      }

      /* at this point, ii=index of last character used up in scan
                        jj=index of last character written to (jj <= ii) */

   } /* end of loop scanning over input/output string */

   if( jj < ll ) str[jj] = '\0' ; /* end string properly */

   return nn ;
}

/*------------------------------------------------------------------------*/
/*! Quotize (and escapize) one string, returning a new string.
    Approximately speaking, this is the inverse of unescape_inplace().
--------------------------------------------------------------------------*/

char * quotize_string( char *str )
{
   int ii,jj , lstr,lout ;
   char *out ;

   lstr = NI_strlen(str) ;
   if( lstr == 0 ){ out = NI_malloc(char, 4); strcpy(out,"\"\""); return out; }
   lout = 8 ;                      /* length of output */
   for( ii=0 ; ii < lstr ; ii++ ){ /* count characters for output */
      switch( str[ii] ){
         case '&':  lout += 5 ; break ;  /* replace '&' with "&amp;" */

         case '<':
         case '>':  lout += 4 ; break ;  /* replace '<' with "&lt;" */

         case '"' :
         case '\'': lout += 6 ; break ;  /* replace '"' with "&quot;" */

         case CR:
         case LF:   lout += 6 ; break ;  /* replace CR with "&#x0d;"
                                                    LF with "&#x0a;" */

         default: lout++ ; break ;      /* copy all other chars */
      }
   }
   out = NI_malloc(char, lout) ;              /* allocate output string */
   out[0] = '"' ;                       /* opening quote mark */
   for( ii=0,jj=1 ; ii < lstr ; ii++ ){
      switch( str[ii] ){
         default: out[jj++] = str[ii] ; break ;  /* normal characters */

         case '&':  memcpy(out+jj,"&amp;",5)  ; jj+=5 ; break ;

         case '<':  memcpy(out+jj,"&lt;",4)   ; jj+=4 ; break ;
         case '>':  memcpy(out+jj,"&gt;",4)   ; jj+=4 ; break ;

         case '"' : memcpy(out+jj,"&quot;",6) ; jj+=6 ; break ;

         case '\'': memcpy(out+jj,"&apos;",6) ; jj+=6 ; break ;

         case CR:   memcpy(out+jj,"&#x0d;",6) ; jj+=6 ; break ;  /* 15 Oct 2002 */
         case LF:   memcpy(out+jj,"&#x0a;",6) ; jj+=6 ; break ;
      }
   }
   out[jj++] = '"'  ;  /* closing quote mark */
   out[jj]   = '\0' ;  /* terminate the string */
   return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize an array of strings into one string,
    separating substrings with sep (setting sep=0 means use commas).
--------------------------------------------------------------------------*/

char * quotize_string_vector( int num , char **str , char sep )
{
   char *out , **qstr ;
   int ii , ntot , ll ;

   /* handle special cases */

   if( num <= 0 || str == NULL )
      return quotize_string(NULL) ;      /* will be string of 2 quotes */

   if( num == 1 )
      return quotize_string( str[0] ) ;  /* just quotize the only string */

   /* default separator */

   if( sep == '\0' ) sep = ',' ;

   /* temp array for quotized individual sub-strings */

   qstr = NI_malloc(char*, sizeof(char *)*num) ;

   for( ntot=ii=0 ; ii < num ; ii++ ){       /* quotize each input string */
      qstr[ii] = quotize_string( str[ii] ) ;
      ntot += NI_strlen( qstr[ii] ) ;      /* length of all quotized strings */
   }

   /* make output, put 1st sub-string into it */

   out = NI_malloc(char, ntot) ;
   strcpy( out , qstr[0] ) ; NI_free(qstr[0]) ;
   for( ii=1 ; ii < num ; ii++ ){
      ll = strlen(out) ;  /* put separator at end of output string, */
      out[ll-1] = sep ;   /* in place of the closing " mark.       */

      strcat(out,qstr[ii]+1) ;  /* catenate with next sub-string, */
                                /* but skip the opening " mark.  */
      NI_free(qstr[ii]) ;       /* toss the quotized trash */
   }

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize a bunch of ints int a string like "1,32,-12".
--------------------------------------------------------------------------*/

char * quotize_int_vector( int num , int *vec , char sep )
{
   int ii , jj ;
   char *out , **qstr ;

   if( num <= 0 || vec == NULL )
      return quotize_string(NULL) ;

   qstr = NI_malloc(char*, sizeof(char *)*num) ;  /* temp array of strings */
   for( ii=0 ; ii < num ; ii++ ){
      qstr[ii] = NI_malloc(char, 16) ;           /* max size of printed int */
      sprintf(qstr[ii],"%d",vec[ii]) ;               /* print int */
      for( jj=strlen(qstr[ii])-1 ;                   /* clip */
           jj > 0 && isspace(qstr[ii][jj]) ; jj-- )  /* trailing */
        qstr[ii][jj] = '\0' ;                        /* blanks */
   }

   out = quotize_string_vector( num , qstr , sep ) ;

   for( ii=0 ; ii < num ; ii++ ) NI_free(qstr[ii]) ;

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize a bunch of floats into a string like "-2.71828,3.1416,1.111".
--------------------------------------------------------------------------*/

char * quotize_float_vector( int num , float *vec , char sep )
{
   int ii , ff ;
   char *out , **qstr , fbuf[32] ;

   if( num <= 0 || vec == NULL )
      return quotize_string(NULL) ;

   qstr = NI_malloc(char*, sizeof(char *)*num) ;
   for( ii=0 ; ii < num ; ii++ ){
      sprintf(fbuf," %14.7g",vec[ii]) ;
      for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) /* skip trailing blanks */
        fbuf[ff] = '\0' ;
      for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;         /* skip leading blanks */
      qstr[ii] = NI_strdup(fbuf+ff) ;              /* array of temp strings */
   }

   out = quotize_string_vector( num , qstr , sep ) ;

   for( ii=0 ; ii < num ; ii++ ) NI_free(qstr[ii]) ;

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Check a string for 'nameness' - that is, consists only of legal
    characters for a NIML 'Name' and also starts with an alphabetic
    character.  Returns 1 if it is a Name and 0 if is not.
--------------------------------------------------------------------------*/

int NI_is_name( char *str )
{
   int ii ;

   if( str == NULL || str[0] == '\0' || !isalpha(str[0]) ) return 0 ;

   for( ii=1 ; str[ii] != '\0' ; ii++ )
     if( !IS_NAME_CHAR(str[ii]) ) return 0 ; /* this one is bad */

   return 1 ;                      /* all were good ==> success */
}

/*------------------------------------------------------------------------*/
/*! Find a trailing name in a pathname.

   For example, for fname = "/bob/cox/is/the/author/of/NIML",
     - the lev=0 trailing name is "NIML",
     - the lev=1 trailing name is "of/NIML",
     - the lev=2 trailing name is "author/of/NIML", and so on.
   That is, "lev" is the number of directory names above the last name
   to keep.  The pointer returned is to some place in the middle of fname;
   that is, this is not a malloc()-ed string, so don't try to free() it!.
--------------------------------------------------------------------------*/

char * trailname( char *fname , int lev )
{
   int fpos , flen , flev ;

   if( fname == NULL || (flen=strlen(fname)) <= 1 ) return fname ;

   if( lev < 0 ) lev = 0 ;

   flev = 0 ;
   fpos = flen ;
   if( fname[fpos-1] == '/' ) fpos-- ;  /* skip trailing slash */

   /* fpos   = index of latest character I've accepted,
      fpos-1 = index of next character to examine,
      flev   = number of directory levels found so far */

   while( fpos > 0 ){

      if( fname[fpos-1] == '/' ){
         flev++ ; if( flev >  lev ) break ;  /* reached the lev we like */
      }
      fpos-- ;  /* scan backwards */
   }

   return (fname+fpos) ;
}

/*--------------------------------------------------------------------------*/
/*! Given an array of strings, determine how many are numbers [11 Sep 2018] */
/*  (After conversion to floats, thd_floatscan() can be used to for fixups) */
/*--------------------------------------------------------------------------*/

#undef  FBAD
#define FBAD(sss)                       \
 ( strcasecmp ((sss),"N/A"  ) == 0 ||   \
   strncasecmp((sss),"NAN",3) == 0 ||   \
   strncasecmp((sss),"INF",3) == 0   )

int NI_count_numbers( int nstr , char **str )
{
   int nnum=0 , ii ; double val ; char *cpt ;

   if( nstr < 1 || str == NULL ) return nnum ;

   for( ii=0 ; ii < nstr ; ii++ ){
     if( str[ii] != NULL ){
       if( FBAD(str[ii]) ) nnum++ ;  /* 17 Oct 2018 */
       else {
         val = strtod(str[ii],&cpt) ;
         if( *cpt == '\0' || isspace(*cpt) ) nnum++ ;
       }
     }
   }
   return nnum ;
}
