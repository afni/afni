#include "niml.h"

/****************************************************************************/
/*********************** Utility functions **********************************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Allocate memory (actually uses calloc); calls exit() if it fails. */

void * NI_malloc( size_t len )
{
   void *p = calloc(1,len) ;
   if( p == NULL ){
      fprintf(stderr,"NI_malloc() fails. Aauugghh!\n") ; exit(1) ;
   }
   return p ;
}

/*--------------------------------------------------------------------------*/
/*! Free memory; NULL pointer is just ignored. */

void NI_free( void *p )
{
   if( p != NULL ) free(p) ;
}

/*! Free and set pointer to NULL. */

#define NI_FREE(p) ( NI_free(p), (p)=NULL )

/*--------------------------------------------------------------------------*/
/*! Reallocate memory; calls exit() if it fails. */

void * NI_realloc( void *p , size_t len )
{
   void *q = realloc( p , len ) ;
   if( q == NULL && len > 0 ){
      fprintf(stderr,"NI_realloc() fails. Ooooogg!\n"); exit(1);
   }
   return q ;
}

/*--------------------------------------------------------------------------*/
/*! Like strncpy, but better (result always ends in NUL character). */

char * NI_strncpy( char *dest , const char *src , size_t n )
{
   if( dest == NULL || n == 0 ) return NULL ;
   if( src  == NULL || n == 1 ){ dest[0] = '\0' ; return dest ; }
   strncpy( dest , src , n-1 ) ;
   dest[n-1] = '\0' ; return dest ;
}

/*------------------------------------------------------------------------*/
/*! Like strlen, but better (input=NULL ==> output=0). */

int NI_strlen( char *str )
{
   if( str == NULL ) return 0 ;
   return strlen(str) ;
}

/*------------------------------------------------------------------------*/
/*! Like strdup, but better (input=NULL ==> output=NULL). */

char * NI_strdup( char *str )
{
   int nn ; char *dup ;
   if( str == NULL ) return NULL ;
   nn = NI_strlen(str); dup = NI_malloc(nn+1); strcpy(dup,str); return dup ;
}

/*------------------------------------------------------------------------*/
/*! Find a string in an array of strings; return index (-1 if not found). */

static int string_index( char *targ , int nstr , char *str[] )
{
   int ii ;

   if( nstr < 1 || str == NULL || targ == NULL ) return -1 ;

   for( ii=0 ; ii < nstr ; ii++ )
      if( str[ii] != NULL && strcmp(str[ii],targ) == 0 ) return ii ;

   return -1 ;
}

/*-------------------------------------------------------------------*/
/*!  Sleep a given # of milliseconds (uses the Unix select routine). */

void NI_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;             /* can't wait into the past */
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Return the file length (-1 if file not found). */

long NI_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
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
      return 0.0 ;
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){
      new_tval.tv_usec += 1000000 ;
      new_tval.tv_sec -- ;
   }

   return (int)( (new_tval.tv_sec  - old_tval.tv_sec )*1000.0
                +(new_tval.tv_usec - old_tval.tv_usec)*0.001 + 0.5 ) ;
}

/*------------------------------------------------------------------------*/
/*! Un-escape a C string inplace.  (This can be done since the replacement
    is always smaller than the input.)  Escapes recognized are:
      *  &lt;   ->  <
      *  &gt;   ->  >
      *  &quot; ->  "
      *  &apos; ->  '
      *  &amp;  ->  &
    Also replace CR LF pair (Microsoft), or CR alone (Macintosh) with
    LF (Unix), per the XML standard.
    Return value is number of replacements made.
--------------------------------------------------------------------------*/

#undef  CR
#undef  LF
#define CR 0x0D
#define LF 0x0A

static int unescape_inplace( char *str )
{
   int ii,jj , nn,ll ;

   if( str == NULL ) return 0 ;                /* no string? */
   ll = NI_strlen(str) ;  if( ll < 4 ) return 0 ; /* too short */

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

         else if( ii+3 < ll        &&
                  str[ii+1] == '#' &&
                  isdigit(str[ii+2]) ){   /* &#dec; */

            unsigned int val='?' ; int kk=ii+3 ;
            while( kk < ll && kk != ';' ) kk++ ;
            sscanf( str+ii+2 , "%u" , &val ) ;
            str[jj] = (char) val ; ii = kk ; nn++ ;
         }

         else if( ii+4 < ll        &&
                  str[ii+1] == '#' &&
                  str[ii+2] == 'x' &&
                  isxdigit(str[ii+3]) ){   /* &#hex; */

            unsigned int val='?' ; int kk=ii+4 ;
            while( kk < ll && kk != ';' ) kk++ ;
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
   }

   if( jj < ll ) str[jj] = '\0' ; /* end string properly */

   return nn ;
}

/*************************************************************************/
/************************ Byte ordering functions ************************/
/*************************************************************************/

/*---------------------------------------------------------------*/
/*! Find the byte order on this system. */

int NI_byteorder(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = 0 ;

   return (fred.ss == 1) ? NI_LSB_FIRST : NI_MSB_FIRST ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 2 bytes (shorts). */

typedef struct { unsigned char a,b ; } twobytes ;

void NI_swap2( int n , void *ar )
{
   register int ii ;
   register twobytes *tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].b ; tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 4 bytes (ints or floats) */

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

void NI_swap4( int n , void *ar )
{
   register int ii ;
   register fourbytes *tb = (fourbytes *) ar ;
   register unsigned char tt , uu ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].d ; tb[ii].d = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].c ; tb[ii].c = uu ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 8 bytes (doubles or 64 bit ints) */

typedef struct { unsigned char a,b,c,d , e,f,g,h ; } eightbytes ;

void NI_swap8( int n , void *ar )
{
   register int ii ;
   register eightbytes *tb = (eightbytes *) ar ;
   register unsigned char tt , uu , vv , ww ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].h ; tb[ii].h = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].g ; tb[ii].g = uu ;
      vv = tb[ii].c ; tb[ii].c = tb[ii].f ; tb[ii].f = vv ;
      ww = tb[ii].d ; tb[ii].d = tb[ii].e ; tb[ii].e = ww ;
   }
   return ;
}

/****************************************************************************/
/****************** Functions to process a NIML header **********************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Deallocate a header_stuff struct. */

static void destroy_header_stuff( header_stuff *hs )
{
   int ii ;
   if( hs == NULL ) return ;
   NI_free(hs->name) ;
   for( ii=0 ; ii < hs->nattr ; ii++ ){
      if( hs->lhs != NULL ) NI_free( hs->lhs[ii] ) ;
      if( hs->rhs != NULL ) NI_free( hs->rhs[ii] ) ;
   }
   NI_free( hs ) ;
}

/*-------------------------------------------------------------------------*/
/*! Characters allowed inside unquoted strings. */

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '=' && (c) != '<'    )

/*! Defines what we consider a quoting character. */

#define IS_QUOTE_CHAR(c)  ( (c) == '"' || (c) == '\'' )

/*-------------------------------------------------------------------------*/
/*! Find an isolated string in the input array of char.

    nst = start position
    nch = total number of data bytes
    ch  = array of data bytes

    Return value is an intpair with the .i component indicating the
    start position of the string in the data and the .j indicating
    the byte AFTER the end of the string.  If the .i component is
    negative, then no string was found.
---------------------------------------------------------------------------*/

static intpair find_string( int nst, int nch, char *ch )
{
   intpair ans = {-1,-1} ;  /* default answer ==> nothing found */
   int ii,jj ;
   char quot ;

#if 0
fprintf(stderr,"  find_string: nst=%d nch=%d\n",nst,nch) ;
#endif

   if( nst >= nch || nch < 2 || ch == NULL ) return ans;        /* bad input */

   for( ii=nst; ii<nch && !IS_STRING_CHAR(ch[ii]); ii++ ) ; /* skip to start */

   if( ii >= nch ) return ans ;                                 /* bad input */

   if( IS_QUOTE_CHAR(ch[ii]) ){                             /* quoted string */
      if( ii == nch-1 ) return ans ;                            /* bad input */
      quot = ch[ii] ; ii++ ;
      for( jj=ii ; jj<nch && ch[jj] != quot ; jj++ ) ;      /* skip to close */
   } else {
      for( jj=ii+1 ; jj<nch && IS_STRING_CHAR(ch[jj]) ; jj++ ) ; /* to blank */
   }

   ans.i = ii ; ans.j = jj ; /* answer starts at ch[ii] and goes to ch[jj-1] */
   return ans ;
}

/*--------------------------------------------------------------------------*/
/*! Parse into strings a <header and=its attributes="stuff">.

    ndat  = number of data bytes
    dat   = data bytes

   *nused = number of bytes consumed (=index of byte after the closing '>').
    Return value is a pointer to a header_stuff struct;
    if NULL is returned, something real bad happened (and *nused won't
    be assigned).
----------------------------------------------------------------------------*/

static header_stuff * parse_header_stuff( int ndat, char *dat, int *nused )
{
   header_stuff *hs ; /* return value */
   int id,jd , nn ;
   intpair ss ;

   if( ndat < 2 || dat == NULL ) return NULL ;        /* bad input */

#if 0
fprintf(stderr,"Enter parse_header_stuff: %.*s\n",ndat,dat) ;
#endif

   for( id=0 ; id < ndat && dat[id] != '<' ; id++ ) ; /* skip to opening */

   if( id >= ndat-1 ) return NULL ;                   /* bad input */

   hs = NI_malloc(sizeof(header_stuff)) ;             /* make output */
   hs->nattr = hs->empty = 0 ;
   hs->name  = NULL ;
   hs->lhs   = hs->rhs = NULL ;

   /* find and assign name string */

   ss = find_string( id+1 , ndat , dat ) ;

   if( ss.i < 0 || ss.j <= ss.i ){
      destroy_header_stuff( hs ) ; return NULL ;   /* no name string */
   }

   nn = ss.j - ss.i ;                               /* string length */
   hs->name = NI_malloc(nn+1) ;
   NI_strncpy( hs->name , dat+ss.i , nn+1 ) ;

#if 0
fprintf(stderr,"  name = %s\n",hs->name) ;
#endif

   /* start scanning for next string at location id */

   id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   /* find and assign attribute strings */

   while(1){

#if 0
fprintf(stderr,"  scan start at id=%d\n",id) ;
#endif

      for( ; id < ndat && isspace(dat[id]) ; id++ ) ; /* skip blanks */

      if( id >= ndat ) break ;                 /* end of input found */

      if( dat[id] == '>' ) break ;                  /* ">" end found */

      if( dat[id] == '/' ){                        /* "/>" end found */
         if( id < ndat-1 ) id++ ;                  /* skip the '>'   */
         hs->empty = 1 ;                   /* mark header as 'empty' */
         break ;                /* done with scanning for attributes */
      }

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

#if 0
fprintf(stderr,"  next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

      /* extend size of attribute arrays */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      /* this is the LHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->lhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->lhs[hs->nattr] , dat+ss.i , nn+1 ) ;
      unescape_inplace( hs->lhs[hs->nattr] ) ;

      hs->rhs[hs->nattr] = NULL ;             /* in case there is no RHS */

      id = ss.j ;
      if( id >= ndat ) break ;    /* end of input ? */
      if( IS_QUOTE_CHAR(dat[id]) ) id++ ;  /* skip closing quote */
      if( id >= ndat ) break ;    /* end of input ? */

      if( dat[id] != '=' ){                   /* no '=' means no RHS */
         (hs->nattr)++ ;
         continue ;                           /* so get next attribute */
      }

      id++ ; if( id >= ndat ) break ;         /* skip the '=' */

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

#if 0
fprintf(stderr,"  next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

      /* this is the RHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->rhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->rhs[hs->nattr] , dat+ss.i , nn+1 ) ;
      unescape_inplace( hs->rhs[hs->nattr] ) ;

      (hs->nattr)++ ;                  /* increment attribute count */

      /* start scanning for next string at location id */

      id = ss.j ;
      if( IS_QUOTE_CHAR(dat[id]) ) id++ ;  /* skip closing quote */

   } /* end of loop over input */

   if( nused != NULL ){
      if( id >= ndat ) id = ndat-1 ;
      *nused = id+1 ;              /* number of bytes used from dat */
   }

   return hs ;                         /* the goal of all that work */
}

/*--------------------------------------------------------------------*/
/*! Decode a single type field.  Return value is an intpair with
    the .i component being the type code and the .j component being
    the number of characters consumed.  If the .i component is -1,
    then no legal type was found (and .j will be 1).
----------------------------------------------------------------------*/

static intpair decode_type_field( char *tf )
{
   intpair ans = {-1,1} ;  /* default answer */

   /* check input for goodness */

   if( tf == NULL ) return ans ;  /* should never happen */

   /* check if tf[0] starts a full datum name,
      or if it is just an initial              */

   switch( tf[0] ){

      default: break ;  /* not a legal datum character */

      case 'b':
        ans.i = NI_BYTE ;
        if( strncmp(tf,"byte"   ,4) == 0 ) ans.j = 4 ;
      break ;

      case 's':
        ans.i = NI_SHORT ;
        if( strncmp(tf,"short"  ,5) == 0 ) ans.j = 5 ;
      break ;

      case 'i':
        ans.i = NI_INT ;
        if( strncmp(tf,"int"    ,3) == 0 ) ans.j = 3 ;
      break ;

      case 'f':
        ans.i = NI_FLOAT ;
        if( strncmp(tf,"float"  ,5) == 0 ) ans.j = 5 ;
      break ;

      case 'c':
        ans.i = NI_COMPLEX ;
        if( strncmp(tf,"complex",7) == 0 ) ans.j = 7 ;
      break ;

      case 'd':
        ans.i = NI_DOUBLE ;
        if( strncmp(tf,"double" ,6) == 0 ) ans.j = 6 ;
      break ;

      case 'r':
        ans.i = NI_RGB ;
        if( strncmp(tf,"rgb"    ,3) == 0 ) ans.j = 3 ;
      break ;

      case 'R':
        ans.i = NI_RGBA ;
        if( strncmp(tf,"RGBA"   ,4) == 0 ) ans.j = 4 ;
      break ;

      case 'S':
        ans.i = NI_STRING ;
        if( strncmp(tf,"STRING" ,6) == 0 ) ans.j = 6 ;
      break ;

      case 'L':
        ans.i = NI_LINE ;
        if( strncmp(tf,"LINE"   ,4) == 0 ) ans.j = 4 ;
      break ;
   }

   return ans ;
}

/*--------------------------------------------------------------------*/
/*! Decode a single string into a bunch of strings, separated
    by characters from the list in sep.
    Passing sep in as NULL means to use "," as the separator.
    In each sub-string, leading and trailing blanks will be excised.
    This can result in 0 length strings (e.g., "1,,2," will result
    in the second and fourth output strings having 0 length).
----------------------------------------------------------------------*/

static str_array * decode_string_list( char *ss , char *sep )
{
   str_array *sar ;
   int num , nn,id,jd , lss ;

   if( ss == NULL || ss[0] == '\0' ) return NULL ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */

   sar = NI_malloc(sizeof(str_array)) ;  /* create output */
   sar->num = 0 ; sar->str = NULL ;

   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id <= lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++ ;

      /* sub-string to save runs from ss[jd] to ss[id-1] */

      sar->str = NI_realloc( sar->str , sizeof(char)*(num+1) ) ;

      nn = id-jd ;                                   /* length of sub-string */
      while( nn > 0 && isspace(ss[jd+nn-1]) ) nn-- ; /* clip trailing blanks */
      sar->str[num] = NI_malloc(nn+1) ;              /* make output string  */
      if( nn > 0 ) memcpy(sar->str[num],ss+jd,nn) ;  /* copy sub-string    */
      sar->str[num++][nn] = '\0' ;                   /* terminate output  */

      id++ ;                                         /* skip separator  */
   }

   sar->num = num ; return sar ;
}

/*--------------------------------------------------------------------*/
/*! Decode a ni_dimen string into an array of integers.

  Returns NULL if the input is bad bad bad.
----------------------------------------------------------------------*/

static int_array * decode_dimen_string( char *ds )
{
   int num , dd,nn,id,jd , lds ;
   int_array *iar ;

   if( ds == NULL || ds[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(int_array)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan string for integers */

   num = id = 0 ;
   lds = NI_strlen(ds) ;
   do{
      /* skip ahead until ds[id] is a digit */

      while( id < lds && !isdigit(ds[id]) ) id++ ;
      if( id == lds ) break ;                      /* end of input */

      /* decode integer starting here */

      nn = jd = 0 ;
      sscanf( ds+id , "%d%n" , &jd , &nn ) ;       /* get the count */
      if( jd <= 0 || nn <= 0 ) break ;             /* something bad */
      id += nn ;                                   /* skip these chars */

      /* extend output array, store new dimension in it */

      iar->ar = NI_realloc( iar->ar , sizeof(int)*(num+1) ) ;
      iar->ar[num++] = jd ;
   } while(1) ;

   if( num == 0 ){ NI_free(iar); return NULL; }    /* bad */

   iar->num = num ; return iar ;
}

/*--------------------------------------------------------------------*/
/*! Decode a data type string into an array of integer codes.

  Returns NULL if the input is bad bad bad.
----------------------------------------------------------------------*/

static int_array * decode_type_string( char *ts )
{
   int num, typ, lts, id,jd, nn,kk ;
   int_array *iar ;
   intpair dc ;

   if( ts == NULL || ts[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(int_array)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan type string to find counts/fields and add to output */

   lts = NI_strlen(ts) ;
   num = 0 ;            /* will be count of fields */

   for( id=kk=0 ; id < lts ; ){  /* loop over input string */

      if( isdigit(ts[id]) ){   /* a count prefix */
         jd = nn = 0 ;
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         if( jd <= 0 || nn <= 0 ){                /* shouldn't happen */
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ;
         }
         id += nn ;                  /* skip count prefix characters */
         if( ts[id] == '*' ) id++ ;  /* allow for "3*float" */
      } else {
         jd = 1 ;       /* default count of 1 */
      }

      dc = decode_type_field( ts+id ) ;

      /* dc.i = type code; dc.j = character count used to get type code */

      id += dc.j ;              /* skip these characters */
      if( dc.i < 0 ) continue ; /* bad type code */

      num += jd ;               /* this many fields so far */

      /* extend output array length */

      iar->ar = NI_realloc( iar->ar , sizeof(int)*num ) ;

      /* put values into output array */

      for( nn=0 ; nn < jd ; nn++ ) iar->ar[kk++] = dc.i ;

   } /* end of loop over input string */

   /* nothing found? */

   if( num <= 0 ){
      NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
   }

   iar->num = num ; return iar ;
}

/*-----------------------------------------------------------------------*/
/* Given a type code, return the character code. */

static char NI_type_char( int typ )
{
   switch( typ ){
        case NI_BYTE:     return 'b' ;
        case NI_SHORT:    return 's' ;
        case NI_INT:      return 'i' ;
        case NI_FLOAT:    return 'f' ;
        case NI_DOUBLE:   return 'd' ;
        case NI_COMPLEX:  return 'c' ;
        case NI_RGB:      return 'r' ;
        case NI_RGBA:     return 'R' ;
        case NI_STRING:   return 'S' ;
        case NI_LINE:     return 'L' ;
        default:          return '\0';
   }
}

/*--------------------------------------------------------------------*/

static int     typedef_nib = 0    ; /*!< Block ni_ typedefs? */
static int     typedef_num = 0    ; /*!< Number of typedefs. */
static char ** typedef_nam = NULL ; /*!< Names of typedefs.  */
static char ** typedef_typ = NULL ; /*!< Types of typedefs.  */
static char ** typedef_dim = NULL ; /*!< Dimens of typedefs. */

/*--------------------------------------------------------------------*/
/*! Implement typedef-ing.
      name = name string for new type
      type = type string for new type
      dimen= dimen string for new type (can be NULL)
    The routine will fail (silently) if name or type is NULL, or
    if name is the same as an existing typedef name, or if the
    type string is indecipherable.
----------------------------------------------------------------------*/

void NI_typedef( char *name , char *type , char *dimen )
{
   int nn ;
   int_array *ar ;

   if( name == NULL || name[0] == '\0' ||
       type == NULL || type[0] == '\0'   ) return ; /* bad input */

   /* check if we allow name to start with "ni_" */

   if( typedef_nib && strncmp(name,"ni_",3) == 0 ) return ;

   /* search for name in current list */

   for( nn=0 ; nn < typedef_num ; nn++ )
      if( strcmp(name,typedef_nam[nn]) == 0 ) break ;

   /* found it ==> error exit */

   if( nn < typedef_num ) return ;

   /* check type string for integrity */

   ar = decode_type_string( type ) ;
   if( ar == NULL || ar->num == 0 ){ NI_free(ar); return; } /* bad */

   /* check dimen string (if any) for integrity */

   if( dimen != NULL ){
      ar = decode_dimen_string( dimen ) ;
      if( ar == NULL || ar->num == 0 ){ NI_free(ar); return; } /* bad */
      NI_free(ar->ar) ; NI_free(ar) ;                          /* OK */
   }

   /* add to typedef_ arrays */

   typedef_num++ ;
   typedef_nam = NI_realloc( typedef_nam , sizeof(char *)*typedef_num ) ;
   typedef_typ = NI_realloc( typedef_typ , sizeof(char *)*typedef_num ) ;
   typedef_dim = NI_realloc( typedef_dim , sizeof(char *)*typedef_num ) ;

   typedef_nam[nn] = NI_strdup(name) ;
   typedef_typ[nn] = NI_strdup(type) ;
   typedef_dim[nn] = NI_strdup(dimen);

   return ;
}

/*--------------------------------------------------------------------*/
/*! Add attributes to a header_stuff struct if it has a special name. */

#define NUM_NIB 10
static char *niblist[NUM_NIB] = {
      "ni_f1"    , "ni_f2"    , "ni_f3" , "ni_f4" ,
      "ni_i1"    , "ni_i2"    , "ni_i3" , "ni_i4" ,
      "ni_irgb"  , "ni_irgba"
} ;

static void enhance_header_stuff( header_stuff *hs )
{
   char *ntype , *ndimen ;
   int nn ;

   /* check for goofy inputs */

   if( hs == NULL || hs->name == NULL ) return ;
   if( strcmp(hs->name,"ni_group") == 0 ) return ;

   /* initialize pre-defined types */

   if( typedef_num == 0 ){
      NI_typedef( "ni_f1" , "f"  , NULL ) ;
      NI_typedef( "ni_f2" , "2f" , NULL ) ;
      NI_typedef( "ni_f3" , "3f" , NULL ) ;
      NI_typedef( "ni_f4" , "4f" , NULL ) ;

      NI_typedef( "ni_i1" , "i"  , NULL ) ;
      NI_typedef( "ni_i2" , "2i" , NULL ) ;
      NI_typedef( "ni_i3" , "3i" , NULL ) ;
      NI_typedef( "ni_i4" , "4i" , NULL ) ;

      NI_typedef( "ni_irgb"  , "i,r" , NULL ) ;
      NI_typedef( "ni_irgba" , "i,R" , NULL ) ;

      typedef_nib = 1 ;  /* block further names starting with "ni_" */
   }

   /* Check for special names with predefined types */

   for( nn=0 ; nn < typedef_num ; nn++ )
      if( strcmp(hs->name,typedef_nam[nn]) == 0 ) break ;
   if( nn == typedef_num ) return ;

   ntype  = typedef_typ[nn] ;
   ndimen = typedef_dim[nn] ;

   /* see if we already have a ni_type attribute */

   nn = string_index( "ni_type" , hs->nattr , hs->lhs ) ;

   if( nn >= 0 ){  /* replace existing RHS */

      NI_free( hs->rhs[nn] ) ;
      hs->rhs[nn] = NI_strdup(ntype) ;

   } else {

      /* otherwise, append the ni_type attribute */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      hs->lhs[hs->nattr] = NI_strdup("ni_type") ;
      hs->rhs[hs->nattr] = NI_strdup(ntype) ;
      hs->nattr++ ;
   }

   /* see if we need to add a ni_dimen attribute:
      only if the header doesn't have one already,
      and also if the NI_typedef included a default dimen */

   nn = string_index( "ni_dimen" , hs->nattr , hs->lhs ) ;
   if( nn < 0 && ndimen != NULL ){
      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      hs->lhs[hs->nattr] = NI_strdup("ni_dimen") ;
      hs->rhs[hs->nattr] = NI_strdup(ndimen) ;
      hs->nattr++ ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Construct an empty data element from a header.

    The data vectors will have space allocated, but they will be
    filled with all zero bytes.  If the header was "empty" (ended in
    "/>"), then no vectors will be allocated, and nel->vec_num=0.
-------------------------------------------------------------------------*/

static NI_element * make_empty_data_element( header_stuff *hs )
{
   NI_element *nel ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;

   /* move name and attributes from hs to new element */

   nel->name = hs->name ; hs->name = NULL ;

   nel->attr_num = hs->nattr ;

   if( nel->attr_num > 0 ){
      nel->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      nel->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      nel->attr_lhs = nel->attr_rhs = NULL ;
   }

   /* set default vector parameters [indicating no data] */

   nel->vec_num = 0 ;
   nel->vec_len = 0 ;
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   nel->vec_filled = 0 ;  /* no data has been filled into vectors */

   nel->vec_rank        = 0 ;
   nel->vec_axis_len    = NULL ;
   nel->vec_axis_delta  = NULL ;
   nel->vec_axis_origin = NULL ;
   nel->vec_axis_unit   = NULL ;
   nel->vec_axis_label  = NULL ;

   if( !hs->empty ){  /* find and process ni_* attributes about vectors */

     /* ni_type attribute */

     ii = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;

     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
       int_array *iar = decode_type_string( nel->attr_rhs[ii] ) ;
       if( iar != NULL ){
         nel->vec_num = iar->num ;  /* number of vectors */
         nel->vec_typ = iar->ar ;   /* vector types */
         NI_free(iar) ;             /* just the shell of the struct */
       }
     }

     /* ni_dimen attribute */

     ii = string_index( "ni_dimen" , nel->attr_num , nel->attr_lhs ) ;

     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
        int_array *dar = decode_dimen_string( nel->attr_rhs[ii] ) ;
        if( dar != NULL && dar->num > 0 ){
           int nd=dar->num , qq,pp ;
           /* compute product of all dimensions */
           for( qq=1,pp=0 ; pp < nd ; pp++ ) qq *= dar->ar[pp] ;
           nel->vec_len      = qq ;      /* length of vectors */
           nel->vec_rank     = nd ;      /* number of dimensions */
           nel->vec_axis_len = dar->ar ; /* array of dimension lengths */
#if 0
fprintf(stderr,"ni_dimen: nd=%d qq=%d\n",nd,qq) ;
#endif
        }
     }

     /* if we had ni_dimen, also use ni_delta */

     ii = string_index( "ni_delta" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_delta = NI_malloc(sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_delta+pp ) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_origin */

     ii = string_index( "ni_origin" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_origin = NI_malloc(sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_origin+pp ) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_units */

     ii = string_index( "ni_units" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_unit = NI_malloc(sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_unit[pp] = NI_strdup(sar->str[pp]) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_axes */

     ii = string_index( "ni_axes" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_label = NI_malloc(sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_label[pp] = NI_strdup(sar->str[pp]) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* supply vector parameters if none was given */

     if( nel->vec_len == 0 ){                    /* default dimensions */
        nel->vec_len         = 1 ;
        nel->vec_rank        = 1 ;
        nel->vec_axis_len    = NI_malloc(sizeof(int)) ;
        nel->vec_axis_len[0] = 1 ;
     }

     if( nel->vec_num == 0 ){                    /* default type */
        nel->vec_num    = 1 ;
        nel->vec_typ    = NI_malloc(sizeof(int)) ;
        nel->vec_typ[0] = NI_BYTE ;
     }

     /* now allocate space for vectors defined above */

     nel->vec = NI_malloc( sizeof(void *)*nel->vec_num ) ;

     for( ii=0 ; ii < nel->vec_num ; ii++ )
       nel->vec[ii] = NI_malloc(NI_type_size(nel->vec_typ[ii]) * nel->vec_len) ;
   }

   return nel ;
}

/*-------------------------------------------------------------------------*/
/*! Make an empty group element from parsed header info.

    To be consistent with the NIML spec, the header name should
    be "ni_group", but nothing is done to check for this.  Also, the
    header should not be empty, but this is not checked at all.

    The attributes in the header are assigned to the group, and the group
    parts are initialized to nothing.
---------------------------------------------------------------------------*/

static NI_group * make_empty_group_element( header_stuff *hs )
{
   NI_group *ngr ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   /* move attributes from hs to new element */

   ngr->attr_num = hs->nattr ;

   if( ngr->attr_num > 0 ){
      ngr->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      ngr->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      ngr->attr_lhs = ngr->attr_rhs = NULL ;
   }

   /* have no pieces-parts yet */

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;

   return ngr ;
}

/*-------------------------------------------------------------------------*/
/*! Name for a given integer type code.  Return value is to static string. */

char * NI_type_name( int tval )
{
   static char *NI_names[NI_NUM_TYPES] =
    { "byte"  , "short"  , "int"     ,
      "float" , "double" , "complex" ,
      "rgb"   , "String" , "Line"    ,
      "Rgba"
    } ;

   if( tval < 0 || tval >= NI_NUM_TYPES ) return NULL ;
   return NI_names[tval] ;
}

/*-------------------------------------------------------------------------*/
/*! Byte size of a given integer type code. */

int NI_type_size( int tval )
{
   switch( tval ){
      case NI_BYTE:     return sizeof(byte)    ;
      case NI_SHORT:    return sizeof(short)   ;
      case NI_INT:      return sizeof(int)     ;
      case NI_FLOAT:    return sizeof(float)   ;
      case NI_DOUBLE:   return sizeof(double)  ;
      case NI_COMPLEX:  return sizeof(complex) ;
      case NI_RGB:      return sizeof(rgb)     ;
      case NI_RGBA:     return sizeof(rgba)    ;
      case NI_STRING:   return sizeof(char *)  ;
      case NI_LINE:     return sizeof(char *)  ;
   }
   return 0 ;
}

#if 0
/*----------------------------------------------------------------------*/
/*! Static table to store byte sizes. */

static int typesize[NI_NUM_TYPES] ;

/*! Function to initialize this. */

static void init_typesize(void)
{
   int first=1 , ii ;
   if( first ){
     first = 0 ;
     for( ii=0 ; ii < NI_NUM_TYPES ; ii++ )
       typesize[ii] = NI_type_size(ii) ;
   }
}
#endif

#if 0
/*-------------------------------------------------------------------------*/
/*! Number of component values of a given integer type code. */

static int NI_type_nval( int tval )
{
   switch( tval ){
      case NI_BYTE:     return 1 ;
      case NI_SHORT:    return 1 ;
      case NI_INT:      return 1 ;
      case NI_FLOAT:    return 1 ;
      case NI_DOUBLE:   return 1 ;
      case NI_COMPLEX:  return 2 ;
      case NI_RGB:      return 3 ;
      case NI_RGBA:     return 4 ;
      case NI_STRING:   return 0 ;
      case NI_LINE:     return 0 ;
   }
   return 0 ;
}
#endif

/*----------------------------------------------------------------------*/
/*! Return the size in bytes of one row in a data element. */

int NI_element_rowsize( NI_element *nel )
{
   int ii , nb ;

   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   for( ii=nb=0 ; ii < nel->vec_num ; ii++ )
      nb += NI_type_size( nel->vec_typ[ii] ) ;

   return nb ;
}

/*----------------------------------------------------------------------*/
/*! Return the size of all the rows in a data element. */

int NI_element_allsize( NI_element *nel )
{
   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_len < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   return (nel->vec_len * NI_element_rowsize(nel)) ;
}

/*************************************************************************/
/********** Functions to create NIML data and group elements *************/
/*************************************************************************/

/*-----------------------------------------------------------------------*/
/*! Return the type of something that points to a NI element.

    The input should be a pointer to a NI_element or a NI_group.
    The return value is NI_ELEMENT_TYPE, NI_GROUP_TYPE, or -1.
-------------------------------------------------------------------------*/

int NI_element_type( void *nini )
{
   NI_element *nel = (NI_element *) nini ;
   NI_group   *ngr = (NI_group *)   nini ;

   if( nini == NULL ) return -1 ;

   if( nel->type == NI_ELEMENT_TYPE ) return NI_ELEMENT_TYPE ;
   if( ngr->type == NI_GROUP_TYPE   ) return NI_GROUP_TYPE   ;

   return -1 ;
}

/*-----------------------------------------------------------------------*/
/*! Expunge a data or group element and its contents from the universe.  */

void NI_free_element( void *nini )
{
   int ii , tt=NI_element_type(nini) , jj ;

   if( tt < 0 ) return ; /* bad input */

   /*-- erase contents of data element --*/

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      NI_free(nel->name) ;
      for( ii=0 ; ii < nel->attr_num ; ii++ ){
         NI_free( nel->attr_lhs[ii] ) ;
         NI_free( nel->attr_rhs[ii] ) ;
      }
      NI_free( nel->attr_lhs ) ;
      NI_free( nel->attr_rhs ) ;

      for( ii=0 ; ii < nel->vec_num ; ii++ ){
         if( nel->vec_typ[ii] == NI_STRING || nel->vec_typ[ii] == NI_LINE ){
            char **vpt = (char **) nel->vec[ii] ;
            for( jj=0 ; jj < nel->vec_len ; jj++ ) NI_free(vpt[jj]) ;
         }
         NI_free( nel->vec[ii] ) ;
      }
      NI_free( nel->vec_typ  ) ;
      NI_free( nel->vec ) ;

      NI_free(nel->vec_axis_len) ;
      NI_free(nel->vec_axis_delta) ;
      NI_free(nel->vec_axis_origin) ;
      NI_free(nel->vec_axis_unit) ;
      NI_free(nel->vec_axis_label) ;

      NI_free( nel ) ;

   /*-- erase contents of group element --*/

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){
         NI_free( ngr->attr_lhs[ii] ) ;
         NI_free( ngr->attr_rhs[ii] ) ;
      }
      NI_free( ngr->attr_lhs ) ;
      NI_free( ngr->attr_rhs ) ;

      for( ii=0 ; ii < ngr->part_num ; ii++ )
         NI_free_element( ngr->part[ii] ) ;     /* recursion */

      NI_free( ngr->part_typ ) ;
      NI_free( ngr->part ) ;
      NI_free( ngr ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new data element.

    name   = string name for header.
    veclen = size of vectors (ni_dimen); set this to zero for "empty"
             elements (those with only headers, no data).

    Return is NULL if inputs are stupid.
-------------------------------------------------------------------------*/

NI_element * NI_new_data_element( char *name , int veclen )
{
   NI_element *nel ;

   if( name == NULL || name[0] == '\0' || veclen < 0 ) return NULL ;

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;  /* mark as being a data element */

   nel->name = NI_strdup(name) ;
   nel->attr_num = 0 ;
   nel->attr_lhs = nel->attr_rhs = NULL ;

   nel->vec_num = 0 ;
   nel->vec_len = veclen ;

   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   if( veclen == 0 ){             /* empty element */
     nel->vec_rank     = 0 ;
     nel->vec_axis_len = NULL ;
   } else {                       /* element with data (to come) */
     nel->vec_rank        = 1 ;
     nel->vec_axis_len    = NI_malloc(sizeof(int)) ;
     nel->vec_axis_len[0] = veclen ;
   }

   nel->vec_axis_delta  = NULL ;
   nel->vec_axis_origin = NULL ;
   nel->vec_axis_unit   = NULL ;
   nel->vec_axis_label  = NULL ;

   return nel ;
}

/*-----------------------------------------------------------------------*/
/*! Add a vector (column) of data to a data element.

    nel = data element to modify
    typ = type code of data (e.g., NI_FLOAT)
    arr = pointer to data values - must be an array of length veclen
          from NI_new_data_element()

    The data array is copied into the element.  If the element was
    specified with veclen=0, then this function will do nothing.
    Since this function has no return value, the only way to check for
    such an error is to see if nel->vec_num was incremented.
-------------------------------------------------------------------------*/

void NI_add_column( NI_element *nel , int typ , void *arr )
{
   int nn , ll , ii ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len == 0 || arr == NULL ) return ;

   if( typ < 0 || typ >= NI_NUM_TYPES ) return ;

   if( nel->type != NI_ELEMENT_TYPE ) return ;

   /* get number of vectors currently in element */

   nn = nel->vec_num ;

   /* add 1 to the vec_typ array */

   nel->vec_typ     = NI_realloc( nel->vec_typ , sizeof(int)*(nn+1) ) ;
   nel->vec_typ[nn] = typ ;

   /* add 1 to the vec array, and copy data into it */

   nel->vec     = NI_realloc( nel->vec , sizeof(void *)*(nn+1) ) ;
   ll           = nel->vec_len * NI_type_size(typ) ;
   nel->vec[nn] = NI_malloc( ll ) ;

   /* for String or Line, must do something different */

   if( typ == NI_STRING || typ == NI_LINE ){
      char **vpt = (char **) nel->vec[nn] ;
      char **iar = (char **) arr ;
      for( ii=0 ; ii < nel->vec_len ; ii++ ) /* duplicate strings */
         vpt[ii] = NI_strdup( iar[ii] ) ;
   } else {
     memcpy( nel->vec[nn] , arr , ll ) ;     /* copy numbers in */
   }

   /* add 1 to the count of vectors */

   nel->vec_num = nn+1 ;
   return ;
}

/*------------------------------------------------------------------------*/
/*! Add an attribute to a data or group element. */

void NI_set_attribute( void *nini , char *attname , char *attvalue )
{
   int nn , tt=NI_element_type(nini) ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      for( nn=0 ; nn < nel->attr_num ; nn++ )
         if( strcmp(nel->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == nel->attr_num ){
        nel->attr_lhs = NI_realloc( nel->attr_lhs , sizeof(char *)*(nn+1) ) ;
        nel->attr_rhs = NI_realloc( nel->attr_rhs , sizeof(char *)*(nn+1) ) ;
        nel->attr_num = nn+1 ;
      } else {
        NI_free(nel->attr_lhs[nn]) ;
        NI_free(nel->attr_rhs[nn]) ;
      }

      nel->attr_lhs[nn] = NI_strdup(attname) ;
      nel->attr_rhs[nn] = NI_strdup(attvalue);

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( nn=0 ; nn < ngr->attr_num ; nn++ )
         if( strcmp(ngr->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == ngr->attr_num ){
        ngr->attr_lhs = NI_realloc( ngr->attr_lhs , sizeof(char *)*(nn+1) ) ;
        ngr->attr_rhs = NI_realloc( ngr->attr_rhs , sizeof(char *)*(nn+1) ) ;
        ngr->attr_num = nn+1 ;
      } else {
        NI_free(ngr->attr_lhs[nn]) ;
        NI_free(ngr->attr_rhs[nn]) ;
      }

      ngr->attr_lhs[nn] = NI_strdup(attname) ;
      ngr->attr_rhs[nn] = NI_strdup(attvalue);
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Get an attribute with the given LHS name.  Returns a pointer to the
    RHS field in the element if the attribute name is found; otherwise
    returns NULL.  If the LHS is found, but the RHS is NULL, returns
    a pointer to an empty C string ("\0").  Do not free() the result
    from this function, since it points to the internal field
    of the element!
-------------------------------------------------------------------------*/

char * NI_get_attribute( void *nini , char *attname )
{
   int nn , tt=NI_element_type(nini) ;
   static char *zorkon = "\0" ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return NULL ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      for( nn=0 ; nn < nel->attr_num ; nn++ )
         if( strcmp(nel->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == nel->attr_num ) return NULL ;

      if( nel->attr_rhs[nn] == NULL ) return zorkon ;

      return nel->attr_rhs[nn] ;

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( nn=0 ; nn < ngr->attr_num ; nn++ )
         if( strcmp(ngr->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == ngr->attr_num ) return NULL ;

      if( ngr->attr_rhs[nn] == NULL ) return zorkon ;

      return ngr->attr_rhs[nn] ;
   }

   return NULL ; /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*! Set the dimen attribute for a data element. */

void NI_set_dimen( NI_element *nel , int rank , int *nd )
{
   int ii , ntot ;

   if( nel == NULL || nel->type != NI_ELEMENT_TYPE ||
       rank < 1    || nd == NULL                     ) return ; /* bad */

   for( ntot=1,ii=0 ; ii < rank ; ii++ ){
      if( nd[ii] <= 0 ) return ;                                /* bad */
      ntot *= nd[ii] ;
   }
   if( ntot != nel->vec_len ) return ;                          /* bad */

   nel->vec_rank = rank ;
   nel->vec_axis_len = NI_realloc( nel->vec_axis_len, sizeof(int)*rank ) ;
   memcpy( nel->vec_axis_len , nd , sizeof(int)*rank ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the delta attribute for a data element. */

void NI_set_delta( NI_element *nel , float *del )
{
   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || del == NULL                    ) return ;

   nel->vec_axis_delta = NI_realloc( nel->vec_axis_delta ,
                                     nel->vec_rank * sizeof(float) ) ;
   memcpy( nel->vec_axis_delta , del , nel->vec_rank * sizeof(float) ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the origin attribute for a data element. */

void NI_set_origin( NI_element *nel , float *org )
{
   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || org == NULL                    ) return ;

   nel->vec_axis_origin = NI_realloc( nel->vec_axis_origin ,
                                      nel->vec_rank * sizeof(float) ) ;
   memcpy( nel->vec_axis_origin , org , nel->vec_rank * sizeof(float) ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the units attribute for a data element. */

void NI_set_units( NI_element *nel , char **units )
{
   int ii ;

   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || units == NULL                  ) return ;

   nel->vec_axis_unit = NI_realloc( nel->vec_axis_unit ,
                                    nel->vec_rank * sizeof(char *) ) ;
   for( ii=0 ; ii < nel->vec_rank ; ii++ )
      nel->vec_axis_unit[ii] = NI_strdup( units[ii] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the axes attribute for a data element. */

void NI_set_axes( NI_element *nel , char **ax )
{
   int ii ;

   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || ax == NULL                     ) return ;

   nel->vec_axis_label = NI_realloc( nel->vec_axis_label ,
                                     nel->vec_rank * sizeof(char *) ) ;
   for( ii=0 ; ii < nel->vec_rank ; ii++ )
      nel->vec_axis_label[ii] = NI_strdup( ax[ii] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new group element. */

NI_group * NI_new_group_element(void)
{
   NI_group *ngr ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   ngr->attr_num = 0 ;
   ngr->attr_lhs = ngr->attr_rhs = NULL ;

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;

   return ngr ;
}

/*-----------------------------------------------------------------------*/
/*! Add an element to a group element. */

void NI_add_to_group( NI_group *ngr , void *nini )
{
   int nn , tt=NI_element_type(nini) ;

   if( ngr == NULL || ngr->type != NI_GROUP_TYPE || tt < 0 ) return ;

   nn = ngr->part_num ;

   ngr->part_typ     = NI_realloc( ngr->part_typ , sizeof(int)*(nn+1) ) ;
   ngr->part_typ[nn] = tt ;

   ngr->part         = NI_realloc( ngr->part , sizeof(void *)*(nn+1) ) ;
   ngr->part[nn]     = nini ;

   ngr->part_num     = nn+1 ;

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Fill one row of an element with some data bytes (numeric only). */

static void NI_fill_vector_row( NI_element *nel , int row , char *buf )
{
   int bpos=0 , col ;
   char tmp[16] ;  /* We copy into here from buf, then into the vector. */
                   /* The reason for this is to ensure proper byte     */
                   /* alignment for the assignment into the vector.   */

   /* check inputs for stupidity */

   if( nel->type != NI_ELEMENT_TYPE ||
       row       <  0               ||
       row       >= nel->vec_len    || buf == NULL ) return ;

   /* loop over columns, taking the requisite number of
      bytes from buf and stuffing them into the vectors */

   for( col=0 ; col < nel->vec_num ; col++ ){
     switch( nel->vec_typ[col] ){
       default:                     /* unimplemented types */
       break ;                      /* (STRING and LINE)  */

       case NI_BYTE:{
         byte *vpt = (byte *) nel->vec[col] ;
         byte *bpt = (byte *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(byte)) ;
         vpt[row]  = *bpt ; bpos += sizeof(byte) ;
       }
       break ;

       case NI_SHORT:{
         short *vpt = (short *) nel->vec[col] ;
         short *bpt = (short *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(short)) ;
         vpt[row]  = *bpt ; bpos += sizeof(short) ;
       }
       break ;

       case NI_INT:{
         int *vpt = (int *) nel->vec[col] ;
         int *bpt = (int *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(int)) ;
         vpt[row]  = *bpt ; bpos += sizeof(int) ;
       }
       break ;

       case NI_FLOAT:{
         float *vpt = (float *) nel->vec[col] ;
         float *bpt = (float *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(float)) ;
         vpt[row]  = *bpt ; bpos += sizeof(float) ;
       }
       break ;

       case NI_DOUBLE:{
         double *vpt = (double *) nel->vec[col] ;
         double *bpt = (double *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(double)) ;
         vpt[row]  = *bpt ; bpos += sizeof(double) ;
       }
       break ;

       case NI_COMPLEX:{
         complex *vpt = (complex *) nel->vec[col] ;
         complex *bpt = (complex *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(complex)) ;
         vpt[row]  = *bpt ; bpos += sizeof(complex) ;
       }
       break ;

       case NI_RGB:{
         rgb *vpt = (rgb *) nel->vec[col] ;
         rgb *bpt = (rgb *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgb)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgb) ;
       }
       break ;

       case NI_RGBA:{
         rgba *vpt = (rgba *) nel->vec[col] ;
         rgba *bpt = (rgba *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgba)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgba) ;
       }
       break ;
     }
   }
   return ;
}

/*------------------------------------------------------------------*/
/*! Swap bytes for an array of type code tval. */

void NI_swap_vector( int tval , int nvec , void *vec )
{
   /* check inputs for stupidity */

   if( nvec <= 0 || vec == NULL ) return ;

   switch( tval ){

      default:  break ;   /* nothing to do */

      case NI_SHORT:    NI_swap2( nvec , vec ) ; break ;

      case NI_INT:
      case NI_FLOAT:    NI_swap4( nvec , vec ) ; break ;

      case NI_DOUBLE:   NI_swap8( nvec , vec ) ; break ;

      case NI_COMPLEX:  NI_swap4( 2*nvec, vec) ; break ;
   }
   return ;
}

/*************************************************************************/
/********************* Functions for NIML I/O ****************************/
/*** See http://www.manualy.sk/sock-faq/unix-socket-faq.html for info. ***/
/*************************************************************************/

/*! To print a system error message. */

#define PERROR(x) perror(x)

#include <signal.h>

/*! For tcp - indicates that SIGPIPE is ignored;
    will be set the first time tcp_send is called. */

static int nosigpipe = 0 ;

/*! How to close a socket, given the descriptor ss. */

#define CLOSEDOWN(ss) ( shutdown((ss),2) , close((ss)) )

/*! This is used to set the send/receive buffer size for sockets **/

#define SOCKET_BUFSIZE  NI_BUFSIZE

/*! This macro is used so I can replace recv() with something else if I want. */

#define tcp_recv recv

/*! This macro is used so I can replace send() with something else if I want. */

#define tcp_send send

#ifndef MIN
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

/*! Next delay in milliseconds, given current delay. */

#define NEXTDMS(dm) MIN(1.1*(dm)+1.01,66.0)

/********************************************************************
  Routines to manipulate TCP/IP stream sockets.
*********************************************************************/

/*-------------------------------------------------------------------*/
/*!  See if the given socket (sd) is ready to read.

   msec is the number of milliseconds to wait:
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to read
      1 = socket has data
---------------------------------------------------------------------*/

static int tcp_readcheck( int sd , int msec )
{
   int ii ;
   fd_set rfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&rfds) ; FD_SET(sd, &rfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, &rfds, NULL, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_readcheck(select)" ) ;
   return ii ;
}

/*-------------------------------------------------------------------*/
/*! See if the given socket is ready to write.

    msec = max amount of time to wait, in milliseconds.
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to write
      1 = OK to write to socket
---------------------------------------------------------------------*/

static int tcp_writecheck( int sd , int msec )
{
   int ii ;
   fd_set wfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&wfds) ; FD_SET(sd, &wfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, NULL , &wfds, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_writecheck(select)" ) ;
   return ii ;
}

/*------------------------------------------------------------------------*/
/*! Set a socket so that it will cutoff quickly when it is closed.

   See http://www.manualy.sk/sock-faq/unix-socket-faq.html for more
   information about this stuff.
--------------------------------------------------------------------------*/

static void tcp_set_cutoff( int sd )
{
   if( sd < 0 ) return ;  /* bad input */

#if 1
   /* Turn off "lingering". */

   { struct linger lg ;
     lg.l_onoff  = 1 ;
     lg.l_linger = 0 ;
     setsockopt(sd, SOL_SOCKET, SO_LINGER, (void *)&lg, sizeof(struct linger)) ;
   }
#endif

#if 1
   /* Let the address be reused quickly,
      in case of another connection from the same host on the same port. */

   { int optval = 1;
     setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;
   }
#endif

   return ;
}

/*-------------------------------------------------------------------*/
/*!  Check if an already active socket is still alive.

   If it is dead, then readcheck will say we can read, but we
   won't actually get any bytes when we try (using peek mode).
   Returns 1 if things are OK, 0 if not.
---------------------------------------------------------------------*/

static int tcp_alivecheck( sd )
{
   int ii ;
   char bbb[4] ;

   ii = tcp_readcheck(sd,0) ;                 /* can I read?          */
   if( ii == 0 ) return 1 ;                   /* can't read is OK     */
   if( ii <  0 ) return 0 ;                   /* some error is bad    */
   errno = 0 ;
   ii = tcp_recv( sd , bbb , 1 , MSG_PEEK ) ; /* try to read one byte */
   if( ii == 1 ) return 1 ;                   /* if we get it, good   */
   if( errno ) PERROR("tcp_alivecheck") ;
   return 0 ;                                 /* no data ==> death!   */
}

/*------------------------------------------------------------------------*/
/*!  Open a socket to the given host, to the given TCP port.

     This function is used to "reach out" to a server that is supposed
     to be listening on the same port.
     Returns socket id; if -1, some error occured (e.g., nobody listening).
--------------------------------------------------------------------------*/

static int tcp_connect( char * host , int port )
{
   int sd , l ;
   struct sockaddr_in sin ;
   struct hostent *   hostp ;

   if( host == NULL || port < 1 ) return -1 ;  /* bad inputs */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_connect(socket)"); return -1; }

   /** set socket options (no delays, large buffers) **/

#if 0
   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
#endif

   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family = AF_INET ;
   sin.sin_port   = htons(port) ;

   /** set remote computer IP address from its name **/

   hostp = gethostbyname(host) ;
   if( hostp == NULL ){
      PERROR("tcp_connect(gethostbyname)"); CLOSEDOWN(sd); return -1;
   }
   sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr ;

   errno = 0 ;
   if( connect(sd,(struct sockaddr *)&sin,sizeof(sin)) == -1 ){
      if( errno != ECONNREFUSED ) PERROR("tcp_connect(connect)") ;
      CLOSEDOWN(sd); return -1;
   }

   tcp_set_cutoff( sd ) ;
   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Set up to listen for a connection on a given port.

   This is intended for use by a server, which will wait for some other
   program to actively connect to this port.  There is no security here -
   connections will be taken from any IP address.

   This function does not actually form the connection.  That must be done
   separately.  Whether someone is trying to connect can be checked for
   with the routine "tcp_readcheck" and then accepted with "tcp_accept".

   The return value is the descriptor for the listening socket.
----------------------------------------------------------------------------*/

static int tcp_listen( int port )
{
   int sd , l ;
   struct sockaddr_in sin ;

   if( port < 1 ) return -1 ; /* bad input */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_listen(socket)"); return -1; }

   /** set socket options (no delays, large buffers) **/

#if 0
   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
#endif

   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family      = AF_INET ;
   sin.sin_port        = htons(port) ;
   sin.sin_addr.s_addr = INADDR_ANY ;  /* reader reads from anybody */

   if( bind(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("tcp_listen(bind)"); CLOSEDOWN(sd); return -1;
   }

   if( listen(sd,1) == -1 ){
      PERROR("tcp_listen(listen)"); CLOSEDOWN(sd); return -1;
   }

   tcp_set_cutoff( sd ) ;
   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Accept incoming connection on a socket.

   Return value is the attached socket (which is not the original socket!).
   If -1 is returned, some error occured.  If the accept works, then the
   original socket is still open and listening for further attachments.
   Under many circumstances, you will want to close the original socket
   immediately.

   If hostname is not NULL, then the char * it points to will be filled
   with a pointer to the official name of the host that connected.

   If hostaddr is not NULL, then the char * it points to will be filled
   with a pointer to the Internet address (in 'dot' form) of the host that
   connected.

   Both the char * pointers returned are from malloc(), and should be
   free()-d when no longer needed.  If they aren't needed at all, just
   pass in NULL for these arguments.

   Note that this routine will block until somebody connects.  You can
   use tcp_readcheck(sd,0) to see if anyone is waiting to connect before
   calling this routine.
---------------------------------------------------------------------------*/

static int tcp_accept( int sd , char ** hostname , char ** hostaddr )
{
   struct sockaddr_in pin ;
   int addrlen , sd_new ;
   struct hostent * hostp ;
   char * sout , * str ;

   /** accept the connection **/

   addrlen = sizeof(pin) ;
   sd_new = accept( sd , (struct sockaddr *)&pin , &addrlen ) ;
   if( sd_new == -1 ){ PERROR("tcp_accept"); return -1; }

   /** get name of connector **/

   if( hostname != NULL ){
      hostp = gethostbyaddr( (char *) (&pin.sin_addr) ,
                             sizeof(struct in_addr) , AF_INET ) ;

      if( hostp != NULL ) sout = NI_strdup(hostp->h_name) ;
      else                sout = NI_strdup("UNKNOWN") ;

      *hostname = sout ;
   }

   /** get address of connector **/

   if( hostaddr != NULL ){
      str = inet_ntoa( pin.sin_addr ) ;
      sout = NI_strdup(str) ;
      *hostaddr = sout ;
   }

   tcp_set_cutoff( sd_new ) ;
   return sd_new ;
}

/*******************************************************************/
/*** Functions to read/write from NI_streams (files or sockets). ***/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open a NIML input or output stream, and return a pointer to it.

    NULL is returned if an error occurs.

  name = "tcp:host:port" to connect a socket to system "host"
             on the given port number.

  name = "file:filename" to open a file for I/O.

  name = "str:" to read/write data from a string

  name = "http://hostname/filename" to read data from a Web site
  name = "ftp://hostname/filename"  to read data from an FTP site

  mode = "w" to open a stream for writing
           * tcp: host must be specified ("w" is for a tcp client)
           *file: filename is opened in write mode (and will be
                  overwritten if already exists)
           * str: data will be written to a buffer in the NI_stream
                  struct; you can later access this buffer with the
                  function NI_stream_getbuf().
           * You can't open "http:" or "ftp:" streams for writing.

  mode = "r" to open a stream for reading
           * tcp: host is ignored (but must be present);
                  ("r" is for a tcp server)
           *file: filename is opened in read mode
           * str: characters after the colon are the source of
                  the input data (will be copied to internal buffer);
                  OR, you can later set the internal buffer string
                  later with function NI_stream_setbuf().
           * ftp: The remote files are fetched and loaded into
            http: memory.  After that, these streams operate
                  pretty much the same as str: streams.

  For a file: or str: stream, you can either read from or write to the
  stream, but not both.  For a tcp: stream, once it is connected, you
  can both read and write.

  The inputs "host" (for tcp:) and "filename" (for file:) are
  limited to a maximum of 127 bytes.  For str:, there is no
  limit for the "r" stream (but clearly you can't have any NUL
  bytes in there).

  Since opening a socket requires sychronizing two processes,
  you can't read or write to a tcp: stream immediately.  Instead
  you have to check if it is "good" first.  This can be done using
  the function NI_stream_goodcheck().

  After an tcp: "r" stream is good, then the string ns->name
  contains the IP address of the connecting host, in "dot" form
  (e.g., "201.202.203.204"); here, "ns" is the NI_stream returned
  by this routine.

  For a file: stream, ns->name contains the filename.

  For a str: stream, ns->name contains a stupid string.
------------------------------------------------------------------------*/

NI_stream NI_stream_open( char *name , char *mode )
{
   NI_stream_type *ns ;
   int do_create , do_accept ;

   /** check if inputs are reasonable **/

   if( NI_strlen(name) < 4 ) return NULL ;

   if( mode == NULL ) return NULL ;

   do_create = (*mode == 'w') ;
   do_accept = (*mode == 'r') ;

   if( !do_create && !do_accept ) return NULL ;

   /***** deal with TCP/IP sockets *****/

   if( strncmp(name,"tcp:",4) == 0 ){
      char host[256] , *hend ;
      int  port=-1 , ii , jj ;

      if( NI_strlen(name) > 127 ) return NULL ;

      /** find "host" substring **/

      hend = strstr( name+4 , ":" ) ;
      if( hend == NULL || hend-name > 255 ) return NULL ;

      for( ii=4 ; name[ii] != ':' ; ii++ ) host[ii-4] = name[ii] ;
      host[ii-4] = '\0' ;

      /** get "port" number **/

      port = strtol( name+ii+1 , NULL , 10 ) ;
      if( port <= 0 ) return NULL ;

      /** initialize NI_stream_type output struct **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type = NI_TCP_TYPE;   /* what kind is this? */
      ns->port = port ;         /* save the port #    */
      ns->nbuf = 0 ;            /* buffer is empty    */
      ns->npos = 0 ;            /* scan starts at 0   */

      ns->buf     = NI_malloc(NI_BUFSIZE) ;
      ns->bufsize = NI_BUFSIZE ;
      ns->name[0] = '\0' ;

      ns->bin_thresh = -1 ;     /* write in text mode */

      /** attach to incoming call "r" **/

      if( do_accept ){
         ns->io_mode = NI_INPUT_MODE ;
         ns->sd = tcp_listen( port ) ;                   /* set up to listen  */
         if( ns->sd < 0 ){                               /* error? must die!  */
            NI_free(ns) ; return NULL ;
         }
         ns->bad = TCP_WAIT_ACCEPT ;                     /* not connected yet */
         ii = tcp_readcheck(ns->sd,1) ;                  /* see if ready      */
         if( ii > 0 ){                                   /* if socket ready:  */
            jj = tcp_accept( ns->sd , NULL,&hend ) ;     /* accept connection */
            if( jj >= 0 ){                               /* if accept worked  */
               CLOSEDOWN( ns->sd ) ;                     /* close old socket  */
               NI_strncpy(ns->name,hend,255) ;           /* put IP into name  */
               NI_free(hend); ns->bad = 0; ns->sd = jj;  /* and ready to go!  */
            }
         }
         return ns ;
      }

      /** place an outgoing call "w" **/

      if( do_create ){
         struct hostent *hostp ;
         ns->io_mode = NI_OUTPUT_MODE ;
         hostp = gethostbyname(host) ;                   /* lookup host on net */
         if( hostp == NULL ){                            /* fails? must die!   */
             NI_free(ns) ; return NULL ;
         }
         ns->sd  = tcp_connect( host , port ) ;          /* connect to host    */
         ns->bad = (ns->sd < 0) ? TCP_WAIT_CONNECT : 0 ; /* fails? must wait   */
         NI_strncpy(ns->name,host,255) ;                 /* save the host name */
         return ns ;
      }
      return NULL ;  /* should never be reached */
   }

   /***** deal with simple files *****/

   if( strncmp(name,"file:",5) == 0 ){

      char *fname = name+5 ;
      FILE *fp ;

      if( NI_strlen(name) > 255 || NI_strlen(fname) < 1 ) return NULL ;

      fp = fopen( fname , do_create ? "wb"     /* always in binary mode */
                                    : "rb" ) ;

      if( fp == NULL ) return NULL ;

      /** initialize NI_stream_type output **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_FILE_TYPE;   /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->fp       = fp ;
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;

      ns->buf      = NI_malloc(NI_BUFSIZE) ;
      ns->bufsize  = NI_BUFSIZE ;

      NI_strncpy( ns->name , fname , 255 ) ;

      if( ns->io_mode == NI_INPUT_MODE )     /* save the file size */
         ns->fsize = NI_filesize( fname ) ;  /* if we are reading  */
      else
         ns->fsize = -1 ;

      return ns ;
   }

   /***** str: string array I/O *****/

   if( strncmp(name,"str:",4) == 0 ){

      int nn = NI_strlen(name+4) ;  /* may be 0 */

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_STRING_TYPE; /* what kind is this? */
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->npos     = 0 ;             /* scan starts at 0   */

      /* Note that bufsize == nbuf+1 for str:
         This is because we don't count the terminal NUL
         in nbuf (number of readable bytes),
         but do count it in bufsize (size of the buf array) */

      if( do_accept ){               /* read from stuff after str: */
         ns->nbuf    = nn ;
         ns->bufsize = nn+1 ;
         ns->buf     = NI_malloc(nn+1) ;
         strcpy(ns->buf,name+4) ;
      } else {                       /* write to a string */
         ns->nbuf    = 0 ;
         ns->bufsize = 1 ;
         ns->buf     = NI_malloc(1) ; /* 1 byte set to zero */
      }

      strcpy( ns->name , "ElvisHasLeftTheBuilding" ) ;
      return ns ;
   }

   /***** http:// or ftp:// I/O *****/

   if( strncmp(name,"http://",7) == 0 || strncmp(name,"ftp://",6) == 0 ){
      int nn ;
      char *data=NULL ;

      if( do_create ) return NULL ;                  /* bad */

      nn = NI_read_URL( name , &data ) ;

      if( data == NULL || nn <= 4 ){                 /* bad */
         NI_free(data); return NULL;
      }

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_REMOTE_TYPE; /* what kind is this? */
      ns->io_mode  = NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->nbuf     = nn ;
      ns->bufsize  = nn ;
      ns->buf      = data ;

      NI_strncpy( ns->name , name , 255 ) ;
      return ns ;
   }

   return NULL ;  /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*! Return 1 if it is legal to read from this stream, 0 if it isn't.
    This doesn't say anything about if it is practical to read
    at this moment; for that, use NI_stream_readcheck().
-------------------------------------------------------------------------*/

int NI_stream_readable( NI_stream_type *ns )
{
   if( ns == NULL ) return 0 ;
   if( ns->type == NI_TCP_TYPE ) return 1 ;
   return (ns->io_mode == NI_INPUT_MODE) ;
}

/*-----------------------------------------------------------------------*/
/*! Return 1 if it is legal to write to this stream, 0 if it isn't.
    This doesn't say anything about if it is practical to write
    at this moment; for that, use NI_stream_writecheck().
-------------------------------------------------------------------------*/

int NI_stream_writeable( NI_stream_type *ns )
{
   if( ns == NULL ) return 0 ;
   if( ns->type == NI_TCP_TYPE ) return 1 ;
   return (ns->io_mode == NI_OUTPUT_MODE) ;
}

/*-----------------------------------------------------------------------*/
/*! Return the name set in the NI_stream header. */

char * NI_stream_name( NI_stream_type *ns )
{
   if( ns == NULL ) return NULL ;
   return ns->name ;
}

/*-----------------------------------------------------------------------*/
/*! Return the output string buffer for a NI_stream of str: type.
    If the input is not a "w" str: stream, then NULL is returned.
    Otherwise a pointer to the internal buffer is returned.
    This will be a NUL terminated string.
-------------------------------------------------------------------------*/

char * NI_stream_getbuf( NI_stream_type *ns )
{
   if( ns          == NULL           ||
       ns->type    != NI_STRING_TYPE ||
       ns->io_mode != NI_OUTPUT_MODE   ) return NULL ;  /* bad inputs */

   return ns->buf ;
}

/*-----------------------------------------------------------------------*/
/*! Clear the buffer of a str: writing NI_stream.  This is intended to
    let you write anew without having to close and open again.
-------------------------------------------------------------------------*/

void NI_stream_clearbuf( NI_stream_type *ns )
{
   if( ns          == NULL           ||
       ns->type    != NI_STRING_TYPE ||
       ns->io_mode != NI_OUTPUT_MODE   ) return ;  /* bad inputs */

   NI_free(ns->buf) ;
   ns->nbuf    = 0 ;
   ns->bufsize = 1 ;
   ns->buf     = NI_malloc(1) ; /* 1 byte set to zero */
}

/*-----------------------------------------------------------------------*/
/*! Reset the input string buffer for a NI_stream of str: type.
    If the input is not a "r" str: stream, then nothing happens.
    Otherwise, the current contents of the buffer are discarded,
    and the buffer is replaced with a copy of the input string.
-------------------------------------------------------------------------*/

void NI_stream_setbuf( NI_stream_type *ns , char *str )
{
   int nn ;

   if( ns          == NULL           ||
       ns->type    != NI_STRING_TYPE ||
       ns->io_mode != NI_INPUT_MODE  ||
       str         == NULL             ) return ;  /* bad inputs */

   NI_free(ns->buf) ;               /* take out the trash */
   nn = NI_strlen(str) ;            /* size of new buffer string */
   ns->nbuf    = nn ;               /* set num char in new buffer */
   ns->npos    = 0  ;               /* reset scan position */
   ns->bufsize = nn+1 ;             /* allow space for NUL byte */
   ns->buf     = NI_malloc(nn+1) ;  /* and make the buffer */
   strcpy(ns->buf,str) ;            /* and set its contents */
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Check if the given NI_stream is properly opened for I/O.

   If not, wait up to msec milliseconds to establish the connection to
   the other end; if msec < 0, will wait nearly forever.
   Returns 1 if ready; 0 if not (but may become good later);
   -1 if an error occurs.
   Possible -1 errors are:
     * ns was connected to a socket, and now has become disconnected
     * ns is passed in as NULL (bad user, bad bad bad)
     * ns is reading a file or a string, and we are already at its end
   The only case in which 0 is returned is if the NI_stream is a
   socket (tcp:) and the socket is waiting for a connection from
   the other end.  This is also the only case in which input parameter
   msec is actually used.
-------------------------------------------------------------------------*/

int NI_stream_goodcheck( NI_stream_type *ns , int msec )
{
   int ii , jj ;
   char * bbb ;

   /** check inputs for OK-osity **/

   if( ns == NULL ) return -1 ;

   switch( ns->type ){

      /** File I/O [there is never any waiting here] **/

      case NI_FILE_TYPE:
        if( ns->fp == NULL ) return -1 ;        /* should never happen */
        if( ns->io_mode == NI_INPUT_MODE )
           return NI_stream_readcheck(ns,0) ;   /* input mode */
        else
           return 1 ;                           /* output mode */

      /** String I/O **/

      case NI_STRING_TYPE:
        if( ns->io_mode == NI_INPUT_MODE )
           return NI_stream_readcheck(ns,0) ;   /* input mode */
        else
           return 1 ;                           /* output mode */

      /** remote Web input */

      case NI_REMOTE_TYPE:
        if( ns->io_mode == NI_INPUT_MODE )
           return NI_stream_readcheck(ns,0) ;   /* input mode */
        else
           return -1 ;                          /* output mode */

      /** Socket I/O **/

      case NI_TCP_TYPE:
        if( ns->bad == 0 ){  /** if good before, then check if is still good **/
           int ich = 1 ;
           ich = tcp_alivecheck(ns->sd) ;
           if( ich == 0 ) return -1 ;
           return 1 ;
        }

        /** wasn't good before, so check if that condition has changed **/

        /** TCP/IP waiting to accept call from another host **/

        if( ns->bad == TCP_WAIT_ACCEPT ){
           ii = tcp_readcheck(ns->sd,msec) ;             /* see if ready      */
           if( ii > 0 ){                                 /* if socket ready:  */
              jj = tcp_accept( ns->sd , NULL,&bbb ) ;    /* accept connection */
              if( jj >= 0 ){                             /* if accept worked  */
                 CLOSEDOWN( ns->sd ) ;                   /* close old socket  */
                 NI_strncpy(ns->name,bbb,255) ;          /* put IP into name  */
                 NI_free(bbb); ns->bad = 0; ns->sd = jj; /* and ready to go!  */
              }
           }
        }

        /** TCP/IP waiting to connect call to another host **/

        else if( ns->bad == TCP_WAIT_CONNECT ){
           int dms=0 , ms ;

           if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */
           for( ms=0 ; ms < msec ; ms += dms ){
             ns->sd = tcp_connect( ns->name , ns->port );  /* try to connect  */
             if( ns->sd >= 0 ) break ;                     /* worked? get out */
             dms = NEXTDMS(dms); dms = MIN(dms,msec-ms); NI_sleep(dms);
           }
           if( ns->sd < 0 )                                  /* one last try? */
              ns->sd  = tcp_connect( ns->name , ns->port ) ;

           if( ns->sd >= 0 ) ns->bad = 0 ;                   /* succeeded?    */
        }

        /** see if it turned from bad to good **/

        return (ns->bad == 0) ;
   }

   return -1 ;  /* unreachable, I hope */
}

/*-----------------------------------------------------------------------*/
/*! Close a NI_stream.  Note that this will also free what ns points to.

    Use the NI_STREAM_CLOSE macro to call this function and then
    also set the pointer "ns" to NULL.
-------------------------------------------------------------------------*/

void NI_stream_close( NI_stream_type *ns )
{
   if( ns == NULL ) return ;

   switch( ns->type ){

      case NI_REMOTE_TYPE:
      case NI_STRING_TYPE:   /* nothing to do */
      break ;

      case NI_FILE_TYPE:
        if( ns->fp != NULL ) fclose(ns->fp) ;
      break ;

      case NI_TCP_TYPE:
        if( ns->sd >= 0 ) CLOSEDOWN(ns->sd) ;
      break ;
   }

   NI_free(ns->buf); NI_free(ns); return;
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data read out of it.

  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data is ready, 0 if not;
  -1 will be returned if some unrecoverable error is detected:
    tcp: the socket connection was dropped
   file: you have reached the end of the file, and are still trying to read.
-----------------------------------------------------------------------------*/

int NI_stream_readcheck( NI_stream_type *ns , int msec )
{
   int ii ;

   switch( ns->type ){

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = NI_stream_goodcheck(ns,0) ;       /* check if it is connected */
        if( ii == -1 ) return -1 ;             /* some error */
        if( ii == 0  ){                        /* not good yet */
           ii = NI_stream_goodcheck(ns,msec) ; /* so wait for it to get good */
           if( ii != 1 ) return ii ;           /* if still not good, exit */
        }
        ii = tcp_alivecheck( ns->sd ) ;        /* see if it is still open  */
        if( !ii ) return -1 ;                  /* if not open, error exit  */
        ii = tcp_readcheck( ns->sd , msec ) ;  /* see if any data is there */
        return ii ;

      /** file: ==> check current file position and length of file **/

      case NI_FILE_TYPE:{
         long f_len , f_pos ;

         if( ns->fp == NULL                ||
             ns->io_mode == NI_OUTPUT_MODE   ) return -1 ; /* never? */

         f_len = ns->fsize ;               /* length of file      */
         if( f_len < 0 ) return -1 ;       /* file not found (?)  */

         f_pos = ftell( ns->fp ) ;         /* where are we now?   */
         if( f_pos < 0 ) return -1 ;       /* should never happen */

         return (f_pos < f_len) ? 1 : -1 ; /* is good or bad, but */
                                           /* never just neutral  */
      }

      /** str: ==> check current buffer position **/

      case NI_REMOTE_TYPE:
      case NI_STRING_TYPE:{
         if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* never? */

         return (ns->npos < ns->nbuf) ? 1 : -1 ;  /* is data left? */
      }
   }

   return -1 ;  /* should never happen */
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data written into it.

  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data can be sent, 0 if not;
  -1 will be returned if some unrecoverable error is detected:
    tcp: the socket closed down at the other end
   file: this should never happen, unless you try to write to
         a readonly NI_stream
-----------------------------------------------------------------------------*/

int NI_stream_writecheck( NI_stream_type *ns , int msec )
{
   int ii ;

   switch( ns->type ){

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = NI_stream_goodcheck(ns,0) ;
        if( ii == -1 ) return -1 ;             /* some error */
        if( ii == 0  ){                        /* not good yet */
           ii = NI_stream_goodcheck(ns,msec);  /* so wait for it to get good */
           if( ii != 1 ) return ii ;           /* if still not good, exit */
        }
        return tcp_writecheck(ns->sd,msec) ;   /* check if we can write bytes */

      /** file: ==> if the file was opened in write mode **/

      case NI_FILE_TYPE:
        return ( (ns->fp != NULL && ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                                   : -1 ) ;

      /** str: ==> if the string was opened in write mode **/

      case NI_STRING_TYPE:
        return ( (ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                 : -1 ) ;
      /** http: or ftp: **/

      case NI_REMOTE_TYPE:   /* can't write to remote files */
        return -1 ;
   }

   return -1 ;  /* should never be reached */
}

/*----------------------------------------------------------------------------*/
/*!  Send nbytes of data from buffer down the NI_stream.

  Return value is the number of bytes actually sent, or is -1 if some error
  occurs (which means that the NI_stream is bad).

  tcp: We use blocking sends, so that all the data should be sent properly
       unless the connection to the other end fails for some reason
       (e.g., the planet explodes in a fiery cataclysm of annihilation).

 file: Everything should be written, unless the filesystem fills up.
       If nothing at all gets written, -1 is returned.

  str: Everything will be written, or the program will crash.
       Do not include the NUL byte at the end of the string in
       the nbytes count.
------------------------------------------------------------------------------*/

int NI_stream_write( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , nsent ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;  /* that was easy */

   ii = NI_stream_writecheck(ns,1) ; /* check if stream is writable */
   if( ii <= 0 ) return ii ;         /* if not, vamoose the ranch */

   switch( ns->type ){

     /** tcp: ==> just use send **/

     case NI_TCP_TYPE:

       /* turn off SIGPIPE signals, which will otherwise be
          raised if we send to a socket when the other end has crashed */

       if( !nosigpipe ){ signal(SIGPIPE,SIG_IGN); nosigpipe = 1; }

       nsent = tcp_send( ns->sd , buffer , nbytes , 0 ) ;
       if( nsent < nbytes ) PERROR("NI_stream_write(send)") ;
       if( nsent == 0 ) nsent = -1 ;
       return nsent ;

     /** file: ==> just fwrite **/

     case NI_FILE_TYPE:
       nsent = fwrite( buffer , 1 , nbytes , ns->fp ) ;
       if( nsent < nbytes ) PERROR("NI_stream_write(fwrite)") ;
       if( nsent == 0 ) nsent = -1 ;
       return nsent ;

     /** str: ==> append to buffer in stream struct **/

     case NI_STRING_TYPE:
#if 0
fprintf(stderr,"NI_stream_write str: input=%s\n",ns->buf) ;
#endif
        ns->buf = NI_realloc( ns->buf , ns->bufsize+nbytes ) ;
        memcpy( ns->buf+ns->nbuf , buffer , nbytes ) ;
        ns->nbuf    += nbytes ; ns->buf[ns->nbuf] = '\0' ;
        ns->bufsize += nbytes ;
#if 0
fprintf(stderr,"NI_stream_write str: output=%s\n",ns->buf) ;
#endif
        return nbytes ;

     /** ftp: or http: ==> can't write! */

     case NI_REMOTE_TYPE:
        return -1 ;
   }

   return -1 ;  /* should not be reached */
}

/*-------------------------------------------------------------------------*/
/*!  Read up to nbytes of data from the NI_stream, into buffer.

   Returns the number of bytes actually read.  For both the case of
   sockets and files, this may be less than nbytes (may even be 0).
   If an error occurs and no data is read, -1 is returned.

   For tcp: streams, if no data is available, this function will wait until
   something can be read.  If this behavior is undesirable, then you should
   use NI_stream_readcheck() before calling this function in order to see
   if any data is available.

   For file: streams, this function simply tries to read from the file.
   Whether or not it succeeds, it will return immediately. It should
   never return -1; if it returns 0, this means end-of-file.
---------------------------------------------------------------------------*/

int NI_stream_read( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;

   switch( ns->type ){

     /** tcp: just use recv **/

     case NI_TCP_TYPE:
       ii = NI_stream_goodcheck(ns,1) ;
       if( ii != 1 ) return ii ;
       ii = tcp_recv( ns->sd , buffer , nbytes , 0 ) ;
       if( ii == -1 ) PERROR("NI_stream_read(recv)") ;
       return ii ;

     /** file: just use fread **/

     case NI_FILE_TYPE:
       if( ns->fp == NULL || ns->io_mode == NI_OUTPUT_MODE ) return -1 ;
       ii = fread( buffer , 1 , nbytes , ns->fp ) ;
       return ii ;

     /** str: copy bytes out of the buffer string **/

     case NI_REMOTE_TYPE:
     case NI_STRING_TYPE:
       if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* bad stream */
       ii = ns->nbuf - ns->npos ;                      /* how much is left */
       if( ii <= 0                       ) return -1 ; /* no data left */
       if( ii > nbytes ) ii = nbytes ;                 /* amount to copy */
       memcpy( buffer , ns->buf+ns->npos , ii ) ;      /* copy it */
       ns->npos += ii ;                                /* advance position */
       return ii ;
   }

   return -1 ;  /* should not be reached */
}

/*-----------------------------------------------------------------------*/
/*! Try to fill up the stream's input buffer.
    Don't call this function until NI_stream_goodcheck() is 1!

    minread = Minimum number of bytes to read.
              Will wait until we get at least this many,
              until the stream is bad or the buffer is full.
              If minread=0, then may read nothing (but will try).

    msec    = Maximum amount of time to wait to satisfy minread,
              in milliseconds.  If msec<0, will wait nearly forever.
              If msec=0, will return after 1st read attempt, even
              if nothing was obtained.

    Returns number of bytes read (-1 if input stream goes bad before
    any data is read).  If the input stream goes bad AFTER some data
    is read, there is no indication of that (until the next time
    you call this, of course).
-------------------------------------------------------------------------*/

static int NI_stream_fillbuf( NI_stream_type *ns, int minread, int msec )
{
   int nn , ii , ntot=0 , ngood=0 , mwait=0 ;
   int start_msec = NI_clock_time() ;

   if( NI_stream_goodcheck(ns,0) <= 0 ) return -1 ; /* bad input */

   if( ns->type == NI_STRING_TYPE ) return -1 ;      /* goofy input */
   if( ns->type == NI_REMOTE_TYPE ) return -1 ;      /* goofy input */

   if( ns->nbuf >= ns->bufsize ) return 0 ; /* buffer already full */

   if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */

   /* read loop */

   while(1){

      ngood = NI_stream_readcheck(ns,mwait); /* check if data can be read */

      if( ngood < 0 ) break ;                /* data stream gone bad, so exit */

      if( ngood > 0 ){                       /* we can read! */

         ii = NI_stream_read( ns, ns->buf+ns->nbuf, ns->bufsize-ns->nbuf ) ;

         if( ii > 0 ){                 /* we got data! */
            ns->nbuf += ii ;           /* buffer is now longer */
            ntot     += ii ;           /* total bytes read here so far */

            /* if buffer is full,
               or we have all the data that was asked for, then exit */

            if( ns->nbuf >= ns->bufsize || ntot >= minread ) break ;

         } else if( ii < 0 ){          /* stream suddenly died horribly? */
            ngood = -1 ; break ;
         }
      }

      /* if we don't require data, then exit no matter what our status is */

      if( minread <= 0 ) break ;

      /* if the max time has elapsed, then exit */

      if( NI_clock_time()-start_msec >= msec ) break ;

      /* otherwise, sleep a little bit before trying again */

      if( mwait < 99 ) mwait++ ;
   }

   /* if didn't get any data, and
      if the NI_stream was bad, return -1 as a flag of displeasure */

   if( ntot == 0 && ngood < 0 ) ntot = -1 ;

   return ntot ;  /* otherwise, return # of bytes read (may be 0) */
}

/**********************************************************************/
/******* Functions to read and write data and group elements. *********/
/**********************************************************************/

/* internal prototypes */

static int decode_one_double( NI_stream_type *, double * ) ;
static int decode_one_string( NI_stream_type *, char ** ) ;
static int scan_for_angles( NI_stream_type *, int ) ;
static void reset_buffer( NI_stream_type * ) ;

#define clear_buffer(ns) ( (ns)->nbuf = (ns)->npos = 0 )

/*--------------------------------------------------------------------*/
/*! Read an element (maybe a group) from the stream, waiting up to
    msec milliseconds for the header to appear.  (After that, this
    function may wait a long time for the rest of the element to
    appear, unless the data stream comes to an end.)

   Return is NULL if nothing can be read at this time.  Otherwise,
   use NI_element_type(return value) to determine if the element
   read is a data element or a group element.

   Note that a header that is longer than ns->bufsize will
   never be read properly, since we must have the entire header in
   the buffer before processing it.  This should only be a problem
   for deranged users.  If such a vast header is encountered, it
   will be flushed.

   If header start '<' and stop '>' are encountered, then this
   function will read data until it can create an element, or until
   the data stream is bad (i.e., the file ends, or the socket closes).

   If NULL is returned, that can be because there is no data to
   read even in the buffer, or because the input data stream has gone
   bad (i.e., will return no more data ever).  To check for the latter
   case, use NI_stream_readcheck().

   This code does not yet Line input.
----------------------------------------------------------------------*/

void * NI_read_element( NI_stream_type *ns , int msec )
{
   int ii,nn,nhs , num_restart ;
   char *cstart , *cstop ;
   header_stuff *hs ;
   int start_time=NI_clock_time() , mleft ;

   if( ns == NULL ) return NULL ;  /* bad input */

   if( msec < 0 ) msec = 999999999 ;  /* a long time (11+ days) */

   /* if we have a socket that hasn't connected,
      then see if it can connect now            */

   if( ns->bad ){
      nn = NI_stream_goodcheck( ns , msec ) ;
      if( nn < 1 ) return NULL ;              /* didn't connect */
   }

   /*-- Try to find the element header --*/

   num_restart = 0 ;
HeadRestart:                            /* loop back here to retry */
   num_restart++ ;
   mleft = msec - (NI_clock_time()-start_time) ;      /* time left */
   if( num_restart > 1 && mleft <= 0 ) return NULL ;  /* don't allow too many loops */

   nn = scan_for_angles( ns , 0 ) ;     /* look for '<stuff>' */

   /* didn't find it */

   if( nn < 0 ){
      if( NI_stream_readcheck(ns,0) < 0 ) return NULL ;   /* connection lost */
      NI_sleep(1); goto HeadRestart;                      /* try again */
   }

   /* ns->buf[ns->npos] = opening '<' ; ns->buf[nn-1] = closing '>' */

   /* see if we found '<>', which is illegal,
      or a trailer '</stuff>', which is also illegal (here) */

   if( nn - ns->npos <= 2 || ns->buf[ns->npos+1] == '/' ){
      ns->npos = nn; reset_buffer(ns); /* toss the '<..>', try again */
      goto HeadRestart ;
   }

   /*----- Parse the header data and prepare to make an element! -----*/

   hs = parse_header_stuff( nn - ns->npos , ns->buf + ns->npos , &nhs ) ;

   if( hs == NULL ){  /* something bad happened there */
      ns->npos = nn; reset_buffer(ns); /* toss the '<..>', try again */
      goto HeadRestart ;
   }

   /*----- If here, have parsed a header (and will not HeadRestart).
           First, expunge the data bytes that were consumed to make
           the header; that is, we can then start reading data from
           ns->buf[ns->npos] .. ns->buf[ns->nbuf-1]                 --*/

   ns->npos = nn ;

   /*--------------- Now make an element of some kind ---------------*/

   if( strcmp(hs->name,"ni_group") == 0 ){  /*--- a group element ---*/

      NI_group *ngr ;
      void *nini ;
      int   empty=hs->empty ;

      start_time = NI_clock_time() ; /* allow up to 10 sec for next */
      msec       = 9999 ;            /* element to appear, before giving up */

      ngr = make_empty_group_element( hs ) ;
      destroy_header_stuff( hs ) ;
      if( empty ) return ngr ;  /* should not happen */

      /* we now have to read the elements within the group */

      num_restart = 0 ;
      while(1){           /* loop to find an element */

         nn = scan_for_angles( ns , 10 ) ;  /* find header/trailer '<...>' */

         mleft = msec - (NI_clock_time()-start_time) ;
         if( mleft < 0 ) mleft = 0 ;

         if( nn <= 0 ){  /* didn't find it */
            if( NI_stream_readcheck(ns,0) < 0 ) break ;  /* real bad */
            if( num_restart > 1 && mleft == 0 ) break ;  /* time's up */
            num_restart++ ;
            continue ;        /* try again (but not forever) */
         }

         /* check if we found a trailer element '</stuff>' */

         if( ns->buf[ns->npos+1] == '/' ){  /* trailer */
            ns->npos = nn ;                 /* so end the group */
            break ;
         }

         /* not a trailer, so try to make an element out of it */

         nini = NI_read_element( ns , mleft ) ;
         if( nini != NULL ){
            NI_add_to_group( ngr , nini ) ;  /* this is good */
            num_restart = 0 ;
            start_time = NI_clock_time() ;   /* restart the wait clock */
         } else {                            /* this is bad */
            if( NI_stream_readcheck(ns,0) < 0 ) break ;    /* real bad */
            mleft = msec - (NI_clock_time()-start_time) ;
            if( num_restart > 1 && mleft <= 0 ) break ;    /* time's up */
            num_restart++ ;
         }
      }

      /* and we are done */

      return ngr ;

   } /* end of reading group element */

   else { /*---------------------- a data element -------------------*/

      NI_element *nel ;
      int form, swap, nbrow , row,col ;

      enhance_header_stuff( hs ) ;
      nel = make_empty_data_element( hs ) ;
      destroy_header_stuff( hs ) ;

      /* check if this is a ni_typedef element */

      if( strcmp(nel->name,"ni_typedef") == 0 ){
         int nn , nt , nd ;
         nn = string_index( "ni_name" , nel->attr_num , nel->attr_lhs ) ;
         nt = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;
         nd = string_index( "ni_dimen", nel->attr_num , nel->attr_lhs ) ;

         /* needs ni_name and ni_type attributes */

         if( nn >= 0 && nt >= 0 )
            NI_typedef( nel->attr_rhs[nn] ,
                        nel->attr_rhs[nt] ,
                        (nd >= 0) ? nel->attr_rhs[nd] : NULL ) ;

         /* try for another element now */

         goto HeadRestart ;
      }

      /* check if this is an empty element */

      if( nel == NULL          ||     /* nel == NULL should never happen. */
          nel->vec_len == 0    ||     /* These other cases are indication */
          nel->vec_num == 0    ||     /* that this is an 'empty' element. */
          nel->vec_typ == NULL ||     /* ==> The header is all there is.  */
          nel->vec     == NULL   ) return nel ;

      /*-- If here, must read data from the buffer into nel->vec --*/

      /* Find the form of the input */

      form = NI_TEXT_MODE ; /* default is text mode */
      swap = 0 ;            /* and (obviously) don't byte swap */

      ii = string_index( "ni_form" , nel->attr_num , nel->attr_lhs ) ;

      if( ii >= 0 && nel->attr_rhs[ii] != NULL ){ /* parse ni_form=rhs */

         /* at present, the only non-text mode is "binary" */

         if( strstr(nel->attr_rhs[ii],"bin") != NULL ){
            int order=NI_MSB_FIRST ; /* default input byteorder */

            form = NI_BINARY_MODE ;

            /* check byteorder in header vs. this CPU */

            if( strstr(nel->attr_rhs[ii],"lsb") != NULL ) order = NI_LSB_FIRST;

            swap = ( order != NI_byteorder() ) ;  /* swap bytes? */
         }
      }

      /*-- Now must actually read data and put it somewhere (oog). */

      row = 0 ;         /* next index in vectors to fill */

      nbrow = NI_element_rowsize( nel ) ; /* how many bytes for one row */

      switch( form ){

        default:  break ;   /* bad!!! should never ever happen */

        /*......................................................*/

        case NI_BINARY_MODE:
         while( row < nel->vec_len ){  /* loop over input rows */

           /* if not enough data left in buffer for 1 row
              of data, then try to read more data into the buffer */

           if( ns->nbuf-ns->npos < nbrow ){

             reset_buffer(ns) ;  /* discard used up data in buffer */

             /* now read at least enough to fill one data row,
                waiting a long time if need be (or until the stream goes bad) */

             (void) NI_stream_fillbuf( ns , nbrow-ns->nbuf , 9999 ) ;

             /* if still don't have a full row of data,
                something bad has happened (end-of-file? closed socket?);
                so put what pitiful data we have into the vectors and exit */

             if( ns->nbuf-ns->npos < nbrow ){
               if( ns->nbuf-ns->npos > 0 ){   /* if we have any data at all */
                 char *qbuf = NI_malloc( sizeof(char)*nbrow ) ;
                 memcpy( qbuf , ns->buf+ns->npos , ns->nbuf-ns->npos ) ;
                 NI_fill_vector_row( nel , row , qbuf ) ; row++ ;
                 NI_free(qbuf) ;
               }
               clear_buffer(ns) ;   /* buffer is used up now */
               goto BinaryDone ;    /* and break out of loop */
             }
           }

           /* normal case: have (at least) a full row of data bytes,
                           so put them into the vectors, and loop    */

           NI_fill_vector_row( nel , row , ns->buf+ns->npos ) ;
           ns->npos += nbrow ;  /* we used up this many bytes */
           row++ ;          /* we filled this row */

         } /* end of loop over vector rows */

BinaryDone:
         nel->vec_filled = row ;  /* how many rows were filled above */

        break ;  /* end of binary input */

        /*......................................................*/

        case NI_TEXT_MODE:{

         while( row < nel->vec_len ){  /* loop over input rows */
          for( col=0 ; col < nel->vec_num ; col++ ){ /* over input vectors */

            /* decode one value from input, according to its type */

            switch( nel->vec_typ[col] ){
              default:                    /* Line is unimplemented */
              break ;

              case NI_STRING:{
                 char *val=NULL ;
                 char **vpt = (char **) nel->vec[col] ;
                 nn = decode_one_string( ns , &val ) ;
                 if( nn == 0 || val == NULL ) goto TextDone ;
                 unescape_inplace(val) ;
                 vpt[row] = val ;
              }
              break ;

              /* numeric types below here */

              case NI_BYTE:{
                 double val ;
                 byte *vpt = (byte *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (byte) val ;
              }
              break ;

              case NI_SHORT:{
                 double val ;
                 short *vpt = (short *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (short) val ;
              }
              break ;

              case NI_INT:{
                 double val ;
                 int *vpt = (int *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (int) val ;
              }
              break ;

              case NI_FLOAT:{
                 double val ;
                 float *vpt = (float *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (float) val ;
              }
              break ;

              case NI_DOUBLE:{
                 double val ;
                 double *vpt = (double *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (double) val ;
              }
              break ;

              case NI_COMPLEX:{
                 double v1,v2 ;
                 complex *vpt = (complex *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (float) v1 ;
                 vpt[row].i = (float) v2 ;
              }
              break ;

              case NI_RGB:{
                 double v1,v2,v3 ;
                 rgb *vpt = (rgb *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v3 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
              }
              break ;

              case NI_RGBA:{
                 double v1,v2,v3,v4 ;
                 rgba *vpt = (rgba *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v3 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v4 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
                 vpt[row].a = (byte) v4 ;
              }
              break ;
            } /* end of switch on type of this data value */

          } /* end of loop over vector columns */

          row++ ;
         } /* end of loop over vector rows */

TextDone:
         nel->vec_filled = row ;  /* how many rows were filled above */
        }
        break ;  /* end of text input */

      } /* end of reading data into the element */

      /*-- At this point, have finished reading into the element
           vectors, and the next character to process in the
           NI_stream input buffer is at location index ns->npos.  --*/

      /*-- Now swap bytes, if needed. --*/

      if( swap ){
        for( col=0 ; col < nel->vec_num ; col++ )
          NI_swap_vector( nel->vec_typ[col], nel->vec_len, nel->vec[col] ) ;
      }

      /*-- Now scan for the end-of-element marker '</something>' and
           skip all input bytes up to (and including) the final '>'. --*/

      num_restart = 0 ;
TailRestart:
      num_restart++ ;

      if( num_restart < 99 ){  /* don't loop forever, dude */
         int is_tail ;

         nn = scan_for_angles( ns , 10 ) ;  /* find '<...>' */

         /* if we didn't find '<...>' at all,
            then if the I/O stream is bad, just exit;
            otherwise, try scanning for '<...>' again */

         if( nn < 0 ){
           if( NI_stream_readcheck(ns,0) < 0 ) return nel ;
           goto TailRestart ;
         }

         /* we have '<...>', but make sure it starts with '</' */

         is_tail = ( ns->buf[ns->npos+1] == '/' ) ;

         if( !is_tail ){                       /* no '/'? */
           ns->npos = nn ; reset_buffer(ns) ;  /* skip '<...>' */
           goto TailRestart ;                  /* and try again */
         }

         ns->npos = nn ; /* skip '</...>' and we are done here! */
      }

      /*-- And are done with the input stream and the data element! --*/

      return nel ;

   } /* end of reading data element */

   return NULL ; /* should never be reached */
}

/*----------------------------------------------------------------------*/
/*! From the NI_stream ns, starting at buffer position ns->npos, decode
    one number into *val.  Return value of this function is 1 if
    we succeeded, 0 if not.  ns->npos will be altered to reflect the
    current buffer position (one after the last character processed)
    when all is done.
------------------------------------------------------------------------*/

#undef  NVBUF
#define NVBUF 127  /* max num chars for one number */

#define IS_USELESS(c) ( isspace(c) || iscntrl(c) )

static int decode_one_double( NI_stream_type *ns, double *val )
{
   int epos , num_restart, need_data, nn ;
   char vbuf[NVBUF+1] ;                    /* number string from buffer */

   /*-- check inputs for stupidness --*/

   if( ns == NULL || val == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data for a number ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ;
   if( num_restart > 19 ) return 0 ;  /*** give up ***/

   /*-- advance over useless characters in the buffer --*/

   while( ns->npos < ns->nbuf && IS_USELESS(ns->buf[ns->npos]) ) ns->npos++ ;

   /*-- check if we ran into the closing '<' prematurely
        (before any useful characters); if we did, then we are done --*/

   if( ns->npos < ns->nbuf && ns->buf[ns->npos] == '<' ) return 0 ;

   /*-- if we need some data, try to get some --*/

   need_data = (ns->nbuf-ns->npos < 2) ; /* need at least 2 unused bytes */

   /*-- An input value is decoded from a string of non-useless
        characters delimited by a useless character (or by the
        element closing '<').
        Note that the 1st character we are now at is non-useless.
        Scan forward to see if we have a useless character later. --*/

   if( !need_data ){  /* so have at least 2 characters */

      for( epos=ns->npos+1 ; epos < ns->nbuf ; epos++ )
        if( ns->buf[epos] == '<' || IS_USELESS(ns->buf[epos]) ) break ;

      /*- epos is either the delimiter position, or the end of data bytes -*/

      need_data = (epos == ns->nbuf) ; /* no delimiter ==> need more data */

      /*- If the string of characters we have is not delimited,
          and it is too long to be a number, throw out all the
          data in the buffer and quit.                         -*/

      if( need_data && epos-ns->npos > NVBUF ){ clear_buffer(ns); return 0; }
   }

   /*-- read more data now if it is needed --*/

   if( need_data ){

      reset_buffer(ns) ; /* discard used up data in buffer */

      /*- read at least 1 byte,
          waiting up to 666 ms (unless the data stream goes bad) -*/

      nn = NI_stream_fillbuf( ns , 1 , 666 ) ;

      if( nn >= 0 ) goto Restart ;  /* check if buffer is adequate now */

      /*- if here, the stream went bad.  If there are still
          data bytes in the stream, we can try to interpret them.
          Otherwise, must quit without success.                  -*/

      if( ns->nbuf == 0 ){ ns->npos=0; return 0; }  /* quitting */

      epos = ns->nbuf ;
   }

   /*-- if here, try to interpret data bytes ns->npos .. epos-1 --*/

   nn = epos-ns->npos ; if( nn > NVBUF ) nn = NVBUF ;     /* # bytes to read   */
   memcpy( vbuf, ns->buf+ns->npos, nn ); vbuf[nn] = '\0'; /* put bytes in vbuf */
   sscanf( vbuf , "%lf" , val ) ;                         /* interpret them    */
   ns->npos = epos ; return 1 ;                           /* retire undefeated */
}

/*----------------------------------------------------------------------*/
/*! From the NI_stream ns, starting at buffer position ns->npos, decode
    one string into newly malloc()-ed space pointed to by *str.
    Return value of this function is 1 if we succeeded, 0 if not.
    ns->npos will be altered to reflect the current buffer position
    (one after the last character processed) when all is done.
------------------------------------------------------------------------*/

static int decode_one_string( NI_stream_type *ns, char **str )
{
   int epos , num_restart, need_data, nn ;
   intpair sp ;

   /*-- check inputs for stupidness --*/

   if( ns == NULL || str == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ;
   if( num_restart > 19 ) return 0 ;  /*** give up ***/

   /*-- advance over useless characters in the buffer --*/

   while( ns->npos < ns->nbuf && IS_USELESS(ns->buf[ns->npos]) ) ns->npos++ ;

   /*-- check if we ran into the closing '<' prematurely
        (before any useful characters); if we did, then we are done --*/

   if( ns->npos < ns->nbuf && ns->buf[ns->npos] == '<' ) return 0 ;

   /*-- if we need some data, try to get some --*/

   need_data = (ns->nbuf-ns->npos < 2) ; /* need at least 2 unused bytes */

   if( !need_data ){  /* so have at least 2 characters */

      /* search for the string from here forward */

      sp = find_string( ns->npos , ns->nbuf , ns->buf ) ;

      need_data = (sp.i < 0)        ||  /* didn't find a string */
                  (sp.j <= sp.i)    ||  /* ditto */
                  (sp.j == ns->nbuf)  ; /* hit end of data bytes */
   }

   /*-- read more data now if it is needed --*/

   if( need_data ){

      reset_buffer(ns) ; /* discard used up data in buffer */

      /*- read at least 1 byte,
          waiting up to 666 ms (unless the data stream goes bad) -*/

      nn = NI_stream_fillbuf( ns , 1 , 666 ) ;

      if( nn >= 0 ) goto Restart ;  /* check if buffer is adequate now */

      /*- if here, the stream went bad.  If there are still
          data bytes in the stream, we can try to interpret them.
          Otherwise, must quit without success.                  -*/

      if( ns->nbuf == 0 ){ ns->npos=0; return 0; }  /* quitting */

      sp.i = 0 ; sp.j = ns->nbuf ;
   }

   /*-- if here, data bytes sp.i .. sp.j-1 are the string --*/

   nn = sp.j - sp.i ;                       /* length of string */
   *str = NI_malloc(nn+1) ;                 /* make the string */
   memcpy( *str , ns->buf+sp.i , nn ) ;     /* copy data to string */
   (*str)[nn] = '\0' ;                      /* terminate string */

   if( sp.j < ns->nbuf && IS_QUOTE_CHAR(ns->buf[sp.j]) ) sp.j++ ;
   ns->npos = sp.j ; return 1 ;
}

/*----------------------------------------------------------------------*/
/*! Reset the unscanned bytes in the buffer to start at position 0
    instead of position ns->npos; then set ns->npos to 0.
------------------------------------------------------------------------*/

static void reset_buffer( NI_stream_type *ns )
{
   if( ns == NULL || ns->npos <= 0 || ns->nbuf <= 0 ) return ;

   if( ns->npos < ns->nbuf ){           /* haven't used up all data yet */
      memmove( ns->buf, ns->buf+ns->npos, ns->nbuf-ns->npos ) ;
      ns->nbuf -= ns->npos ;
   } else {
      ns->nbuf = 0 ;                   /* all data in buffer is used up */
   }
   ns->npos = 0 ;               /* further scanning starts at beginning */
}

/*----------------------------------------------------------------------*/
/*! Scan stream for an element header or trailer:'<characters>',
    starting at byte offset ns->npos, and waiting msec milliseconds.

    Returns with the stream buffer set so that the opening '<' is at
    ns->buf[ns->npos] and the closing '>' is at ns->buf[q-1], where q
    is this function's return value.  Note that read operations may
    change ns->npos from its input value.

    If the return value is -1, then we couldn't find a '<stuff>' string.
    This may be due to:
      * there is no '<...>' in the buffer, and we can't read from
         the input stream; call NI_readcheck(ns,0) to confirm this
      * time ran out (alas)
      * The '<...' part filled the entire buffer (64K).  In this case,
         all the input buffer is thrown away - we don't support
         headers or trailers this long!
------------------------------------------------------------------------*/

static int scan_for_angles( NI_stream_type *ns, int msec )
{
   int nn, epos, need_data, num_restart ;
   char goal ;
   int start_time = NI_clock_time() , mleft , nbmin ;

   if( ns == NULL ) return -1 ;  /* bad input */

   epos = ns->npos ;

   if( msec < 0 ) msec = 999999999 ;   /* a long time (11+ days) */

   /*-- Will loop back here if we have to re-read/re-scan --*/

   goal        = '<' ;  /* first goal is opening '<' (second goal is '>') */
   num_restart = 0   ;
Restart:                                       /* loop back here to retry */
   num_restart++ ;
   mleft = msec - (NI_clock_time()-start_time) ;             /* time left */

   if( num_restart > 3 && mleft <= 0 ){                        /* failure */
      reset_buffer(ns) ;                               /* and out of time */
      return -1 ;
   }

   /*-- skip ahead to find goal in the buffer --*/

   while( epos < ns->nbuf && ns->buf[epos] != goal ) epos++ ;

   /*-- if we found our goal, do something about it --*/

   if( epos < ns->nbuf ){

     /*-- if our goal was the closing '>', we are done! --*/

     if( goal == '>' ) return epos+1 ;  /* marks the character after '>' */

     /*-- if here, our goal was the opening '<';
          set the buffer position to this location,
          set the new goal, and scan for the new goal --*/

      ns->npos = epos ;  /* mark where we found '<' */
      goal     = '>'  ;  /* the new goal */
      goto Restart    ;  /* scan again! */
   }

   /*-- if we get to here, we didn't find our goal:
        (a) if the goal was the opening '<', then throw
             away all data in the buffer, and get some more data
        (b) if the goal was the closing '>', then we need more data
            in the buffer, but need to keep the existing data
        (c) UNLESS the buffer is full
             - in this case, the universe ends right here and now --*/

   if( goal == '<' ){                   /* case (a) */
      ns->nbuf = ns->npos = epos = 0 ;
   } else if( ns->nbuf < ns->bufsize ){  /* case (b) */
      reset_buffer(ns) ; epos = 0 ;
   } else {                             /* case (c) */
      ns->nbuf = 0 ; return -1 ;
   }

   /*-- if we are here, we need more data before scanning again --*/

   /*-- read at least nbmin bytes,
        waiting up to mleft ms (unless the data stream goes bad) --*/

   if( mleft <= 0 ) mleft = 1 ;
   nbmin = (goal == '<') ? 3 : 1 ;

   nn = NI_stream_fillbuf( ns , nbmin , mleft ) ;

   if( nn >= 0 ) goto Restart ; /* scan some more */

   /*-- if here, the stream went bad, so exit --*/

   ns->nbuf = ns->npos = 0 ; return -1 ;
}

#if 0
/*-----------------------------------------------------------------*/
/*! Set the binary threshold size for NI_write_element.

    If a data element takes up more than 'size' bytes, then it
    will be written in binary form, otherwise in text form.
    If size=0, then all elements are written in binary.
    If size<0, then all elements are written in text.
    This function only affects what happens when you write
    data elements.  Reading is controlled by the contents of
    each element header (i.e., the ni_form attribute).
-------------------------------------------------------------------*/

void NI_binary_threshold( NI_stream_type *ns , int size )
{
   if( ns == NULL ) return ;
   ns->bin_thresh = size ;
   return ;
}
#endif

/*------------------------------------------------------------------------*/
/* Quotize (and escapize) a string, returning a new string. */

static char * quotize_string( char *str )
{
   int ii,jj , lstr,lout ;
   char *out ;

   lstr = NI_strlen(str) ;
   if( lstr == 0 ){ out = NI_malloc(4); strcpy(out,"\"\""); return out; }
   lout = 4 ;
   for( ii=0 ; ii < lstr ; ii++ ){
      switch( str[ii] ){
         case '&': lout += 5 ; break ;

         case '<':
         case '>': lout += 4 ; break ;

         case '"' :
         case '\'': lout +=6 ; break ;

         default: lout++ ; break ;
      }
   }
   out = NI_malloc(lout) ;
   out[0] = '"' ;
   for( ii=0,jj=1 ; ii < lstr ; ii++ ){
      switch( str[ii] ){
         default: out[jj++] = str[ii] ; break ;

         case '&':  memcpy(out+jj,"&amp;",5)  ; jj+=5 ; break ;

         case '<':  memcpy(out+jj,"&lt;",4)   ; jj+=4 ; break ;
         case '>':  memcpy(out+jj,"&gt;",4)   ; jj+=4 ; break ;

         case '"' : memcpy(out+jj,"&quot;",6) ; jj+=6 ; break ;

         case '\'': memcpy(out+jj,"&apos;",6) ; jj+=6 ; break ;
      }
   }
   out[jj++] = '"' ; out[jj] = '\0' ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize an array of strings, separating substrings with sep. */

static char * quotize_string_vector( int num , char **str , char sep )
{
   char *out , **qstr ;
   int ii , ntot , ll,nn ;

   /* handle special cases */

   if( num <= 0 || str == NULL )
      return quotize_string(NULL) ;

   if( num == 1 )
      return quotize_string( str[0] ) ;

   /* default separator */

   if( sep == '\0' ) sep = ',' ;

   /* temp array for quotized individual sub-strings */

   qstr = NI_malloc(sizeof(char *)*num) ;

   for( ntot=ii=0 ; ii < num ; ii++ ){
      qstr[ii] = quotize_string( str[ii] ) ;
      ntot += NI_strlen( qstr[ii] ) ;
   }

   /* make output, put 1st sub-string into it */

   out = NI_malloc(ntot) ;
   strcpy( out , qstr[0] ) ; NI_free(qstr[0]) ;
   for( ii=1 ; ii < num ; ii++ ){
      ll = strlen(out) ;  /* put separator at end of string string, */
      out[ll-1] = sep ;   /* in place of the closing " mark.       */

      strcat(out,qstr[ii]+1) ;  /* catenate with next sub-string, */
                                    /* but skip the opening " mark.  */
      NI_free(qstr[ii]) ;
   }

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize a bunch of ints. */

static char * quotize_int_vector( int num , int *vec , char sep )
{
   int ii , jj ;
   char *out , **qstr ;

   if( num <= 0 || vec == NULL )
      return quotize_string(NULL) ;

   qstr = NI_malloc(sizeof(char *)*num) ;
   for( ii=0 ; ii < num ; ii++ ){
      qstr[ii] = NI_malloc(16) ;
      sprintf(qstr[ii],"%d",vec[ii]) ;
      for( jj=strlen(qstr[ii])-1 ;                   /* clip */
           jj > 0 && isspace(qstr[ii][jj]) ; jj-- )  /* trailing */
        qstr[ii][jj] = '\0' ;                        /* blanks */
   }

   out = quotize_string_vector( num , qstr , sep ) ;

   for( ii=0 ; ii < num ; ii++ ) NI_free(qstr[ii]) ;

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Quotize a bunch of floats. */

static char * quotize_float_vector( int num , float *vec , char sep )
{
   int ii , jj , ff ;
   char *out , **qstr , fbuf[32] ;

   if( num <= 0 || vec == NULL )
      return quotize_string(NULL) ;

   qstr = NI_malloc(sizeof(char *)*num) ;
   for( ii=0 ; ii < num ; ii++ ){
      sprintf(fbuf," %12.6g",vec[ii]) ;
      for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
      for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
      qstr[ii] = NI_strdup(fbuf+ff) ;
   }

   out = quotize_string_vector( num , qstr , sep ) ;

   for( ii=0 ; ii < num ; ii++ ) NI_free(qstr[ii]) ;

   NI_free(qstr) ; return out ;
}

/*------------------------------------------------------------------------*/
/*! Check a string for 'nameness' - that is, consists only of legal
    characters for a NIML 'Name'.  Returns 1 if it is a Name and 0 if
    is not.
--------------------------------------------------------------------------*/

static int NI_is_name( char *str )
{
   int ii , ll ;

   if( str == NULL || str[0] == '\0' || !isalpha(str[0]) ) return 0 ;

   ll = NI_strlen(str) ;

   for( ii=0 ; ii < ll ; ii++ ){
      if( isalnum(str[ii]) || str[ii] == '_'   ||
          str[ii] == '.'   || str[ii] == '-'     ) continue ;
      return 0 ;
   }

   return 1 ;
}

/*------------------------------------------------------------------------*/
/*! Write an element (data or group) to a stream.
    Return value is number of bytes written to the stream.
    If return is -1, something bad happened.  You should check
    the stream with NI_stream_goodcheck(), for example.

    If the stream is temporarily unable to write (e.g., the socket
    buffer is full), then this function will wait until it is ready.
    If you don't want that behavior, you should use NI_stream_writecheck()
    before calling this function.
--------------------------------------------------------------------------*/

int NI_write_element( NI_stream_type *ns , void *nini , int tmode )
{
   char *wbuf , *att=NULL , *qtt , *btt ;
   int  nwbuf , ii,jj,row,col , tt=NI_element_type(nini) , ntot=0,nout ;

   /* ADDOUT = after writing, add byte count if OK, else quit */
   /* AF     = thing to NI_free() if ADDOUT is quitting */

#undef  AF
#define AF     NULL
#define ADDOUT if(nout<0){NI_free(AF);return -1;} else ntot += nout

   if( ns == NULL ) return -1 ;

   if( ns->bad ){                        /* socket that hasn't connected yet */
      jj = NI_stream_goodcheck(ns,1) ;   /* try to connect it */
      if( jj < 1 ) return jj ;           /* 0 is nothing yet, -1 is death */
   } else {                              /* check if good ns has gone bad */
      jj = NI_stream_writecheck(ns,1) ;
      if( jj < 0 ) return -1 ;
   }

   if( ns->type == NI_STRING_TYPE )      /* string output must be in text mode */
      tmode = NI_TEXT_MODE ;

   /*------------------ write a group element ------------------*/

   if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      /*- group header -*/

      nout = NI_stream_write( ns , "<ni_group" , 9 ) ;
      ADDOUT ;

      /*- attributes -*/

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){

         jj = NI_strlen( ngr->attr_lhs[ii] ) ; if( jj == 0 ) continue ;
         nout = NI_stream_write( ns , " " , 1 ) ;
         ADDOUT ;
         if( NI_is_name(ngr->attr_lhs[ii]) ){
           nout = NI_stream_write( ns , ngr->attr_lhs[ii] , jj ) ;
         } else {
           att = quotize_string( ngr->attr_lhs[ii] ) ;
           nout = NI_stream_write( ns , att , strlen(att) ) ; NI_free(att) ;
         }
         ADDOUT ;

         jj = NI_strlen( ngr->attr_rhs[ii] ) ; if( jj == 0 ) continue ;
         nout = NI_stream_write( ns , "=" , 1 ) ;
         ADDOUT ;
         att = quotize_string( ngr->attr_rhs[ii] ) ;
         nout = NI_stream_write( ns , att , strlen(att) ) ; NI_free(att) ;
         ADDOUT ;
      }

      /*- close header -*/

      nout = NI_stream_write( ns , ">\n" , 2 ) ;
      ADDOUT ;

      /*- write the group parts -*/

      for( ii=0 ; ii < ngr->part_num ; ii++ ){
         nout = NI_write_element( ns , ngr->part[ii] , tmode ) ;
         ADDOUT ;
      }

      /*- group trailer -*/

      nout = NI_stream_write( ns , "</ni_group>\n" , 12 ) ;
      ADDOUT ;

      return ntot ;

   /*------------------ write a data element ------------------*/

   } else if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      /*- sanity check (should never fail) -*/

      jj = NI_strlen(nel->name) ; if( jj == 0 ) return -1 ;

      /*- select the data output mode -*/

      /* Strings and Lines can only be written in text mode */

      if( tmode != NI_TEXT_MODE ){
        for( jj=0 ; jj < nel->vec_num ; jj++ ){
          if( nel->vec_typ[jj]==NI_STRING || nel->vec_typ[jj]==NI_LINE ){
             tmode = NI_TEXT_MODE ; break ;
          }
        }
      }

      switch( tmode ){
         default: tmode = NI_TEXT_MODE ; break ; /* currently the only mode */
         case NI_BINARY_MODE: break ;
#if 0
         case NI_BASE64_MODE: break ;
#endif
      }

      /* space to hold attribute strings */

      att = NI_malloc( 4096 + 2*nel->vec_num + 128*nel->vec_rank ) ;

#undef  AF
#define AF att  /* free att if we have to quit early */

      /* write start of header "<name" */

      strcpy(att,"<") ; strcat(att,nel->name) ;
      nout = NI_stream_write( ns , att , strlen(att) ) ;
      ADDOUT ;

      /*- write "special" attributes, if not an empty element -*/

      if( nel->vec_len > 0 && nel->vec_num > 0 ){
         int ll , tt ;

         /* ni_form (depends on tmode) */

         switch( tmode ){
           default:
           case NI_TEXT_MODE:
#if 0
             strcpy(att," ni_form=\"text\"") ;
#else
             *att = '\0' ;   /* text form is default */
#endif
           break ;

           case NI_BINARY_MODE:
           case NI_BASE64_MODE:
             sprintf(att," ni_form=\"%s.%s\"" ,
                    (tmode == NI_BINARY_MODE)      ? "binary"   : "base64"  ,
                    (NI_byteorder()==NI_LSB_FIRST) ? "lsbfirst" : "msbfirst" );
            break ;
         }
         if( *att != '\0' ){
            nout = NI_stream_write( ns , att , strlen(att) ) ;
            ADDOUT ;
         }

         /** do ni_type if this is NOT a typedef-ed type **/

         tt = string_index(nel->name,typedef_num,typedef_nam) ;
         if( tt < 0 ){

           /* ni_type */

           strcpy(att," ni_type=\"") ;
           for( ll=-1,ii=0 ; ii < nel->vec_num ; ii++ ){
            if( nel->vec_typ[ii] != ll ){  /* not the last type */
               if( ll >= 0 ){              /* write the last type out now */
                  btt = att + strlen(att) ;
                  if( jj > 1 ) sprintf(btt,"%d%c,",jj,NI_type_char(ll)) ;
                  else         sprintf(btt,"%c,"  ,   NI_type_char(ll)) ;
               }
               ll = nel->vec_typ[ii] ;     /* save new type code */
               jj = 1 ;                    /* it now has count 1 */

            } else {                       /* same as last type */
               jj++ ;                      /* so add 1 to its count */
            }
           }
           /* write the last type */
           btt = att + strlen(att) ;
           if( jj > 1 ) sprintf(btt,"%d%c\"",jj,NI_type_char(ll)) ;
           else         sprintf(btt,"%c\""  ,   NI_type_char(ll)) ;

           nout = NI_stream_write( ns , att , strlen(att) ) ;
           ADDOUT ;
         }

         /** do ni_dimen stuff:
              if this is NOT typedef-ed, OR
              if it IS typedef-ed but without a ni_dimen attribute
               AND the actual element dimension is greater than 1  **/

         if( tt < 0 || (typedef_dim[tt] == NULL && nel->vec_len > 1) ){

           /* ni_dimen */

           strcpy(att," ni_dimen=") ;
           qtt = quotize_int_vector( nel->vec_rank ,
                                     nel->vec_axis_len , ',' ) ;
           strcat(att,qtt) ; NI_free(qtt) ;
           nout = NI_stream_write( ns , att , strlen(att) ) ;
           ADDOUT ;

           /* extras: ni_veclen and ni_vecnum attributes */

           sprintf(att," ni_veclen=\"%d\"",nel->vec_len) ;
           nout = NI_stream_write( ns , att , strlen(att) ) ;
           ADDOUT ;

           sprintf(att," ni_vecnum=\"%d\"",nel->vec_num) ;
           nout = NI_stream_write( ns , att , strlen(att) ) ;
           ADDOUT ;
         }

         /* ni_delta */

         if( nel->vec_axis_delta != NULL ){
            strcpy(att," ni_delta=") ;
            qtt = quotize_float_vector( nel->vec_rank ,
                                        nel->vec_axis_delta , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_write( ns , att , strlen(att) ) ;
            ADDOUT ;
         }

         /* ni_origin */

         if( nel->vec_axis_origin != NULL ){
            strcpy(att," ni_origin=") ;
            qtt = quotize_float_vector( nel->vec_rank ,
                                        nel->vec_axis_origin , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_write( ns , att , strlen(att) ) ;
            ADDOUT ;
         }

         /* ni_units */

         if( nel->vec_axis_unit != NULL ){
            strcpy(att," ni_units=") ;
            qtt = quotize_string_vector( nel->vec_rank ,
                                         nel->vec_axis_unit , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_write( ns , att , strlen(att) ) ;
            ADDOUT ;
         }

         /* ni_axes */

         if( nel->vec_axis_label != NULL ){
            strcpy(att," ni_axes=") ;
            qtt = quotize_string_vector( nel->vec_rank ,
                                         nel->vec_axis_label , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_write( ns , att , strlen(att) ) ;
            ADDOUT ;
         }

      }

      /*- other attributes -*/

      for( ii=0 ; ii < nel->attr_num ; ii++ ){

         jj = NI_strlen( nel->attr_lhs[ii] ) ; if( jj == 0 ) continue ;

         /* skip "special" attributes */

         if( strcmp(nel->attr_lhs[ii],"ni_form")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_type")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_dimen")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_veclen") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_vecnum") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_delta")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_origin") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_units")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_axes")   == 0 ) continue ;

         /* do the work */

         strcpy(att," ") ;

         if( NI_is_name(nel->attr_lhs[ii]) ){
           strcat(att,nel->attr_lhs[ii]) ;
         } else {
           qtt = quotize_string( nel->attr_lhs[ii] ) ;
           strcat(att,qtt) ; NI_free(qtt) ;
         }

         jj = NI_strlen( nel->attr_rhs[ii] ) ;
         if( jj > 0 ){
            strcat(att,"=") ;
            qtt = quotize_string( nel->attr_rhs[ii] ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
         }
         nout = NI_stream_write( ns , att , strlen(att) ) ;
         ADDOUT ;
      }

      NI_free(att) ; att = NULL ; /* done with attributes */

#undef  AF
#define AF NULL  /* free nothing if we have to quit early */

      /*- close header -*/

      if( nel->vec_len == 0    ||     /* An 'empty' element (no data) */
          nel->vec_num == 0    ||
          nel->vec_typ == NULL ||
          nel->vec     == NULL   ){

        nout = NI_stream_write( ns , "/>" , 2 ) ;
        ADDOUT ;
        return ntot ;                 /* done with empty element */
      }

      /*- if here, must write data out -*/

      /* first, terminate the header,
         and allocate space for the write buffer (1 row at a time) */

      switch( tmode ){
         default:
         case NI_TEXT_MODE:
            btt = ">\n" ;                             /* add a newline */
            nwbuf = 5*NI_element_rowsize(nel) + 16 ;  /* text buffer  */
         break ;

         case NI_BINARY_MODE:
            btt = ">" ;                               /* no newline   */
            nwbuf = NI_element_rowsize(nel) ;         /* binary buffer */
         break ;

#if 0
         case NI_BASE64_MODE:
            btt = ">\n" ;
            nwbuf = NI_element_rowsize(nel) ;
         break ;
#endif
      }

      nout = NI_stream_write( ns , btt , strlen(btt) ) ;
      ADDOUT ;

      wbuf = NI_malloc(nwbuf+128) ;  /* 128 for the hell of it */

#undef  AF
#define AF wbuf  /* free wbuf if we have to quit early */

      /*- loop over output rows and write results -*/

      for( row=0 ; row < nel->vec_len ; row++ ){

        /* initialize this row's output */

        switch( tmode ){
           case NI_TEXT_MODE:    wbuf[0] = '\0'; break; /* clear buffer */
           case NI_BINARY_MODE:  jj = 0 ;        break; /* clear byte count */
        }

        /* write data for this row into wbuf */

        for( col=0 ; col < nel->vec_num ; col++ ){

         switch( tmode ){

          /*----- encode one value to output, according to its type -----*/
          case NI_TEXT_MODE:{
           jj = strlen(wbuf) ;
           switch( nel->vec_typ[col] ){
            default:                    /* Line is unimplemented */
            break ;

            case NI_STRING:{
              char **vpt = (char **) nel->vec[col] ;
              char *str = quotize_string(vpt[row]) ;  /* format for output */
              int nn = strlen(str) ;                  /* how much? */
              int nadd = jj+nn+8-nwbuf ;
              if( nadd > 0 ){                         /* too long for wbuf? */
                 nwbuf += nadd ;
                 wbuf = NI_realloc(wbuf,nwbuf+128) ;  /* extend wbuf size */
              }
              sprintf(wbuf+jj," %s",str); free(str);  /* write output */
            }
            break ;

            /* numeric types below here */

            case NI_BYTE:{
              byte *vpt = (byte *) nel->vec[col] ;
              sprintf(wbuf+jj," %u",(unsigned int)vpt[row]) ;
            }
            break ;

            case NI_SHORT:{
              short *vpt = (short *) nel->vec[col] ;
              sprintf(wbuf+jj," %d",(int)vpt[row]) ;
            }
            break ;

            case NI_INT:{
              int *vpt = (int *) nel->vec[col] ;
              sprintf(wbuf+jj," %d",vpt[row]) ;
            }
            break ;

            /* multiple byte structs */

            case NI_RGB:{
              rgb *vpt = (rgb *) nel->vec[col] ;
              sprintf(wbuf+jj,"  %u %u %u",vpt[row].r,vpt[row].g,vpt[row].b) ;
            }
            break ;

            case NI_RGBA:{
              rgba *vpt = (rgba *) nel->vec[col] ;
              sprintf(wbuf+jj,"  %u %u %u %u",
                      vpt[row].r,vpt[row].g,vpt[row].b,vpt[row].a) ;
            }
            break ;

            /* for floating point outputs,
               first print to a temp string,
               then clip trailing and leading blanks */

            case NI_FLOAT:{
              float *vpt = (float *) nel->vec[col] ;
              char fbuf[32] ; int ff ;
              sprintf(fbuf," %12.6g",vpt[row]) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(wbuf+jj," %s",fbuf+ff) ;
            }
            break ;

            case NI_DOUBLE:{
              double *vpt = (double *) nel->vec[col] ;
              char fbuf[32] ; int ff ;
              sprintf(fbuf," %18.12g",vpt[row]) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(wbuf+jj," %s",fbuf+ff) ;
            }
            break ;

            case NI_COMPLEX:{
              complex *vpt = (complex *) nel->vec[col] ;
              char fbuf[32],gbuf[32] ; int ff,gg ;
              sprintf(fbuf," %12.6g",vpt[row].r) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(gbuf," %12.6g",vpt[row].i) ;
              for( gg=strlen(gbuf) ; gbuf[gg]==' ' ; gg-- ) gbuf[gg] = '\0' ;
              for( gg=0 ; gbuf[gg] == ' ' ; gg++ ) ;
              sprintf(wbuf+jj,"  %s %s",fbuf+ff,gbuf+gg) ;
            }
            break ;

           } /* end of switch on datum type */
          }
          break ; /* end of NI_TEXT_MODE */

          /*----- put the binary form of this element into wbuf+jj -----*/
          case NI_BINARY_MODE:{
           switch( nel->vec_typ[col] ){
            default: break ;              /* should not happen */

            case NI_BYTE:{
              byte *vpt = (byte *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(byte));
              jj += sizeof(byte);
            }
            break ;

            case NI_SHORT:{
              short *vpt = (short *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(short));
              jj += sizeof(short);
            }
            break ;

            case NI_INT:{
              int *vpt = (int *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(int));
              jj += sizeof(int);
            }
            break ;

            case NI_FLOAT:{
              float *vpt = (float *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(float));
              jj += sizeof(float);
            }
            break ;

            case NI_DOUBLE:{
              double *vpt = (double *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(double));
              jj += sizeof(double);
            }
            break ;

            case NI_COMPLEX:{
              complex *vpt = (complex *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(complex));
              jj += sizeof(complex);
            }
            break ;

            case NI_RGB:{
              rgb *vpt = (rgb *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(rgb));
              jj += sizeof(rgb);
            }
            break ;

            case NI_RGBA:{
              rgba *vpt = (rgba *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(rgba));
              jj += sizeof(rgba);
            }
            break ;

           } /* end of switch on datum type */
          }
          break ; /* end of NI_BINARY_MODE */

         } /* end of switch on tmode */
        } /* end of loop over columns */

        /*- actually write this row of data out -*/

        switch( tmode ){
          case NI_TEXT_MODE:     /* each row is on a separate line */
            strcat(wbuf,"\n") ;
            nout = NI_stream_write( ns , wbuf , strlen(wbuf) ) ;
            ADDOUT ;
          break ;

          case NI_BINARY_MODE:
            nout = NI_stream_write( ns , wbuf , nwbuf ) ;
            ADDOUT ;
          break ;
        }

      } /* end of loop over rows */

      NI_free(wbuf) ;  /* don't need row buffer no more no how */

#undef  AF
#define AF NULL  /* free nothing if we quit early */

      /*- write element trailer -*/

      nout = NI_stream_write( ns , "</" , 2 ) ;
      ADDOUT ;
      nout = NI_stream_write( ns , nel->name , strlen(nel->name) ) ;
      ADDOUT ;
      nout = NI_stream_write( ns , ">\n" , 2 ) ;
      ADDOUT ;

      return ntot ;
   } /* end of write data element */

   return -1 ; /* should never be reachable */
}

/*************************************************************************/
/********************** Functions for fetching URLs **********************/
/**********************  [adapted from thd_http.c]  **********************/
/*************************************************************************/

static int www_debug = 0 ;
#define FAILED     if(www_debug)fprintf(stderr," **FAILED\n")
#define DMESS(s,t) if(www_debug)fprintf(stderr,s,t)

/*---------------------------------------------------------------------*/
static char tmpdir[512] = "\0" ;

static void setup_tmpdir(void)
{
   char *td ;

   if( tmpdir[0] != '\0' ) return ;

                    td = getenv("TMPDIR")  ;
   if( td == NULL ) td = getenv("TEMPDIR") ;

   if( td == NULL || td[0] == '\0' || strlen(td) > 222 ){
      strcpy(tmpdir,"/tmp/") ;
   } else {
      int ltd ;
      NI_strncpy(tmpdir,td,511) ;
      ltd = strlen(tmpdir) ;
      if( tmpdir[ltd-1] != '/' ) strcat(tmpdir,"/") ;
   }
}

/*---------------------------------------------------------------------
  Open an "http://" URL in host, port, and filename pieces.
  Wait up to msec milliseconds for network functions to occur.
  If an error occurs, return NULL, otherwise the caller can read
  from this NI_stream.
-----------------------------------------------------------------------*/

static NI_stream_type * open_URL_hpf( char *host, int port,
                                      char *file, int msec )
{
   NI_stream_type *ns ;
   char str[1024] ;
   int ii ;

   if( host == NULL || port <= 0 || file == NULL ) return NULL ;

   sprintf(str,"tcp:%s:%d",host,port) ;
   DMESS(" ++Opening %s",str);
   ns = NI_stream_open( str , "w" ) ;
   if( ns == NULL ){ FAILED; return NULL; }
   ii = NI_stream_writecheck( ns , msec ) ;
   if( ii <= 0 ){ FAILED; NI_stream_close(ns); return NULL; }

   DMESS(" ++GET %s",file);
   sprintf(str,"GET %s\n",file) ;                     /* HTTP 0.9 */
   ii = NI_stream_write( ns , str , strlen(str) ) ;
   if( ii <= 0 ){ FAILED; NI_stream_close(ns); return NULL; }

   ii = NI_stream_readcheck( ns , msec ) ;
   if( ii <= 0 ){ FAILED; NI_stream_close(ns); return NULL; }
   DMESS("%s"," **OPENED");
   return ns ;
}

/*----------------------------------------------------------------------
  Open an "http://" URL and prepare to read it (but the caller must
  actually do the reading).  If NULL is returned, an error occurred.
------------------------------------------------------------------------*/

#define HTTP     "http://"
#define HTTPLEN  7

#define FTP      "ftp://"
#define FTPLEN   6

static NI_stream_type * open_URL_http( char *url , int msec )
{
  char *s, *h , *file ;
  char hostname[512] ;
  int port;
  NI_stream_type *ns ;

  /* check inputs */

  if( url == NULL || strstr(url,HTTP) != url ) return NULL ;

  /* parse hostname */

  for( s=url+HTTPLEN , h=hostname ;
       (*s != '\0') && (*s != ':') && (*s != '/') ; s++ , h++ ) *h = *s ;

  *h = '\0' ; if( hostname[0] == '\0' ) return NULL ;

  /* parse port number if present */

  port = 0 ;
  if( *s == ':' ){ port = strtol( ++s , &h , 10 ) ; s = h ; }
  if( port <= 0 ) port = 80 ;

  /* get the file name (keep leading "/") */

  file = (*s == '/') ? s : "/" ;

  /* do the actual work */

  ns = open_URL_hpf( hostname , port , file , msec ) ;
  return ns ;
}

/*---------------------------------------------------------------
  Read an "http://" URL, with network waits of up to msec
  milliseconds allowed.  Returns number of bytes read -- if this
  is > 0, then *data will be a pointer to malloc-ed bytes holding
  the contents of the file.

  If the file is gzip-ed, then it will be un-gzip-ed before being
  loaded into memory.  This uses temporary files in $TMPDIR or
  /tmp, which must have space to hold the compressed and
  uncompressed file.  If the file is not compressed, then input
  is directly to memory and no temporary files are used.
-----------------------------------------------------------------*/

#define QBUF 4096

static int read_URL_http( char *url , int msec , char **data )
{
   NI_stream_type *ns ;
   char *buf=NULL , *cpt , qbuf[QBUF] , qname[256] ;
   int ii,jj , nall=0 , nuse ;
   int cflag , first ;
   FILE *cfile=NULL ;

   /* sanity check */

   if( url == NULL || data == NULL || msec < 0 ) return( -1 );

   /* open http channel to get url */

   ns = open_URL_http( url , msec ) ;
   if( ns == NULL ){ DMESS("%s","\n"); return( -1 ); }

   /* check if url will be returned gzip-ed */

   ii = strlen(url) ;
   if( ii > 3 ){
      cpt = url + (ii-3) ; cflag = (strcmp(cpt,".gz") == 0) ;
   } else {
      cflag = 0 ;
   }

   if( cflag ){
      setup_tmpdir() ;
      strcpy(qname,tmpdir) ; strcat(qname,"ElvisXXXXXX") ;
      mktemp(qname) ;
      if( qname[0] != '\0' ){
         strcat(qname,".gz") ; cfile = fopen( qname , "wb" ) ;
         if( cfile == NULL ) cflag == 0 ;
      } else {
         cflag = 0 ;
      }

      if( cflag == 0 ){
         DMESS(" **Temp file %s FAILS\n",qname); NI_stream_close(ns); return(-1);
      }
      DMESS(" ++Temp file=%s",qname);
   }

   /* read all of url */

   if( !cflag ){ buf = malloc( QBUF ) ; nall = QBUF ; }
   nuse = 0 ; first = 1 ;

   do{
      if(www_debug)fprintf(stderr,".");
      ii = NI_stream_readcheck( ns , msec ) ;  /* wait for data to be ready */
      if( ii <= 0 ) break ;                    /* quit if no data */
      ii = NI_stream_read( ns , qbuf , QBUF ) ;
      if( ii <= 0 ) break ;                  /* quit if no data */

      if( first ){                           /* check for "not found" */
         if( buf == NULL ){ buf = malloc(ii) ; }
         memcpy( buf , qbuf , ii ) ;
         for( jj=0 ; jj < ii ; jj++ ) buf[jj] = toupper(buf[jj]) ;
         buf[ii-1] = '\0' ;
         cpt = strstr(buf,"NOT FOUND") ;
         if( cpt != NULL ){
            if( cflag ){ fclose(cfile) ; unlink(qname) ; }
            DMESS("%s"," **NOT FOUND\n");
            free(buf) ; NI_stream_close(ns) ; return( -1 );
         }
         first = 0 ;
         if( cflag ){ free(buf) ; buf = NULL ; }
      }

      if( cflag ){                           /* write to temp file */
         nall = fwrite( qbuf , 1 , ii , cfile ) ;
         if( nall != ii ){                   /* write failed? */
            DMESS("\n** Write to temp file %s FAILED!\n",qname);
            fclose(cfile) ; unlink(qname) ;
            NI_stream_close(ns) ; return( -1 );
         }
      } else {                               /* save to buffer */
         if( nuse+ii > nall ){               /* enlarge buffer? */
            nall += QBUF ;
            buf   = realloc( buf , nall ) ;
         }
         memcpy( buf+nuse , qbuf , ii ) ;    /* copy data into buffer */
      }
      nuse += ii ;                           /* how many bytes so far */
   } while(1) ;
   NI_stream_close(ns) ;

   /* didn't get anything? */

   if( nuse <= 0 ){
      if( cflag ){ fclose(cfile) ; unlink(qname) ; }
      else       { free(buf) ; }
      FAILED; return(-1);
   }
   if(www_debug)fprintf(stderr,"!\n");

   /* uncompression time? */

   if( cflag ){
      fclose(cfile) ;
      sprintf( qbuf , "gzip -dq %s" , qname ) ;     /* execute gzip */
      ii = system(qbuf) ;
      if( ii != 0 ){ DMESS("%s"," **gzip failed!\n");
                     unlink(qname) ; return( -1 );   }  /* gzip failed  */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = NI_filesize( qname ) ;                 /* find how big */
      if( nuse <= 0 ){ DMESS("%s"," **gzip failed!\n");
                       unlink(qname) ; return( -1 );   }

      cfile = fopen( qname , "rb" ) ;
      if( cfile == NULL ){ DMESS("%s"," **gzip failed!\n");
                           unlink(qname) ; return( -1 );   }
      buf = malloc(nuse) ;
      fread( buf , 1 , nuse , cfile ) ;             /* read file in */
      fclose(cfile) ; unlink(qname) ;
   }

   /* data is in buf, nuse bytes of it */

   DMESS("%s","\n"); *data = buf ; return( nuse );
}

/*---------------------------------------------------------------------*/

static char ftp_name[512] = "anonymous" ;
static char ftp_pwd[512]  = "NIML@nowhere.org" ;

void NI_set_URL_ftp_ident( char *name , char *pwd )
{
   int ll ;

   if( name == NULL || pwd == NULL ) return ;

   ll = strlen(name) ; if( ll < 1 || ll > 511 ) return ;
   ll = strlen(pwd)  ; if( ll < 1 || ll > 511 ) return ;

   strcpy(ftp_name,name) ; strcpy(ftp_pwd,pwd) ; return ;
}

/*---------------------------------------------------------------------
  Reads an "ftp://" URL, similarly to read_URL_http above;
  however, staging is always done through a temporary file.
-----------------------------------------------------------------------*/

static int read_URL_ftp( char *url , char **data )
{
   char *s, *h , *file , qname[256] , sname[256] , *cpt , *buf ;
   char hostname[512] ;
   int port , ii , cflag , nuse ;
   FILE *sp ;

   /* sanity check */

   if( url == NULL || data == NULL || strstr(url,FTP) != url ) return( -1 );

   /* parse hostname */

   for( s=url+FTPLEN , h=hostname ;
        (*s != '\0') && (*s != ':') && (*s != '/') ; s++ , h++ ) *h = *s ;

   *h = '\0' ; if( hostname[0] == '\0' ) return( -1 );

   /* parse port number, if present */

   port = 0 ;
   if( *s == ':' ){ port = strtol( ++s , &h , 10 ) ; s = h ; }

   /* get the file name (strip off leading "/") */

   if( *s == '/' ){
      file = s+1 ; if( file[0] == '\0' ) return( -1 );
   } else {
                                         return( -1 );
   }

   /* check if file will be returned gzip-ed */

   ii = strlen(file) ;
   if( ii > 3 ){
      cpt = file + (ii-3) ; cflag = (strcmp(cpt,".gz") == 0) ;
   } else {
      cflag = 0 ;
   }

   /* make name for output file */

   setup_tmpdir() ;
   strcpy(qname,tmpdir) ; strcat(qname,"EthelXXXXXX") ;
   mktemp(qname) ;
   if( qname[0] == '\0' ) return( -1 );
   if( cflag ) strcat(qname,".gz") ;

   /* write the script file that will be used to run ftp */

   strcpy(sname,tmpdir) ; strcat(sname,"DahmerXXXXXX") ;
   mktemp(sname) ;             if( sname[0] == '\0' ) return( -1 );
   sp = fopen( sname , "w" ) ; if( sp == NULL )       return( -1 );

   fprintf( sp , "#!/bin/sh\n" ) ;
   fprintf( sp , "ftp -n << EEEEE &> /dev/null\n") ;
   if( port > 0 )
      fprintf( sp , "open %s %d\n" , hostname , port ) ;
   else
      fprintf( sp , "open %s\n" , hostname ) ;
   fprintf( sp , "user %s %s\n" , ftp_name, ftp_pwd ) ;
   fprintf( sp , "binary\n" ) ;
   fprintf( sp , "get %s %s\n" , file , qname ) ;
   fprintf( sp , "bye\n" ) ;
   fprintf( sp , "EEEEE\n" ) ;
   fprintf( sp , "exit\n" ) ;
   fclose( sp ) ;
   chmod( sname , S_IRUSR | S_IWUSR | S_IXUSR ) ;

   /* execute the script, then delete it */

   system( sname ) ; unlink( sname ) ;

   /* check the size of the output file */

   nuse = NI_filesize( qname ) ;
   if( nuse <= 0 ){ unlink(qname) ; return( -1 ); }

   /* uncompress the file, if needed */

   if( cflag ){
      sprintf( sname , "gzip -dq %s" , qname ) ;    /* execute gzip */
      ii = system(sname) ;
      if( ii != 0 ){ unlink(qname) ; return( -1 ); }  /* gzip failed  */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = NI_filesize( qname ) ;                 /* find how big */
      if( nuse <= 0 ){ unlink(qname) ; return( -1 ); }
   }

   /* suck the file into memory */

   sp = fopen( qname , "rb" ) ;
   if( sp == NULL ){ unlink(qname) ; return( -1 ); }
   buf = malloc(nuse) ; if( buf == NULL ){ unlink(qname) ; return( -1 ); }

   fread( buf , 1 , nuse , sp ) ;  /* AT LAST! */
   fclose(sp) ; unlink(qname) ;

   /* data is in buf, nuse bytes of it */

   *data = buf ; return( nuse );
}

/*-------------------------------------------------------------------
   Read a URL (ftp:// or http://) into memory.  The return value
   is the number of bytes read, and *data points to the data.
   If the return value is negative, then something bad happened.
---------------------------------------------------------------------*/

int NI_read_URL( char *url , char **data )
{
   int nn ;
   if( url == NULL || data == NULL ) return( -1 );

   if( getenv("NIML_WWW_DEBUG") != NULL ) www_debug = 1 ;

   if( strstr(url,HTTP) == url ){
      nn = read_URL_http( url , 4444 , data ) ; return(nn) ;
   }

   else if( strstr(url,FTP) == url ){
      nn = read_URL_ftp( url , data ) ; return(nn) ;
   }

   return( -1 );
}

/*-----------------------------------------------------------------*/
/*! Find a 'trailing name in a pathname.

   For example, for fname = "/bob/cox/is/the/author/of/NIML",
     - the lev=0 trailing name is "NIML",
     - the lev=1 trailing name is "of/NIML",
     - the lev=2 trailing name is "author/of/NIML", and so on.
   That is, "lev" is the number of directory names above the
   last name to keep.  The pointer returned is to some place
   in the middle of fname; that is, this is not a malloc()-ed
   string, so don't try to free() it!.
-------------------------------------------------------------------*/

static char * trailname( char *fname , int lev )
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

/*------------------------------------------------------------------
  Read a URL and save it to disk in tmpdir.  The filename
  it is saved in is returned in the malloc-ed space *tname.
  The byte count is the return value of the function;
  if <= 0, then an error transpired (and *tname is not set).
--------------------------------------------------------------------*/

int NI_read_URL_tmpdir( char *url , char **tname )
{
   int nn , ll ;
   char *data , *fname , *tt ;
   FILE *fp ;

   if( url == NULL || tname == NULL ) return( -1 );

   nn = NI_read_URL( url , &data ) ;  /* get the data into memory */
   if( nn <= 0 ) return( -1 );        /* bad */

   /* make the output filename */

   setup_tmpdir() ;
   fname = malloc(strlen(url)+strlen(tmpdir)+1) ;
   tt    = trailname(url,0) ;
   strcpy(fname,tmpdir) ; strcat(fname,tt) ; ll = strlen(fname) ;
   if( ll > 3 && strcmp(fname+(ll-3),".gz") == 0 ) fname[ll-3] = '\0' ;

   /* open and write output */

   fp = fopen( fname , "wb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"** Can't open temporary file %s\n",fname);
      free(data) ; return( -1 );
   }
   ll = fwrite(data,1,nn,fp) ; fclose(fp) ; free(data) ;
   if( ll != nn ){ unlink(fname); return( -1 ); } /* write failed */

   *tname = fname ; return( nn );
}

/*************************************************************************/
/************************ Functions for Base64 ***************************/
/********************* [adapted from thd_base64.c] ***********************/
/*************************************************************************/

static int  dtable_mode = -1 ;    /* 1=encode, 2=decode, -1=neither */
static byte dtable[256] ;         /* encode/decode table */
static int linelen = 72 ;         /* line length (max 76) */
static int ncrlf   = 1 ;

#define B64_goodchar(c) (dtable[c] != 0x80)  /* for decode only */

#define B64_EOL1 '\r'   /* CR */
#define B64_EOL2 '\n'   /* LF */

/*----------------------------------------------------------------------*/
/*! Set the number of characters to use for end of line:
    1 = Unix standard (LF only); 2 = DOS standard (CR LF).
------------------------------------------------------------------------*/

static void B64_set_crlf( int nn )
{
   if( nn >= 1 && nn <= 2 ) ncrlf = nn ;
   return ;
}

/*----------------------------------------------------------------------*/
/*! Set the length of a line of output in base64. */

void B64_set_linelen( int ll )
{
   if( ll >= 16 && ll <= 76 ) linelen = 4*(ll/4) ; /* multiple of 4 */
   else                       linelen = 72 ;
   return ;
}

/*----------------------------------------------------------------------*/
/*! Load the base64 encoding table. */

static void load_encode_table(void)
{
    int i ;
    if( dtable_mode == 1 ) return ;
    for (i = 0; i < 26; i++) {
        dtable[i] = 'A' + i;
        dtable[26 + i] = 'a' + i;
    }
    for (i = 0; i < 10; i++) dtable[52 + i] = '0' + i;
    dtable[62] = '+'; dtable[63] = '/'; dtable_mode = 1 ;
    return ;
}

/*----------------------------------------------------------------------*/
/*! Load the base64 decoding table. */

static void load_decode_table(void)
{
    int i;
    if( dtable_mode == 2 ) return ;
    for (i = 0  ; i < 255 ; i++) dtable[i] = 0x80;             /* bad */
    for (i = 'A'; i <= 'Z'; i++) dtable[i] =  0 + (i - 'A');
    for (i = 'a'; i <= 'z'; i++) dtable[i] = 26 + (i - 'a');
    for (i = '0'; i <= '9'; i++) dtable[i] = 52 + (i - '0');
    dtable['+'] = 62; dtable['/'] = 63; dtable['='] = 0; dtable_mode = 2 ;
    return ;
}

/*! Encode 3 bytes (a,b,c) into 4 bytes (w,x,y,z) */

#define B64_encode3(a,b,c,w,x,y,z)                 \
     ( w = dtable[(a)>>2]                      ,   \
       x = dtable[((a & 3) << 4) | (b >> 4)]   ,   \
       y = dtable[((b & 0xF) << 2) | (c >> 6)] ,   \
       z = dtable[c & 0x3F]                     )

/*! Encode 2 bytes (a,b) into 4 bytes (w,x,y,z) */

#define B64_encode2(a,b,w,x,y,z)                   \
     ( B64_encode3(a,b,0,w,x,y,z) , z = '=' )

/*! Encode 1 byte (a) into 4 bytes (w,x,y,z) */

#define B64_encode1(a,w,x,y,z)                     \
     ( B64_encode3(a,0,0,w,x,y,z) , y=z = '=' )

/*! Decode 4 bytes (w,x,y,z) into 3 bytes (a,b,c) */

#define B64_decode4(w,x,y,z,a,b,c)                 \
     ( a = (dtable[w] << 2) | (dtable[x] >> 4) ,   \
       b = (dtable[x] << 4) | (dtable[y] >> 2) ,   \
       c = (dtable[y] << 6) | dtable[z]         )

/*! Determine how many output bytes are encoded in a quad (w,x,y,z) */

#define B64_decode_count(w,x,y,z)                  \
     ( ((w)=='='||(x)=='=') ? 0                    \
                            : ((y)=='=') ? 1       \
                            : ((z)=='=') ? 2 : 3 )

/*----------------------------------------------------------------------*/
/*! Convert base64 encoding to a binary array

   Inputs: nb64 = number of bytes in b64
            b64 = array of base64 encoding bytes
                  values not in the base64 encoding set will be skipped

   Outputs: *nbin = number of binary bytes [*nbin==0 flags an error]
             *bin = pointer to newly malloc()-ed space with bytes
------------------------------------------------------------------------*/

void B64_to_binary( int nb64 , byte * b64 , int * nbin , byte ** bin )
{
   int ii,jj , nn ;
   byte a,b,c , w,x,y,z ;

   /*- sanity checks -*/

   if( nbin == NULL || bin == NULL ) return ;

   if( nb64 < 4 || b64 == NULL ){ *nbin = 0 ; *bin = NULL ; return ; }

   *bin = (byte *) malloc(sizeof(byte)*(2+3*nb64/4)) ;
   if( *bin == NULL ){ *nbin = 0 ; return ; }

   /*- some work -*/

   load_decode_table() ;
   for( ii=jj=0 ; ii < nb64 ; ){  /* scan inputs, skipping bad characters */
      w = b64[ii++] ;
      while( !B64_goodchar(w) && ii < nb64 ) w = b64[ii++] ;
      x = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(x) && ii < nb64 ) x = b64[ii++] ;
      y = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(y) && ii < nb64 ) y = b64[ii++] ;
      z = (ii < nb64) ? b64[ii++] : '=' ;
      while( !B64_goodchar(z) && ii < nb64 ) z = b64[ii++] ;

      B64_decode4(w,x,y,z,a,b,c) ;

      if( z == '=' ){                        /* got to the end? */
         nn = B64_decode_count(w,x,y,z) ;    /* see how many to save */
         if( nn > 0 ) (*bin)[jj++] = a ;
         if( nn > 1 ) (*bin)[jj++] = b ;
         break ;
      }

      /* not at the end => save all 3 outputs */

      (*bin)[jj++] = a ; (*bin)[jj++] = b ; (*bin)[jj++] = c ;
   }

   *bin  = (byte *) realloc( *bin , sizeof(byte)*jj ) ;
   *nbin = jj ;
   return ;
}

/*----------------------------------------------------------------------*/
/*! Convert binary array to base64 encoding

   Inputs: nbin = number of bytes in bin
            bin = array of binary bytes to encode

   Outputs: *nb64 = number of base64 bytes [*nb64==0 flags an error]
             *b64 = pointer to newly malloc()-ed space with bytes

   The output array (*b64) line length can be set by
      B64_set_linelen(n)
   where n is from 16 to 76.  The default is 72.  Note, however, that
   encoded bytes will always be written out in groups of 4.
   The output array line separator can be the LF character only (Unix)
   or the CR-LF combination (DOS, etc.).  This is controlled by
      B64_set_crlf(n)
   where n=1 for LF, n=2 for CR-LF.  The default is LF.  The output
   array will be terminated with a line separator.  There will be
   no ASCII NUL character at the end of *b64 -- that is, the output
   is not a C string.
------------------------------------------------------------------------*/

void B64_to_base64( int nbin , byte * bin , int * nb64 , byte ** b64 )
{
   int ii,jj , nn,n3 ;
   byte a,b,c , w,x,y,z ;

   /*- sanity checks -*/

   if( nb64 == NULL || b64 == NULL ) return ;
   if( nbin <= 0    || bin == NULL ){ *nb64 = 0 ; *b64 = NULL ; return ; }

   nn   = (4.0*(linelen+ncrlf+1.0)/(3.0*linelen))*nbin + 256 ;
   *b64 = (byte *) malloc(sizeof(byte)*nn) ;
   if( *b64 == NULL ){ *nb64 = 0 ; return ; }

   /*- do blocks of 3 -*/

   load_encode_table() ;
   n3 = (nbin/3)*3 ;
   for( nn=jj=ii=0 ; ii < n3 ; ){
      a = bin[ii++] ; b = bin[ii++] ; c = bin[ii++] ;
      B64_encode3(a,b,c,w,x,y,z) ;
      (*b64)[jj++] = w ;
      (*b64)[jj++] = x ;
      (*b64)[jj++] = y ;
      (*b64)[jj++] = z ;
      nn += 4 ; if( nn >= linelen ){
                   if( ncrlf == 2 ) (*b64)[jj++] = B64_EOL1 ;
                   (*b64)[jj++] = B64_EOL2 ;
                   nn = 0 ;
                }
   }

   /*- do the leftovers, if any (1 or 2 bytes) -*/

   if( ii < nbin ){
      if( ii == nbin-2 )
         B64_encode2(bin[ii],bin[ii+1],w,x,y,z) ;
      else
         B64_encode1(bin[ii],w,x,y,z) ;

      (*b64)[jj++] = w ;
      (*b64)[jj++] = x ;
      (*b64)[jj++] = y ;
      (*b64)[jj++] = z ; nn += 4 ;
   }

   if( nn > 0 ){
      if( ncrlf == 2 ) (*b64)[jj++] = B64_EOL1 ;
      (*b64)[jj++] = B64_EOL2 ;
   }

   *b64  = (byte *) realloc( *b64 , sizeof(byte)*jj ) ;
   *nb64 = jj ;
   return ;
}

/**********************************************************************/
/*************** Stuff for MD5 hashing [from thd_md5.c] ***************/
/**********************************************************************/

/**********************************************************************
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All    *
 * rights reserved.                                                   *
 *                                                                    *
 * License to copy and use this software is granted provided that it  *
 * is identified as the "RSA Data Security, Inc. MD5 Message-Digest   *
 * Algorithm" in all material mentioning or referencing this software *
 * or this function.                                                  *
 *                                                                    *
 * License is also granted to make and use derivative works provided  *
 * that such works are identified as "derived from the RSA Data       *
 * Security, Inc. MD5 Message-Digest Algorithm" in all material       *
 * mentioning or referencing the derived work.                        *
 *                                                                    *
 * RSA Data Security, Inc. makes no representations concerning either *
 * the merchantability of this software or the suitability of this    *
 * software for any particular purpose. It is provided "as is"        *
 * without express or implied warranty of any kind.                   *
 *                                                                    *
 * These notices must be retained in any copies of any part of this   *
 * documentation and/or software.                                     *
 **********************************************************************/

/*======= Modified by RWCox for inclusion in the NIML package ========*/
/*------- These changes are released to the public domain     --------*/

typedef unsigned char *POINTER;   /* POINTER defines a generic pointer type */
typedef unsigned short int UINT2; /* UINT2 defines a two byte word */
typedef unsigned long int UINT4;  /* UINT4 defines a four byte word */

/* MD5 context data type */

typedef struct {
  UINT4 state[4];                                        /* state (ABCD) */
  UINT4 count[2];             /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];                              /* input buffer */
} MD5_CTX;

/* prototypes for some internal functions */

static void MD5Init (MD5_CTX *);
static void MD5Update (MD5_CTX *, unsigned char *, unsigned int);
static void MD5Final (unsigned char [16], MD5_CTX *);

static void MD5Transform (UINT4 [4], unsigned char [64]);
static void Encode (unsigned char *, UINT4 *, unsigned int);
static void Decode (UINT4 *, unsigned char *, unsigned int);

/* Constants for MD5Transform routine.  */

#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* F, G, H and I are basic MD5 functions.  */

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

/* ROTATE_LEFT rotates x left n bits.  */

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))

/* FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
   Rotation is separate from addition to prevent recomputation.  */

#define FF(a, b, c, d, x, s, ac) { \
 (a) += F ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#define GG(a, b, c, d, x, s, ac) { \
 (a) += G ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#define HH(a, b, c, d, x, s, ac) { \
 (a) += H ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

#define II(a, b, c, d, x, s, ac) { \
 (a) += I ((b), (c), (d)) + (x) + (UINT4)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

/*----------------------------------------------------------------------*/
/*! MD5 initialization. Begins an MD5 operation, writing a new context.
------------------------------------------------------------------------*/

static void MD5Init (MD5_CTX * context)
{
  context->count[0] = context->count[1] = 0;

  /* Load magic initialization constants */

  context->state[0] = 0x67452301;
  context->state[1] = 0xefcdab89;
  context->state[2] = 0x98badcfe;
  context->state[3] = 0x10325476;
}

/*----------------------------------------------------------------------*/
/*! MD5 block update operation. Continues an MD5 message-digest
   operation, processing another message block, and updating the
   context.
------------------------------------------------------------------------*/

static void MD5Update (MD5_CTX * context, unsigned char * input,
                                          unsigned int inputLen  )
{
  unsigned int i, index, partLen;

  /* Compute number of bytes mod 64 */

  index = (unsigned int)((context->count[0] >> 3) & 0x3F);

  /* Update number of bits */

  if( (context->count[0] += ((UINT4)inputLen << 3)) < ((UINT4)inputLen << 3) )
    context->count[1]++;

  context->count[1] += ((UINT4)inputLen >> 29);

  partLen = 64 - index;

  /* Transform as many times as possible.  */

  if (inputLen >= partLen) {

   memcpy ((POINTER)&context->buffer[index], (POINTER)input, partLen);

   MD5Transform (context->state, context->buffer);

   for (i = partLen; i + 63 < inputLen; i += 64)
     MD5Transform (context->state, &input[i]);

   index = 0;
  }
  else
   i = 0;

  /* Buffer remaining input */

  memcpy ((POINTER)&context->buffer[index], (POINTER)&input[i],
          inputLen-i);
}

/*----------------------------------------------------------------------*/
/*! MD5 finalization. Ends an MD5 message-digest operation, writing the
   the message digest and zeroizing the context.
------------------------------------------------------------------------*/

static void MD5Final (unsigned char digest[16], MD5_CTX * context)
{
  unsigned char bits[8];
  unsigned int index, padLen;

  /* Save number of bits */

  Encode (bits, context->count, 8);

  /* Pad out to 56 mod 64.  */

  index = (unsigned int)((context->count[0] >> 3) & 0x3f);
  padLen = (index < 56) ? (56 - index) : (120 - index);
  MD5Update (context, PADDING, padLen);

  /* Append length (before padding) */

  MD5Update (context, bits, 8);

  /* Store state in digest */

  Encode (digest, context->state, 16);

  /* Zeroize sensitive information. */

  memset ((POINTER)context, 0, sizeof (*context));
}

/*----------------------------------------------------------------------*/
/*! MD5 basic transformation. Transforms state based on block.
------------------------------------------------------------------------*/

static void MD5Transform (UINT4 state[4], unsigned char block[64])
{
  UINT4 a = state[0], b = state[1], c = state[2], d = state[3], x[16];

  Decode (x, block, 64);

  /* Round 1 */

  FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
  FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
  FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
  FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
  FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
  FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
  FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
  FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
  FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
  FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
  FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
  FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
  FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
  FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
  FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
  FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

  /* Round 2 */

  GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
  GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
  GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
  GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
  GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
  GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
  GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
  GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
  GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
  GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
  GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
  GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
  GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
  GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
  GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
  GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

  /* Round 3 */

  HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
  HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
  HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
  HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
  HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
  HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
  HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
  HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
  HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
  HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
  HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
  HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
  HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
  HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
  HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
  HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

  /* Round 4 */

  II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
  II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
  II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
  II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
  II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
  II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
  II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
  II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
  II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
  II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
  II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
  II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
  II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
  II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
  II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
  II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

  state[0] += a;
  state[1] += b;
  state[2] += c;
  state[3] += d;

  /* Zeroize sensitive information. */

  memset ((POINTER)x, 0, sizeof (x));
}

/*----------------------------------------------------------------------*/
/*! Encodes input (UINT4) into output (unsigned char). Assumes len is
   a multiple of 4.
------------------------------------------------------------------------*/

static void Encode (unsigned char *output, UINT4 *input, unsigned int len)
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4) {
    output[j] = (unsigned char)(input[i] & 0xff);
    output[j+1] = (unsigned char)((input[i] >> 8) & 0xff);
    output[j+2] = (unsigned char)((input[i] >> 16) & 0xff);
    output[j+3] = (unsigned char)((input[i] >> 24) & 0xff);
  }
}

/*----------------------------------------------------------------------*/
/*! Decodes input (unsigned char) into output (UINT4). Assumes len is
   a multiple of 4.
------------------------------------------------------------------------*/

static void Decode (UINT4 *output, unsigned char *input, unsigned int len)
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4)
    output[i] = ((UINT4)input[j])          | (((UINT4)input[j+1]) << 8) |
               (((UINT4)input[j+2]) << 16) | (((UINT4)input[j+3]) << 24) ;
}

/*======================================================================
   The stuff below is some interface routines, by RWCox
========================================================================*/

/*----------------------------------------------------------------------*/
/*! Function to print a 128 bit digest into a static 32 char string.
------------------------------------------------------------------------*/

static char * MD5_static_printf( unsigned char digest[16] )
{
  static char st[33] ;

  sprintf(st,
          "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x" ,
          digest[0] , digest[1] , digest[2] , digest[3] , digest[4] ,
          digest[5] , digest[6] , digest[7] , digest[8] , digest[9] ,
          digest[10], digest[11], digest[12], digest[13], digest[14],
          digest[15]
         ) ;

  return st ;
}

/*----------------------------------------------------------------------*/
/*! Digest an array and returns the printable string of the result,
    stored in a static array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_array( int n , char * bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_static_printf(digest) ;
}

/*----------------------------------------------------------------------*/
/*! Digest an array and returns the printable string of the result,
    stored in a malloc()-ed array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_array( int n , char * bytes )
{
   char *st , *dy ;
   st = MD5_static_array( n , bytes ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*----------------------------------------------------------------------*/
/*! Digest a C string and returns the printable string of the result,
    stored in a static array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_string( char * string )
{
   if( string == NULL ) return NULL ;
   return MD5_static_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------*/
/*! Digest a C string and returns the printable string of the result,
    stored in a malloc()-ed array (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_string( char * string )
{
   if( string == NULL ) return NULL ;
   return MD5_malloc_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------*/
/*! Digests a file and prints the result, stored in a static array
    (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_static_file(char * filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file = fopen (filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( len = fread(buffer, 1, 1024, file) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_static_printf( digest ) ;
}

/*----------------------------------------------------------------------*/
/*! Digests a file and prints the result, stored in a malloc()-ed array
    (length=32+1 bytes).
------------------------------------------------------------------------*/

char * MD5_malloc_file(char * filename)
{
   char *st , *dy ;

   st = MD5_static_file( filename ) ;
   if( st == NULL ) return NULL ;
   dy = (char *) malloc(33) ; strcpy(dy,st) ; return dy ;
}

/*----------------------------------------------------------------------------*/
/*! Convert a MD5 hex string to a Base64-ed string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

static char * MD5_to_B64( unsigned char digest[16] )
{
   int nb64=0 ; byte *b64=NULL ;

   B64_to_base64( 16 , (char *)digest , &nb64 , &b64 ) ;  /* thd_base64.c */
   if( nb64 <= 0 || b64 == NULL ) return NULL ;
   b64[nb64-3] = '\0' ;                           /* remove trailing "==" */
   return (char *)b64 ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of an array as a Base64 string, instead of a hex
    string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_array( int n , char * bytes )
{
   MD5_CTX context;
   unsigned char digest[16];

   if( n < 0 || bytes == NULL ) return NULL ;

   MD5Init( &context ) ;
   MD5Update( &context, bytes, n ) ;
   MD5Final( digest, &context ) ;

   return MD5_to_B64( digest ) ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of a C string as a Base64 string, instead of a hex
    string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_string( char * string )
{
   if( string == NULL ) return NULL ;
   return MD5_B64_array( strlen(string) , string ) ;
}

/*----------------------------------------------------------------------------*/
/*! Return the MD5 hash of a file as a Base64 string, instead of a hex
    string.
    * strlen(result) is 22 instead of 32
    * result is malloc()-ed and should be free()-d when appropriate
------------------------------------------------------------------------------*/

char * MD5_B64_file(char * filename)
{
  FILE *file;
  MD5_CTX context;
  int len;
  unsigned char buffer[1024] ;
  unsigned char digest[16] ;

  if( (file = fopen (filename, "rb")) == NULL ) return NULL ;

  MD5Init( &context ) ;

  while( len = fread(buffer, 1, 1024, file) )
      MD5Update( &context, buffer, len ) ;

  MD5Final( digest, &context );
  fclose (file);

  return MD5_to_B64( digest ) ;
}

/*-----------------------------------------------------------------------*/

#include <sys/utsname.h>  /* These 4 files are needed by */
#include <sys/time.h>     /* the UNIQ_ functions below.  */
#include <unistd.h>
#include <ctype.h>

/*-----------------------------------------------------------------------*/
/*! Return a globally unique identifier (I hope).  This is a malloc()-ed
  string of length <= 31 (plus the NUL byte; the whole thing will fit
  into a char[32] array).  The output does not contain any '/'s, so
  it could be used as a temporary filename.
  Method: generate a string from the system identfier information and
          the current time of day; MD5 hash this to a 128 byte code;
          Base64 encode this to a 22 byte string; replace '/' with '-'
          and '+' with '_'; add 4 character prefix (1st 3 characters
          of environment variable IDCODE_PREFIX plus '_').
-------------------------------------------------------------------------*/

char * UNIQ_idcode(void)
{
   struct utsname ubuf ;
   struct timeval tv ;
   int    nn , ii ;
   int  nbuf ;
   char *buf , *idc , *eee ;
   static int ncall=0 ;                /* number of times I've been called */

   /* get info about this system */

   nn = uname( &ubuf ) ;               /* get info about this system */
   if( nn == -1 ){                     /* should never happen */
      strcpy( ubuf.nodename , "E" ) ;
      strcpy( ubuf.sysname  , "L" ) ;
      strcpy( ubuf.release  , "V" ) ;
      strcpy( ubuf.version  , "I" ) ;
      strcpy( ubuf.machine  , "S" ) ;
   }

   /* store system info into a string buffer */

   nbuf = strlen(ubuf.nodename)+strlen(ubuf.sysname)
         +strlen(ubuf.release )+strlen(ubuf.version)+strlen(ubuf.machine) ;

   buf = malloc(nbuf+64) ;      /* include some extra space */
   strcpy(buf,ubuf.nodename) ;
   strcat(buf,ubuf.sysname ) ;
   strcat(buf,ubuf.release ) ;
   strcat(buf,ubuf.version ) ;
   strcat(buf,ubuf.machine ) ;

   idc = calloc(1,32) ;         /* will be output string */

   /* get time and store into buf (along with process id and ncall) */

   nn = gettimeofday( &tv , NULL ) ;
   if( nn == -1 ){              /* should never happen */
      tv.tv_sec  = (long) buf ;
      tv.tv_usec = (long) idc ;
   }

   /* even if called twice in rapid succession,
      at least ncall will differ, so we'll get different ID codes  */

   sprintf(buf+nbuf,"%d%d%d%d",
          (int)tv.tv_sec,(int)tv.tv_usec,(int)getpid(),ncall) ;
   ncall++ ;

   /* get prefix for idcode from environment, if present */

   eee = getenv("IDCODE_PREFIX") ;
   if( eee != NULL && isalpha(eee[0]) ){
     for( ii=0 ; ii < 3 && isalnum(eee[ii]) ; ii++ )
       idc[ii] = eee[ii] ;
   } else {
     strcpy(idc,"XYZ") ;  /* innocent default prefix */
   }
   strcat(idc,"_") ;  /* recall idc was calloc()-ed */

   /* MD5+Base64 encode buf to be latter part of the idcode */

   eee = MD5_B64_string( buf ) ;
   if( eee != NULL ){                     /* should always work */
      nn = strlen(eee) ;
      for( ii=0 ; ii < nn ; ii++ ){
              if( eee[ii] == '/' ) eee[ii] = '-' ;  /* / -> - */
         else if( eee[ii] == '+' ) eee[ii] = '_' ;  /* + -> _ */
      }
      strcat(idc,eee) ; free(eee) ;
   } else {                               /* should never happen */
     nn = strlen(idc) ;
     sprintf(idc+nn,"%d_%d_%d",(int)tv.tv_sec,(int)tv.tv_usec,ncall) ;
   }

   /* free workspace and get outta here */

   free(buf) ; return idc ;
}

/*----------------------------------------------------------------------*/
/*! Fill a user-supplied buffer (length at least 32) with an idcode.
------------------------------------------------------------------------*/

void UNIQ_idcode_fill( char *idc )
{
   char *bbb ;
   if( idc == NULL ) return ;
   bbb = UNIQ_idcode() ;
   strcpy(idc,bbb) ; free(bbb) ; return ;
}
